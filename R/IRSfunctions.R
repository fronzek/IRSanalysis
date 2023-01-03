###################
#' Define colour scheme.
#'
#' \code{getColors} returns a vector with colours.
#'
#' @param n Number of colour classes, used if levels are NA.
#' @param levels Levels defining the classes, if NA.
#' @param colSc Colour scheme: [normal|normal_reversed|normalwhite|sharp|sharp100|sharp_redgreenblue|sharp_cyanbrown|any of the RColorBrewer color palettes|vector of colours]; defaults to normal which uses topo.colors(); sharp* requires levels to be set that cross 0 (100); if colSc is not any of the pre-defined strings for colour schemes, colSc is returned and assumed to be a vector that defines colours
#' @return Vector of colours.
#' @export
getColors <- function(n, levels=NA, colSc="normal") {
	requireNamespace("RColorBrewer")	# defines a number of color palettes
	if(!is.na(levels[1]))
		n <- length(levels)
	if(colSc[1]=="sharp")
		return(c(colorRampPalette(c("darkred","red","pink"))(sum(levels<=0)-1), colorRampPalette(c("lightgreen","green","darkgreen"))(length(levels)-sum(levels<=0)+1)))
	if(colSc[1]=="sharp_redgreenblue")
		return(c(colorRampPalette(c("darkred","red","pink"))(sum(levels<=0)-1), colorRampPalette(c("lightgreen","lightblue","blue","darkblue"))(length(levels)-sum(levels<=0)+1)))
	if(colSc[1]=="BuYlRd")
		return(c(colorRampPalette(c("#4575B4","#E0F3F8"))(sum(levels<=0)-1), colorRampPalette(c("#FEE090","#D73027"))(length(levels)-sum(levels<=0)+1)))
	else if(colSc[1]=="sharp100")
		return(c(colorRampPalette(c("LemonChiffon","Yellow","Orange"))(sum(levels<=100)-1), colorRampPalette(c("lightblue","blue","Darkblue"))(length(levels)-sum(levels<=100)+1)))
	if(colSc[1]=="sharp_cyanbrown")
		return(c(colorRampPalette(c("#8c510a","#f6e8c3"))(sum(levels<=0)-1), colorRampPalette(c("#c7eae5","#01665e"))(length(levels)-sum(levels<=0)+1)))
	else if(sum(row.names(brewer.pal.info)==colSc[1])==1) {	# check if colSc is defined in RColorBrewer
		return(brewer.pal(n, colSc))	# throws a warning if n is larger than max no of colours
	} else if(colSc[1]=="normal_reversed")
		return(topo.colors(n=n)[n:1])
	else if(colSc[1]=="normalwhite")
		return(c("white",topo.colors(n=n)[2:n]))
	else if(colSc[1]=="normal")
		return(topo.colors(n=n))
	else
		return(colSc)
#return(c(colorRampPalette(c("#01665e","#c7eae5"))(sum(levels<=0)-1), colorRampPalette(c("#f6e8c3","#8c510a"))(length(levels)-sum(levels<=0)+1)))
}


#' Plot a single IRS without axes titles, wrapper for \code{\link{plotIRS}}
#'
#' \code{plotIRSnoaxes} no return values.
#'
#' @param ... Arguments passed over to \code{\link{plotIRS}}.
#' @return Vector of the levels used for the plot.
#' @export
plotIRSnoaxes <- function(...) {
	return(plotIRS(..., xaxt="n", yaxt="n"))
}

#required function from www.menugget.blogspot.com (also see sinkr package)
matrix.poly <- function(x, y, z=mat, n=NULL){
 if(missing(z)) stop("Must define matrix 'z'")
 if(missing(n)) stop("Must define at least 1 grid location 'n'")
 if(missing(x)) x <- seq(0,1,,dim(z)[1])
 if(missing(y)) y <- seq(0,1,,dim(z)[2])
 poly <- vector(mode="list", length(n))
 for(i in seq(length(n))){
 #for(i in seq(n)){	# bug corrected, see http://menugget.blogspot.fi/2012/04/create-polygons-from-matrix.html
  ROW <- ((n[i]-1) %% dim(z)[1]) +1
	#print(paste("ROW:",ROW, "dim(z)[1]:", dim(z)[1], "n[i]:", n[i],"i:",i))
  COL <- ((n[i]-1) %/% dim(z)[1]) +1

  dist.left <- (x[ROW]-x[ROW-1])/2
  dist.right <- (x[ROW+1]-x[ROW])/2
  if(ROW==1) dist.left <- dist.right
  if(ROW==dim(z)[1]) dist.right <- dist.left

  dist.down <- (y[COL]-y[COL-1])/2
  dist.up <- (y[COL+1]-y[COL])/2
  if(COL==1) dist.down <- dist.up
  if(COL==dim(z)[2]) dist.up <- dist.down

  xs <- c(x[ROW]-dist.left, x[ROW]-dist.left, x[ROW]+dist.right, x[ROW]+dist.right)
  ys <- c(y[COL]-dist.down, y[COL]+dist.up, y[COL]+dist.up, y[COL]-dist.down)
  poly[[i]] <- data.frame(x=xs, y=ys)
 }
 return(poly)
}

# ToDo: . some of the args could be treated as "..." as they are just handed over to plot
#       . the loop to convert the table to a matrix could be replaced by more efficient data.table code using cast
#       . if colSc is a color vector, check that the lenght is one less than levels

#' Plot a single IRS.
#'
#' \code{plotIRS} no return values.
#'
#' @param dat data.table object that contains three columns for x-, y- and z-dimension
#' @param xvar Column name for the x-axis
#' @param yvar Column name for the y-axis
#' @param varn Column name for the z-axis
#' @param xlab x-axis label text
#' @param ylab y-axis label text
#' @param add if FALSE, starts a new plot, if TRUE, plots ontop on the current device
#' @param col colour of contour lines, if NA, no contours are plotted
#' @param lty line type of contour lines
#' @param lwd line width of contour lines
#' @param title plot title
#' @param levels levels for contour lines
#' @param cex font size
#' @param labcex font size for contour labels
#' @param colSc colour scheme defined in \code{\link{getColors}} or NULL if no filled contours should be plot
#' @param xlim Ranges of the x- and y-axes to plot (defaults to the range specified in dat$xvar and dat$yvar)
#' @param ylim Ranges of the x- and y-axes to plot (defaults to the range specified in dat$xvar and dat$yvar)
#' @param legend if true adds a legend to the plot
#' @param contours if true plots data as contours; if false, plots the data as a grid; can also be used with transparent colour plotted on top of filled contour plots,	e.g. \code{plotIRSnoaxes(dat=myDat, var="Z5", colSc=rgb(0,0,0, alpha=.2), levels=c(.5,1.5), contours=FALSE, add=TRUE)}
#' @param stippled if contours==FALSE, plot selected grid cells as stippled or hatched area
#' @return Vector of the levels used for the plot.
#' @export
plotIRS <- function(dat, xvar="deltaT", yvar="Pchange", xlab=expression(paste(plain("Temperature change ("),degree,plain("C)"))), ylab="Precipitation change (%)", var="grain_DM", add=FALSE, col="black", lty=1, lwd=1, title=NA, levels=NULL, cex=1, labcex=0.6, drawlabels=TRUE, colSc="normal", xlim=NULL, ylim=NULL, xaxt="s", yaxt="s", legend=FALSE, contours=TRUE, stippled=TRUE) {
	#require(data.table)
	print("Test2")
	# create a local copy of the data with common column names
	#!datlocal <- dat[,mget(c(xvar, yvar, var)]
	# the following line doesn't work as there seems to be a naming conflict with base::get
	datlocal <- dat[,list(xvar=get(xvar, "package:IRSanalysis"), yvar=get(yvar, "package:IRSanalysis"), var=get(var, "package:IRSanalysis"))]
	#datlocal <- dat[,list(xvar=get(xvar), yvar=get(yvar), var=get(var))]
	# make sure data are orderd by deltaT and Pchange
	setkey(datlocal,xvar,yvar)
	if(is.null(xlim))
		xlim <- datlocal[,range(xvar)]
	if(is.null(ylim))
		ylim <- datlocal[,range(yvar)]
	if(!add) {
	## empty plot ##
		plot(-99,-99,cex=cex,col="blue" , xlab=xlab, ylab=ylab, main=title, xlim=xlim, ylim=ylim, xaxt=xaxt, yaxt=yaxt)
	add<-TRUE
	}

	# create a matrix with the values to be plotted (contour() requires a matrix)
	# get the dimensions
	deltaTs <- datlocal[,unique(xvar)]; pchs <- datlocal[,unique(yvar)]
	# create empty matrix
	mat<-matrix(NA,nrow=length(deltaTs),ncol=length(pchs))
	# loop through the deltaT and Pchange values and copy values to the matrix
	for(t in deltaTs) {
		for(p in pchs) {
			nt<-(1:length(deltaTs))[deltaTs==t]
			np<-(1:length(pchs))[pchs==p]
			#mat[nt,np]<-dat[deltaT==t&Pchange==p,grain_DM]
			# some files have doublicates values
			val <- unlist(subset(datlocal,xvar==t&yvar==p)[,var])
			if(length(val)==1)
				mat[nt,np]<-as.numeric(val)
			if(length(val)>1) {
				if(length(val)>2)
					warning(paste(length(val),"dublicate values"))
				if(!is.na(val[1]==val[2])) {
				if(val[1]==val[2]) {
					warning("Dublicate, but identical values, which are assumed to be correct.")
					mat[nt,np]<-as.numeric(val[1])
				} else {
					warning(paste("Dublicate, NOT identical values (",val[1]," and ",val[2],") at ",t,"/",p,", using the first."))
					mat[nt,np]<-as.numeric(val[1])
				}
				} else {
					warning("Dublicate NA values.")
					mat[nt,np]<-as.numeric(val[1])
				}
			}
		}
	}

	# only plot if matrix does not only consist of NA values
	if(sum(is.na(mat))<length(mat))
		if(length(unique(as.vector(mat)))==1)
			print("All equal")
	if(is.null(levels))
		levels <- seq(min(mat),max(mat),length.out=8)
	if(is.unsorted(levels, strictly=TRUE)) {
		#print(paste("levels:",levels))
		warning("levels not increasing, leaving plotIRS() with empty plot")
		return(c(0,1))
	}
	# if values are outside the range given for the levels, add the outer limit to include it/extend the largest value
	if(contours) {	# don't do this for image plots that may want to have some areas blanked out
		if(max(mat, na.rm=TRUE)>max(levels)) {
			levels[length(levels)] <- ceiling(max(mat, na.rm=TRUE))+1
			#levels <- c(levels,ceiling(max(mat))+1)
		}
		#else # otherwise just add a dummy value
			#levels <- c(levels,max(levels)+1)
		if(min(mat, na.rm=TRUE)<min(levels))
			levels <- c(floor(levels[1])-1,levels[2:length(levels)])
	}
	# filled contours if color scheme is not null
	# filled.contour() always prepares a new plot with axes and legend, call the internal function instead
	if(!is.null(colSc) & contours)
		.filled.contour(datlocal[,unique(xvar)],datlocal[,unique(yvar)],mat, col=getColors(levels=levels,colSc=colSc), levels=levels)
	if(!contours) {
		if(stippled) {
			# hatching of selected grid cells
			# solution from https://stackoverflow.com/questions/11736996/adding-stippling-to-image-contour-plot
			# this doesn't work if there is only a single cell selected in the centre:
			# > mymat <- matrix(c(0,0,0, 0,1,0, 0,0,0),nrow=3)
			# > matrix.poly(c(1,2,3), c(1,2,3), mymat, which(testmat==1))
			#  Error in if (ROW == 1) dist.left <- dist.right : missing value where TRUE/FALSE needed
			polys <- matrix.poly(datlocal[,unique(xvar)],datlocal[,unique(yvar)],mat,which(mat==1))
			for(i in seq(polys)) {
    				polygon(polys[[i]], density=20, angle=45, border=NA, col="gray50")
    				#polygon(polys[[i]], density=20, angle=-45, border=NA, col="blue")
			}
			# alternative attempts
			if(0) {
			# plot gridded data without interpolating to contour lines
			pts <- expand.grid(x = datlocal[,unique(xvar)], y = datlocal[,unique(yvar)])
			pts$over <- as.vector(mat)
			pts$over <- (pts$over <= 0)
			print(pts$over)
			points(pts$x[pts$over], pts$y[pts$over], cex = 0.6, col="black", pch=16)
			}
			if(0) {
			# hatching of an area defined by a contour line (doesn't fully work yet)
			cl <- contourLines(datlocal[,unique(xvar)],datlocal[,unique(yvar)],mat, levels=levels)
			xs <- cl[[1]]$x; ys <- cl[[1]]$y # this only takes the first level if there are more
			#print(xs); print(ys)
			if(!11 %in% xs) { xs <- c(xs, 11); ys <- c(ys, 40) }
			if(!-1 %in% xs) { xs <- c(-1, xs); ys <- c(40, ys) }
			polygon(c(xs,11,-1),c(ys,-60,-60), col="gray", density=30, lty=3, border=1)
			}
		} else {
			image(datlocal[,unique(xvar)],datlocal[,unique(yvar)],mat,
				col=getColors(levels=levels,colSc=colSc), breaks=levels, add=TRUE)
		}
	}
	# 0-lines
	lines(x=c(0,0),y=extendrange(ylim, f=1),col="grey")
	lines(y=c(0,0),x=extendrange(xlim, f=1),col="grey")
	if(contours & !is.na(col)) {
		contour(datlocal[,unique(xvar)],datlocal[,unique(yvar)],mat, col=col, lty=lty, lwd=lwd, add=add, method="flattest", levels=levels, labcex=labcex, drawlabels=drawlabels)
	}
	# add legend to the right of the plot
	if(legend)
		addLegend(colSc=colSc, levels=levels, add=TRUE,
			pos=c(extendrange(xlim,f=.02)[2], ylim[1], extendrange(xlim,f=.05)[2], ylim[2]))
	return(levels)
}

######
#' Plot a legend
#'
#' \code{addLegend} no return values.
#'
#' @param colSc string defining the colour scheme
#' @param txt text to print on top of the legend, e.g. the unit
#' @param topcolclass boolean; the top colour class isn't properly plotted in the IRS and left out by default (FALSE) in the legend as well
#' @param pos position of the legend given as a vector c(xl,yb,xr,yt) that define the lower left and upper right coordinates of the rectange of colors in user coordinates
#' @param add boolean variable, TRUE to add to current plot, FALSE to start a new plot
#' @param showlevels if not null, a vector of two integer defining from which to which class to plot the legend, e.g. c(5,14) will not plot the first 4 levels and levels beyond the 14th
#' @param cex scaling factor for plotting text and symbols
#' @param title.cex scaling factor for title font size
#' @return No return value
#' @export
addLegend <- function(colSc="normal", levels=1:10, txt=NULL, pos=c(-1.3,0.0,-0.8,1.0), add=FALSE, topcolclass=FALSE, showlevels=NULL, cex=0.8, title.cex=1.4) {
	requireNamespace("plotrix")	# for color.legend()
	# prepare an empty plot
	if(!add)
		plot(c(0,1),c(0,1), type="n", axes=F, xlab="", ylab="")
	# define colours
	cols <- getColors(levels=levels, colSc=colSc)
	if(!is.null(showlevels)) {
		cols <- cols[showlevels[1]:showlevels[2]]
		levels <- levels[showlevels[1]:showlevels[2]]
	}
	# plot colour legend with values inbetween the colour classes
	# the top colour class isn't properly plotted in the IRS, so leave it out in the legend as well
	if(topcolclass)
		color.legend(pos[1],pos[2],pos[3],pos[4],legend=c("",as.vector(rbind(levels[2:(length(levels))],""))),rect.col=cols, cex=cex, align="rb", gradient="y")
	else
		color.legend(pos[1],pos[2],pos[3],pos[4],legend=c("",as.vector(rbind(levels[2:(length(levels)-1)],""))),rect.col=cols[1:(length(cols)-1)], cex=cex, align="rb", gradient="y")
	# add some text (usually describing the unit) on top of the legend
	if(!is.null(txt)) {
		oldxpd<-par()$xpd
		par(xpd=NA) # to allow plotting text outside the plotting area
		if(length(pos)==5)
			xpostxt <- pos[5]
		else
			xpostxt <- pos[3]-.1
		text(x=xpostxt, y=pos[4], labels=txt,pos=3, cex=title.cex)
		#text(x=-0.7, y=1.0, labels=txt,pos=3, cex=1.4)
		par(xpd=oldxpd) # set back to what it was before
	}
}

######
#' Interpolate points on an impact response surface
#'
#' \code{interpIRS} returns values interpolated from an impact response surface
#'
#' @param dat data.table object with 3 columns (x, y, z)
#' @param xvar Character vector containing the column name for the x dimension
#' @param yvar Character vector containing the column name for the y dimension
#' @param zvar Character vector containing the column name for the z dimension
#' @param xo vector of x coordinates defining points to interpolate to (same length as yo)
#' @param yo vector of y coordinates defining points to interpolate to (same length as xo)
#' @param ... additional arguments for akima::interpp()
#' @return Vector of interpolated values
#' @export
interp.IRS <- function(dat, xvar="X", yvar="Y", zvar="Z", xo, yo, ...) {
  #cat("interp.IRS:",dim(dat),"xx",length(xo), "\n")
  #cat("interp.IRS: number of rows of dat =",nrow(dat),"\n")
  # todo: give warning if points are outside the range covered by the IRS
  # todo: interpp calls interpp.old in akima 0.6-2, which always throws a warning
  #       "interpp.old() is deprecated"
  #       or, alternatively, used akima:bilinear() instead of akima:interpp()
  suppressWarnings(# this is to avoid the warning about the depriciated interpp.old function call
    ret <- as.vector(akima::interpp(x=unlist(dat[,xvar,with=FALSE]),
                                    y=unlist(dat[,yvar,with=FALSE]),
                                    z=unlist(dat[,zvar,with=FALSE]),
                                    xo=xo, yo=yo, ...)$z)
  )
  # akima::bilinear gives 0-values for points outside the range of x and y
  #!ret <- as.vector(akima::bilinear(x=unlist(dat[,xvar,with=FALSE]),
  #!                                y=unlist(dat[,yvar,with=FALSE]),
  #!                                z=matrix(data=unlist(dat[,zvar,with=FALSE]),
  #!                                         nrow=length(unlist(dat[,xvar,with=FALSE])),
  #!                                         ncol=length(unlist(dat[,yvar,with=FALSE]))),
  #!                                x0=xo, y0=yo)$z)
  #cat("interp.IRS:",length(unlist(dat[,xvar,with=FALSE])),"::",length(ret), "\n")
  return(ret)
}

######
#' Calculate the proportion of vector elements above a threshold (or below or any other operator) to estimate risk.
#'
#' \code{get.risk} returns the proportion of vector values below or above a threshold.
#'
#' @param vec vector
#' @param thres treshold value
#' @param na.rm a logical value indicating whether NA values should be stripped before the computation proceeds.
#' @return Vector of interpolated values
#' @export
get.risk <- function(vec, thres, op=">", na.rm=FALSE) {
  if(na.rm) { vec <- vec[!is.na(vec)] }
  return(length(vec[do.call(op,list(vec, thres))])/length(vec))
}
