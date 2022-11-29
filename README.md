# IRSanalysis
An R package to plot impact response surfaces, 2-dimensional contour plots of tabled data that define values (in z-dim) to pairs of coordinates (x- and y-dimensions). These can be results of model simulations from a sensitivity analysis of two variables that have been perturbed jointly, e.g. the sensitivity of an impact model to changes in temperature and precipitation. This package was published as a supplement to [Fronzek et al. (2022)](https://dx.doi.org/10.1016/j.crm.2022.100466). Earlier versions of the code were used by [Fronzek et al. (2011)](https://dx.doi.org/10.5194/nhess-11-2981-2011), [Pirttioja et al. (2015)](https://dx.doi.org/10.3354/cr01322), [Fronzek et al. (2018)](https://dx.doi.org/10.1016/j.agsy.2017.08.004), [Fronzek et al. (2019)](https://dx.doi.org/10.1007/s10113-018-1421-8) and [Pirttioja et al. (2019)](https://dx.doi.org/10.1016/j.agrformet.2018.10.006).
## Install
```R
# from github:
devtools::install_github("fronzek/IRSanalysis")
```
## Simple example
```R
## winter wheat yield emulator by Olesen et al. (2007, DOI:10.1007/s10584-006-9216-1)
## based on DAISY simulations; returns DM yield in t ha-1
# default values for temperature (Ta, T3) and precipitation (P2, P3) are just examples
# deltaT, Pchange - temperature change (oC), precipitation change (%)
# Ta, T3 - annual (Ta), May-July seasonal temperature (T3) (oC)
# P2, P3 - seasonal precipitation sum (mm), P2 Feb-April, P3 May-July
# CO2 - atmospheric CO2 concentration (ppm)
# soilwhc - soil water-holding capacity (mm)
wheatYield <- function(deltaT=0, Pchange=0, Ta=10, T3=12, P2=200, P3=300, CO2=360, soilwhc=150) {
	return(
		1.66 +0.270*sqrt(CO2) + 0.0113*soilwhc
		+0.523*(Ta+deltaT) -0.0224*(Ta+deltaT)*(Ta+deltaT)
		-0.316*(T3+deltaT)
		+0.0158*(100+Pchange)*P2/100
		-0.0042*(100+Pchange)*P3/100
	)
}
## prepare some example sensitivity data using a simple winter wheat yield model
# create a table with all T (-2 -- +7oC) and P (-30 -- +40%) change combinations
datYieldEx <- data.table(expand.grid(deltaT=-2:7,Pchange=(-3:4)*10))
# Ta=10oC, T3=12oC, P2=200 mm, P3=300 mm, CO2=360 ppm, soilwhc=150 mm
datYieldEx[,yield:=wheatYield(deltaT=deltaT,Pchange=Pchange)]
# calculate the %-change in yield relative to the unperturbed estimate
datYieldEx[,yieldChange:=100*(yield/datYieldEx[deltaT==0&Pchange==0,yield]-1)]
plotIRS(dat=datYieldEx, var="yield", levels=seq(6,13.5,.5),
	title="DAISY emulator\nWinter wheat yield (t DM ha-1)", colSc="normal")
addLegend(levels=seq(6,13.5,.5),add=TRUE, pos=c(7.2,-32,7.5,42))
```
