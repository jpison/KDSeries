############################################################################################## 
# Written by: Dr. Francisco Javier Martnez de Pisn Ascacbar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members=Francisco Javier Martnez de Pisn Ascacibar (fjmartin@dim.unirioja.es), Manuel Castejn Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Perna Espinoza (alpha.veronica@dim.unirioja.es), Joaqun B. Ordieres Mer (joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara Gonzlez (eliseo.vergara@dim.unirioja.es), Fernando Alba Elas (fernando.alba@dim.unirioja.es)
# ,Ana Gonzlez Marcos (ana.gonzalez@unileon.es)
############################################################################################## 

############################################################################################## 
# (kdfilter.R)Filter a Time Series
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# WidthW=Filter's Window Width
# Filter=Type of Filter. 	"mean"=rectangular window, "gauss"=gauss window, "median"=median from window
# 							"max"=select max point from window, "min"=select min point from window
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# SerieFilt=Time Series Filtered
# --------------------------------------------------------------------------------------------

kdmiguel <- function (TSerie)
{
storage.mode(TSerie) <- "double"
SerieFilt <- .Call("searchminmax",TSerie,PACKAGE="KDSeries")
print(paste("MAXIMO:",SerieFilt[1]))
print(paste("MINIMO:",SerieFilt[2]))
return(SerieFilt)
}
	
