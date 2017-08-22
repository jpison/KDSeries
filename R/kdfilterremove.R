############################################################################################## 
# Written by: Dr. Francisco Javier Mart�ez de Pis� Ascac�ar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members=Francisco Javier Mart�ez de Pis� Ascacibar (fjmartin@dim.unirioja.es), Manuel Castej� Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Pern� Espinoza (alpha.veronica@dim.unirioja.es), Joaqu� B. Ordieres Mer�(joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara Gonz�ez (eliseo.vergara@dim.unirioja.es), Fernando Alba El�s (fernando.alba@dim.unirioja.es)
# ,Ana Gonz�ez Marcos (ana.gonzalez@unileon.es)
############################################################################################## 

############################################################################################## 
# (kdfilterremove.R)Remove Filter a Time Series
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

kdfilterremove <- function (TSerie, MinT, Filter="min")
{
storage.mode(TSerie) <- "double"
if (Filter=="min")
	{
	SerieFilt <- .Call("removefiltermin",TSerie,MinT,PACKAGE="KDSeries")
	return(SerieFilt)
	}
	
if (Filter=="max")
	{
	SerieFilt <- .Call("removefiltermax",TSerie,MinT,PACKAGE="KDSeries")
	return(SerieFilt)
	}
	
if (Filter=="range")
	{
	SerieFilt <- .Call("removefilterrange",TSerie,MinT,PACKAGE="KDSeries")
	return(SerieFilt)
	}	
	
if (Filter=="rangeinv")
	{
	SerieFilt <- .Call("removefilterrangeinv",TSerie,MinT,PACKAGE="KDSeries")
	return(SerieFilt)
	}	
	
print("This Type of Filter is NOT Defined");
return

}
