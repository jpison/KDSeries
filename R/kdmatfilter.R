############################################################################################## 
# Written by: Dr. Francisco Javier Martínez de Pisón Ascacíbar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members=Francisco Javier Martínez de Pisón Ascacibar (fjmartin@dim.unirioja.es), Manuel Castejón Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Pernía Espinoza (alpha.veronica@dim.unirioja.es), Joaquín B. Ordieres Meré (joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara González (eliseo.vergara@dim.unirioja.es), Fernando Alba Elías (fernando.alba@dim.unirioja.es)
# ,Ana González Marcos (ana.gonzalez@unileon.es)
##############################################################################################

############################################################################################## 
# (kdmatfilter.R) # Obtain a Matrix with Different Filters
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Serie
# WidthWVect=Vector with differents filter's windows width
# Filter=Type of Filter. 	"mean"=rectangular window, "gauss"=gauss window, "median"=median from window
# 							"max"=select max point from window, "min"=select min point from window
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 1. Matrix with Different Filters (each file is a time serie filtered)
# --------------------------------------------------------------------------------------------

kdmatfilter <- function (TSerie, WidthWVect, Filter="gauss")
{
	storage.mode(WidthWVect) <- "double"	
	
	# Results MAtrix
	storage.mode(TSerie) <- "double"	
	
	MFilt <- matrix(0,length(WidthWVect),length(TSerie))
	storage.mode(MFilt) <- "double"	
	
	# ZeroCrossings Matrix
	
	CrossZ <- matrix(0,length(WidthWVect),length(TSerie))
	storage.mode(CrossZ) <- "integer"
	
	# Filter Window's Width Vector
	WinW <- WidthWVect
	
	# Vector of ZeroCrossings  Summatory
	NumCZ <- rep(1,length(WidthWVect))
	
	j=0
	for (h in WidthWVect)
		{
		j=j+1
		ifelse (h==1, MFilt[j,] <- TSerie, MFilt[j,] <- kdfilter(TSerie,h,Filter))
		CrossZ[j,] <- .Call("zerocrossings",MFilt[j,], PACKAGE="KDSeries")
		NumCZ[j] <- sum(CrossZ[j,])
		}
	MAT <- list(TSerie=TSerie,MFilt=MFilt,WinW=WinW,CrossZ=CrossZ, NumCZ=NumCZ)
	return(MAT)
}
