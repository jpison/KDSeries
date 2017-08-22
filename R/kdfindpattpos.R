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
# (kdfindpattpos) Finds Position of Patterns in a Time Serie
############################################################################################## 

# INPUT PARAMETERS:
# MATSort=Matrix with position and code patterns (From kdclusterpatt()$MATSort)
# MATPatterns=Matrix with Patterns with differents level (From kdfindpatt())
# PrintMe=(1 show results) (0=none)
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 	1.- $FinalSerie=Final Patterns Time Serie with: 
#					[col1=Position, col2=Pattern From Level X, col3=Level X)]
#			2.- $PosPat[[Level]]=Pattern's Position From Level
#					[col1=Position, col2=Pattern From Level X]
# --------------------------------------------------------------------------------------------

kdfindpattpos <- function(MATSort, MATPatterns, PrintMe=0)
{
	
	storage.mode(MATSort) <- "integer"

	LevelEnd <- length(MATPatterns)-1
	PosPat <- NULL
	for (Level in 1:LevelEnd)
		{
		VectParam <- c(PrintMe, Level)		
		storage.mode(VectParam) <- "integer"
		MatPatt <- as.matrix(MATPatterns[[Level+1]])
		storage.mode(MatPatt) <- "integer"
		# print(MatPatt)
		PattLN <- .Call("findpositionpat_levelN",MATSort[,2],VectParam,MatPatt,MATSort[,1],PACKAGE="KDSeries")
		if (length(PattLN)!=0)
			{
			PosPat <- c(PosPat, list(PattLN))
			names(PosPat)[Level] <- paste("PosPatt Level ",Level,sep="")
			colnames(PosPat[[Level]]) <- c("pos","codepatt")
			}
		
		
		}
		
	if (length(PosPat)==0)
		{
		print("NO PATTERNS FOUND!!!")
		return
		}
	
	# Final Serie
	FinalSerie <- cbind(PosPat[[1]],1)
	colnames(FinalSerie) <- c("pos","codepatt","levelpatt")
	for (h in 2:length(PosPat))
		{
		FinalSerie[FinalSerie[,1] %in% PosPat[[h]][,1]] <- cbind(PosPat[[h]],h)
		}
	SeriesPatt <- list(FinalSerie=FinalSerie, PosPat=PosPat)
		
	return(SeriesPatt)
}
