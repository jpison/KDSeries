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
# (kdfindpatt.R)# Finds Similar Patterns from a Time Serie
############################################################################################## 

# INPUT PARAMETERS:
# MATSort=Matrix with position and code patterns (From kdclusterpatt()$MATSort)
# Support=Min % of patterns to be considered
# PrintMe=(1 show results) (0=none)
# MaxLevel=Max num of pattern's level (Num of chain elements) to be searched
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns list with:[[1]]= GeneralParameters:
#						[col1=Support in %, col2=Support in Num of Patterns, col3=Total Patt.]
#					[[2]]= Patterns Level 1 (One Element) with: 
#							First Line=[col1=Num. Patt. meet "Support", col2=Num. Total Patt.]
#							Next Lines=[col1=Code Patt., col2=Num. Patt. Found]
#					[[3]]= Patterns Level 2 (Two Elements) with: 
#							First Line=[col1=Num. Patt. meet "Support", col2=Num. Total Patt.]
#							Next Lines=[col1=Code 1th Element, col2=Code 2th Element, col3=Num. Found]
#					[[X]]= Patterns Level X-1 (X-1 Elements)...
# --------------------------------------------------------------------------------------------

kdfindpatt <- function(MATSort, Support=0.15, PrintMe=0, MaxLevel=Inf)
{
	
	storage.mode(MATSort) <- "integer"
	
	
	SupportNum = Support*length(MATSort)
	if (SupportNum<2)
		{
		print("Support TOO SMALL!!!")
		print("SupportNum=Support*NumPatterns MUST BE > 1 !!!");
		return;
		}
	
	VectParam <- c(SupportNum, PrintMe)
	storage.mode(VectParam) <- "integer"

	
	SimiPatt <- list(c(Support=Support, SupportNum=SupportNum, TotalPat=length(MATSort[,1])))
	names(SimiPatt)[1] <- "General Parameters"
	
	# Finds Similiar Patterns in one Time Series
	PattL1 <- .Call("findsimilarpat_level1",MATSort,VectParam, PACKAGE="KDSeries")
	
	SimiPatt <- c(SimiPatt, list(PattL1))
	names(SimiPatt)[2] <- "Patterns Level 1"
	colnames(SimiPatt[[2]]) <- c("code","num")
	
	NumPat <- PattL1[1,1];
	Level=2;
	PattLN=PattL1;
	while (NumPat!=0 && Level<=MaxLevel)
		{
		VectParam <- c(SupportNum, PrintMe, Level)
		storage.mode(VectParam) <- "integer"
		PattLN <- .Call("findsimilarpat_levelN",MATSort,VectParam,PattL1[,1],PattLN[,1:(Level-1)],PACKAGE="KDSeries")
		SimiPatt <- c(SimiPatt, list(PattLN))
		names(SimiPatt)[Level+1] <- paste("Patterns Level ",Level,sep="")
		colnames(SimiPatt[[Level+1]]) <- c(rep("code",Level),"num")
		NumPat <- PattLN[1,1]
		Level=Level+1;
		}
		
	return(SimiPatt)
}
