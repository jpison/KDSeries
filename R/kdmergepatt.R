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
# (kdmergepatt.R) Merge segments from MAT=(from kdextract())
############################################################################################## 

# INPUT PARAMETERS:
# MAT=(from kdextract())
# Delete=Delete segments with a length minor or equal than x=(INC, DEC, HOR)
# MinHeigth=(INC,DEC) thresholds to be considerated HOR, INC, DEC (INC= (if > INC) else HOR, DEC= (if < DEC) else HOR
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 1. MAT=Matrix with merged segments
# --------------------------------------------------------------------------------------------

kdmergepatt <- function(MAT, Delete=c(0,0,1), MinHeight=c(0.05,-0.05))
{
	# INC=1, DEC=2, HOR=3
	
	
	MATPatt <- rbind(cbind(MAT$INC[MAT$INC[,2]>Delete[1],],1),cbind(MAT$DEC[MAT$DEC[,2]>Delete[2],],2),cbind(MAT$HOR[MAT$HOR[,2]>Delete[3],],3))
	
	SortP <- 0
	while (SortP==0)
	{
		SortP <- 1
	
		# Sort Patterns
		MATPatt <- MATPatt[order(MATPatt[,1]),]
		LenPatt <- dim(MATPatt)[1]	
		MATM <- NULL
		i=1
		while (i<LenPatt)
			{
			Pos=MATPatt[i,1]
			Len=MATPatt[i,2]
			Alt=MATPatt[i,3]
			Tipo=MATPatt[i,4]
			j=i+1
			while (MATPatt[j,4]==Tipo && j<=LenPatt)
					{
					SortP <- 0
					Len=Len+MATPatt[j,2]	
					Alt=Alt+MATPatt[j,3]	
					j=j+1
					}
			MATM <- rbind(MATM, c(Pos, Len, Alt, Tipo))
			i=j
			}
		MATPatt=MATM
	}
	MATFIN <- list(TSerie=MAT$TSerie, DEC=MATM[MATM[,4]==2,1:3], HOR=MATM[MATM[,4]==3,1:3], INC=MATM[MATM[,4]==1,1:3])
	
	return(MATFIN)
}
