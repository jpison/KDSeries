############################################################################################## 
# Written by: Dr. Francisco Javier Mart�nez de Pis�n Ascac�bar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members=Francisco Javier Mart�nez de Pis�n Ascacibar (fjmartin@dim.unirioja.es), Manuel Castej�n Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Pern�a Espinoza (alpha.veronica@dim.unirioja.es), Joaqu�n B. Ordieres Mer� (joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara Gonz�lez (eliseo.vergara@dim.unirioja.es), Fernando Alba El�as (fernando.alba@dim.unirioja.es)
# ,Ana Gonz�lez Marcos (ana.gonzalez@unileon.es)
##############################################################################################

############################################################################################## 
# (kdplotnumz.R) Plot Number ZerosCrossing Curve vs Filter's Windows Width
############################################################################################## 

# INPUT PARAMETERS:
# MAT=Matrix with Differents Filters(from kdmatfilter())
# Positions=Plot filter placed in Positions
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 1. None
# --------------------------------------------------------------------------------------------

kdplotnumz <- function(MAT, Positions=1:dim(MAT$MFilt)[1])
{
	LL <- length(Positions)
	plot.ts(MAT$NumCZ[Positions],xlab="Width",ylab="Num. Zero-Crossings",type="b",col="blue",axes=FALSE)
	axis(1,1:LL,MAT$WinW[Positions])
	axis(2,round(seq(min(MAT$NumCZ[Positions]),max(MAT$NumCZ[Positions]),length=LL)))
}
