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
# (kdplotscales.R) Plot Tree_Scales
############################################################################################## 

# INPUT PARAMETERS:
# MAT=Matrix with Differents Filters(from kdmatfilter())
# Positions=Plot filter placed in Positions
# Ini=First point
# End=Last point
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 1. None
# --------------------------------------------------------------------------------------------

kdplotscales <- function(MAT, Positions=1:dim(MAT$MFilt)[1],Ini=1,End=dim(MAT$MFilt)[2])
{
	image(t(MAT$CrossZ[Positions,Ini:End]),ylab="Scale",xlab="Time",axes=FALSE,main="Scale-Space Primal Sketch",col=c("White","Black"))
	LL <- length(Positions)

	axis(2,(0:(LL-1))/(LL-1),MAT$WinW[Positions])
	axis(1,seq(0,1,length=10),round(seq(Ini,End,length=10)))
}


