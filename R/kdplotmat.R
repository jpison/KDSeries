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
# (kdplotmat.R) Plot Matrix with Differents Filters
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

kdplotmat <- function(MAT, Positions=1:dim(MAT$MFilt)[1],Ini=1,End=dim(MAT$MFilt)[2])
{
	LL <- length(Positions)

	# mar=margin (Bot,Left,Top,Right), cex.axis=text_axis, 
	# lab=div axis y title size
	# mgp=Axis Text Dist (title, labels, line)

	par(bg="white", mar=c(1,1,1.5,0.5), lab=c(10,5,6), mgp=c(1,0.2,0), mfrow=c(LL,1))
	for (h in 1:LL)
		{
		Etiq=paste("Window's Filter Width=",MAT$WinW[Positions[h]],sep="")
		plot(MAT$MFilt[Positions[h], Ini:End],main=Etiq, type="l", ylab="f(t)")
		}
}
