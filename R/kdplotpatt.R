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
# (kdplotpatt.R) Plot Segments Extracted from kdextract()
############################################################################################## 

# INPUT PARAMETERS:
# Pat=Different type of segments found (FROM kdclusterpatt())
# Ini=First point
# End=Last point
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 1. None
# --------------------------------------------------------------------------------------------

kdplotpatt <- function (Pat, Ini=1, End=length(Pat$Serie))
{
# mar=margins (Bot,Left,Top,Right), cex.axis=axis's text
# lab=axis y size title
# mgp=Text Axis's Distance (title, labels, line)

	par(bg="white", mar=c(2,3,0.5,0.5), cex.axis=0.7, cex.lab=0.8, lab=c(10,5,6), mgp=c(1,0.5,0), xaxs="i")
	NF <- layout(matrix(c(1,2),2,1,byrow=TRUE), widths=c(1,1),heights=c(3,2), respect=FALSE)
	
	# Plot Temporal Series
	plot (Pat$TSerie[Ini:End],ylab="f(t)",xlab="t",type="l")

	# Plot Bars
	NumPat <- length(Pat)-1
	HeighB <- rep(0,NumPat+2)

	par(las=1, mgp=c(1,0.3,0))
	mp <- barplot(HeighB, names.arg=names(c("",Pat[2: (NumPat+1)],"")),xlim=c(Ini,End),horiz=TRUE,axes=FALSE)

	Incr <- (max(mp)/NumPat)/5
	for (h in 1:NumPat)
	{
		if (length(Pat[[h+1]])!=0)
			{
			rect(Pat[[h+1]][,1],mp[h+1]-Incr, Pat[[h+1]][,1]+Pat[[h+1]][,2], mp[h+1]+Incr, col=h)
			}
	}
}
