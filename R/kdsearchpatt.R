############################################################################################## 
# Written by: Dr. Francisco Javier Martínez de Pisón Ascacíbar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members= Francisco Javier Martínez de Pisón Ascacibar (fjmartin@dim.unirioja.es), Ana González Marcos (ana.gonzalez@unirioja.es),
# ,Manuel Castejón Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Pernía Espinoza (alpha.veronica@dim.unirioja.es), Joaquín B. Ordieres Meré (joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara González (eliseo.vergara@dim.unirioja.es), Fernando Alba Elías (fernando.alba@dim.unirioja.es)
# ,
############################################################################################## 

############################################################################################## 
# (kdsearchpatt.R) Search Patterns from Subppatern's Matrix
############################################################################################## 

# INPUT PARAMETERS:
# MAT=rbind of severals MATRIX from kdextractsubpatt.R

# SubPatterns= Grep subpatterns match (see ?regexp and ?grep with perl=TRUE)
# WinW= Maximum windows windth to search a pattern
# namePatt= Name of the searched segments
# Plot= if TRUE plots SerieP with Patterns found
# SerieP= This is the time Series to plot
# Xlim=c(MinX,MaxX) to plot
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS (list):
# PATTERN=Patterns found matrix
# MAT2=SubPatterns combinations matrix
# --------------------------------------------------------------------------------------------



kdsearchpatt <- function(MAT, SubPatterns, WinW, namePatt, Plot=FALSE, SerieP=0,Xlim=c(1,length(SerieP)))
{
	# Order subpatterns
	MAT <- MAT[order(MAT[,1]),]
	NUMC <- length(SubPatterns)
	
	if (nrow(MAT)<=NUMC) return;
	if (nrow(MAT)<1) return;
	if (NUMC<1) return;
	if (WinW<1) return;

	# Fuse N sub patterns
	FILAS <- nrow(MAT)
	MAT2 <- MAT[1:(1+FILAS-NUMC),]
	for (h in 2:NUMC)
		{	
		MAT2 <- data.frame(MAT2,MAT[h:(h+FILAS-NUMC),])
		}

	TOTALW <- MAT2[,ncol(MAT2)-4]+	MAT2[,ncol(MAT2)-3]-MAT2[,1]
	MAT2 <- data.frame(MAT2,TOTALW=TOTALW)

	# Remove window's size > WinW
	MAT2 <- MAT2[MAT2$TOTALW<=WinW,]
	
	# Search patterns
	CUALES <- rep(TRUE,nrow(MAT2))
	for (h in 1:NUMC)
		{
		MASCARA <- rep(FALSE,nrow(MAT2))
		MASCARA[grep(SubPatterns[h],MAT2[,h*5],perl=TRUE)] <- TRUE
		CUALES <- CUALES & MASCARA
		}
	MAT2 <- MAT2[CUALES,]
	if (nrow(MAT2)<1) return (list(PATTERN=NULL,MAT2=NULL))
	PATTERN <- data.frame(PosP=MAT2[,1],LongP=MAT2$TOTALW,namePatt=namePatt)
	if (Plot==TRUE && SerieP!=0)
		{
		plot.ts(SerieP,xlim=Xlim)
	
		for (h in 0:(NUMC-1))
			{
			segments(x0=MAT2[,1+5*h],y0=MAT2[,3+5*h],x1=MAT2[,1+5*h]+MAT2[,2+5*h],y1=MAT2[,3+5*h]+MAT2[,4+5*h],col=h+2)
			segments(x0=PATTERN[,1],y0=min(SerieP[Xlim[1]:Xlim[2]]),x1=PATTERN[,1]+PATTERN[,2],y1=min(SerieP[Xlim[1]:Xlim[2]]),col=1,lwd=3)
			text(MAT2[,1+5*h],MAT2[,3+5*h],rownames(MAT2),col=h+2)
			text(PATTERN[,1],min(SerieP[Xlim[1]:Xlim[2]]),row.names(PATTERN))
			
			}			
		}
	
	return(list(PATTERN=PATTERN,MAT2=MAT2))
}
