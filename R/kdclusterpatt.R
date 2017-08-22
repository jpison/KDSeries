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
# (kdclusterpatt.R) Cluster Similar Segment from Segment Matrix (MATPatt=OBTAINED FROM kdextract.R)
############################################################################################## 

# INPUT PARAMETERS:
# MATPatt=Different type of segment found (FROM kdextract() or kdmergepatt())
# INCParam=	1.- Number of Segments. Divide one Segment into frames in order to obtain shape distance
#			2.- Cluster dendrogram from shape distance by groups (g) or by height (h)
#			3.- Value of threshold dendrogram from shape distance
#			4.- Cluster dendrogram from length distance by groups (g) or by height (h)
#			5.- Value of threshold dendrogram from length distance
# DECParam=	1.- Number of Segments. Divide one Segment into frames in order to obtain shape distance
#			2.- Cluster dendrogram from shape distance by groups (g) or by height (h)
#			3.- Value of threshold dendrogram from shape distance
#			4.- Cluster dendrogram from length distance by groups (g) or by height (h)
#			5.- Value of threshold dendrogram from length distance
# HORParam=	1.- Cluster dendrogram from length distance by groups (g) or by height (h)
#			2.- Value of threshold dendrogram from length distance
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# 			 1. $MATSort:Patterns Sorted
#					[col1=Position, col2=Code Pattern (FamilyCode (see below))
#			 2. $FamilyINC: Patterns of family INC as they appear
#			 3. $FamilyDEC: Patterns of family DEC as they appear
#			 4. $FamilyHOR: Patterns of family HOR as they appear		
#			 5. $PointsINC: Points used to cluster INC patterns
#			 6. $GroupsINCShape[[X]]: INC Patterns Family X by Shape
#			 7. $GroupsINCLength[[X]]: INC Patterns Family X by Length
#			 8. $LengthPattINC[[X]]: INC Pattern's Length of Family X
#			 9. $PointsDEC: Points used to cluster DEC patterns
#			10. $GroupsDECShape[[X]]: DEC Patterns Family X by Shape
#			11. $GroupsDECLength[[X]]: DEC Patterns Family X by Length
#			12. $LengthPattDEC[[X]]: DEC Pattern's Length of Family X
#			13. $GroupsHORLength[[X]]: HOR Patterns Family X by Length
#			14. $LengthPattHOR[[X]]: HOR Pattern's Length of Family X
# --------------------------------------------------------------------------------------------

kdclusterpatt <- function(MATPatt, INCParam=c(10,"g",4,"g",4), DECParam=c(10,"g","4","g",4), HORParam=c("g","4"))
{
	
	SimiPatt <- NULL
	
	###################### Obtains groups using hierarchical clustering (INC)  ###########################	
	
	# mar=margins (Bot,Left,Top,Right), cex.axis=axis's text
	# lab=axis y size title
	# mgp=Text Axis's Distance (title, labels, line)
	par(bg="white", mar=c(2,2,2,2), cex.axis=0.7, cex.lab=0.8, lab=c(10,5,6), mgp=c(1,0.5,0), xaxs="i")
	NF <- layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
	BorderPlot=2

	# FamilyINC=0001ssssssllllll  (0001=INC, s=shape type, l=length type)
	FamilyINC <- rep(0,dim(MATPatt$INC)[1])
	
	# Cluster INCREMENTAL Segments
	MATPos <- MATPatt$INC[,1]
	MATLen <- MATPatt$INC[,2]
	PointsINC <- NULL
	for (i in 1:as.numeric(INCParam[1]))
		{
		PointsINC <- cbind(PointsINC, MATPatt$TSerie[MATPos+MATLen*(i/as.numeric(INCParam[1]))]-MATPatt$TSerie[MATPos])
		}
	
	# Cluster by Shape
	Hc <- hclust(dist(PointsINC))

	plot(Hc,hang=-1,main="Cluster Dendrogram INC Patterns by SHAPE",xlab="INCPatterns",labels=FALSE)
		
	if (INCParam[2]=="h")
		{
		GroupsINCShape <- rect.hclust(Hc, h=as.numeric(INCParam[3]), border=1:30)
		}
	else
		{
		GroupsINCShape <- rect.hclust(Hc, k=as.numeric(INCParam[3]), border=1:as.numeric(INCParam[3]))
		}
			
	# Cluster by Length
	Hc <- hclust(dist(MATLen))
	
	plot(Hc,hang=-1,main="Cluster Dendrogram INC Patterns by LENGHT",xlab="INCPatterns",labels=FALSE)
		
	if (INCParam[4]=="h")
		{
		GroupsINCLength <- rect.hclust(Hc, h=as.numeric(INCParam[5]), border=1:30)
		}
	else
		{
		GroupsINCLength <- rect.hclust(Hc, k=as.numeric(INCParam[5]), border=1:as.numeric(INCParam[5]))
		}			
			

	# Plots Cluster INCSegments by SHAPE
	ColorsPat <- rep(0,dim(PointsINC)[1])
	for (i in 1:length(GroupsINCShape))
		{
		ColorsPat[GroupsINCShape[[i]]] <- i
		# FamilyINC=0001ssssssllllll  (0001=INC, s=shape type, l=length type)
		FamilyINC[GroupsINCShape[[i]]] <- 4096+i*64		
		}
	plot(PointsINC[1,],type="l",ylim=range(PointsINC),col=ColorsPat[1],main="INC Clusters by Shape")
	for (i in 2:(dim(PointsINC)[1]))
		{
		lines(PointsINC[i,],col=ColorsPat[i])
		}
		
	# Plots Cluster INCSegmentsby LENGTH
	TablePat <- rep(0, length(GroupsINCLength))
	LengthMax <- TablePat
	LengthPattINC <- as.list(NULL)
	
	for (i in 1:length(GroupsINCLength))
		{
		# FamilyINC=0001ssssssllllll  (0001=INC, s=shape type, l=length type)
		FamilyINC[GroupsINCLength[[i]]] <- FamilyINC[GroupsINCLength[[i]]]+i		
		TablePat[i] <- length(GroupsINCLength[[i]])
		LengthMax[i] <- max(MATLen[GroupsINCLength[[i]]])
		LengthPattINC <- c(LengthPattINC, list(MATLen[GroupsINCLength[[i]]]))
		}
	
	boxplot(LengthPattINC,col=1:length(TablePat),ylim=c(0,max(LengthMax)+5),main="Boxplot INC Length Cluster",xlab="Cluster",ylab="Length")
	text(1:length(TablePat),LengthMax+2,TablePat)
	
########################################## END INC SEGMENTS ##########################################



	###################### Obtains groups using hierarchical clustering (DEC)  ###########################	
	
	# mar=margins (Bot,Left,Top,Right), cex.axis=axis's text
	# lab=axis y size title
	# mgp=Text Axis's Distance (title, labels, line)
	X11()
	par(bg="white", mar=c(2,2,2,2), cex.axis=0.7, cex.lab=0.8, lab=c(10,5,6), mgp=c(1,0.5,0), xaxs="i")
	NF <- layout(matrix(c(1,2,3,4),2,2,byrow=TRUE))
	BorderPlot=2

	# FamilyDEC=0002ssssssllllll  (0002=DEC, s=shape type, l=length type)
	FamilyDEC <- rep(0,dim(MATPatt$DEC)[1])
	
	# Cluster DECREMENTAL Patterns
	MATPos <- MATPatt$DEC[,1]
	MATLen <- MATPatt$DEC[,2]
	PointsDEC <- NULL
	for (i in 1:as.numeric(DECParam[1]))
		{
		PointsDEC <- cbind(PointsDEC, MATPatt$TSerie[MATPos+MATLen*(i/as.numeric(DECParam[1]))]-MATPatt$TSerie[MATPos])
		}
	
	# Cluster by Shape
	Hc <- hclust(dist(PointsDEC))

	plot(Hc,hang=-1,main="Cluster Dendrogram DEC Patterns by SHAPE",xlab="DECPatterns",labels=FALSE)
		
	if (DECParam[2]=="h")
		{
		GroupsDECShape <- rect.hclust(Hc, h=as.numeric(DECParam[3]), border=1:30)
		}
	else
		{
		GroupsDECShape <- rect.hclust(Hc, k=as.numeric(DECParam[3]), border=1:as.numeric(DECParam[3]))
		}
			
	# Cluster by Length
	Hc <- hclust(dist(MATLen))
	
	plot(Hc,hang=-1,main="Cluster Dendrogram DEC Patterns by LENGHT",xlab="DECPatterns",labels=FALSE)
		
	if (DECParam[4]=="h")
		{
		GroupsDECLength <- rect.hclust(Hc, h=as.numeric(DECParam[5]), border=1:30)
		}
	else
		{
		GroupsDECLength <- rect.hclust(Hc, k=as.numeric(DECParam[5]), border=1:as.numeric(DECParam[5]))
		}			
			

	# Plots Cluster DECPatterns by SHAPE
	ColorsPat <- rep(0,dim(PointsDEC)[1])
	for (i in 1:length(GroupsDECShape))
		{
		# FamilyDEC=0002ssssssllllll  (0002=DEC, s=shape type, l=length type)
		FamilyDEC[GroupsDECShape[[i]]] <- 8192+i*64		
		ColorsPat[GroupsDECShape[[i]]] <- i
		}
	plot(PointsDEC[1,],type="l",ylim=range(PointsDEC),col=ColorsPat[1],main="DEC Clusters by Shape")
	for (i in 2:(dim(PointsDEC)[1]))
		{
		lines(PointsDEC[i,],col=ColorsPat[i])
		}
		
	# Plots Cluster DECPatterns by LENGTH
	TablePat <- rep(0, length(GroupsDECLength))
	LengthMax <- TablePat
	LengthPattDEC <- as.list(NULL)
	
	for (i in 1:length(GroupsDECLength))
		{
		# FamilyDEC=0002ssssssllllll  (0002=DEC, s=shape type, l=length type)
		FamilyDEC[GroupsDECLength[[i]]] <- FamilyDEC[GroupsDECLength[[i]]]+i		
		TablePat[i] <- length(GroupsDECLength[[i]])
		LengthMax[i] <- max(MATLen[GroupsDECLength[[i]]])
		LengthPattDEC <- c(LengthPattDEC, list(MATLen[GroupsDECLength[[i]]]))
		}
	
	boxplot(LengthPattDEC,col=1:length(TablePat),ylim=c(0,max(LengthMax)+5),main="Boxplot DEC Length Cluster",xlab="Cluster",ylab="Length")
	text(1:length(TablePat),LengthMax+2,TablePat)
	
########################################## END DEC PATTERNS ##########################################


###################### Obtains groups using hierarchical clustering (HOR)  ###########################	
	
	# mar=margins (Bot,Left,Top,Right), cex.axis=axis's text
	# lab=axis y size title
	# mgp=Text Axis's Distance (title, labels, line)
	X11()
	par(bg="white", mar=c(2,2,2,2), cex.axis=0.7, cex.lab=0.8, lab=c(10,5,6), mgp=c(1,0.5,0), xaxs="i")
	NF <- layout(matrix(c(1,2),2,1,byrow=TRUE))
	BorderPlot=2

	# FamilyHOR=0003000000llllll  (0003=HOR, s=shape type, l=length type)
	FamilyHOR <- rep(0,dim(MATPatt$HOR)[1])

	# Cluster HORIZONTAL Patterns
	MATLen <- MATPatt$HOR[,2]
			
	# Cluster by Length
	Hc <- hclust(dist(MATLen))
	
	plot(Hc,hang=-1,main="Cluster Dendrogram HOR Patterns by LENGHT",xlab="HORPatterns",labels=FALSE)
		
	if (HORParam[1]=="h")
		{
		GroupsHORLength <- rect.hclust(Hc, h=as.numeric(HORParam[2]), border=1:30)
		}
	else
		{
		GroupsHORLength <- rect.hclust(Hc, k=as.numeric(HORParam[2]), border=1:as.numeric(HORParam[2]))
		}			
			

	# Plots Cluster HORPatterns by LENGTH
	TablePat <- rep(0, length(GroupsHORLength))
	LengthMax <- TablePat
	LengthPattHOR <- as.list(NULL)
	
	for (i in 1:length(GroupsHORLength))
		{
		# FamilyHOR=0003000000llllll  (0003=HOR, s=shape type, l=length type)
		FamilyHOR[GroupsHORLength[[i]]] <- 12288+i		
		TablePat[i] <- length(GroupsHORLength[[i]])
		LengthMax[i] <- max(MATLen[GroupsHORLength[[i]]])
		LengthPattHOR <- c(LengthPattHOR, list(MATLen[GroupsHORLength[[i]]]))
		}
	
	boxplot(LengthPattHOR,col=1:length(TablePat),ylim=c(0,max(LengthMax)+5),main="Boxplot HOR Length Cluster",xlab="Cluster",ylab="Length")
	text(1:length(TablePat),LengthMax+2,TablePat)
	
########################################## END DEC PATTERNS ##########################################

	# Creates serie using nominal patterns
	MATSort <- rbind(cbind(Pos=MATPatt$INC[,1],Code=FamilyINC),cbind(Pos=MATPatt$DEC[,1],Code=FamilyDEC),cbind(Pos=MATPatt$HOR[,1],Code=FamilyHOR))
	MATSort <- MATSort[order(MATSort[,1]),]

		
	SimiPatt <- list(MATSort=MATSort, FamilyINC=FamilyINC, FamilyHOR=FamilyHOR, FamilyDEC=FamilyDEC, PointsINC=PointsINC, GroupsINCShape=GroupsINCShape, GroupsINCLength=GroupsINCLength, LengthPattINC=LengthPattINC,  PointsDEC=PointsDEC, GroupsDECShape=GroupsDECShape, GroupsDECLength=GroupsDECLength, LengthPattDEC=LengthPattDEC, GroupsHORLength=GroupsHORLength, LengthPattHOR=LengthPattHOR)
	return(SimiPatt)
}
