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
# (kdfastsammon.R) # Obtain a Hybrid Fast Sammon Projection
############################################################################################## 

# INPUT PARAMETERS:
# Mat=K-Dimensional Data Matrix
# P=Percentage of data to generate clasical Sammon (using sammon() from MASS library)
# niter=The maximum number of iterations (paramter to sammon() from MASS library).
# trace: Logical for tracing optimization. Default 'FALSE' (paramter to sammon() from MASS library).
# magic: initial value of the step size constant in diagonal Newton method (paramter to sammon() from MASS library).
# tol: Tolerance for stopping, in units of stress (paramter to sammon() from MASS library).
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 1. Matrix with 2D Sammon Projection
# --------------------------------------------------------------------------------------------

kdfastsammon <- function (Mat, P=0.1, niterS = 100, traceS = FALSE, magicS = 0.2, tolS = 1e-4)

{
	require(MASS)
	
	# Obtain P percentage of Center Clusters from Data Matrix using k-means
	N <- dim(Mat)[1]
	K <- dim(Mat)[2]
	
	samcen <- kmeans(Mat,P*N, iter.max=1)
	
	# Calculate dist matrix from "samcen"
	distsamcen <- dist(samcen$centers,upper=TRUE,diag=TRUE)
	
	# Select first near centers
	nearcenters <- apply(as.matrix(distsamcen),2,order)[2:3,]
	
	# Calculate sammon 2D Projection
	centers2D <- sammon(dist(samcen$centers), k = 2, niter = niterS, trace = traceS, magic = magicS, tol = tolS)
	centers2Dpoints <- centers2D$points
	
	# Solve linear ecuation to include data points in 2D Sammon Projection
	Proj2D <- NULL
	for (NumP in 1:N)
		{
		ClustNear <- samcen$cluster[NumP]
		
		MatProj <- rbind(samcen$centers[ClustNear,],samcen$centers[nearcenters[,ClustNear],])
		XYProj <- rbind(centers2D$points[ClustNear,],centers2D$points[nearcenters[,ClustNear],])
		
		DistPK <- dist(rbind(Mat[NumP,],MatProj))
		DistPointK <- DistPK[1:3]
		DistPKC <- DistPK[4:5]		
		
		DistXY <- dist(XYProj)[1:2]
		# Si la distancia entre centroides es menor que la distancia del punto a los dos centroides
		# calculamos el punto de intersección
		if ((DistPointK[1]+DistPointK[2])>=DistPKC[1])
			{
			
			
			}
		
		
		
		
		
		
		Xcoef <- solve(MatProj,XYProj[,1], tol=1e-5, LINPACK=TRUE)
		Ycoef <- solve(MatProj,XYProj[,2], tol=1e-5, LINPACK=TRUE)
		Proj2D <- rbind(Proj2D,c(Xcoef%*%Mat[NumP,], Ycoef%*%Mat[NumP,]))
		}
	
	
	
	
	
	#storage.mode(Mat) <- "double"	
	#storage.mode(samcen$centers) <- "double"
	#storage.mode(samcen$cluster) <- "integer"	
	#storage.mode(nearcenters) <- "integer"	
	#storage.mode(centers2Dpoints) <- "double"	
	
	#Proj2D <- .Call("sammontriangulation",Mat, N, K, samcen$centers, samcen$cluster, nearcenters, centers2Dpoints, PACKAGE="KDSeries")
	Lista <- list(Proj2D=Proj2D,centers2D=centers2D)
	return(Lista)
}
