############################################################################################## 
# Written by: Dr. Francisco Javier Mart�ez de Pis� Ascac�ar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members=Francisco Javier Martínez de Pisón Ascacibar (fjmartin@dim.unirioja.es), Manuel Castej� Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Pern� Espinoza (alpha.veronica@dim.unirioja.es), Joaqu� B. Ordieres Mer�(joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara Gonz�ez (eliseo.vergara@dim.unirioja.es), Fernando Alba El�s (fernando.alba@dim.unirioja.es)
# ,Ana Gonz�ez Marcos (ana.gonzalez@unileon.es)
############################################################################################## 

############################################################################################## 
# (kdfilterWAVELET.R)Filter a Time Series by WAVELET function
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# Eliminate=frecuencies have to be eliminated
# graphic=plot wavelet components and exit
# 
# 							
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# SerieFilt=Time Series Filtered
# --------------------------------------------------------------------------------------------

kdfilterWAVELET <- function (TSerie, Eliminate="Low",graphic=FALSE)
{


if (Eliminate!="Low" & Eliminate!="High")
	{
	print("The parameter Eliminate must be Low or High. ")
	return()
	}

if (graphic)
	{
	series<-dwt(TSerie)
	plot.dwt(series)
	return()
	}



if (Eliminate=="Low")
	{
	
		series<-dwt(TSerie,n.levels=8)
		series@W$W8[,1]<-0  
		series@W$W7[,1]<-0  
		series@W$W6[,1]<-(0.1*series@W$W6[,1])  
		series@W$W5[,1]<-(0.3*series@W$W5[,1])    
		series@W$W4[,1]<-(0.5*series@W$W4[,1]) 
		series2<-idwt(series) 
		series3<-series@W$W1[1]
		series3[2:( length(series2)+1 )]<-series2
		par(mfrow=c(3,1))
		plot(TSerie,type="l")
		lines(series2,type="l",col="red")
		plot(TSerie,type="l")
		plot(series2,type="l",col="red")
		return(series2)


	}

if (Eliminate=="High")
	{
	
		series<-dwt(TSerie)  
		series@W$W3[,1]<-(0.5*series@W$W3[,1])  
		series@W$W2[,1]<-(0.3*series@W$W2[,1])    
		series@W$W1[,1]<-(0.1*series@W$W1[,1]) 
		series2<-idwt(series) 
		series3<-series@W$W1[1]
		series3[2:( length(series2)+1 )]<-series2
		par(mfrow=c(3,1))
		plot(TSerie,type="l")
		lines(series2,type="l",col="red")
		plot(TSerie,type="l")
		plot(series2,type="l",col="red")
		return(series2)


	}

}

	

