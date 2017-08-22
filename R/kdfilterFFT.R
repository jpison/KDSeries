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
# (kdfilterFFT.R)Filter a Time Series by FFT function
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# WidthW=Filter's Window Width
# Slide=Displacement of the window filter
# Filter=Type of Filter. "mean"=rectangular window, "gauss"=gauss window, "median"=median from window
# Range=Range of frecuencies that we want to eliminate in % (min % of total length,max % of total length)
# 							
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# SerieFilt=Time Series Filtered
# --------------------------------------------------------------------------------------------

kdfilterFFT <- function (TSerie, WidthW=0.02*length(TSerie), Slide=WidthW , Filter="mean" , Range=c(20,70))
{
AuxTSerie<-TSerie
if (WidthW>length(TSerie)) 
{
print("WidthW must be smaller than length of the TSerie")
return()
}
storage.mode(TSerie) <- "double"
i1<-1
i2<-WidthW
#####################################
while (i2!=length(TSerie))
{
	if (i2>=length(TSerie)) {i1<-i1+Slide;i2<-length(TSerie)}
	
	if (Filter=="rect" || Filter=="mean")
		SerieFilt <- .Call("meanfilter",TSerie[i1:i2],WidthW,PACKAGE="KDSeries")
			
	if (Filter=="gauss")
	{
		t <- 1: WidthW
		FiltW <- (1/(sqrt(2*pi)*sd(t)))*exp((-(t-mean(t))^2)/(2*sd(t)^2))
		FiltW <- FiltW/sum(FiltW)
		SerieFilt <- filter(TSerie[i1:i2], FiltW, sides=2, circular=TRUE)
	}
	SerieFilt.frec<-fft(SerieFilt)
	aux1<-SerieFilt.frec[-1]
	margin1<-as.integer((Range[1]/100)*(length(SerieFilt.frec)%/%2))
	margin2<-as.integer((Range[2]/100)*(length(SerieFilt.frec)%/%2)	)
	aux1[(margin1):(margin2)]<-0
	aux1[((length(SerieFilt.frec))-margin1):((length(SerieFilt.frec))-margin2)]<-0
	aux1<-c(SerieFilt.frec[1],aux1)
	aux2<-Re(fft(aux1,inverse=T))/(length(aux1))
	AuxTSerie[i1:i2]<-aux2
	if (i2<length(TSerie)) {i1<-i1+Slide;i2<-i2+Slide}

}
##################################################################End while
plot(TSerie,type="l")
lines(AuxTSerie,col="red",type="l")
return(AuxTSerie)	
	
	
	
}
