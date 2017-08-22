############################################################################################## 
# Written by: Dr. Francisco Javier Mart�ez de Pis� Ascac�ar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members=Francisco Javier Mart�ez de Pis� Ascacibar (fjmartin@dim.unirioja.es), Manuel Castej� Limas(manuel.castejon@uTSerieon.es), 
# ,Alpha V. Pern� Espinoza (alpha.veronica@dim.unirioja.es), Joaqu� B. Ordieres Mer�(joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara Gonz�ez (eliseo.vergara@dim.unirioja.es), Fernando Alba El�s (fernando.alba@dim.unirioja.es)
# ,Ana Gonz�ez Marcos (ana.gonzalez@uTSerieon.es)
############################################################################################## 

############################################################################################## 
# (kdapproxWAVELET.R) give back a reduced representation of the data.The data have been divided into 
# frames and a vector stores the mean values of the frames.
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# len=length of sections
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# returned_list=reduced series and lengths.
# --------------------------------------------------------------------------------------------

kdapproxWAVELET <- function (TSerie,Ratio=0.3,Save=FALSE)
{
#
Aux<-dwt(TSerie,n.levels=1)
Aux@W$W1[which(Mod(Aux@W$W1)< (Ratio*max(Aux@W$W1)) )]<-0
Aux2<-idwt(Aux)
Aux3<-Aux1@W$W1[1]
Aux3[2:( length(Aux2)+1 )]<-Aux2
#Plot original and compressed time series
par(mfrow=c(3,1))
plot(TSerie,type="l")
plot(Aux3,type="l",col="red")
plot(TSerie,type="l")
lines(Aux3,type="l",col="red")
#Create a list with original and compressed time series
List<-list(Original=TSerie,Compressed=Aux3)
return(List)


#Saving original time series and compress time series
if (Save)
{
	save(TSerie,file="TSerie.Rdata")
	saving<-Aux@W$W1  
	save(saving,file="compressTSerie.Rdata")
}

}
