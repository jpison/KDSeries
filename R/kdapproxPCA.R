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
# (kdapproxPCA.R) give back a reduced representation of the data.The data have been divided into 
# frames and a vector stores the mean values of the frames.
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# len=length of sections
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# returned_list=reduced series and lengths.
# --------------------------------------------------------------------------------------------

kdapproxPCA <- function (TSerie,len=length(TSerie)/15)
{
SerieFilt<-c(1,2)
i<-1
position<-1
aux<-1
aux1<-c(1,2)
max<-position+len
while (position<length(TSerie))
{
	min<-position
	aux<-TSerie[min:max]
	aux1[i]<-mean(aux)
	position<-max
	max<-position+len
	i<-i+1

}
	min<-position
	max<-length(TSerie)
	aux<-TSerie[min:max]
	aux1[i]<-mean(aux)
	SerieFilt<-aux1
	plot(SerieFilt,type="l",col="red")
	returned_list<-list(compressed_series=SerieFilt,compressed_length=length(SerieFilt),original_length=length(TSerie))
	return(returned_list)
	
}
