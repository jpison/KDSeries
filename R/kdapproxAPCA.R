############################################################################################## 
# Written by: Dr. Francisco Javier Martinez de Pison (fjmartin@unirioja.es) (2008)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP. http://www.mineriadatos.com
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members=Francisco Javier Martinez de Pison Ascacibar, Pablo Guillen 
# ,Manuel Castejón Limas, Eduardo Martínez de Pisón
# ,Alpha V. Pernía Espinoza, Joaquin Ordieres Meré
# ,Fernando Alba Elías, Ana Gonzalez Marcos
############################################################################################## 

############################################################################################## 
# (kdapproxAPCA.R) returns a reduced representation of the data.The data have been divided into 
# variable length frames.
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# len=length of sections
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# returned_list=reduced series and lengths.
# --------------------------------------------------------------------------------------------

kdapproxAPCA <- function (TSerie,len=length(TSerie)/15)
{
storage.mode(TSerie) <- "double"
zeros<-.Call("zerocrossings",TSerie,PACKAGE="KDSeries")
zerospos<-grep(1,zeros)

values<-c(1,2)
positions<-c(1,2)
i2<-1

SerieFilt<-c(1,2)
i<-1
i1<-1
position<-1
aux<-1
aux1<-c(1,2)
max<-zerospos[i1]
i1<-i1+1
not<-0
while ((position+len)<length(TSerie))
{
	min<-position
	aux<-TSerie[min:max]
	aux1<-mean(aux)
	values[i2]<-aux1
	positions[i2]<-max-min
	i2<-i2+1
	while(i<=max)
	{
		SerieFilt[i]<-aux1
		i<-i+1
	}	
	position<-max
	if (not==0) max<-zerospos[i1]
 	if ((not==1)&((position+len)<length(TSerie))) max<-position+len
	i1<-i1+1
	if (i1>length(zerospos)) not<-1

}
	min<-position
	max<-length(TSerie)
	aux<-TSerie[min:max]
	aux1<-mean(aux)
	values[i2]<-aux1
	positions[i2]<-max-min
	while(i<=max)
	{
		SerieFilt[i]<-aux1
		i<-i+1
	}	
	plot(TSerie,type="l")
	lines(SerieFilt,type="l",col="red")
	list_return<-list(Values=values,Number_positions=positions,SerieFilt)
	return(list_return)
	
}
