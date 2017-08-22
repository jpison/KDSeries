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
# (kdapproxPAA.R) Constant approximation by frames (mean approximation). 
#.
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# len=length of sections
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# SerieFilt=Time Series filtered
# --------------------------------------------------------------------------------------------

kdapproxPAA <- function (TSerie,len=length(TSerie)/15)
{
SerieFilt<-c(1,2)
i<-1
position<-1
aux<-1
max<-position+len
while (position<length(TSerie))
{
	min<-position
	aux<-TSerie[min:max]
	aux1<-mean(aux)
	while(i<=max)
	{
		SerieFilt[i]<-aux1
		i<-i+1
	}
	position<-max
	max<-position+len

}
	min<-position
	max<-length(TSerie)
	aux<-TSerie[min:max]
	aux1<-mean(aux)
	SerieFilt[i]<-aux1
	plot(TSerie,type="l")
	lines(SerieFilt,type="l",col="red")
	
	return(SerieFilt)
	
}
