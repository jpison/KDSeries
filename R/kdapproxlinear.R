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
######################################################################################################################

###################################################################################################################### 
# (kdapproxlinear.R) give back a reduced representation of the data.The new time series can be maked 
# up by relatives max and mín (zerocrossings).
###################################################################################################################### 

# INPUT PARAMETERS:
# TSerie=Time Series
# len=length of sections
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# returned_list=reduced series and lengths.
# --------------------------------------------------------------------------------------------

kdapproxlinear <- function (TSerie)
{
storage.mode(TSerie) <- "double"
zeros<-.Call("zerocrossings",TSerie,PACKAGE="KDSeries")
zerospos<-grep(1,zeros)
values<-velocidades[zerospos]
TSerie2<-TSerie
TSerie2[-zerospos]<-0

plot(TSerie,type="l")
lines(TSerie2,type="p",col="red")


list_return<-list(Values=values,positions=zerospos)
return(list_return)
	
}
