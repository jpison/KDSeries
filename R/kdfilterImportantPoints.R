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
# (kdfilterImportantPoints.R)Filter a Time Series with a rate compression by selecting the 
#important points.
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# R=rate compression. Values beetween 1 and 1.9 (times that a maximum or a minimum has to be greater or smaller than the other points.)
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# SerieFilt=Time Series Filtered
# --------------------------------------------------------------------------------------------

kdfilterImportantPoints <- function (TSerie, R,type="points")
{
storage.mode(TSerie) <- "double"
R<-c(R)
storage.mode(R)<-"double"

if (R<1 | R>1.9)
{
	print("R must greater than 1 and smaller than 1.9.");
	return();
}
aux<-.Call("memo",TSerie,PACKAGE="KDSeries")
SerieFilt <- .Call("importantpoints",TSerie,R,PACKAGE="KDSeries")
positions <- .Call("posfunction",TSerie,PACKAGE="KDSeries")
compression<-(length(SerieFilt)/length(TSerie))*100
compression<-(100-compression)

plot(TSerie,type="l")
if(type=="points")
lines(positions,SerieFilt,type="p",col="red")
if (type=="lines")
lines(positions,SerieFilt,type="l",col="red")
SerieResult<-list(Positions_of_Important=positions,FilteredSeries=SerieFilt,CompressionRate=compression)
return(SerieResult)



}
