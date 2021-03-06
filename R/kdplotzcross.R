############################################################################################## 
# Written by: Dr. Francisco Javier Mart�nez de Pis�n Ascac�bar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members=Francisco Javier Mart�nez de Pis�n Ascacibar (fjmartin@dim.unirioja.es), Manuel Castej�n Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Pern�a Espinoza (alpha.veronica@dim.unirioja.es), Joaqu�n B. Ordieres Mer� (joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara Gonz�lez (eliseo.vergara@dim.unirioja.es), Fernando Alba El�as (fernando.alba@dim.unirioja.es)
# ,Ana Gonz�lez Marcos (ana.gonzalez@unileon.es)
##############################################################################################

############################################################################################## 
# (kdplotzcross.R) Plot Time Series and ZeroCrossings
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Serie
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 1. None
# --------------------------------------------------------------------------------------------

kdplotzcross <- function (TSerie)
{
	plot.ts(TSerie,col="blue")
	storage.mode(TSerie) <- "double"
	ZCross <- rep("-",length(TSerie))
	if (length(TSerie)>1) ZCross[.Call("zerocrossings",TSerie,PACKAGE="KDSeries")==1] <- "+"
	points(TSerie,pch=ZCross,cex=0.8)
}
