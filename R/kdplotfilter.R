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
# (kdplotfilter.R) Plot series from kdfilter
############################################################################################## 

# INPUT PARAMETERS:
# TSerie = from kdfilter()
# TSerieOrigin = original series
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: none.
# --------------------------------------------------------------------------------------------

kdplotfilter <- function(TSerie,TSerieOrigin)
{
	plot.ts(TSerie,col="red",ylab="ORIGINAL AND FILTERED SERIES (black)")
	lines(TSerieOrigin,col="black")
}
 
