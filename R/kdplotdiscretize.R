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
# (kdplotdiscretize.R) Plot graphics from kddiscretize 
############################################################################################## 

# INPUT PARAMETERS:
# TSerieD=discretized series
# TSerie=original series
# Threshold=margins for chaging the label
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 1. None
# --------------------------------------------------------------------------------------------

kdplotdiscretize <- function (TSerieD,TSerie,Threshold,Labels)
{
AuxSerie<-TSerie

for (i in 1:length(TSerieD))
{	
j<-1
	repeat
	{
		if (Labels[j]==TSerieD[i])
		{
		AuxSerie[i]<-Threshold[j]
		break
		}
	if (j>length(Labels)) 
	{
	print("An error has ocurred.")
	return
	}
	j<-j+1
	}
j<-0
}	
plot.ts(TSerie,type="p")
lines(AuxSerie,type="s",col="red")
return(AuxSerie)
}











