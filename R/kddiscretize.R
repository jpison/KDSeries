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
# (kddiscretize.R)discretize a Time Series.
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# Threshold=margins for chaging the label
# Labels=characters for describing the different levels of Threshold
# 							
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# SerieFilt=Time Series discretized
# --------------------------------------------------------------------------------------------

kddiscretize <- function (TSerie, Threshold,Labels)
{
TSerieFilt<-TSerie
#Check errors 
if ((length(Threshold)) < (length(Labels)))
{
print("The length of Threshold can't be bigger than the length of Labels")
return()
}

for (i in 1:length(TSerie))
{
j<-1
	repeat
		{
		if (j==length(Threshold))
		{
		TSerieFilt[i]<-Labels[length(Labels)]
		break
		}
		if ((TSerie[i]>Threshold[j]) & (TSerie[i]<Threshold[j+1])) 
		{
		TSerieFilt[i]<-Labels[j]
		break
		}
		if (TSerie[i]==Threshold[j]) 
		{
		TSerieFilt[i]<-Labels[j]
		break
		}
		if (j>length(Threshold))
		{
		print("An error has ocurred.")
		return
		}
		j<-j+1
		}

}
return(TSerieFilt)

}


















