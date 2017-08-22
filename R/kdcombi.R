############################################################################################## 
# Written by: Dr. Francisco Javier Martínez de Pisón Ascacíbar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members=Francisco Javier Martínez de Pisón Ascacibar (fjmartin@dim.unirioja.es), Manuel Castejón Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Pernía Espinoza (alpha.veronica@dim.unirioja.es), Joaquín B. Ordieres Meré (joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara González (eliseo.vergara@dim.unirioja.es), Fernando Alba Elías (fernando.alba@dim.unirioja.es)
# ,Ana González Marcos (ana.gonzalez@unileon.es)
############################################################################################## 

############################################################################################## 
# (kdcombi.R)Obtain all combinations without repetition from N elements caughted by MaxLevel
############################################################################################## 

# INPUT PARAMETERS:
# N=Number of elements
# MaxLevel=Number of elements in a group
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Combinational Groups (NumMaxGroups=factorial(N)/(factorial(N-MaxLevel)*factorial(MaxLevel)))
# --------------------------------------------------------------------------------------------

kdcombi <- function(N=6, MaxLevel=3)
{
# N=Number of elements
# MaxLevel=Number of elements in a group
# NumMaxGroups=factorial(N)/(factorial(N-MaxLevel)*factorial(MaxLevel))
i <- 0
PosIni <- rep(0,MaxLevel)
Pos <- PosIni
Comb <- NULL
while (i<MaxLevel)
	{
	i <- i+1
	PosIni[i] <- i
	Pos[i] <- i
	}

Level=MaxLevel

Cont=0
while (Level>0)
	{
	if (Level==MaxLevel)
		{
		Comb <- rbind(Comb,as.vector(Pos))
		Cont <- Cont+1
		}
	Pos[Level] <-  Pos[Level]+1
	if (Level!=MaxLevel)
		{
		for (i in (Level+1):MaxLevel)
			{
			Pos[i]= Pos[i-1]+1
			}	
		}
	if (Pos[Level]>(N-MaxLevel+Level))
		{
		Level <- Level-1
		
		}
	else
		{
		Level=MaxLevel
		}
	}
return (Comb)
}