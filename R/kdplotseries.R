#ESTA FUNCIÓN HA SIDO MODIFICADA EL 23-02-07 POR MIGUEL
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
# (kdplotseries.R) Plot Time Series In X Parts with Differents Filters
#PARTIMOS DE UNA MATRIZ CON UN NUMERO N DE COLUMNAS DE LAS QUE REPRESENTAMOS UN NUMERO=Num DESDE
#LA FILA Ini HASTA LA End (si no se pone nada la última).
#
############################################################################################## 

# INPUT PARAMETERS:
# Series=Matrix with Time Series (Each Column is one Time Serie)
# Num=Num of Horizontal plots
# Ini=First point
# End=Last point
# Colors=Color for each Time Series
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 1. None
# --------------------------------------------------------------------------------------------


kdplotseries <- function(Series, Num=4,Ini=0,End=nrow(Series),Colors=rep(1:6,length=ncol(Series)))
{	#COLORES-> 1 NEGRO ,2 ROJO,3 VERDE,4 AZUL,5 CYAN, 6 MAGENTA
	# mar=margin (Bot,Left,Top,Right), cex.axis=text_axis, 
	# lab=div axis y title size
	# mgp=Axis Text Dist (title, labels, line)

	par(bg="white", mar=c(1.5,2,1.5,1), lab=c(10,5,6), mgp=c(1,0.2,0), mfrow=c(Num,1))
	#DEFINE PARAMETROS GRAFICOS 
	Long=(End-Ini+1)/Num
	for (h in 1:Num)
		{
		Etiq=paste("Series Part:",h,sep="")
		#TITULO DE LA GRAFICA
		MAXIMO <- max(Series)
		MINIMO <- min(Series)
		
		
		plot(c((1+Long*(h-1)):(Long*h)),Series[(1+Long*(h-1)):(Long*h),1],main=Etiq, type="l", ylab="f(t)",xlab="",col=Colors[1],ylim=c(MINIMO,MAXIMO))
		
		 Ancho=(Long*h)-(1+Long*(h-1))
         NumAleatx <- round(1+Long*(h-1)+Ancho*runif(1))
         NumAleaty <- Series[NumAleatx, 1]
   	     text(NumAleatx, NumAleaty, names(Series)[1])

  		 if (ncol(Series)>1)
			{
			#PRIMERA SERIE TEMPORAL REPRESENTADA EN NEGRO
			for (j in 2:ncol(Series))
				{
				lines(c((1+Long*(h-1)):(Long*h)),Series[(1+Long*(h-1)):(Long*h),j],col=Colors[j], ylim=c(MINIMO,MAXIMO))
		 		Ancho=(Long*h)-(1+Long*(h-1))
         		NumAleatx <- round(1+Long*(h-1)+Ancho*runif(1))
         		NumAleaty <- Series[NumAleatx, j]
   	     		text(NumAleatx, NumAleaty, names(Series)[j],col=Colors[j])
				#text(c((1+Long*(h-1)),Series[(1+Long*(h-1))),j	
				}
			}
		}
}






















