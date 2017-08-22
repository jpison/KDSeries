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
# (kdfilters.R)Filter a Time Series with different type of filters
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series
# WidthW=Filter's Window Width
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# SerieFilt= matrix that each column is a series filtered by a different filter.
# --------------------------------------------------------------------------------------------

kdfilters <- function (TSerie, WidthW=10)
{
#to change the storage mode of TSerie (real->double)
storage.mode(TSerie) <- "double"
matresult<-matrix(0,length(TSerie),5)

#meanfilter
matresult[,1] <- .Call("meanfilter",TSerie,WidthW,PACKAGE="KDSeries")
#gaussfilter
t <- 1: WidthW
FiltW <- (1/(sqrt(2*pi)*sd(t)))*exp((-(t-mean(t))^2)/(2*sd(t)^2))
FiltW <- FiltW/sum(FiltW)
matresult[,2] <- filter(TSerie, FiltW, sides=2, circular=TRUE)
#medianfilter
matresult[,3]<- .Call("medianfilter",TSerie,WidthW, PACKAGE="KDSeries")
#max
matresult[,4]<- .Call("maxfilter",TSerie,WidthW, PACKAGE="KDSeries")	
#min
matresult[,5] <- .Call("minfilter",TSerie,WidthW, PACKAGE="KDSeries")

colnames(matresult)<-c("mean","gauss","median","max","min")

return(matresult)

}

###########################################################################################33	

