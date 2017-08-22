############################################################################################## 
# Written by: Dr. Francisco Javier Martínez de Pisón Ascacíbar (fjmartin@dim.unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members= Ana González Marcos (ana.gonzalez@unirioja.es), Francisco Javier Martínez de Pisón Ascacibar (fjmartin@dim.unirioja.es),
# ,Manuel Castejón Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Pernía Espinoza (alpha.veronica@dim.unirioja.es), Joaquín B. Ordieres Meré (joaquin.ordieres@dim.unirioja.es),
# ,Eliseo P. Vergara González (eliseo.vergara@dim.unirioja.es), Fernando Alba Elías (fernando.alba@dim.unirioja.es)
# ,
############################################################################################## 

############################################################################################## 
# (kdextractSubPatt.R) Extract Segments from Time Series Vector Filtered
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series (Usually filtered)

# pattType= Type of segment to extract: incremental ("I"), decremental ("D"), horizontal ("H") or threshold ("T")
# pattRangeX= Minimum and maximum Delta(x) values to consider a pattern
# pattRangeY= Minimum and maximum Delta(y) values to consider a pattern
# rangeType= Vector with the type range of x and y: fixed number ("N") or percentage ("P"). The default is c("N","N")
# threshold= Threshold value to search patterns. The default is NULL
# level= Patterns will be search above ("+"), below ("-") or between ("+-") the threshold values. The default is NULL.
# namePatt= Name of the searched segments
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# MATPatt=Patterns found matrix: 
#		[col1=Position, col2=length, col3=height, col4=NamePatt]
# --------------------------------------------------------------------------------------------

kdextractsubpatt <- function(TSerie, pattType, pattRangeX=NULL, pattRangeY=NULL, rangeType=c("N","N"), threshold=NULL, level=NULL, namePatt)
{
	storage.mode(TSerie) <- "double"
	SerieP <- list(TSerie=TSerie)
	
	# Scale between 0 and 1
	MinTSerie = min(TSerie)
	RangeTSerie=max(TSerie)-min(TSerie)
	TSerieNorm <- (TSerie-MinTSerie)/RangeTSerie

	# Obtains Zeros of First Derivate
	Zeros <- .Call("zerocrossings",TSerieNorm, PACKAGE="KDSeries")
	Zeros[length(Zeros)] <- 1
	
	# Where is Zeros?
	WhereZeros <- 1:length(Zeros)
	WhereZeros <- WhereZeros[Zeros %in% 1]

	# Get Long, Height and Position
	PosicionPat <- c(1,WhereZeros[-length(WhereZeros)])
	LongPat <- WhereZeros-PosicionPat
	PosicionY <- TSerie[PosicionPat]
	AltPat <- TSerie[WhereZeros]- TSerie[PosicionPat]


	if (pattType=="I") {
		Cuales <- AltPat>0
		
		# Remove Segments outside the threshold
		if (!is.null(threshold) & !is.null(level)) {
			if (level=="+") {
				Cuales <- Cuales & (PosicionY+AltPat)>=threshold
			} else if (level=="-") {
				Cuales <- Cuales & (PosicionY)<=threshold
			} else if (level=="+-") {
				Cuales <- Cuales & (PosicionY+AltPat)>=threshold[1] & (PosicionY)<=threshold[2]
			}
		}
		
		PosP <- PosicionPat[Cuales]
		LongP <- LongPat[Cuales]
		PosY <- PosicionY[Cuales]
		AltP <- AltPat[Cuales]

		# Remove Segments outside the x-y range
		if (!is.null(pattRangeX)) {
			if (rangeType[1] == "N") {
				XRange <- pattRangeX
			} else {
				XRange <- (pattRangeX/100) * length(TSerie)
			}
			
			PosP <- PosP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			PosY <- PosY[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			AltP <- AltP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			LongP <- LongP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
		}
		if (!is.null(pattRangeY)) {
			if (rangeType[2] == "N") {
				# Scale range
				YRange <- pattRangeY
			} else {
				YRange <- pattRangeY*RangeTSerie/100
			}
			
			PosP <- PosP[which((AltP>=YRange[1] & AltP<=YRange[2]))]
			LongP <- LongP[which((AltP>=YRange[1] & AltP<=YRange[2]))]	
			PosY <- PosY[which((AltP>=YRange[1] & AltP<=YRange[2]))]
			AltP <- AltP[which((AltP>=YRange[1] & AltP<=YRange[2]))]
		}
		
					
		namePatt <- rep(namePatt,length(LongP))
		PATT <- data.frame(PosP, LongP, PosY, AltP, namePatt)
		SerieP <- list(SerieP=TSerie, PATT=PATT)
	
	} else if (pattType=="D") {
		Cuales <- AltPat<0
		
		if (!is.null(threshold) & !is.null(level)) {
			if (level=="+") {
				Cuales <- Cuales & (PosicionY)>=threshold
			} else if (level=="-") {
				Cuales <- Cuales & (PosicionY+AltPat)<=threshold
			} else if (level=="+-") {
				Cuales <- Cuales & (PosicionY)>=threshold[1] & (PosicionY+AltPat)<=threshold[2]
			}
		}
		
		PosP <- PosicionPat[Cuales]
		LongP <- LongPat[Cuales]
		PosY <- PosicionY[Cuales]
		AltP <- AltPat[Cuales]

		# Remove Segments outside the x-y range
		if (!is.null(pattRangeX)) {
			if (rangeType[1] == "N") {
				XRange <- pattRangeX
			} else {
				XRange <- (pattRangeX/100) * length(TSerie)
			}
			
			PosP <- PosP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			PosY <- PosY[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			AltP <- AltP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			LongP <- LongP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
		}
		if (!is.null(pattRangeY)) {
			if (rangeType[2] == "N") {
				# Scale range
				YRange <- abs(pattRangeY)
			} else {
				YRange <- abs(pattRangeY)*RangeTSerie/100
			}
			
			PosP <- PosP[which((abs(AltP)>=YRange[1] & abs(AltP)<=YRange[2]))]
			LongP <- LongP[which((abs(AltP)>=YRange[1] & abs(AltP)<=YRange[2]))]	
			PosY <- PosY[which((abs(AltP)>=YRange[1] & abs(AltP)<=YRange[2]))]
			AltP <- AltP[which((abs(AltP)>=YRange[1] & abs(AltP)<=YRange[2]))]
		}
		
		namePatt <- rep(namePatt,length(LongP))
		PATT <- data.frame(PosP, LongP, PosY, AltP, namePatt)
		SerieP <- list(SerieP=TSerie, PATT=PATT)
			
	} else if (pattType=="H") {
		if (!is.null(pattRangeY)) {
			if (rangeType[2] == "N") {
				# Scale range
				YRange <- pattRangeY
			} else {
				YRange <- pattRangeY*RangeTSerie/100
			}
			Cuales <- AltPat>=YRange[1] & AltPat<=YRange[2]
		} else {
			Cuales <- AltPat == 0
		}
		
		# Remove Segments outside the threshold
		if (!is.null(threshold) & !is.null(level)) {
			if (level=="+") {
				Cuales <- Cuales & (PosicionY+AltPat)>=threshold
			} else if (level=="-") {
				Cuales <- Cuales & (PosicionY)<=threshold
			} else if (level=="+-") {
				Cuales <- Cuales & (PosicionY+AltPat)>=threshold[1] & (PosicionY)<=threshold[2]
			}
		}
			
		PosP <- PosicionPat[Cuales]
		LongP <- LongPat[Cuales]
		PosY <- PosicionY[Cuales]
		AltP <- AltPat[Cuales]

		# Remove Segments outside the x-y range
		if (!is.null(pattRangeX)) {
			if (rangeType[1] == "N") {
				XRange <- pattRangeX
			} else {
				XRange <- (pattRangeX/100) * length(TSerie)
			}
		
			PosP <- PosP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			PosY <- PosY[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			AltP <- AltP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			LongP <- LongP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
		}

		namePatt <- rep(namePatt,length(LongP))
		PATT <- data.frame(PosP, LongP, PosY, AltP, namePatt)
		SerieP <- list(SerieP=TSerie, PATT=PATT)
				
	} else if (pattType=="T") {
		if (!is.null(threshold) & !is.null(level)) {
			if (level=="+") {
				TSerieThresh <- TSerie - threshold
				TSerieThresh[TSerieThresh<0] <- 0
				TSerieThresh[TSerieThresh>0] <- 1
			} else if (level=="-") {
				TSerieThresh <- TSerie - threshold
				TSerieThresh[TSerieThresh>0] <- 0
				TSerieThresh[TSerieThresh<0] <- 1
			} else if (level=="+-") {
				TSerieThresh <- TSerie - threshold[1]
				TSerieThresh2 <- TSerie - threshold[2]
				TSerieThresh[TSerieThresh<0 & TSerieThresh2>0] <- 0
				TSerieThresh[TSerieThresh2<0 & TSerieThresh>0] <- 1
			}
			# Where is Ones?
			WhereOnes <- 1:length(TSerieThresh)
			WhereOnes <- WhereOnes[TSerieThresh %in% 1]

			longs <- WhereOnes[-1]-WhereOnes[-length(WhereOnes)]
			WhereLongs <- which(longs>1)
			PosP <- c(WhereOnes[c(1,WhereLongs+1)])
			LongP <- c(WhereLongs[1],WhereLongs[-1] - WhereLongs[-length(WhereLongs)],length(WhereOnes)-WhereLongs[length(WhereLongs)])
		

			# Remove Segments outside the x-y range
			if (!is.null(pattRangeX)) {
				if (rangeType[1] == "N") {
					XRange <- pattRangeX
				} else {
					XRange <- (pattRangeX/100) * length(TSerie)
				}
			
				PosP <- PosP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
				LongP <- LongP[which((LongP>=XRange[1] & LongP<=XRange[2]))]
			}
			PosY <- rep(threshold[1],length(LongP))
			AltP <- rep(0,length(LongP))
			namePatt <- rep(namePatt,length(LongP))
			PATT <- data.frame(PosP, LongP, PosY, AltP, namePatt)
			SerieP <- list(SerieP=TSerie, PATT=PATT)
		}
	}
	return(SerieP)
}
