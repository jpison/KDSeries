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
# (kdextract.R) Extract Segments from Time Series Vector Filtered
############################################################################################## 

# INPUT PARAMETERS:
# TSerie=Time Series (Usually filtered)

# Param[1]=TRUE (Measures INCREMENTAL)
# Param[2]=TRUE (Measures DECREMENTAL)
# Param[3]=TRUE (Measures HORIZONTAL)
# Param[4]=TRUE (Measures INCHIGH)
# Param[5]=TRUE (Measures INCLOW)
# Param[6]=TRUE (Measures DECHIGH)
# Param[7]=TRUE (Measures DECLOW)
# Param[8]=TRUE (Measures VALHIGH)
# Param[9]=TRUE (Measures VALLOW)
# Param[10]=TRUE (Measures VALZERO)
# Param[11]=TRUE (Measures VALMED (Values Between VALHIGH and VALLOW)
# Param[12]=TRUE (Measures INCMED (Values Between INCHIGH and INCLOW) 
# Param[13]=TRUE (Measures DECMED (Values Between DECHIGH and DECLOW) 

# Threshold[1]=% of range(TSerie)=if INC and OVER this value is INCREMENTAL
# Threshold[2]=% of range(TSerie)=if DEC and OVER this value is DECREMENTAL
# Threshold[3]=% of range(TSerie)=if INC or DEC but BELOW this value is HORIZONAL
# Threshold[4]=% of range(TSerie)=elseif INC and OVER this value is INCHIGH
# Threshold[5]=% of range(TSerie)=elseif INC and BELOW this value is INCLOW
# Threshold[6]=% of range(TSerie)=elseif DEC and OVER this value is DECHIGH
# Threshold[7]=% of range(TSerie)=elseif DEC and BELOW this value is DECLOW
# Threshold[8]=% of range(TSerie)=if TSerie is OVER this value is VALHIGH
# Threshold[9]=% of range(TSerie)=if TSerie is BELOW this value is VALLOW
# Threshold[10]=% of max(abs(TSerie))=if abs(TSerie) BETWEEN 0 and this value is VALZERO

# LongMin[1-13]= Number of Min. Elements to be considered pattern
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# MATPatt=Different type of patterns found:
# 			1. $TSerie=Time Series
#			2. $DEC=DEC Segments 
#					[col1=Position, col2=length, col3=height]
#			3. $INC=INC Segments 
#					[col1=Position, col2=length, col3=height]
#			4. $HOR=HOR Segments 
#					[col1=Position, col2=length, col3=height]
#			5. Etc.....
# --------------------------------------------------------------------------------------------

kdextract <- function(TSerie, Param=c(rep(TRUE,13)), Threshold=c(0.05,0.05,0.05,0.40,0.10,0.40,0.10,0.8,0.2,0.05), LongMin=rep(0,13))
{
	storage.mode(TSerie) <- "double"
	SerieP <- list(TSerie=TSerie)
	
	# Scale bettwen 0 and 1
	MinTSerie = min(TSerie)
	RangeTSerie=max(TSerie)-min(TSerie)
	TSerie <- (TSerie-MinTSerie)/RangeTSerie

	# Obtains Zeros of First Derivate
	Zeros <- .Call("zerocrossings",TSerie, PACKAGE="KDSeries")
	Zeros[length(Zeros)] <- 1
	
	# Where is Zeros?
	WhereZeros <- 1:length(Zeros)
	WhereZeros <- WhereZeros[Zeros %in% 1]

	# Get Long, Height and Position
	PosicionPat <- c(1,WhereZeros[-length(WhereZeros)])
	LongPat <- WhereZeros-PosicionPat
	AltPat <- TSerie[WhereZeros]- TSerie[PosicionPat]



	# Extract Segments
	
	# DECREMENTAL
	if (Param[2])
	{
		Cuales <- AltPat<(-Threshold[2])
		
		# Remove Segments with long lower than LongMin
		PosP <- PosicionPat[Cuales]
		AltP <- AltPat[Cuales]
		LongP <- LongPat[Cuales]

		PosP <- PosP[LongP>=LongMin[2]]
		AltP <- AltP[LongP>=LongMin[2]]
		LongP <- LongP[LongP>=LongMin[2]]
	
		DEC <- cbind(PosP, LongP, AltP)
		SerieP <- c(SerieP, list(DEC=DEC))
		}

	# HORIZONTAL
	if (Param[3])
		{
		Cuales <- abs(AltPat)<Threshold[3]
		
		# Remove patterns with long lower than LongMin
		PosP <- PosicionPat[Cuales]
		AltP <- AltPat[Cuales]
		LongP <- LongPat[Cuales]

		PosP <- PosP[LongP>=LongMin[3]]
		AltP <- AltP[LongP>=LongMin[3]]
		LongP <- LongP[LongP>=LongMin[3]]
	
		HOR <- cbind(PosP, LongP, AltP)
		SerieP <- c(SerieP, list(HOR=HOR))
		}

	# INCREMENTAL
	if (Param[1])
	{
		Cuales <- AltPat>Threshold[1]
		
		# Remove Segments with long lower than LongMin
		PosP <- PosicionPat[Cuales]
		AltP <- AltPat[Cuales]
		LongP <- LongPat[Cuales]

		PosP <- PosP[LongP>=LongMin[1]]
		AltP <- AltP[LongP>=LongMin[1]]
		LongP <- LongP[LongP>=LongMin[1]]
	
		INC <- cbind(PosP, LongP, AltP)
		SerieP <- c(SerieP, list(INC=INC))
	}

		
	# INCLOW
	if (Param[5])
		{
		Cuales <- AltPat>=Threshold[1] & AltPat<Threshold[5]

		# Remove Segments with long lower than LongMin
		PosP <- PosicionPat[Cuales]
		AltP <- AltPat[Cuales]
		LongP <- LongPat[Cuales]

		PosP <- PosP[LongP>=LongMin[5]]
		AltP <- AltP[LongP>=LongMin[5]]
		LongP <- LongP[LongP>=LongMin[5]]
	
		INCLOW <- cbind(PosP, LongP, AltP)		
		SerieP <- c(SerieP, list(INCLOW=INCLOW))
		}

		
	# INCMED
	if (Param[12])
		{
		Cuales <- AltPat>=Threshold[5] & AltPat<=Threshold[4]

		# Remove Segments with long lower than LongMin
		PosP <- PosicionPat[Cuales]
		AltP <- AltPat[Cuales]
		LongP <- LongPat[Cuales]

		PosP <- PosP[LongP>=LongMin[12]]
		AltP <- AltP[LongP>=LongMin[12]]
		LongP <- LongP[LongP>=LongMin[12]]
	
		INCMED <- cbind(PosP, LongP, AltP)		
		SerieP <- c(SerieP, list(INCMED=INCMED))
		}		
		
	# INCHIGH
	if (Param[4])
		{
		Cuales <- AltPat>Threshold[4]

		# Remove Segments with long lower than LongMin
		PosP <- PosicionPat[Cuales]
		AltP <- AltPat[Cuales]
		LongP <- LongPat[Cuales]

		PosP <- PosP[LongP>=LongMin[4]]
		AltP <- AltP[LongP>=LongMin[4]]
		LongP <- LongP[LongP>=LongMin[4]]
	
		INCHIGH <- cbind(PosP, LongP, AltP)
		SerieP <- c(SerieP, list(INCHIGH=INCHIGH))
		}
	

		
	# DECLOW
	if (Param[7])
		{
		Cuales <- AltPat<=(-Threshold[2]) & AltPat>(-Threshold[7])
		
		# Remove Segments with long lower than LongMin
		PosP <- PosicionPat[Cuales]
		AltP <- AltPat[Cuales]
		LongP <- LongPat[Cuales]

		PosP <- PosP[LongP>=LongMin[7]]
		AltP <- AltP[LongP>=LongMin[7]]
		LongP <- LongP[LongP>=LongMin[7]]
	
		DECLOW <- cbind(PosP, LongP, AltP)
		SerieP <- c(SerieP, list(DECLOW=DECLOW))
		}
		
		
	# DECMED
	if (Param[13])
		{
		Cuales <- AltPat>=(-Threshold[6]) & AltPat<=(-Threshold[7])
			
		# Remove Segments with long lower than LongMin
		PosP <- PosicionPat[Cuales]
		AltP <- AltPat[Cuales]
		LongP <- LongPat[Cuales]

		PosP <- PosP[LongP>=LongMin[13]]
		AltP <- AltP[LongP>=LongMin[13]]
		LongP <- LongP[LongP>=LongMin[13]]
	
		DECMED <- cbind(PosP, LongP, AltP)
		SerieP <- c(SerieP, list(DECMED=DECMED))
		}


	# DECHIGH
	if (Param[6])
		{
		Cuales <- AltPat<(-Threshold[6])
			
		# Remove Segments with long lower than LongMin
		PosP <- PosicionPat[Cuales]
		AltP <- AltPat[Cuales]
		LongP <- LongPat[Cuales]

		PosP <- PosP[LongP>=LongMin[6]]
		AltP <- AltP[LongP>=LongMin[6]]
		LongP <- LongP[LongP>=LongMin[6]]
	
		DECHIGH <- cbind(PosP, LongP, AltP)
		SerieP <- c(SerieP, list(DECHIGH=DECHIGH))
		}
		


	# VALLOW
	if (Param[9])
		{
		# Extract Segments with Low Values
		SerieH <- as.numeric(TSerie<Threshold[9])

		# Obtains Zeroscrossing
		CerosH <- c(0,SerieH[-length(SerieH)])-SerieH
	
		# Where is zeros?
		DondeCerosH <- 1:length(CerosH)
		DondeCerosH <- DondeCerosH[abs(CerosH) %in% 1]

		# Get Long, Height and Position
		PosicionPatH <- c(1,DondeCerosH[-length(DondeCerosH)])
		LongPatH <- DondeCerosH-PosicionPatH

		# Only with -1 start points
		LongPatH <- LongPatH[CerosH[PosicionPatH]==-1]
		PosicionPatH <- PosicionPatH[CerosH[PosicionPatH]==-1]
		
		# Remove Segments with long lower than LongMin
		PosP <- PosicionPatH
		LongP <- LongPatH

		PosP <- PosP[LongP>=LongMin[9]]
		LongP <- LongP[LongP>=LongMin[9]]
	
		VALLOW <- cbind(PosP, LongP)
		
		SerieP <- c(SerieP, list(VALLOW=VALLOW))
		}

		
	# VALMED
	if (Param[11])
		{
		# Extract Segments Between with Low Values
		SerieH <- as.numeric(TSerie>=Threshold[9] & TSerie<=Threshold[8])

		# Obtains Zeroscrossing
		CerosH <- c(0,SerieH[-length(SerieH)])-SerieH
	
		# Where is zeros?
		DondeCerosH <- 1:length(CerosH)
		DondeCerosH <- DondeCerosH[abs(CerosH) %in% 1]

		# Get Long, Height and Position
		PosicionPatH <- c(1,DondeCerosH[-length(DondeCerosH)])
		LongPatH <- DondeCerosH-PosicionPatH

		# Only with -1 start points
		LongPatH <- LongPatH[CerosH[PosicionPatH]==-1]
		PosicionPatH <- PosicionPatH[CerosH[PosicionPatH]==-1]
		
		# Remove Segments with long lower than LongMin
		PosP <- PosicionPatH
		LongP <- LongPatH

		PosP <- PosP[LongP>=LongMin[11]]
		LongP <- LongP[LongP>=LongMin[11]]
	
		VALMED <- cbind(PosP, LongP)
		
		SerieP <- c(SerieP, list(VALMED=VALMED))
		}
					
	# VALHIGH
	if (Param[8])
		{
		# Extract Segments with High Values
		SerieH <- as.numeric(TSerie>Threshold[8])

		# Obtains Zeroscrossing
		CerosH <- c(0,SerieH[-length(SerieH)])-SerieH
	
		# Where is zeros?
		DondeCerosH <- 1:length(CerosH)
		DondeCerosH <- DondeCerosH[abs(CerosH) %in% 1]

		# Get Long, Height and Position
		PosicionPatH <- c(1,DondeCerosH[-length(DondeCerosH)])
		LongPatH <- DondeCerosH-PosicionPatH

		# Only with -1 start points
		LongPatH <- LongPatH[CerosH[PosicionPatH]==-1]
		PosicionPatH <- PosicionPatH[CerosH[PosicionPatH]==-1]
		
		# Remove Segments with long lower than LongMin
		PosP <- PosicionPatH
		LongP <- LongPatH

		PosP <- PosP[LongP>=LongMin[8]]
		LongP <- LongP[LongP>=LongMin[8]]
	
		VALHIGH <- cbind(PosP, LongP)
		
		SerieP <- c(SerieP, list(VALHIGH=VALHIGH))
		}

	# VALZERO
	if (Param[10])
		{
		# Extract Segments with Zero Values
		TSerie <- SerieP[[1]]/RangeTSerie
		ThresholAdapt=max(abs(TSerie))*Threshold[10]
		SerieH <- as.numeric(abs(TSerie)<ThresholAdapt)

		# Obtains Zeroscrossing
		CerosH <- c(0,SerieH[-length(SerieH)])-SerieH
	
		# Where is zeros?
		DondeCerosH <- 1:length(CerosH)
		DondeCerosH <- DondeCerosH[abs(CerosH) %in% 1]

		# Get Long, Height and Position
		PosicionPatH <- c(1,DondeCerosH[-length(DondeCerosH)])
		LongPatH <- DondeCerosH-PosicionPatH

		# Only with -1 start points
		LongPatH <- LongPatH[CerosH[PosicionPatH]==-1]
		PosicionPatH <- PosicionPatH[CerosH[PosicionPatH]==-1]
		
		# Remove Segments with long lower than LongMin
		PosP <- PosicionPatH
		LongP <- LongPatH

		PosP <- PosP[LongP>=LongMin[10]]
		LongP <- LongP[LongP>=LongMin[10]]
	
		VALZERO <- cbind(PosP, LongP)
		SerieP <- c(SerieP, list(VALZERO=VALZERO))
		}
				
return(SerieP)
}
