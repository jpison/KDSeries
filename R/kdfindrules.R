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
# (kdfindrules.R) # Finds Rules in Time Series
############################################################################################## 

# INPUT PARAMETERS:
# FinalSeriesLIST=LIST of Several Patterns Series List (obtained from MAT$FinalSerie=kdfindpattpos())
# 				  (1th Column=Pos, 2th Column=patt, 3th Column=level patt)
# Support=Numbers or % of time the rule holds in the database
# Confidence=Conditional Probability (in %) that Consequent occurs given that Antecedent occurs 
# WinAnte=Antecedent's Maximum Windows Width
# WinConse=Consequent's Maximum Windows Width
# MaxLag=Maximum Time's Lag to be considered (between Antedent and Consequent)
# SortBy= Sort MATRules by: 0. None, 1. Confidence, 2. Support, 3. Rule Length, 4. Antecedent Length, 5. Consecuent Length
# NoRep= 1. Only one similar pattern by window, 0. Counts similar pattern into window (Confidence can be > 1 !!!!)
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS:
# Returns: 	1.- 
# --------------------------------------------------------------------------------------------

kdfindrules <- function(FinalSeriesLIST, Support=0.05, Confidence=0.70, MaxWin=30, PrintMe=0, MaxLevel=7, SortBy=1, NoRep=1) 
{
	
	if (length(FinalSeriesLIST)==0)
		{
		print("NO Time Series FOUND!!!")
		return
		}	

	# --------------------------------------------------
	# Merge all Time Series
	
	# Binary CodePattern (32bits)= vvvv vvvv llll llll nnnn nnnn nnnn nnnn
	# v=Variable Number (start with 0) (First V is 0, Second is 1, ...)
	# l=level (subpatterns's number-1) (Level 1 is 0, 2 is 1, ...
	# n=Pattern code number in this level
	MergedSeries <- NULL
	
	for (h in 1:length(FinalSeriesLIST))
		{
		CodePattern <- rep((h-1)*2^24,dim(FinalSeriesLIST[[h]])[1])
		CodePattern <- CodePattern+(FinalSeriesLIST[[h]][,3]-1)*2^16+FinalSeriesLIST[[h]][,2]
		MergedSeries <- rbind(MergedSeries,cbind(FinalSeriesLIST[[h]][,1],CodePattern))
		}
	
	 MergedSeries <- MergedSeries[order(MergedSeries[,1]),]
	 LengthMerg <- dim(MergedSeries)[1]
	 RelativePos <- c(0,MergedSeries[2:LengthMerg,1]-MergedSeries[1:(LengthMerg-1),1])
	 MergedSeries <- cbind(MergedSeries,RelativePos)
	 colnames(MergedSeries) <- c("pos","codepatt","relativepos")

	 	 
	 # Search patterns with one element that meet Support #
	 ######################################################
	 VECTSort <- MergedSeries[,2]
	 storage.mode(VECTSort) <- "integer"
	 
	 SupportNum = Support*length(VECTSort)
	 if (SupportNum<2)
		{
		print("Support TOO SMALL!!!")
		print("SupportNum=Support*NumPatterns MUST BE > 1 !!!");
		return
		}
		
	 # Save General Parameters
	 RulesList <- list(c(Support=Support, SupportNum=SupportNum, TotalPat=length(VECTSort)))
	 names(RulesList)[length(RulesList)] <- "General Parameters"	
		
	 VectParam <- c(SupportNum, PrintMe)
	 storage.mode(VectParam) <- "integer"

	 cat(paste("NumPatterns=",length(VECTSort),", MinSupport=",SupportNum,"\n",sep=""))
	 cat("##################################################\n")
	 cat("SEARCHING PATTERNS LEVEL 1:\n");
	 
	 RulesList <- c(RulesList, list(MergedSeries))
	 names(RulesList)[length(RulesList)] <-"MergedSeries"
	 
	 # Finds Similiar Patterns (width one) in one Time Series
	 PattL1 <- .Call("findsimilarpat_level1",VECTSort,VectParam, PACKAGE="KDSeries")
	 
	  cat(paste("NumPattSupport=",PattL1[1,1],", NumTotalPatterns=",PattL1[1,2],"\n",sep=""))
	 
	 # Remove Patterns that not meet Support
	 NewSerie <- MergedSeries[MergedSeries[,2] %in% PattL1[-1,1],1:2]
	 LengthMerg <- dim(NewSerie)[1]
	 RelativePos <- c(0,NewSerie[2:LengthMerg,1]-NewSerie[1:(LengthMerg-1),1])
	 NewSerie <- cbind(NewSerie,RelativePos)
	 colnames(NewSerie) <- c("pos","codepatt","relativepos")
	 #RulesList <- c(RulesList, list(NewSerie))
	 #names(RulesList)[length(RulesList)] <- paste("NewSerieL",1,sep="")
	 # --------------------------------------------------
	 
	 RulesList <- c(RulesList, list(PattL1))
	 names(RulesList)[length(RulesList)] <- "PatternsL1"
	 colnames(RulesList[[length(RulesList)]]) <- c("code","num")
	 
     # Search patterns with N elements into a slide Window that meet Support #
	 ######################################################################### 
	
	NumPat <- PattL1[1,1];
	Level=2;
	PattLN=PattL1;
	while (NumPat!=0 && Level<=MaxLevel && length(NewSerie)!=0)
		{
		cat("##################################################\n")
		cat(paste("SEARCHING PATTERNS LEVEL ",Level,":\n",sep=""));
	 
		VectParam <- c(SupportNum, PrintMe, Level, MaxWin, NoRep)
		storage.mode(VectParam) <- "integer"
		storage.mode(NewSerie) <- "integer"
		PattLN <- .Call("findcombinatorypat_levelN",NewSerie[,2:3],VectParam,PattLN[-1,],PACKAGE="KDSeries")
		RulesList <- c(RulesList, list(PattLN))
		names(RulesList)[length(RulesList)] <- paste("PatternsL",Level,sep="")
		colnames(RulesList[[length(RulesList)]]) <- c(rep("code",Level),"num")
		
		cat(paste("NumPattSupport=",PattLN[1,1],", NumTotalPatterns=",PattLN[1,2],"\n",sep=""))
		
		# Remove Patterns L1 that not meet Support in this level
	 	#PattL1 <- as.vector(PattLN[-1,-(Level+1)])
	 	#NewSerie <- NewSerie[NewSerie[,2] %in% PattL1,1:2]
	 	#LengthMerg <- dim(NewSerie)[1]
	 	#RelativePos <- c(0,NewSerie[2:LengthMerg,1]-NewSerie[1:(LengthMerg-1),1])
	 	#NewSerie <- cbind(NewSerie,RelativePos)
	 	#colnames(NewSerie) <- c("pos","codepatt","relativepos")
		#RulesList <- c(RulesList, list(NewSerie))
		#names(RulesList)[length(RulesList)] <- paste("NewSerieL",Level,sep="")
				
		NumPat <- PattLN[1,1]
		Level=Level+1;
		}
	 cat("##################################################\n")

	 ########################################################
	 # 					Searching Rules 					#
	 ########################################################
	 ########################################################
	 RulesMAT <- NULL
	 for (h in 3:(length(RulesList)-2))
	 	{
		 	for (j in (h+1):(length(RulesList)-1))
		 		{
			 		
			 		VectParam <- c(Confidence*1000, h-2, j-2, PrintMe)
					storage.mode(VectParam) <- "integer"
					PatAnte=RulesList[[h]][-1,]
					PatTotal=RulesList[[j]][-1,]
					storage.mode(PatAnte) <- "integer"
					storage.mode(PatTotal) <- "integer"
					RulesLN <- .Call("findrules",PatAnte,PatTotal,VectParam,PACKAGE="KDSeries")
					RulesLN[,4] <- RulesLN[,4]/1000
					RulesMAT <- rbind(RulesMAT, cbind(h,j,RulesLN))
					colnames(RulesMAT) <- c("MatAnte","MatConse","PosAnte","PosConse","Support","Confidence")		 		
					
					if (PrintMe==10)
			 			{
			 			cat(paste("\nH=",h," J=",j,"\n"))
						print(RulesLN)
						}
		 		}
		}
	
	# Sorting by confidence
	if (SortBy==1)
		{
		RulesMAT <- RulesMAT[order(RulesMAT[,6],decreasing=TRUE),]
		}
	
	# Sorting by support
	if (SortBy==2)
		{
		RulesMAT <- RulesMAT[order(RulesMAT[,6],decreasing=TRUE),]
		RulesMAT <- RulesMAT[order(RulesMAT[,5],decreasing=TRUE),]
		}
	
	# Sorting by Rule Length
	if (SortBy==3)
		{
		RulesMAT <- RulesMAT[order(RulesMAT[,6],decreasing=TRUE),]
		RulesMAT <- RulesMAT[order(RulesMAT[,2],decreasing=TRUE),]
		}

	# Sorting by Antecedent Length
	if (SortBy==4)
		{
		RulesMAT <- RulesMAT[order(RulesMAT[,6],decreasing=TRUE),]
		RulesMAT <- RulesMAT[order(RulesMAT[,1],decreasing=TRUE),]
		}
					
	# Sorting by Consecuent Length
	if (SortBy==5)
		{
		RulesMAT <- RulesMAT[order(RulesMAT[,6],decreasing=TRUE),]
		ConsecL <- RulesMAT[,2]-RulesMAT[,1]
		RulesMAT <- RulesMAT[order(ConsecL, decreasing=TRUE),]
		}
	
	RulesList <- c(RulesList, list(RulesMAT))
	names(RulesList)[length(RulesList)] <- "Rules"
	 
return(RulesList)
}
		
	
