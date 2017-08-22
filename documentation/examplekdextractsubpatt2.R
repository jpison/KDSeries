library(KDSeries)
data(COILS)

#########################
# Furnace's Temperature #
#########################
# Preprocessing Furnace Temp
FURTEMP <- kdfilterremove(COILS$THC1,400)
FURTEMP <- kdfilter(FURTEMP,WidthW=12,Filter="median")
FURTEMP <- kdfilter(FURTEMP,WidthW=20,Filter="gauss")
plot.ts(COILS$THC1[1:2000],ylim=c(600,900))
lines(FURTEMP[1:2000],col="red")

# Extract Subpatterns INCREMENTAL
DATA <- FURTEMP
I <- kdextractsubpatt(DATA,"I",pattRangeY=c(30,200),namePatt="THC1INC")
I$PATT[1:10,]
segments(x0=I$PATT[,1],y0=I$PATT[,3],x1=I$PATT[,1]+I$PATT[,2],y1=I$PATT[,3]+I$PATT[,4],col="blue",lwd=3)

# Extract Subpatterns DECREMENTAL
D <- kdextractsubpatt(DATA,"D",pattRangeY=c(30,200),namePatt="THC1DEC")
D$PATT[1:10,]
segments(x0=D$PATT[,1],y0=D$PATT[,3],x1=D$PATT[,1]+D$PATT[,2],y1=D$PATT[,3]+D$PATT[,4],col="magenta",lwd=3)

# Extract Patterns INCREMENTAL, DECREMENTAL
MATTOTAL <- rbind(I$PATT,D$PATT)
THC1PATTERNS <- kdsearchpatt(MATTOTAL,SubPatterns=c("THC1INC","THC1DEC"),WinW=300, namePatt="THC1INCDEC", Plot=TRUE, SerieP=DATA,Xlim=c(1,3000))

# Extract Patterns INCREMENTAL, DECREMENTAL
THC1PATTERNS2 <- kdsearchpatt(MATTOTAL,SubPatterns=c("THC1DEC","THC1INC"),WinW=300, namePatt="THC1DECINC", Plot=TRUE, SerieP=DATA,Xlim=c(1,3000))

#MAT <- rbind(THC1PATTERNS$PATT,THC1PATTERNS2$PATT)
#kdpattwindow(MAT,300)


#####################
# COIL's Input Temp #
#####################

FURTEMP <- kdfilterremove(COILS$THC1,400)
FURTEMP <- kdfilter(FURTEMP,WidthW=12,Filter="median")
FURTEMP <- kdfilter(FURTEMP,WidthW=20,Filter="gauss")
plot.ts(COILS$THC1[1:2000],ylim=c(600,900))
lines(FURTEMP[1:2000],col="red")

INTEMP <- kdfilterremove(COILS$TMP1M,100)
INTEMP <- kdfilter(INTEMP,WidthW=12,Filter="median")
INTEMP <- kdfilter(INTEMP,WidthW=20,Filter="gauss")
INTEMP <- kdfilter(INTEMP,WidthW=20,Filter="mean")
plot.ts(COILS$TMP1M[1:1000],ylim=c(180,270))
lines(INTEMP[1:1000],col="red")

# Extract Subpatterns INCREMENTAL
DATA <- INTEMP
I <- kdextractsubpatt(DATA,"I",pattRangeY=c(30,200),namePatt="TMP1INC")
I$PATT[1:10,]
segments(x0=I$PATT[,1],y0=I$PATT[,3],x1=I$PATT[,1]+I$PATT[,2],y1=I$PATT[,3]+I$PATT[,4],col="blue",lwd=3)

# Extract Subpatterns DECREMENTAL
D <- kdextractsubpatt(DATA,"D",pattRangeY=c(30,200),namePatt="TMP1DEC")
D$PATT[1:10,]
segments(x0=D$PATT[,1],y0=D$PATT[,3],x1=D$PATT[,1]+D$PATT[,2],y1=D$PATT[,3]+D$PATT[,4],col="magenta",lwd=3)


#MAT <- rbind(THC1PATTERNS$PATT,THC1PATTERNS2$PATT,I$PATT[,c(1,2,5)], D$PATT[,c(1,2,5)])
#kdpattwindow(MAT,300)

#####################
# ERROR Output Temp #
#####################


# Preprocessing Temp 2
DATA <- COILS$TMP2M
DATA2 <- kdfilterremove(DATA,200)
plot.ts(DATA2[1:1000],col="black")
DATA3 <- kdfilter(DATA2,20)
lines(DATA3[1:1000],col="blue")
TMP2MR <- DATA3

# Preprocessing Target Temp
DATA <- COILS$TMP2C
DATA2 <- kdfilterremove(DATA,200)
plot.ts(DATA2[1:1000],col="black")
DATA3 <- kdfilter(DATA2,20)
lines(DATA3[1:1000],col="blue")
TMP2CR <- DATA3

ERROR <- abs(TMP2CR-TMP2MR)
plot.ts(ERROR[1:5000])
T <- kdextractsubpatt(ERROR,"T",pattRangeX=c(0,1000),threshold=30,namePatt="ERROR")
segments(x0=T$PATT[,1],y0=T$PATT[,3],x1=T$PATT[,1]+T$PATT[,2],y1=T$PATT[,3]+T$PATT[,4],col="magenta",lwd=3)









