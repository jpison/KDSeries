library(KDSeries)
data(COILS)

# Preprocessing Temp 2
DATA <- COILS$TMP2M
plot.ts(DATA)
DATA2 <- kdfilterremove(DATA,200)
plot.ts(DATA2[1:1000])
DATA3 <- kdfilter(DATA2,20)
plot.ts(DATA3[1:1000])
#I <- kdextractsubpatt(DATA3,"I",pattRangeY=c(50,100),namePatt="INC1")
I <- kdextractsubpatt(DATA3,"I",pattRangeY=c(50,100),namePatt="INC1",threshold=c(780,800),level="+-")
I$PATT[1:10,]
segments(x0=I$PATT[,1],y0=I$PATT[,3],x1=I$PATT[,1]+I$PATT[,2],y1=I$PATT[,3]+I$PATT[,4],col="red")
D <- kdextractsubpatt(DATA3,"D",pattRangeY=c(50,100),namePatt="DEC1")
D$PATT[1:10,]
segments(x0=D$PATT[,1],y0=D$PATT[,3],x1=D$PATT[,1]+D$PATT[,2],y1=D$PATT[,3]+D$PATT[,4],col="blue")

H <- kdextractsubpatt(DATA3,"H",pattRangeX=c(20,1000),pattRangeY=c(0,10),namePatt="HOR1")
H$PATT[1:10,]
segments(x0=H$PATT[,1],y0=H$PATT[,3],x1=H$PATT[,1]+H$PATT[,2],y1=H$PATT[,3]+H$PATT[,4],col="magenta")





MATTOTAL <- rbind(D$PATT,I$PATT, H$PATT)
PATRONES <- kdsearchpatt(MATTOTAL,SubPatterns=c("INC1","DEC1"),WinW=150, namePatt="INC_DEC", Plot=TRUE, SerieP=H$SerieP,Xlim=c(4000,5000))