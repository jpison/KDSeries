############################################################################################## 
# Written by: Dr. Francisco Javier Martínez de Pisón Ascacíbar (fjmartin@unirioja.es) (2005)
# EDMANS (Engineering Data Mining And Numerical Simulations) Research GROUP.
# License: GPL version 2 or newer
# --------------------------------------------------------------------------------------------
# Project Engineering Group
# Department of Mechanical Engineering. C/ Luis de Ulloa, 20.
# 26004. Universidad de La Rioja. Spain
# --------------------------------------------------------------------------------------------
# EDMANS Members= Francisco Javier Martínez de Pisón Ascacibar (fjmartin@unirioja.es), Ana González Marcos (ana.gonzalez@unirioja.es),
# ,Manuel Castejón Limas(manuel.castejon@unileon.es), 
# ,Alpha V. Pernía Espinoza (alpha.veronica@unirioja.es), Joaquín B. Ordieres Meré (joaquin.ordieres@unirioja.es),
# ,Eliseo P. Vergara González (eliseo.vergara@unirioja.es), Fernando Alba Elías (fernando.alba@unirioja.es)
# ,
############################################################################################## 

############################################################################################## 
# (kdpattwindow.R) Search Patterns using a slide window
############################################################################################## 

# INPUT PARAMETERS:
# MAT=rbind of severals MATRIX from kdsearchpatt

# WinW= Maximum windows width to search a pattern
# Slide= Slide Window (patterns)
# Posit=Put position of patterns
# --------------------------------------------------------------------------------------------

# OUTPUT PARAMETERS (list):
# PATTERNLIST=Patterns found matrix
# --------------------------------------------------------------------------------------------


kdpattwindow <- function (MAT, WinW, Slide = 1, Posit = FALSE, Ini=1, End=max(MAT[,1])) 
{
    MAT <- MAT[order(MAT[, 1]), ]
    MAT <- MAT[MAT[,1]>=Ini & MAT[,1]<=End,]
    if (nrow(MAT) < 1) 
        return
    if (WinW < 1) 
        return
    PATT <- NULL
    NROW <- nrow(MAT)
    POS <- MAT[, 1] + MAT[, 2]
    for (h in seq(1, nrow(MAT) - Slide, Slide)) {
        CAEN <- POS >= (MAT[h, 1] + MAT[h, 2]) & POS < (MAT[h, 
            1] + WinW)
        if (Posit) 
            PATT <- c(PATT, list(c(as.character(MAT[h, 1]), as.character(t(MAT[CAEN, 
                3])))))
        else PATT <- c(PATT, list(c(as.character(t(MAT[CAEN, 
            3])))))
    }
    return(PATTERNLIST = PATT)
}
