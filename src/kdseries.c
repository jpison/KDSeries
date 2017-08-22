#include <string.h>
#include <R.h>
#include <S.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <math.h>

#define MAXLEVEL 7	/* MAX PATTERN LEVEL */
#define MAXLONG 22 /* MAX PATTERN LONG */
#define MAXCOMBISIZE 20000 /* MAX COMBINATORY ELEMENTS SIZE */

/* Find similar patterns in a Time Series level1*/
/* WinS={Support, printme}*/
SEXP findsimilarpat_level1(SEXP VectPattern, SEXP WinS)
{
	long int h, i, j, k;
	long int limitpatt, blockpatt, countpatt, Nump;
	long int countpattfound, Numpfound;
	 
	int printme, incpatt, Support;
	
	int *pattern;
	
	SEXP VectResult;
	
	Support=(int)INTEGER(WinS)[0];
	printme=(int)INTEGER(WinS)[1];
			
	Nump=length(VectPattern);
	if (Nump==0)
		{
		return (VectResult);
		}
	
	/* Alloc buffer memory */
	blockpatt=1000*2;
	limitpatt=blockpatt;
	pattern=(int *)S_alloc(limitpatt, sizeof(int));
	
	/*Initializate pointer*/
	countpatt=0;
		
	for (h=0; h<Nump; h++)
		{
		incpatt=0;
		for (j=0;j<countpatt;j++)
			{
			if (*(pattern+(j*2))==INTEGER(VectPattern)[h]) 
				{
					*(pattern+(j*2)+1)+=1;
					incpatt=1;
					break;
				}
			}
		if (incpatt==0)
			{
				*(pattern+(countpatt*2))=INTEGER(VectPattern)[h];
				*(pattern+1+(countpatt*2))=1;
				countpatt++;
				if (countpatt*2>=limitpatt)
					{
					limitpatt+=blockpatt;
					pattern=(int *)S_realloc((char *)pattern, limitpatt, limitpatt-blockpatt, sizeof(int));
					}
			}
		}	
	
	
	
	/* Print patterns level one*/
	if (printme==1) 
		{
		Rprintf("######################\n");
		Rprintf("NumPat=%d,Support=%d\n",countpatt,Support);
		Rprintf("######################\n");
		for (j=0; j<countpatt; j++)
			{
			Rprintf("Num.=%d\tCode=%d,Len=%d\n", j, *(pattern+(j*2)), *(pattern+1+(j*2)));			
			}			
		}
		
	
	/*Initializates pointer*/
	Numpfound=0;
	
	/*Count Patterns with Num>=Support*/	
	for (j=0; j<countpatt; j++)
		{
		if (*(pattern+1+(j*2))>=Support) Numpfound++;
		}		
	
	/* Save patterns with Num>=Support */
	PROTECT(VectResult=allocMatrix(INTSXP, Numpfound+1, 2));
	
	INTEGER(VectResult)[0]=Numpfound;
	INTEGER(VectResult)[Numpfound+1]=countpatt;
	countpattfound=1;
	for (j=0; j<countpatt; j++)
		{
		if (*(pattern+1+(j*2))>=Support)
			{
			/* Save patterns with number>=Support */
			INTEGER(VectResult)[countpattfound]=*(pattern+(j*2));
			INTEGER(VectResult)[countpattfound+(Numpfound+1)]=*(pattern+1+(j*2));
			countpattfound++;
			}
		}			
			
	UNPROTECT(1);
	return(VectResult);
}

/* Find similar consecutive patterns in a Time Series levelN*/
/* WinS={Support, printme, level}*/
SEXP findsimilarpat_levelN(SEXP VectPattern, SEXP WinS, SEXP VectPat1, SEXP VectPatN)
{
	long int h, i, j, k, l, m;
	long int limitpatt, blockpatt, countpatt, Nump;
	long int countpattfound, Numpfound;
	 
	int printme, incpatt, Support, level;
	int searchp, searchp2, NumpN, Nump1;
	int *pattern;
	int patternbuff[50];
	
	SEXP VectResult;
	
	Support=(int)INTEGER(WinS)[0];
	printme=(int)INTEGER(WinS)[1];
	level=(int)INTEGER(WinS)[2];
	
	Nump=length(VectPattern);
	if (Nump==0)
		{
		return (VectResult);
		}
	/* Alloc buffer memory */
	blockpatt=1000*(level+1);
	limitpatt=blockpatt;
	pattern=(int *)S_alloc(limitpatt, sizeof(int));
		
	/*Initializates pointer*/
	countpatt=0;
	/* h=each pattern */	
	Nump1=INTEGER(VectPat1)[0];
	NumpN=INTEGER(VectPatN)[0];
	for (h=0; h<(Nump-level+1); h++)
			{
			/* Searches if pattern VectPatN[n] is in "h" position */
			for (i=1;i<=NumpN;i++)
				{
				searchp=1;
				for (j=0;j<(level-1);j++)
					{
					patternbuff[j]=INTEGER(VectPattern)[h+j];
					if (INTEGER(VectPatN)[i+(j*(NumpN+1))]!=patternbuff[j])
						{
							searchp=0;
							break;
						}
					}
				/* if this pattern is in this position then*/
				if (searchp==1)
					{
					/* Is next_position in VectPat1? */
					for (j=1;j<=Nump1;j++)
						{
						/* If next_position is in VectPat1 save pattern o increment if it exists*/
						patternbuff[level-1]=INTEGER(VectPattern)[h+level-1];
						if (INTEGER(VectPat1)[j]==patternbuff[level-1])
							{
							/*
							Rprintf("\nh=%d:",h);
							for (l=0;l<level;l++)
								{
								Rprintf("%d,",patternbuff[l]);
								}
							*/
								
							searchp2=0;
							for (k=0;k<countpatt;k++)
								{
								searchp2=1;
								for (l=0;l<level;l++)
									{
									if (*(pattern+l+(k*(level+1)))!=patternbuff[l])
										{
											searchp2=0;
											break;
										}
									}
								if (searchp2==1) break; 
								}	
							if (searchp2==1)
								{
								*(pattern+level+(k*(level+1)))+=1;
								}
							else
								{
								for (l=0;l<level;l++)
									{
									*(pattern+l+(countpatt*(level+1)))=patternbuff[l];
									}
								*(pattern+level+(countpatt*(level+1)))=1;
								countpatt++;
								if (countpatt*(level+1)>=limitpatt)
									{
									limitpatt+=blockpatt;
									pattern=(int *)S_realloc((char *)pattern, limitpatt, limitpatt-blockpatt, sizeof(int));
									}
								}
							break;
							}
						}
					break;
					}
				}
		}
	
		
	/* Print patterns level N*/
	if (printme==1) 
		{
		Rprintf("##########################################\n");
		Rprintf("NumPat=%d,Support=%d,Level=%d\n",countpatt,Support,level);
		Rprintf("##########################################\n");
		for (j=0; j<countpatt; j++)
			{
			Rprintf("Num.=%d\tLen=%d\t={", j, *(pattern+level+(j*(level+1))));
			for (k=0;k<=level;k++)
				{
				Rprintf("Code%d=%d,", k, *(pattern+k+(j*(level+1))));
				}
			Rprintf("}\n");
			}			
		}
					
	/*Initializates pointer*/
	Numpfound=0;
	
	/*Counts Patterns with Num>=Support*/	
	for (j=0; j<countpatt; j++)
		{
		if (*(pattern+level+(j*(level+1)))>=Support) Numpfound++;
		}		
	
	/* Save patterns with Num>=Support */
	PROTECT(VectResult=allocMatrix(INTSXP, Numpfound+1, level+1));
	
	for (k=0;k<=level;k++)
		{
		INTEGER(VectResult)[k*(Numpfound+1)]=0;
		}
	INTEGER(VectResult)[0]=Numpfound;
	INTEGER(VectResult)[Numpfound+1]=countpatt;
	countpattfound=1;
	
	for (j=0; j<countpatt; j++)
		{
		if (*(pattern+level+(j*(level+1)))>=Support)
			{
			/* Saves patterns with number>=Support */
			for (k=0;k<=level;k++)
				{
				INTEGER(VectResult)[countpattfound+(k*(Numpfound+1))]=*(pattern+k+(j*(level+1)));
				}
			countpattfound++;
			}
		}			
			
	UNPROTECT(1);
	return(VectResult);
}

/* Find position of patterns*/
/* WinS={printme, level}*/
SEXP findpositionpat_levelN(SEXP VectPattern, SEXP WinS, SEXP VectPatN, SEXP Position)
{
	long int h, i, j, k, l, m;
	long int limitpatt, blockpatt, countpatt, Nump;
	 
	int printme, incpatt, Support, level;
	int searchp, searchp2, NumpN, Nump1;
	int *pattern;
	int patternbuff[50];
	
	SEXP VectResult;
	
	printme=(int)INTEGER(WinS)[0];
	level=(int)INTEGER(WinS)[1];
	
	Nump=length(VectPattern);
	if (Nump==0)
		{
		return (VectResult);
		}
	
	/* Alloc buffer memory */
	blockpatt=1000*2;
	limitpatt=blockpatt;
	pattern=(int *)S_alloc(limitpatt, sizeof(int));
		
	/*Initializate pointer*/
	countpatt=0;
	/* h=each pattern size=1 */	
	NumpN=INTEGER(VectPatN)[0];
	/* Rprintf("Level=%d,Nump=%d,NumpN=%d\n",level,Nump,NumpN);*/
	for (h=0; h<(Nump-level+1); h++)
		{
		/* Search if pattern VectPatN[n] is in "h" position */
		for (i=1;i<=NumpN;i++)
			{
			searchp=1;
			for (j=0;j<level;j++)
				{
				/*if (h==1) Rprintf("h=%d,i=%d,j=%d\n,Num1=%d,Num2=%d\n",h,i,j,INTEGER(VectPatN)[i+(j*(NumpN+1))],INTEGER(VectPattern)[h+j]);*/
				if (INTEGER(VectPatN)[i+(j*(NumpN+1))]!=INTEGER(VectPattern)[h+j])
					{
					searchp=0;
					break;
					}
				}
				/* if this pattern is in this position then save it and position*/
			if (searchp==1)
				{
				*(pattern+(countpatt*2))=i;
				*(pattern+1+(countpatt*2))=INTEGER(Position)[h];
				countpatt++;
				if ((countpatt*2)>=limitpatt)
					{
					limitpatt+=blockpatt;
					pattern=(int *)S_realloc((char *)pattern, limitpatt, limitpatt-blockpatt, sizeof(int));
					}
				break;
				}
			}
		}
	
	/* Save patterns with Num>=Support */
	PROTECT(VectResult=allocMatrix(INTSXP, countpatt, 2));
	
	for (h=0;h<countpatt;h++)
		{
		INTEGER(VectResult)[h]=*(pattern+1+(h*2));
		INTEGER(VectResult)[h+countpatt]=*(pattern+(h*2));
		}
			
	UNPROTECT(1);
	return(VectResult);
}


/* Obtain combinations without repetition
 N=Number of elements
 Level=Num of elements of each combination
 */
SEXP combinations(SEXP N, SEXP Levels)
{
	long int h, i, j, k, m;
	long int NumCombi, Cont;
	int *combi;
	int Num, Lev, L;
	
	Num=(int)INTEGER(N)[0];
	Lev=(int)INTEGER(Levels)[0];

	
/* NumCombi=N!/((N-Levels)!*Level!) */	
	NumCombi=1;
	for (h=(Num-Lev+1);h<=Num;h++)
		{
		NumCombi*=h;
		}
	for (h=2;h<=Lev;h++)
		{
			NumCombi/=h;
		}
	
	/*Rprintf("N=%d\tLevels=%d\tNumCombi=%d\n",Num,Lev,NumCombi);*/
	SEXP MatResult;
	
	/* Save patterns with Num>=Support */
	PROTECT(MatResult=allocMatrix(INTSXP, NumCombi, Lev));
	
	/* Fills first combination */
	combi=(int *)S_alloc(Lev, sizeof(int));
	h=1;
	while (h<=Lev)
		{
		*(combi+h-1)=h++;			
		}
	
	Cont=0;
	L=Lev;
	/* Lev=MAXLEVEL, L=Current Level*/
	while (L>0)
		{
		if (L==Lev)
			{
			/*Rprintf("\n");*/
			for (h=0;h<Lev;h++)
				{
				INTEGER(MatResult)[Cont+(h*NumCombi)]=*(combi+h);
				/*Rprintf("%d:",*(combi+h));*/
				}
			Cont++;
			}
		*(combi+L-1)+=1;
		if (L!=Lev)
			{
			for (h=(L+1);h<=Lev;h++)
				{
				*(combi+h-1)=*(combi+h-2)+1;
				}
			}
		if (*(combi+L-1)>(Num-Lev+L))
			{
				L--;
			}
		else
			{
			L=Lev;
			}
		}
			
	UNPROTECT(1);
	return(MatResult);
}
 	
 

/* Obtaining combinations
 Num=Number of elements
 Lev=Num of elements of each combination
 NumCombi=Num Combinations
 */

void c_combinations(int *Buffer, int Num, int Lev, int NumCombi)
{
	long int h, i, j, k, m;
	long Cont;
	int combi[100];
	int L;
	
	h=1;
	while (h<=Lev)
		{
		*(combi+h-1)=h++;			
		}
	
	Cont=0;
	L=Lev;
	/* Lev=MAXLEVEL, L=Current Level*/
	while (L>0)
		{
		if (L==Lev)
			{
			/*Rprintf("\n");*/
			for (h=0;h<Lev;h++)
				{
				*(Buffer+Cont+(h*NumCombi))=*(combi+h);
				/*Rprintf("Cont:%d,%d:",Cont,*(combi+h));*/
				}
			Cont++;
			}
		*(combi+L-1)+=1;
		if (L!=Lev)
			{
			for (h=(L+1);h<=Lev;h++)
				{
				*(combi+h-1)=*(combi+h-2)+1;
				}
			}
		if (*(combi+L-1)>(Num-Lev+L))
			{
				L--;
			}
		else
			{
			L=Lev;
			}
		}
	return;
}

/* Search pattern in a Buffer with elements by columns*/
/* Buffer=Patterns+NumPat [1:(Level+1)] */
/* totalpatt=Num of total patterns (row) */
/* Pattern=Pattern to Search */
/* Level=Num Level Pattern */
long int col_findpat(int *Buffer, long int totalpatt, int *Pattern, int Level)
{
	long int h, i;
	int searchp;
	for (h=0; h<totalpatt; h++)
		{
		/* Searches if pattern VectPatN[n] is in "h" position */
		searchp=1;
		for (i=0;i<Level;i++)
			{
			if (*(Pattern+i)!=*(Buffer+h+(i*totalpatt)))
				{
				searchp=0;
				break;
				}
			}
		/* if this pattern is in this position then return position*/
		if (searchp==1)
			{
			return(h);
			}
		}
		
	/* NOT FOUND */
	return(-1);
}


/* Search pattern in a Buffer with elements by rows*/
/* Buffer=Patterns+NumPat [1:(Level+1)] */
/* totalpatt=Num of total patterns (row) */
/* Pattern=Pattern to Search */
/* Level=Num Level Pattern */
long int row_findpat(int *Buffer, long int totalpatt, int *Pattern, int Level, int Width)
{
	long int h, i;
	int searchp;
	for (h=0; h<totalpatt; h++)
		{
		/* Searches if pattern VectPatN[n] is in "h" position */
		searchp=1;
		for (i=0;i<Level;i++)
			{
			if (*(Pattern+i)!=*(Buffer+i+(h*Width)))
				{
				searchp=0;
				break;
				}
			}
		/* if this pattern is in this position then return position*/
		if (searchp==1)
			{
			return(h);
			}
		}
		
	/* NOT FOUND */
	return(-1);
}



/* Find similar combinatory patterns in a Time Series levelN using one slide window*/
/* WinS={Support, printme, level}*/
SEXP findcombinatorypat_levelN(SEXP VectPattern, SEXP WinS, SEXP VectPatN)
{
	long int h, i;
	int j, k, l, m;
	long int limitpatt, blockpatt, countpatt, Nump, NumpN, sizecombi;
	long int countpattfound, Numpfound;
	 
	int printme, incpatt, Support, level;
	int searchp, searchp2, ActualWidWin, maxwinwidth;
	int *pattern;
	int *pattVectPatN;
	int patternbuff[1000];
	int combinat[MAXCOMBISIZE*MAXLEVEL];
	int localpatt[MAXLEVEL];
	int NumCombi, NumCmax;
	int norep;
	
	SEXP VectResult;
	
	Support=(int)INTEGER(WinS)[0];
	printme=(int)INTEGER(WinS)[1];
	level=(int)INTEGER(WinS)[2];
	maxwinwidth=(int)INTEGER(WinS)[3];
	norep=(int)INTEGER(WinS)[4];
		
	Nump=length(VectPattern)/2;
	 /* Rprintf("Nump=%d",Nump); */
	
	if (Nump==0)
		{
		return (VectResult);
		}
	if (level<2 || maxwinwidth<1 || level>MAXLEVEL)
		{
		return (VectResult);
		}
	
		
	/* Alloc buffer memory */
	blockpatt=1000*(level+1+norep);
	limitpatt=blockpatt;
	pattern=(int *)S_alloc(limitpatt, sizeof(int));
	
	/* Alloc buffer previous pattern memory */
	NumpN=length(VectPatN)/level;
	pattVectPatN=(int *)S_alloc(NumpN*level, sizeof(int));

	/*Rprintf("NumpN=%d, Level=%d\n",NumpN,level);*/
	/* Filling Previous pattern buffer */
	for (k=1;k<NumpN;k++)
		{
		for (j=0;j<(level-1);j++)
			{
			*(pattVectPatN+k-1+(j*NumpN))=INTEGER(VectPatN)[k+(NumpN*j)]; 
			/*Rprintf("Pos=%d,k=%d, l=%d, Val=%d\n",k-1+(j*NumpN),k,j,INTEGER(VectPatN)[k+(NumpN*j)]);*/
			}
		}
	
	/* FINDS PATTERNS LEVEL "level" */
	/*Initializates pointer*/
	countpatt=0;
	/* h=each position */
	
	for (h=0; h<(Nump-level+1); h++)
		{
		/* Obtain elements which are into slide windows */
		/* ============================================ */
		i=1;
		/* Get distance with second element*/
		*(patternbuff)=INTEGER(VectPattern)[h];
		ActualWidWin=INTEGER(VectPattern)[(h+1)+Nump];
		if (printme==5) Rprintf("\nPos %d=%d(%d):",h+1,*(patternbuff+i-1),ActualWidWin);
		while (ActualWidWin<=maxwinwidth && (h+1+i)<=Nump && i<1000)
			{
			*(patternbuff+i)=INTEGER(VectPattern)[h+i];
			if ((h+1+i)<Nump) ActualWidWin+=INTEGER(VectPattern)[(h+1+i)+Nump]; 
			if (printme==5) Rprintf("%d(%d):",*(patternbuff+i),ActualWidWin);
			i++;
			}
		/*if ((h+i)!=Nump) i--;*/
		if (printme==5) Rprintf("\t(N=%d,MaxWidth=%d, ActualWidth=%d)",i,maxwinwidth,ActualWidWin-INTEGER(VectPattern)[(h+i)+Nump]);
		
		/* Generating serial combinations into window's width i-1 selecting level-1 elements*/
		/* Obtaining combinatory positions */
		if (i>=level)
			{
			/* Computing num combinatory elements */
			/* NumCombi=N!/((N-Levels)!*Level!) */	
			if ((level-1)==1)
				{
					NumCombi=i-1;
					for (j=1;j<=NumCombi;j++)
						{
						*(combinat+j-1)=j;
						}
				}
			else
				{
				NumCombi=1;
				for (j=((i-1)-(level-1)+1);j<=(i-1);j++)
					{
					NumCombi*=j;
					}
				for (j=2;j<=(level-1);j++)
					{
					NumCombi/=j;
					}
				/* Obtaining combinatory positions */
				if (NumCombi>=MAXCOMBISIZE)
					{
					Rprintf("ERROR. Max number of combinations excedeed. Change: MAXCOMBISIZE!!!");
					return(VectResult);
					}
				c_combinations(combinat,i-1,level-1,NumCombi);
				}
			if (printme==2) Rprintf("\n(N-1)=%d,(level-1)=%d,NumCombi=%d",i-1,level-1,NumCombi);
			
			/* fill with 0's patterns norep buffer */
			if (norep)
				{
				for (j=0;j<countpatt;j++)
					{
					*(pattern+level+1+((level+2)*j))=0;	
					}
				}
			
			
			/* Getting local patterns */
			localpatt[0]=INTEGER(VectPattern)[h];
			for (k=0;k<NumCombi;k++)
				{
					if (printme==7) Rprintf("\n h=%d,N=%d,P=%d",h+1,NumCombi,localpatt[0]);
					for (l=0;l<(level-1);l++)
						{
						localpatt[l+1]=INTEGER(VectPattern)[h+*(combinat+k+(l*NumCombi))]; 
						if (printme==7) Rprintf(":%d",localpatt[l+1]);
						}
					/* (level-1) elements from pattern into VectPatN buffer? */
					if (col_findpat(pattVectPatN, NumpN, localpatt, level-1)!=-1)
						{
						/* There are (level) elements into pattern buffer? */
						countpattfound=row_findpat(pattern, countpatt, localpatt, level, level+1+norep);
						if (countpattfound==-1)
							{
							countpatt++;
							if (printme==3) Rprintf("\nCounpatt=%d",countpatt);
							if ((countpatt*(level+1+norep))>=limitpatt)
								{
								limitpatt+=blockpatt;
								pattern=(int *)S_realloc((char *)pattern, limitpatt, limitpatt-blockpatt, sizeof(int));
									if (pattern==NULL)
									{
									Rprintf("\n#############\nREALLOCATING PATTERN MEMORY ERROR (Size=%ld Bytes)!!!\n#############\n",limitpatt*sizeof(int));
									return(VectResult);
									}
								}
							/* Saves new pattern */
							if (norep)
								{
								for (l=0;l<level;l++)
									{
									*(pattern+l+((level+2)*(countpatt-1)))=*(localpatt+l);
									}
								*(pattern+level+((level+2)*(countpatt-1)))=1;							
								*(pattern+level+1+((level+2)*(countpatt-1)))=1;
								}
							else
								{
								for (l=0;l<level;l++)
									{
									*(pattern+l+((level+1)*(countpatt-1)))=*(localpatt+l);
									}
								*(pattern+level+((level+1)*(countpatt-1)))=1;							
								}
							}
						else
							{
							/* if pattern exits and there is not a similar pattern into the window increments number patt*/
							if (norep)
								{
								if (*(pattern+level+1+((level+2)*countpattfound))==0)
									{
									*(pattern+level+((level+2)*countpattfound))+=1;
									*(pattern+level+1+((level+2)*countpattfound))=1;
									}
								}
							else
								{
								*(pattern+level+((level+1)*countpattfound))+=1;
								}
							}
						}
				}
			}
		}
	
		
	if (printme==3)
		{
		for (h=0;h<countpatt;h++)
			{
			Rprintf("\nP%d=",h);
			for (l=0;l<=level;l++)
				{
				Rprintf("::%d",*(pattern+l+((level+1+norep)*h)));
				}	
			}
		}
	
	/*Counts Patterns with Num>=Support*/	
	Numpfound=0;
	NumCmax=0;
	for (j=0; j<countpatt; j++)
		{
		if (*(pattern+level+((level+1+norep)*j))>=Support) Numpfound++;
		if (*(pattern+level+((level+1+norep)*j))>NumCmax) NumCmax=*(pattern+level+((level+1+norep)*j));
		}		
	
	/* ALLOC MATRIX */
	PROTECT(VectResult=allocMatrix(INTSXP, Numpfound+1, level+1));
	
	/* First row with number of patterns wthat meet supports and total patterns*/
	for (k=0;k<=level;k++)
		{
		INTEGER(VectResult)[k*(Numpfound+1)]=0;
		}

	INTEGER(VectResult)[0]=Numpfound; /* Num patterns that meet supports in this level */
	INTEGER(VectResult)[Numpfound+1]=countpatt; /* Num total patterns in this level */
	INTEGER(VectResult)[2*(Numpfound+1)]=NumCmax; /* Num max similar elements found */
	
	/* Save patterns with Num>=Support */
	countpattfound=1;
	for (j=0; j<countpatt; j++)
		{
		if (*(pattern+level+((level+1+norep)*j))>=Support)
			{
			/* Saves patterns with number>=Support */
			for (k=0;k<=level;k++)
				{
				INTEGER(VectResult)[countpattfound+(k*(Numpfound+1))]=*(pattern+k+((level+1+norep)*j));
				}
			countpattfound++;
			}
		}	
	

	UNPROTECT(1);
	return(VectResult);
}





/* Find rules from pat1 to pat 2 that meet confidence*/
/* AntePat=antecedent patterns, TotalPat=(antecedent+consecuent), patterns */
/* Param={Confidence, LevelAntepatt, LevelTotalpatt, printme}*/
SEXP findrules(SEXP AntePat, SEXP TotalPat, SEXP Param)
{
	long int h, i;
	int j, k, l, m;
	
	int printme;
	float SuppAnte, SuppCons;
	float Confidence, Found;
	int LevelAntepatt, LevelTotalpatt;
	
	int *pattern;
	int pattbuffTotal[1000];
	int NumAnte, NumTotal;
	int path, patj;
	int countrules;
	long int limitpatt, blockpatt, countpatt;
	
	SEXP VectResult;
	
	Confidence=((float)INTEGER(Param)[0])/1000.0;
	LevelAntepatt=(int)INTEGER(Param)[1];
	LevelTotalpatt=(int)INTEGER(Param)[2];
	printme=(int)INTEGER(Param)[3];
	
	NumAnte=length(AntePat)/(LevelAntepatt+1);
	NumTotal=length(TotalPat)/(LevelTotalpatt+1);
	
	
	
	if (NumAnte<1 || NumTotal<1 || LevelAntepatt>=LevelTotalpatt || LevelAntepatt<1)
		{
		return (VectResult);
		}
	
		
	if (printme==1) Rprintf("\nNumAnte=%d, NumTotal=%d",NumAnte,NumTotal);		
	/* Alloc buffer memory */
	blockpatt=1000*4;
	limitpatt=blockpatt;
	pattern=(int *)S_alloc(limitpatt, sizeof(int));
	
	/*Initializates pointer*/
	countrules=0;
	/* Searching rules */
	for (h=0;h<NumTotal;h++)
		{
		/* Filling TotalPatt Buffer */
		for (j=0;j<LevelAntepatt;j++)
			{
			*(pattbuffTotal+j)=INTEGER(TotalPat)[h+(NumTotal*j)]; 
			}
			
		/* Searching where is antecent */
		for (k=0;k<NumAnte;k++)
			{
			/* Filling Total Buffer */
			Found=1;
			for (j=0;j<LevelAntepatt;j++)
				{
				if (*(pattbuffTotal+j)!=INTEGER(AntePat)[k+(NumAnte*j)])
					{
					Found=0;
					break;
					}
				}
			if (Found==1)
				{
				SuppAnte=(float)INTEGER(AntePat)[k+(NumAnte*LevelAntepatt)];
				SuppCons=(float)INTEGER(TotalPat)[h+(NumTotal*LevelTotalpatt)]; 
				if (Confidence<=(SuppCons/SuppAnte))
					{
					/* Save position Ante, position Consecuent, Support, Confidence */
					*(pattern+(countrules*4))=k+2;
					*(pattern+1+(countrules*4))=h+2;
					*(pattern+2+(countrules*4))=(int)SuppAnte;
					*(pattern+3+(countrules*4))=(int)1000.0*(SuppCons/SuppAnte);
											
					countrules++;
					if (printme==3) Rprintf("\nCounrules=%d",countrules);
					if ((countrules*4)>=limitpatt)
						{
						limitpatt+=blockpatt;
						pattern=(int *)S_realloc((char *)pattern, limitpatt, limitpatt-blockpatt, sizeof(int));
						if (pattern==NULL)
							{
							Rprintf("\n#############\nREALLOCATING PATTERN MEMORY ERROR (Size=%ld Bytes)!!!\n#############\n",limitpatt*sizeof(int));
							return(VectResult);
							}
						}	
					}
				break;
				}
			}
		}
	
	/* ALLOC MATRIX */
	PROTECT(VectResult=allocMatrix(INTSXP, countrules, 4));
	
	/* Save rules with meet confidence */
	
	for (j=0; j<countrules; j++)
		{
		INTEGER(VectResult)[j]=*(pattern+(j*4));
		INTEGER(VectResult)[j+countrules]=*(pattern+1+(j*4));
		INTEGER(VectResult)[j+(2*countrules)]=*(pattern+2+(j*4));
		INTEGER(VectResult)[j+(3*countrules)]=*(pattern+3+(j*4));
		}	
	
	UNPROTECT(1);
	return(VectResult);
}



/*Find length of the frames
SEXP divideFrames(SEXP zerospos, int intervals)
{
int i=1;
SEXP aux;
PROTECT(aux=allocVector(INTSXP,length(zerospos)));
aux[0]=zerospos[0];
while (i<length(zerospos))
	{
	
	i++;	
	}

UNPROTECT(aux);
return();
}
*/






