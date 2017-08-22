#include <string.h>
#include <R.h>
#include <S.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <math.h>
/*index of importantpoints functions*/
int q,cont,last;
SEXP positions,auxpos;
/*************** Functions**********************************************/

SEXP out(SEXP x, SEXP y)
	{	
	int i, j, nx, ny;
	double tmp;
	SEXP ans;
	nx = length(x); ny = length(y);
	PROTECT(ans = allocMatrix(REALSXP, nx, ny));
	for(i = 0; i < nx; i++)
		{
		tmp = REAL(x)[i];
		for(j = 0; j < ny; j++)	REAL(ans)[i + nx*j] = tmp * REAL(y)[j];
		}
	UNPROTECT(1);
	return(ans);
}

/* Obtain where are zerocrossings in a Time Series */
SEXP zerocrossings(SEXP Vect)
{
	int i, LV;
	double Slope1, Slope2;
	SEXP VectResult;
	LV = length(Vect);
	PROTECT(VectResult=allocVector(INTSXP, LV));
	INTEGER(VectResult)[0]=0;
	INTEGER(VectResult)[LV-1]=0;
	for (i=1; i<(LV-1); i++)
		{
			Slope1=REAL(Vect)[i]-REAL(Vect)[i-1];
			Slope2=REAL(Vect)[i+1]-REAL(Vect)[i];
			/* INTEGER(VectResult)[i]=((Slope1<0.0) & (Slope2>=0.0)) | ((Slope1>=0.0) & (Slope2<0.0)); */
			INTEGER(VectResult)[i]=((Slope1<0.0) & (Slope2>=0.0)) | ((Slope1>=0.0) & (Slope2<0.0)) | 
			((Slope1==0.0) & (Slope2>0.0)) | ((Slope1>0.0) & (Slope2==0.0));
		}
	UNPROTECT(1);	
	return(VectResult);
}

/* Make a median filter in a Time Series */
SEXP medianfilter(SEXP Vect, SEXP NumW)
{
	int h,i,j,LV,min,posi,LW;
	double temporal;
	double VectWin[(int)REAL(NumW)[0]];
	LV = length(Vect);
	SEXP VectResult;
	LW=(int)REAL(NumW)[0];
	PROTECT(VectResult=allocVector(REALSXP, LV));
	
	for (h=0;h<LV;h++)
		{
			/* Put elements into the slide window */
			for(i=0;i<LW;i++) 
			{
				posi=i+h-LW/2;
				if (posi>=LV) posi=posi-LV;
				if (posi<0) posi=LV+posi;
				VectWin[i]=REAL(Vect)[posi];
			}
			
			/* Sorts slide window's elements using "Selection Sort" Algorithm */
			for(i=0; i<(LW-1); i++) 
				{
				min=i;
				for(j=i+1; j<LW; j++) 
					{
					if(VectWin[j] < VectWin[min])	min = j;
					}
				temporal=VectWin[i];
				VectWin[i]=VectWin[min];
				VectWin[min]=temporal;
				}
			
		/* Save median element */
		if (LW%2==1)	REAL(VectResult)[h]=VectWin[LW/2];
		else REAL(VectResult)[h]=(VectWin[(LW/2)-1]+VectWin[LW/2])/2.0;
		}
	UNPROTECT(1);	
	return(VectResult);
}

/* Make a min filter in a Time Series */
SEXP minfilter(SEXP Vect, SEXP NumW)
{
	int h,i,LV,posi,LW;
	double min;
	LV = length(Vect);
	SEXP VectResult;
	LW=(int)REAL(NumW)[0];
	PROTECT(VectResult=allocVector(REALSXP, LV));
	
	for (h=0;h<LV;h++)
		{
			/* Search min element in window */
			posi=h-LW/2;
			if (posi>=LV) posi=posi-LV;
			if (posi<0) posi=LV+posi;
			min=REAL(Vect)[posi];
			
			for(i=1;i<LW;i++) 
			{
				posi=i+h-LW/2;
				if (posi>=LV) posi=posi-LV;
				if (posi<0) posi=LV+posi;
				if (REAL(Vect)[posi]<min) min=REAL(Vect)[posi];
			}
			/* Save min element */
			REAL(VectResult)[h]=min;
		}
	UNPROTECT(1);	
	return(VectResult);
}

/* Make a max filter in a Time Series */
SEXP maxfilter(SEXP Vect, SEXP NumW)
{
	int h,i,LV,posi,LW;
	double max;
	LV = length(Vect);
	SEXP VectResult;
	LW=(int)REAL(NumW)[0];
	PROTECT(VectResult=allocVector(REALSXP, LV));
	
	for (h=0;h<LV;h++)
		{
			/* Search min element in window */
			posi=h-LW/2;
			if (posi>=LV) posi=posi-LV;
			if (posi<0) posi=LV+posi;
			max=REAL(Vect)[posi];
			
			for(i=1;i<LW;i++) 
			{
				posi=i+h-LW/2;
				if (posi>=LV) posi=posi-LV;
				if (posi<0) posi=LV+posi;
				if (REAL(Vect)[posi]>max) max=REAL(Vect)[posi];
			}
			/* Save min element */
			REAL(VectResult)[h]=max;
		}
	UNPROTECT(1);	
	return(VectResult);
}

/* Makes a mean filter in a Time Series */
SEXP meanfilter(SEXP Vect, SEXP NumW)
{
	int h,i,LV,posi,LW;
	double mean;
	LV = length(Vect);
	SEXP VectResult;
	LW=(int)REAL(NumW)[0];
	PROTECT(VectResult=allocVector(REALSXP, LV));
	
	for (h=0;h<LV;h++)
		{
			/* Search min element in window */
			mean=0.0;
			
			for(i=0;i<LW;i++) 
			{
				posi=i+h-LW/2;
				if (posi>=LV) posi=posi-LV;
				if (posi<0) posi=LV+posi;
				mean+=REAL(Vect)[posi];
			}
			/* Save min element */
			REAL(VectResult)[h]=mean/LW;
		}
	UNPROTECT(1);	
	return(VectResult);
}


/* Makes a mean filter in a Time Series */
SEXP removefiltermin(SEXP Vect, SEXP NumT)
{
	int h,LV;
	double yval,mint;
	LV = length(Vect);
	SEXP VectResult;
	mint=(int)REAL(NumT)[0];
	PROTECT(VectResult=allocVector(REALSXP, LV));
	yval=REAL(Vect)[0];
	if (yval<mint) yval=mint;
	REAL(VectResult)[0]=yval;
	
	for (h=1;h<LV;h++)
		{
			if (REAL(Vect)[h]<mint)
				REAL(VectResult)[h]=yval;
			else
				{
				REAL(VectResult)[h]=REAL(Vect)[h];
				yval=REAL(Vect)[h];
				}
		}
	UNPROTECT(1);	
	return(VectResult);
}

/* Makes a mean filter in a Time Series */
SEXP removefiltermax(SEXP Vect, SEXP NumT)
{
	int h,LV;
	double yval,maxt;
	LV = length(Vect);
	SEXP VectResult;
	maxt=(int)REAL(NumT)[0];
	PROTECT(VectResult=allocVector(REALSXP, LV));
	yval=REAL(Vect)[0];
	if (yval>maxt) yval=maxt;
	REAL(VectResult)[0]=yval;
	
	for (h=1;h<LV;h++)
		{
			if (REAL(Vect)[h]>maxt)
				REAL(VectResult)[h]=yval;
			else
				{
				REAL(VectResult)[h]=REAL(Vect)[h];
				yval=REAL(Vect)[h];
				}
		}
	UNPROTECT(1);	
	return(VectResult);
}

/* Makes a mean filter in a Time Series */
SEXP removefilterrange(SEXP Vect, SEXP NumT)
{
	int h,LV;
	double yval,mint,maxt;
	LV = length(Vect);
	SEXP VectResult;
	mint=(int)REAL(NumT)[0];
	maxt=(int)REAL(NumT)[1];
	PROTECT(VectResult=allocVector(REALSXP, LV));
	yval=REAL(Vect)[0];
	if (yval<mint) yval=mint;
	if (yval>maxt) yval=maxt;

	REAL(VectResult)[0]=yval;
	
	for (h=1;h<LV;h++)
		{
			if (REAL(Vect)[h]<mint || REAL(Vect)[h]>maxt) REAL(VectResult)[h]=yval;
			else
				{
				REAL(VectResult)[h]=REAL(Vect)[h];
				yval=REAL(Vect)[h];
				}
		}
	UNPROTECT(1);	
	return(VectResult);
}

/* Makes a mean filter in a Time Series */
SEXP removefilterrangeinv(SEXP Vect, SEXP NumT)
{
	int h,LV;
	double yval,mint,maxt;
	LV = length(Vect);
	SEXP VectResult;
	mint=(int)REAL(NumT)[0];
	maxt=(int)REAL(NumT)[1];
	PROTECT(VectResult=allocVector(REALSXP, LV));
	yval=REAL(Vect)[0];
	if (yval>mint && yval<maxt) yval=mint;
	
	REAL(VectResult)[0]=yval;
	
	for (h=1;h<LV;h++)
		{
			if (REAL(Vect)[h]>mint && REAL(Vect)[h]<maxt) REAL(VectResult)[h]=yval;
			else
				{
				REAL(VectResult)[h]=REAL(Vect)[h];
				yval=REAL(Vect)[h];
				}
		}
	UNPROTECT(1);	
	return(VectResult);
}




/*****************************************************************************************************/
/*****************************************************************************************************/
/*****************************************************************************************************/


/*C functions for the kdfilterImportantPoints R function*/
/*Find the first important point */
double findfirst(SEXP vect, float R)
	{	
	int imin=1;
	int imax=1;
	int LV;
	LV = length(vect);
	while((q<LV) & (((REAL(vect)[q]/REAL(vect)[imin])<R) || ((REAL(vect)[imax]/REAL(vect)[q])<R)))
	{
		if(REAL(vect)[q]<REAL(vect)[imin]) 
		imin=q;
			
		if(REAL(vect)[q]>REAL(vect)[imax]) 
		imax=q;
	q++;
	}
	
	if(imin<imax)
		return(REAL(vect)[imax]);
		
	else
		return(REAL(vect)[imin]);
		

}

/*--------------------------------------------------------------------*/

/*Find the first important minimum after the qth point */
double findminimum(SEXP vect, float R)
	{	
	int imin=q;
	int LV;
	LV = length(vect);

	while((q<LV)&((REAL(vect)[q]/REAL(vect)[imin])<R))
	{
		if(REAL(vect)[q]<REAL(vect)[imin]) 
		imin=q;
		q++;
	}
	if((q<LV)&(REAL(vect)[imin]<REAL(vect)[q]))
		{cont=imin;return(REAL(vect)[imin]);}
	return(REAL(vect)[imin]);
}

/*--------------------------------------------------------------------*/

/*Find the first important maximum after the qth point */
double findmaximum(SEXP vect, float R)
{	
	int imax=q;
	int LV;
	LV = length(vect);

	while((q<LV)&((REAL(vect)[imax]/REAL(vect)[q])<R))
	{
		if(REAL(vect)[q]>REAL(vect)[imax]) 
		 imax=q;
		q++;
	}
	if((q<LV)&(REAL(vect)[imax]>REAL(vect)[q]))
		{cont=imax;return(REAL(vect)[imax]);}
	return(REAL(vect)[imax]);
	
}
/*--------------------------------------------------------------------*/

/*protect memory*/
double memo(SEXP vect)
{
int LV;
LV=length(vect);
PROTECT(positions=allocVector(INTSXP,LV));
return(LV);
}
/*--------------------------------------------------------------------*/
/*Top-level function for finding important points.*/
SEXP importantpoints(SEXP vect,SEXP vectorR)
	{	
	float R;
	int i,LV;
	int rep=0;
	q=0;
	i=0;
	R=REAL(vectorR)[0];
	SEXP VectResult,auxvect1;
	LV = length(vect);
	PROTECT(VectResult=allocVector(REALSXP, LV));

	REAL(VectResult)[i]=REAL(vect)[q];
	INTEGER(positions)[i]=(q+1);
	/*Index of VectResult*/
	i++;
	q++;
	/*Looking for the index of first important point*/
	REAL(VectResult)[i]=findfirst(vect,R);
	INTEGER(positions)[i]=q;
	cont=q;
	i++;
	if((q<LV) & (REAL(vect)[q]>REAL(vect)[0])) 
	{REAL(VectResult)[i]=findmaximum(vect,R);INTEGER(positions)[i]=cont+1;}
	i++;
	/*Looking for the alternative minimum and a maximun*/
	while ((i<LV)&(rep==0))
	{	
		if (rep==0)
		REAL(VectResult)[i]=findminimum(vect,R);
		if(rep==0)
		INTEGER(positions)[i]=cont+1;
		if(INTEGER(positions)[i]==INTEGER(positions)[i-1])
		{rep=1;last=i;}
		i++;
		if (rep==0)
		REAL(VectResult)[i]=findmaximum(vect,R);
		if(rep==0)
		INTEGER(positions)[i]=cont+1;
		if(INTEGER(positions)[i]==INTEGER(positions)[i-1])
		{rep=1;last=i;}
		i++;
	}
	PROTECT(auxvect1=allocVector(REALSXP, last));
	PROTECT(auxpos=allocVector(INTSXP, last));

	for (i=0;i<last;i++)
	{
	REAL(auxvect1)[i]=REAL(VectResult)[i];
	}
	for (i=0;i<last;i++)
	{
	INTEGER(auxpos)[i]=INTEGER(positions)[i];
	}
	UNPROTECT(1);
	return(auxvect1);		
}

/*--------------------------------------------------------------------*/

/*Returns the positions of the important points*/
SEXP posfunction(SEXP vect)
	{	
	return(auxpos);
}
/*--------------------------------------------------------------------*/



