#include <string.h>
#include <R.h>
#include <S.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <math.h>
/*BORRAR int j PORQUE ES SOLO PARA HACER PRUEBAS CON VARIABLES GLOBALES*/
int j;
/* Search min and max in a Time Series */
SEXP searchminmax(SEXP Vect)
{
	int h;
	double maxim,minim;
	int LV;
	LV = length(Vect);
	SEXP VectResult;
	PROTECT(VectResult=allocVector(REALSXP, 2));
	minim=+9999999999.0;
	maxim=-9999999999.0;
	for (h=0;h<LV;h++)
		{
		/* Search min and max element in vector */
		if (minim>REAL(Vect)[h]) minim=REAL(Vect)[h];
		if (maxim<REAL(Vect)[h]) maxim=REAL(Vect)[h];		
		}
	/* Save min element */
	REAL(VectResult)[0]=maxim;
	REAL(VectResult)[1]=minim;
	UNPROTECT(1);
	j=1;
	return(VectResult);
}
/*BORRAR PORQUE ES SÓLO PARA HACER PRUEBAS*/
/*
SEXP prueba(SEXP Vect)
{
	int h,a;
	double maxim,minim;
	int LV;
	LV = length(Vect);
	SEXP VectResult1;
	PROTECT(VectResult1=allocVector(REALSXP, 4));
	VectResult1=searchminmax(Vect);
	UNPROTECT(1);
	REAL(VectResult1)[0]=999;		
	j++;
	REAL(VectResult1)[3]=j;
	return(VectResult1);
}*/

