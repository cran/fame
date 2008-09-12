#include "R.h"

void dummyFameFunction(int *status){
  return;
}

#ifdef fame

#ifdef WIN32
#include "fame.h"
#else
#include "hli.h"
#endif

/* R CMD check complains if any foreign function calls do not use the
   'PACKAGE' argument to restrict the symbol lookup to the package dll.
   But on Windows the chli functions reside only in the FAME-supplied
   chli.dll, not the package dll built by R CMD INSTALL, so they are not
   found if PACKAGE = "fame" is set.  To get around this, I don't call 
   chli function directly from R, only from C, even though it means I had 
   to write stupid wrapper functions fameInit, fameOpenWorkDb, and fameStop. 
*/ 

void fameInit(int *status){
  cfmini(status);
  return;
}

void fameOpenWorkDb(int *status, int *key){
  cfmopwk(status, key);
  return;
}

void fameStop(int *status){
  cfmfin(status);
  return;
}

void fameCommand(int *status, char **command, char **errorMsg){
  int errStatus = 0;
  cfmfame(status, *command);
  if(*status == HFAMER)
	  cfmferr(&errStatus, *errorMsg);
  return;
}

void fameOpenDatabase(int *status, int *key, char **name, int *mode){
  cfmopdb(status, key, *name, *mode);
  return;
}

void fameCloseDatabase(int *status, int *key){
  cfmcldb(status, *key);
  return;
}

void fameSetRange(int *status, int *freq, 
				  int *fyear, int *fprd, int *lyear, int *lprd, 
				  int *obs, int *range){
  cfmsrng(status, *freq, fyear, fprd, lyear, lprd, range, obs);
  return;
}

void fameDateFromYearMonthDay(int *status, int *freq, int *date,
					 int *year, int *month, int*day){
  cfmddat(status, *freq, date, *year, *month, *day);
  return;
}

void yearMonthDayFromFameDate(int *status, int *freq, int *date,
							  int *year, int *month, int *day){
  cfmdatd(status, *freq, *date, year, month, day);
}

void fameWhat(int *status, int *dbkey, char **objnam, int *class,
			 int *type, int *freq, int *basis, int *observ,
			 int *fyear, int *fprd, int *lyear, int *lprd, 
			 int *obs, int *range, 
			 int * getdoc, char **desPtr, char **docPtr){
  /* Get info about an object. Note that range should be an int[3] on input */
  int cyear, cmonth, cday, myear, mmonth, mday;
  int i;
  char fdes[256], fdoc[256];
  
  if(*getdoc){
	if(strlen(*desPtr) < 256 || strlen(*docPtr) < 256){
	  *status = HBNCHR;
	  return;
	}
	for(i = 0; i < 255; ++i) fdes[i] = fdoc[i] = ' ';
  }
  fdes[255] = fdoc[255] =  '\0';
  
  cfmwhat(status, *dbkey, *objnam, class, type, freq, basis, observ,
		  fyear, fprd, lyear, lprd, &cyear, &cmonth, &cday, &myear,
		  &mmonth, &mday, fdes, fdoc);
  if(*getdoc){
	strncpy(*desPtr, fdes, 256);
	strncpy(*docPtr, fdoc, 256);
  }
  if(*status == 0 && *class == HSERIE)
    cfmsrng(status, *freq, fyear, fprd, lyear, lprd, range, obs);
  return;
}

void fameReadIntegerSeries(int *status, int *dbKey, char **objnam,
						   int *range, int *valary){
  int mistt[3];
  cfmsbm(status, NA_INTEGER, NA_INTEGER, NA_INTEGER, mistt);
  cfmrrng(status, *dbKey, *objnam, range, valary, HTMIS, mistt);
  return;
}

void fameReadPrecisionSeries(int *status, int *dbKey, char **objnam,
							  int *range, double *valary){
  double mistt[3];
  cfmspm(status, R_NaN, NA_REAL, NA_REAL, mistt);
  cfmrrng(status, *dbKey, *objnam, range, valary, HTMIS, mistt);
  return;
}

void fameReadNumericSeries(int *status, int *dbKey, char **objnam,
						   int *range, double *valary){
  /* some extra work here to return array of doubles, rather than floats */
  int i, obs;
  float mistt[3], *farray, fval;
  obs = range[2] - range[1] + 1;
  cfmsnm(status, FNUMNC, FNUMNA, FNUMNA, mistt);
  farray = (float *) calloc(obs, sizeof(float));
  cfmrrng(status, *dbKey, *objnam, range, farray, HTMIS, mistt);
  for(i = 0; i < obs; ++i){
	fval = farray[i];
	if(fval == FNUMNA) valary[i] = NA_REAL;
    else {
	  if(fval == FNUMNC) valary[i] = R_NaN;
	  else               valary[i] = fval;
	}
  }
  free(farray);
  return;
}

void fameGetStringLengths(int *status, int *dbKey, char **objnam,
						  int *range, int *lenary){
  cfmlsts(status, *dbKey, *objnam, range, lenary);
  return;
}

void fameReadStringSeries(int *status, int *dbKey, char **objnam,
						  int *range, char **strary, int *strlength){
  int obs, inlen, *misary, i;
  int *outlen = 0; /* a NULL ptr */
  char naString[] = "NA";
  if(*strlength < 3){ 
	/* need at least 3 chars to hold null-terminated naString */
	*status = HBLEN;
	return;
  }
  obs = range[2] - range[1] + 1;
  inlen = 1 - *strlength;
  misary = (int *) calloc(obs, sizeof(int));
  cfmgtsts(status, *dbKey, *objnam, range, strary, misary, &inlen, outlen);
  for(i = 0; i < obs; ++i){
	if(misary[i] != HNMVAL) strcpy(strary[i], naString);
  }
  free(misary);
  return;
}

void fameInitializeWildcard(int *status, int *dbKey, char **wildcard){
  cfminwc(status, *dbKey, *wildcard);
  return;
}

void fameGetNextMatch(int *status, int *dbKey, char **name,
					  int *class, int *type, int *freq){
  cfmnxwc(status, *dbKey, *name, class, type, freq);
  return;
}

void fameDeleteObject(int *status, int *dbKey, char **objnam){
  /* delete an object from the database */
  cfmdlob(status, *dbKey, *objnam);
  return;
}

void fameWriteRange(int *status, int *dbKey, char **objnam, 
					int *freq, int *type, 
					int *basis, int *observ, 
					int *startYear, int *startPeriod, int *len, 
					char **desc, char **doc, 
					double *data,	
					int *update, int *checkBasisAndObserved){
  /* write a range of data for a series to a database. startYear, startPeriod
     and len are given. Basis and observed can be 0, in which case existing
     values will be used, or the defaults (daily, averaged) if the series
     doesn't already exist in the database. Updates of existing series affect
     only the series data, not its attributes. 
     incoming data should always be array of doubles, but can be written to a
     boolean or a numeric if type argument so specifies.
*/  
  int eclass, etype, efreq, ebasis, eobserv,
	cyear, cmonth, cday, 
	myear, mmonth, mday, 
	lyear, lprd,
	fyear, fprd, 
	obj_exists, temp, range[3], imistt[3], i, *idata;
  float  fmistt[3], *fdata;
  double dmistt[3];
  char tempname[] = "FI_TEMP_OBJ";
  char descjunk[] = " ";
  char docjunk[] = " ";
  
  fyear = *startYear;
  fprd  = *startPeriod;
  /* get info on existing object (if any) with the same name */
  cfmwhat(status, *dbKey, *objnam, 
		  &eclass, &etype, &efreq, &ebasis, &eobserv, 
		  &fyear, &fprd, &lyear, &lprd, 
		  &cyear, &cmonth, &cday, &myear, &mmonth, &mday, 
		  descjunk, docjunk);
  obj_exists = !*status;
  if((!obj_exists) && (*status != HNOOBJ)) return;
  
  /* If basis or observ == 0, fill them in */
  if(!*basis){
    if(obj_exists) 
      *basis = ebasis;
    else
      *basis = HBSDAY;
  }
  if(!*observ){
    if(obj_exists)
      *observ = eobserv;
    else
      *observ = HOBAVG;  /*Averaged*/
  }

  if(*update){
    if(obj_exists){
      /* consistency checks */
      if(eclass  != HSERIE ){ *status = HBCLAS; return; }
      if(etype   != *type  ){ *status = HBOBJT; return; }
      if(efreq   != *freq  ){ *status = HBFREQ; return; }
	  if(*checkBasisAndObserved){
		if(ebasis  != *basis ){ *status = HBBASI; return; }
		if(eobserv != *observ){ *status = HBOBSV; return; }
	  }
    }
    else {  /* create it */
      cfmnwob(status, *dbKey, *objnam, HSERIE, *freq, *type, *basis, *observ);
    }
  }
  else { 
    /* Rename existing object so it can be recovered. */
    cfmdlob(status, *dbKey, tempname); /* removing fi_temp_obj */
    if(obj_exists){
      cfmrnob(status, *dbKey, *objnam, tempname);  /* rename to fi_temp_obj */
      if(*status) return;
    }
    /* Create the new object */
    cfmnwob(status, *dbKey, *objnam, HSERIE, *freq, *type, *basis, *observ);
    if(*status){
      if(obj_exists) /* attempt to put fi_temp_obj back*/
		cfmrnob(&temp, *dbKey, tempname, *objnam);
      return;
    }
  }

  /* Add documentation and description attributes iff this is NOT an update to */
  /* an existing object AND the doc or desc was passed in. */
  if(!(*update && obj_exists)){
    if(strlen(*desc)){
      cfmsdes(status, *dbKey, *objnam, *desc); 
      if(*status) return;
    }
    if(strlen(*doc)){
      cfmsdoc(status, *dbKey, *objnam, *doc);  
      if(*status) return;
    }
  }

  /* The object exists and has correct attributes. */
  /*   set the range */
  lyear = lprd = -1;
  cfmsrng(status, *freq, startYear, startPeriod, &lyear, &lprd, range, len);
  if(*status) return;
  switch(*type){
  case HBOOLN:
    /* translate R NA's to Fame NA's */
	idata = (int *) malloc(*len * sizeof(int));
	for(i = 0; i < *len; ++i){
	  if(ISNA(data[i])) idata[i] = FBOONA;
	  else {
		if(ISNAN(data[i])) idata[i] = FBOONC;
		else idata[i] = (data[i] != 0);
	  }
	}
	cfmwrng(status, *dbKey, *objnam, range, idata, HNTMIS, imistt);
	free(idata);
    break;
  case HNUMRC:
	fdata = (float *) malloc(*len * sizeof(float));
	for(i = 0; i < *len; ++i){
	  if(ISNA(data[i])) fdata[i] = FNUMNA;
	  else {
		if(!R_FINITE(data[i])) fdata[i] = FNUMNC;
		else fdata[i] = data[i];
	  }
	}
	cfmwrng(status, *dbKey, *objnam, range, fdata, HNTMIS, fmistt);
	free(fdata);
    break;
  case HPRECN:
	for(i = 0; i < *len; ++i){
	  if(ISNA(data[i]))           data[i] = FPRCNA;
	  else if(!R_FINITE(data[i])) data[i] = FPRCNC;
	}
	cfmwrng(status, *dbKey, *objnam, range, data, HNTMIS, dmistt);
    break;
  default:
    *status = HBOBJT;
    break;
  }
  if(*status & obj_exists & !*update) /* attempt to put fi_temp_obj back */
    cfmrnob(&temp, *dbKey, tempname, *objnam);
  /* delete temp object if it's still around */
  cfmdlob(&temp, *dbKey, tempname);
  return;
}

void fameEval(int *status, int *dbkey, char *expr, char *optns,
			  int *wdbkey, char *objnam){
  cfmrmev(status, *dbkey, expr, optns, *wdbkey, objnam);
}
 
#endif /* fame */
