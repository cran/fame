#ifndef HLI_INCLUDED
#define HLI_INCLUDED 1

/* Used only in building Windows fame.dll.  Linux version builds only on a
   machine with FAME installed and uses its hli.h */

#ifdef WIN32
  #define DLLENTRY __declspec(dllexport)
#else
  #define DLLENTRY
#endif

/* Status codes */
#define HNOOBJ	13	/* The given object does not exist.*/
#define HBOBJT	16	/* A bad object type given or object has wrong type.*/
#define HBFREQ	17	/* A bad frequency given or object has wrong frequency.*/
#define HBCLAS	26	/* A bad object class given or object has wrong class. */
#define HBOBSV	27	/* A bad OBSERVED attribute was given.*/
#define HBBASI	28	/* A bad BASIS attribute was given.*/
#define HBNCHR	35	/* A bad number of characters was given.*/
#define HBLEN	70	/* A bad length was given or a string arg too long */
#define HFAMER	513	/* Error from a FAME-like server.*/

/* Other constants */
#define HSERIE    1	/* SERIES	*/
#define HNUMRC    1	/* NUMERIC	*/
#define HBOOLN    3	/* BOOLEAN	*/
#define HPRECN    5	/* PRECISION	*/
#define HBSDAY    1	/* DAILY	*/
#define HOBAVG    3	/* AVERAGED	*/
#define HNTMIS    0	/* Do not translate missing values	*/
#define HTMIS     1	/* Translate missing values		*/
#define HNMVAL    0	/* Normal value; not missing or magic	*/

extern DLLENTRY float FNUMNA ;		/* NUMERIC NA	*/
extern DLLENTRY float FNUMNC ;		/* NUMERIC NC	*/
extern DLLENTRY float FNUMND ;		/* NUMERIC ND	*/
extern DLLENTRY double FPRCNA ;		/* PRECISION NA	*/
extern DLLENTRY double FPRCNC ;		/* PRECISION NC	*/
extern DLLENTRY double FPRCND ;		/* PRECISION ND	*/
extern DLLENTRY int   FBOONA ;		/* BOOLEAN NA	*/
extern DLLENTRY int   FBOONC ;		/* BOOLEAN NC	*/
extern DLLENTRY int   FBOOND ;		/* BOOLEAN ND	*/

#define A(x)	x

DLLENTRY void cfmcldb  A((int *, int));
DLLENTRY void cfmdatd  A((int *, int, int, int *, int *, int *));
DLLENTRY void cfmddat  A((int *, int, int *, int, int, int));
DLLENTRY void cfmdlob  A((int *, int, char *));
DLLENTRY void cfmfame  A((int *, char *));
DLLENTRY void cfmferr  A((int *, char *));
DLLENTRY void cfmfin   A((int *));
DLLENTRY void cfmfrng  A((int *, int, int *, int *, int *, int *, int *, int *, int, int));
DLLENTRY void cfmini   A((int *));
DLLENTRY void cfminwc  A((int *, int, char *));
DLLENTRY void cfmgtsts A((int *, int, char *, const int *, char **, int *, const int *, int *));
DLLENTRY void cfmlsts  A((int *, int, char *, const int *, int *));
DLLENTRY void cfmnwob  A((int *, int, char *, int, int, int, int, int));
DLLENTRY void cfmnxwc  A((int *, int, char *, int *, int *, int *));
DLLENTRY void cfmopwk  A((int *, int *));
DLLENTRY void cfmopdb  A((int *, int *, char *, int));
DLLENTRY void cfmrmev  A((int *, int, char *, char *, int , char *));
DLLENTRY void cfmrnob  A((int *, int, char *, char *));
DLLENTRY void cfmsbm   A((int *, int, int, int, int *));
DLLENTRY void cfmsdes  A((int *, int, char *, char *));
DLLENTRY void cfmsdoc  A((int *, int, char *, char *));
DLLENTRY void cfmsnm   A((int *, float, float, float, float *));
DLLENTRY void cfmspm   A((int *, double, double, double, double *));
DLLENTRY void cfmsrng  A((int *, int, int *, int *, int *, int *, int *, int *));
DLLENTRY void cfmwhat  A((int *, int, char *, int *, int *, int *, int *, int *,
						  int *, int *, int *, int *, int *, int *, int *, int *,
						  int *, int *, char *, char *));

#define cfmrrng(status, dbkey, objnam, range, valary, tmiss, mistt) \
      cfmrrng_f(status, dbkey, objnam, range, valary, tmiss, mistt)
DLLENTRY void cfmrrng_f  (int *, int, char *, const int *, void *, int, void *);

#define cfmwrng(status, dbkey, objnam, range, valary, tmiss, mistt) \
      cfmwrng_f(status, dbkey, objnam, range, valary, tmiss, mistt)
DLLENTRY void cfmwrng_f  (int *, int, char *, const int *, void *, int, void *);

#endif
