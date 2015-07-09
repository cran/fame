/* This file contains dummy entry points for members of the fame chli library.
   It is only needed for the WINDOWS build process and does not need to be
   installed since the Window .onLoad() function picks up the real chli.dll. */
   
void dummyFunctionToAvoidEmptySourceFileWarning() { return;}
#ifdef DUMMY_CALLS

void cfmcldb()   { return;}
void cfmdlo()    { return;}
void cfmdlob()   { return;}
void cfmfame()   { return;}
void cfmferr()   { return;}
void cfminwc()   { return;}
void cfmopcn()   { return;}
void cfmclcn()   { return;}
void cfmgcid()   { return;}
void cfmopdb()   { return;}
void cfmopdc()   { return;}
void cfmnwob()   { return;}
void cfmnxwc()   { return;}
void cfmrmev()   { return;}
void cfmrnob()   { return;}
void cfmrrng_f() { return;}
void cfmsbm()    { return;}
void cfmsdes()   { return;}
void cfmsdoc()   { return;}
void cfmsnm()    { return;}
void cfmspm()    { return;}
void cfmsrng()   { return;}
void cfmtody()   { return;}
void cfmwhat()   { return;}
void cfmwrng_f() { return;}
void cfmgtsts()  { return;}
void cfmlsts()   { return;}
void cfmdatd()   { return;}
void cfmdatt()   { return;}
void cfmddat()   { return;}
void cfmfin()    { return;}
void cfmini()    { return;}
void cfmopwk()   { return;}

float FNUMNA() { return 0;}
float FNUMNC() { return 0;}
float FNUMND() { return 0;}
int FBOONA() { return 0;}
int FBOONC() { return 0;}
int FBOOND() { return 0;}
double FPRCNA() { return 0;}
double FPRCNC() { return 0;}
double FPRCND() { return 0;}

#endif
