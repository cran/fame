/* This file contains dummy entry points for members of the fame chli library.
   It is only needed for the WINDOWS build process and does not need to be
   installed since the Window .onLoad() function picks up the real chli.dll. */
   

#ifdef DUMMY_CALLS

cfmcldb() { return -999999;}
cfmdlo() { return -999999;}
cfmdlob() { return -999999;}
cfmfame() { return -999999;}
cfmferr() { return -999999;}
cfminwc() { return -999999;}
cfmopdb() { return -999999;}
cfmnwob() { return -999999;}
cfmnxwc() { return -999999;}
cfmrmev() { return -999999;}
cfmrnob() { return -999999;}
cfmrrng_f() { return -999999;}
cfmsbm() { return -999999;}
cfmsdes() { return -999999;}
cfmsdoc() { return -999999;}
cfmsnm() { return -999999;}
cfmspm() { return -999999;}
cfmsrng() { return -999999;}
cfmtody() { return -999999;}
cfmwhat() { return -999999;}
cfmwrng_f() { return -999999;}
cfmgtsts() { return -999999;}
cfmlsts() { return -999999;}
cfmdatd() { return -999999;}
cfmddat() { return -999999;}
cfmfin() { return -999999;}
cfmini() { return -999999;}
cfmopwk() { return -999999;}

FNUMNA() { return 0;}
FNUMNC() { return 0;}
FNUMND() { return 0;}
FBOONA() { return 0;}
FBOONC() { return 0;}
FBOOND() { return 0;}
FPRCNA() { return 0;}
FPRCNC() { return 0;}
FPRCND() { return 0;}

#endif
