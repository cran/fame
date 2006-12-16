#include <stdlib.h>

void lintegrate(double *x,         /* (input) x coordinates of knots of F */
		double *y,         /* (input) y coordinates of knots of F */
		int *xlenp,        /* (input) length of x and y */
		double *intervals, /* (input/output) On entry, contains the x
				      coordinates of the endpoints of the
				      (ILEN-1) intervals to integrate over.  On
				      exit, intervals[0]:intervals[ILEN-1]
				      contain the values of the integrals. */
		int *ilenp,        /* length of the vector intervals */
		int *stepfunp,     /* = 0 : F is continuous and piecewise
				            linear over the range of x 
				      = 1 : F is a left-continuous step function
				            over the range of x (Note that the
					    last value of y is never accessed
					    since F is left continuous) */
		int *rulep){       /* rule for extending F beyond the range of x.
				      = 0 : F = 0 outside the range of x
				      = 1 : F is linearly extended beyond the
				      range of x. */
  int xlen, ilen, stepfun, rule, i, j, inc_j;
  double *a, *b, left, right, isum;
  
  /* Put these in a more convenient form. */
  xlen    = *xlenp;
  ilen    = *ilenp;
  stepfun = *stepfunp;
  rule    = *rulep;
  
  /* F over interval i is the line y = a[i]*x + b[i] */
  a = (double *) calloc(xlen + 1, sizeof(double));
  b = (double *) calloc(xlen + 1, sizeof(double));
  
  if(stepfun){  /* Don't bother with a */
    for(i = 1; i < xlen; ++i)  
      b[i] = y[i-1]; 
  }
  else {
    for(i = 1; i < xlen; ++i){
      a[i] = (y[i] - y[i-1]) / (x[i] - x[i-1]);
      b[i] = y[i] - a[i]*x[i];
    }
  }
  
  if(rule == 0)  /* extend F with 0's */
    a[0] = b[0] = a[xlen] = b[xlen] = 0;
  else {  /* linearly extend F */
    a[0]    = a[1];
    b[0]    = b[1];
    a[xlen] = a[xlen-1];
    b[xlen] = b[xlen-1];
  }
  
  /* To do the piecewise integration, we use i to index the intervals over which 
     we want to integrate, while j indexes the segments of F. */
  for(i = j = 0; i < ilen - 1; i++){
    isum = 0;
    left = intervals[i];
    while( j < xlen && x[j] <= left) j++;
    while(1){
      inc_j = j < xlen && x[j] < intervals[i+1];

      if(inc_j)	right = x[j];
      else 	right = intervals[i+1];

      if(stepfun)  /* This special case can go faster. */
	isum += b[j]*(right - left);
      else
	isum += (a[j]/2)*(right*right - left*left) + b[j]*(right - left);

      left = right;

      if(inc_j) j++;
      else break;
    }
    intervals[i] = isum;
  }
  free(a);
  free(b);
  return;
}      
    
    
    
    
