/* This is used in solve.tridiag */
#define Abs(x) ((x) >= 0 ? (x) : -(x))
#define Max(a,b) ((a) >= (b) ? (a) : (b))

int dgtsv(int *n, int *nrhs, double *L, double *D, 
		  double *U, double *B, int *ldb, int *info){
  /* System generated locals */
  int bDim1, bOffset, i1, i2;
  double d1, d2;

  /* Local variables */
  static int j, k;
  static double temp, mult;

  /*  DGTSV  solves the equation */
  /*     A*X = B, */
  /*  where A is an N-by-N tridiagonal matrix, by Gaussian elimination with */
  /*  partial pivoting. */
  /*  Note that the equation  A'*X = B  may be solved by interchanging the */
  /*  order of the arguments U and L. */
  /*  Arguments */
  /*  N      (input) INT */
  /*          The order of the matrix A.  N >= 0. */
  /*  NRHS   (input) INT */
  /*          The number of right hand sides, i.e., the number of columns */
  /*          of the matrix B.  NRHS >= 0. */
  /*  L     (input/output) DOUBLE array, dimension(N-1) */
  /*          On entry, L must contain the(n-1) subdiagonal elements of */
  /*          A. */
  /*          On exit, L is overwritten by the(n-2) elements of the */
  /*          second superdiagonal of the upper triangular matrix U from */
  /*          the LU factorization of A, in L(1), ..., L(n-2). */
  /*  D      (input/output) DOUBLE array, dimension(N) */
  /*          On entry, D must contain the diagonal elements of A. */
  /*          On exit, D is overwritten by the n diagonal elements of U. */
  /*  U     (input/output) DOUBLE array, dimension(N-1) */
  /*          On entry, U must contain the(n-1) superdiagonal elements */
  /*          of A. */
  /*          On exit, U is overwritten by the(n-1) elements of the first */
  /*          superdiagonal of U. */
  /*  B      (input/output) DOUBLE array, dimension(LDB,NRHS) */
  /*          On entry, the N-by-NRHS right hand side matrix B. */
  /*          On exit, if INFO = 0, the N-by-NRHS solution matrix X. */
  /*  LDB    (input) INT */
  /*          The leading dimension of the array B.  LDB >= Max(1,N). */
  /*  INFO   (output) INT */
  /*          = 0:  successful exit */
  /*          < 0:  if INFO = -i, the i-th argument had an illegal value */
  /*          > 0:  if INFO = i, U(i,i) is exactly zero, and the solution */
  /*                has not been computed.  The factorization has not been */
  /*                completed unless i = N. */
  /* Parameter adjustments */
  --L;
  --D;
  --U;
  bDim1 = *ldb;
  bOffset = 1+bDim1*1;
  B -= bOffset;

  /* Function Body */
  *info = 0;
  if(*n < 0){
	*info = -1;
  } else if(*nrhs < 0){
	*info = -2;
  } else if(*ldb < Max(1,*n)){
	*info = -7;
  }
  if(*info != 0) return 0;
  if(*n == 0)    return 0;

  i1 = *n-1;
  for(k = 1; k <= i1; ++k){
	if(L[k] == 0.){ /* Subdiagonal is zero, no elimination is required. */
	  if(D[k] == 0.){ /* Diagonal is zero: a unique solution can not be found. */
		*info = k;
		return 0;
	  }
	} else if((d1 = D[k], Abs(d1)) >=(d2 = L[k], Abs(d2))){ 
	  /* No row interchange required */
	  mult = L[k]/D[k];
	  D[k+1] -= mult*U[k];
	  i2 = *nrhs;
	  for(j = 1; j <= i2; ++j)
		B[k+1+j*bDim1] -= mult*B[k+j*bDim1];
	  if(k < *n-1)
		L[k] = 0.;
	} else { /* Interchange rows K and K+1 */
	  mult = D[k]/L[k];
	  D[k] = L[k];
	  temp = D[k+1];
	  D[k+1] = U[k]-mult*temp;
	  if(k < *n-1){
		L[k] = U[k+1];
		U[k+1] = -mult*L[k];
	  }
	  U[k] = temp;
	  i2 = *nrhs;
	  for(j = 1; j <= i2; ++j){
		temp = B[k+j*bDim1];
		B[k+j*bDim1] = B[k+1+j*bDim1];
		B[k+1+j*bDim1] = temp-mult*B[k+1+j*bDim1];
	  }
	}
  }
  if(D[*n] == 0.){
	*info = *n;
	return 0;
  }
  /*     Back solve with the matrix U from the factorization. */
  i1 = *nrhs;
  for(j = 1; j <= i1; ++j){
	B[*n+j*bDim1] /= D[*n];
	if(*n > 1)
	  B[*n-1+j*bDim1] =(B[*n-1+j*bDim1]-U[*n-1]*B[*n+j*bDim1])/D[*n-1];
	for(k = *n-2; k >= 1; --k)
	  B[k+j*bDim1] =(B[k+j*bDim1]-U[k]*B[k+1+j*bDim1]-L[k]*B[k+2+j*bDim1])/D[k];
  }
  return 0;
}

