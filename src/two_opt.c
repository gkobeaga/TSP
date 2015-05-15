
#include <R.h>
#include <Rdefines.h>

#include "matrix_pos.h"


/* 2-opt 
 *
 * R_matrix ... matrix with distances
 * R_t .. tour
 *
 * inf and NA is not allowed!
 *
 */

SEXP two_opt(SEXP R_matrix, SEXP R_t) {
  
    int i, j, n;
    int swaps;
    int swap1, swap2;
    double e1, e2, e1_swap, e2_swap;
    double imp, cur_imp;
    int tmp;
    double *matrix = REAL(R_matrix);
    int *t = INTEGER(R_t);

    
    // check   
    n = INTEGER(GET_DIM(R_matrix))[0];
    if (LENGTH(R_t) != n)
        error("tour has invalid length");

    for (i = 0; i < n; i++)
        if (t[i] < 1 || t[i] > n)
            error("tour contains invalid values");

    // make a copy
    PROTECT(R_t = duplicate(R_t));
    t = INTEGER(R_t);

    // main loop
    do{
        swaps = 0;
        imp = 0.0;

        for (i = 0; i < (n-2); i++){
            e1 = matrix[M_POS(n, t[i]-1, t[i+1]-1)]; 

            for (j = (i+1); j < (n-1); j++){
                e2 = matrix[M_POS(n, t[j]-1, t[j+1]-1)];
                e1_swap = matrix[M_POS(n, t[i]-1, t[j]-1)];
                e2_swap = matrix[M_POS(n, t[i+1]-1, t[j+1]-1)];
                
                
                /* // handle pos inf
                if (e1_swap == R_PosInf || e2_swap == R_PosInf) 
                    cur_imp = 0;
                else if (e1 == R_PosInf || e2 == R_PosInf) 
                    cur_imp = R_PosInf;
                else */
                
                cur_imp = (e1+e2) - (e1_swap+e2_swap);

                if(cur_imp > 0) {
                    swaps++;
                    if(cur_imp > imp) {
                        imp = cur_imp;
                        swap1 = i+1; swap2 = j;
                    }
                }
            }
        
            // swap including last city 
            e2 = matrix[M_POS(n, t[n-1]-1, t[0]-1)];
            e1_swap = matrix[M_POS(n, t[i]-1, t[n-1]-1)];
            e2_swap = matrix[M_POS(n, t[i+1]-1, t[0]-1)];

            cur_imp = (e1+e2) - (e1_swap+e2_swap);

            if(cur_imp > 0) {
                swaps++;
                if(cur_imp > imp) {
                    imp = cur_imp;
                    swap1 = i+1; swap2 = n-1;
                }
            }
        }

        //printf("%d possible swaps\n", swaps);
        //printf("%f best improvement\n", imp);
        //printf("swapping %d to %d\n", swap1, swap2);
        
        // invert
        if(swaps > 0){
            for(i = 0; i < (swap2-swap1+1)/2; i++) { // +1 for even length
                tmp = t[swap1+i];
                t[swap1+i] = t[swap2-i];
                t[swap2-i] = tmp;
            }
        }

        R_CheckUserInterrupt();
    }while (swaps >0);

    UNPROTECT(1);
    return R_t;
}


