#ifndef UTILS_H
#define UTILS_H
#include "Rcpp.h"

// Integer typedefs to avoid grief when comparing types.

typedef R_xlen_t VecSize_t;

typedef int MatDim_t;

typedef int CellIndex_t;

typedef int NumNeighbors_t;

// Input functions.

Rcpp::IntegerVector check_indices(Rcpp::IntegerVector, int);

NumNeighbors_t check_k(int);

Rcpp::NumericVector check_distances(Rcpp::NumericVector, VecSize_t);

#endif
