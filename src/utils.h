#ifndef UTILS_H
#define UTILS_H
#include "Rcpp.h"

// Integer typedefs to avoid grief when comparing types.

typedef R_xlen_t VecSize_t;

typedef int MatDim_t;

typedef int CellIndex_t;

typedef int NumNeighbors_t;

// Input functions.

bool check_logical_scalar(Rcpp::RObject x, const char* thing);

int check_integer_scalar(Rcpp::RObject x, const char* thing);

double check_numeric_scalar(Rcpp::RObject x, const char* thing);

std::string check_string(Rcpp::RObject x, const char* thing);

Rcpp::IntegerVector check_indices(Rcpp::IntegerVector, int);

NumNeighbors_t check_k(Rcpp::RObject);

Rcpp::NumericVector check_distances(Rcpp::NumericVector, VecSize_t);

#endif
