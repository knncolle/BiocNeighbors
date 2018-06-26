#ifndef UTILS_H
#define UTILS_H
#include "Rcpp.h"

bool check_logical_scalar(Rcpp::RObject x, const char* thing);

int check_integer_scalar(Rcpp::RObject x, const char* thing);

double check_numeric_scalar(Rcpp::RObject x, const char* thing);

const char* check_string(Rcpp::RObject x, const char* thing);

Rcpp::IntegerVector check_indices(Rcpp::RObject, size_t);

#endif
