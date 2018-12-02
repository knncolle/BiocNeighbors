#ifndef UTILS_H
#define UTILS_H
#include "Rcpp.h"

bool check_logical_scalar(Rcpp::RObject x, const char* thing);

int check_integer_scalar(Rcpp::RObject x, const char* thing);

double check_numeric_scalar(Rcpp::RObject x, const char* thing);

std::string check_string(Rcpp::RObject x, const char* thing);

Rcpp::IntegerVector check_indices(Rcpp::IntegerVector, size_t);

size_t check_k(Rcpp::RObject);

Rcpp::NumericVector check_distances(Rcpp::NumericVector, size_t);

void check_ties(bool&, std::deque<size_t>&, std::deque<double>&, size_t);

#endif
