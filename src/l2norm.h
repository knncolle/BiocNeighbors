#ifndef L2NORM_H
#define L2NORM_H

#include "Rcpp.h"

void l2norm(double*, size_t);

Rcpp::NumericMatrix l2norm(const Rcpp::NumericMatrix&);

#endif

