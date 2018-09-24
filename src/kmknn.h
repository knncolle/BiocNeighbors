#ifndef CYDAR_H
#define CYDAR_H

#include <stdexcept>
#include <algorithm>
#include <memory>
#include <deque>
#include <queue>

#include "Rcpp.h"

extern "C" {

SEXP find_knn(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 

SEXP query_knn(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 

SEXP find_neighbors(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 

SEXP query_neighbors(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 

SEXP build_annoy(SEXP, SEXP, SEXP);

SEXP find_annoy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

}

#endif 
