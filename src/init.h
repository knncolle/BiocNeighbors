#ifndef INIT_H
#define INIT_H

#include "Rcpp.h"

extern "C" {

SEXP find_kmknn(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 

SEXP query_kmknn(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 

SEXP range_find_kmknn(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 

SEXP query_neighbors(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP); 

SEXP build_annoy(SEXP, SEXP, SEXP);

SEXP find_annoy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP query_annoy(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP build_vptree(SEXP);

SEXP find_vptree(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

SEXP query_vptree(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

}

#endif 
