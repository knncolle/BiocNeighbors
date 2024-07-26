#ifndef GENERICS_H
#define GENERICS_H

#include "Rcpp.h"
#include "knncolle/knncolle.hpp"

typedef knncolle::SimpleMatrix<int, int, double> WrappedMatrix;

typedef knncolle::Builder<WrappedMatrix, double> BiocNeighborsBuilder;

typedef knncolle::Prebuilt<int, int, double> BiocNeighborsPrebuilt;

typedef Rcpp::XPtr<BiocNeighborsPrebuilt> BiocNeighborsPrebuiltPointer;

#endif
