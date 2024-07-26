#ifndef GENERICS_H
#define GENERICS_H

#include "Rcpp.h"
#include "knncolle/knncolle.hpp"

typedef knncolle::SimpleMatrix<int, int, double> WrappedMatrix;

typedef knncolle::Builder<WrappedMatrix, double> BiocNeighborsBuilder;

typedef knncolle::Prebuilt<int, int, double> BiocNeighborsPrebuilt;

typedef Rcpp::XPtr<BiocNeighborsPrebuilt> BiocNeighborsPrebuiltPointer;

SEXP generic_build(const BiocNeighborsBuilder&, Rcpp::NumericMatrix);

#ifndef _OPENMP
#include <thread>

template<typename Function_>
void generic_parallelize(int njobs, int nthreads, Function_ fun) {
    if (nthreads <= 1) {
        fun(0, njobs);
        return;
    }

    int jobs_per_thread = (njobs / nthreads) + (njobs % nthreads > 0);
    std::vector<std::thread> jobs;
    jobs.reserve(nthreads);

    for (int j = 0; j < nthreads; ++j) {
        int start = jobs_per_thread * j;
        if (start >= njobs) {
            break;
        }
        int length = std::min(njobs - start, jobs_per_thread);
        jobs.emplace_back(fun, start, length);
    }

    for (auto& j : jobs) {
        j.join();
    }
}
#endif

#endif
