#ifndef GENERICS_H
#define GENERICS_H

#include "Rcpp.h"
#include "knncolle/knncolle.hpp"
#include "BiocNeighbors.h"

typedef knncolle::SimpleMatrix<int, int, double> WrappedMatrix;

typedef knncolle::Builder<WrappedMatrix, double> BiocNeighborsBuilder;

BiocNeighbors::PrebuiltPointer generic_build(const BiocNeighborsBuilder&, Rcpp::NumericMatrix);

#ifndef _OPENMP
#include <thread>

template<typename Index_, typename Function_>
void generic_parallelize(Index_ njobs, int nthreads, Function_ fun) {
    if (nthreads <= 1) {
        fun(0, njobs);
        return;
    }

    Index_ jobs_per_thread = (njobs / nthreads) + (njobs % nthreads > 0);
    std::vector<std::thread> jobs;
    jobs.reserve(nthreads);

    for (int j = 0; j < nthreads; ++j) {
        Index_ start = jobs_per_thread * j;
        if (start >= njobs) {
            break;
        }
        Index_ length = std::min(njobs - start, jobs_per_thread);
        jobs.emplace_back(fun, start, length);
    }

    for (auto& j : jobs) {
        j.join();
    }
}
#endif

#endif
