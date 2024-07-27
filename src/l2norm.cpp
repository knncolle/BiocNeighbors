#include "l2norm.h"
#include "Rcpp.h"
#include <cmath>

void l2norm(double* vec, size_t n) {
    double l2 = 0;
#ifdef _OPENMP
    #pragma omp simd
#endif
    for (size_t i = 0; i < n; ++i) {
        auto val = vec[i];
        l2 += val * val;
    }

    if (l2 > 0) {
        l2 = std::sqrt(l2);
#ifdef _OPENMP
        #pragma omp simd
#endif
        for (size_t i = 0; i < n; ++i) {
            vec[i] /= l2;
        }
    }
}

Rcpp::NumericMatrix l2norm(const Rcpp::NumericMatrix& x, int num_threads) {
    Rcpp::NumericMatrix output = Rcpp::clone(x);
    size_t NR = output.rows();
    size_t NC = output.cols();
    double* optr = output.begin(); 
    for (size_t c = 0; c < NC; ++c) {
        size_t offset = c * NR; 
        l2norm(optr + offset, NR);
    }
    return output;
}
