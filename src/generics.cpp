#include "generics.h"
#include "Rcpp.h"
#include "knncolle/knncolle.hpp"

#include <algorithm>
#include <vector>
#ifndef _OPENMP
#include <thread>
#endif

SEXP generic_build(const BiocNeighborsBuilder& builder, Rcpp::NumericMatrix data) {
    return BiocNeighborsPrebuiltPointer(builder.build_raw(WrappedMatrix(data.rows(), data.cols(), data.begin())), true);
} 

static int sanitize_k(int k, int nobs) {
    if (k < nobs) {
        return k;
    } else if (nobs >= 1) {
        return nobs - 1;
    } else {
        return 0;
    }
}

template<typename Value_, typename Matrix_>
Value_* prepare_output(Matrix_& mat, bool report, int k, int nobs) {
    if (report) {
        mat = Matrix_(k, nobs);
        return mat.begin();
    } else {
        return NULL;
    }
}

template<typename Value_>
std::vector<Value_>* prepare_buffer(std::vector<Value_>& buffer, bool report, int k) {
    if (report) {
        buffer.reserve(k);
        return &buffer;
    } else {
        return NULL;
    }
}

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

SEXP generic_find_knn(const BiocNeighborsPrebuiltPointer& prebuilt_ptr, int k, int num_threads, bool report_index, bool report_distance) {
    const BiocNeighborsPrebuilt& prebuilt = *prebuilt_ptr;
    int nobs = prebuilt.num_observations();

    k = sanitize_k(k, nobs);

    Rcpp::IntegerMatrix out_i;
    int* out_i_ptr = prepare_output<int>(out_i, report_index, k, nobs);
    Rcpp::NumericMatrix out_d;
    double* out_d_ptr = prepare_output<double>(out_d, report_distance, k, nobs);

#ifdef _OPENMP
    #pragma omp parallel num_threads(num_threads)
    {
#else
    generic_parallelize(nobs, num_threads, [&](int start, int length) {
#endif

        auto searcher = prebuilt.initialize();
        std::vector<int> tmp_i;
        auto tmp_i_ptr = prepare_buffer(tmp_i, report_index, k);
        std::vector<double> tmp_d;
        auto tmp_d_ptr = prepare_buffer(tmp_d, report_distance, k);

#ifdef _OPENMP
        #pragma omp for
        for (int o = 0; o < nobs; ++o) {
            size_t out_offset = static_cast<size_t>(o) * static_cast<size_t>(k); // using size_t to avoid overflow.
#else
        size_t out_offset = static_cast<size_t>(start) * static_cast<size_t>(k); // using size_t to avoid overflow.
        for (int o = start, end = start + length; o < end; ++o, out_offset += k) {
#endif

            searcher->search(o, k, tmp_i_ptr, tmp_d_ptr);
            if (report_index) {
                std::copy_n(tmp_i.begin(), k, out_i_ptr + out_offset); 
            }
            if (report_distance) {
                std::copy_n(tmp_d.begin(), k, out_d_ptr + out_offset); 
            }

#ifdef _OPENMP
        }
    }
#else
        }
    });
#endif

    return Rcpp::List::create(
        Rcpp::Named("index") = out_i,                
        Rcpp::Named("distance") = out_d
    );
} 

SEXP generic_find_knn_subset(const BiocNeighborsPrebuiltPointer& prebuilt_ptr, Rcpp::IntegerVector chosen, int k, int num_threads, bool report_index, bool report_distance) {
    const BiocNeighborsPrebuilt& prebuilt = *prebuilt_ptr;
    int nobs = prebuilt.num_observations();

    k = sanitize_k(k, nobs);

    const int* chosen_ptr = chosen.begin();
    int nchosen = chosen.size();

    Rcpp::IntegerMatrix out_i;
    int* out_i_ptr = prepare_output<int>(out_i, report_index, k, nchosen);
    Rcpp::NumericMatrix out_d;
    double* out_d_ptr = prepare_output<double>(out_d, report_distance, k, nchosen);

#ifdef _OPENMP
    #pragma omp parallel num_threads(num_threads)
    {
#else
    generic_parallelize(nchosen, num_threads, [&](int start, int length) {
#endif

        auto searcher = prebuilt.initialize();
        std::vector<int> tmp_i;
        auto tmp_i_ptr = prepare_buffer(tmp_i, report_index, k);
        std::vector<double> tmp_d;
        auto tmp_d_ptr = prepare_buffer(tmp_d, report_distance, k);

#ifdef _OPENMP
        #pragma omp for
        for (int o = 0; o < nchosen; ++o) {
            size_t out_offset = static_cast<size_t>(o) * static_cast<size_t>(k); // using size_t to avoid overflow.
#else
        size_t out_offset = static_cast<size_t>(start) * static_cast<size_t>(k); // using size_t to avoid overflow.
        for (int o = start, end = start + length; o < end; ++o, out_offset += k) {
#endif

            searcher->search(chosen_ptr[o], k, tmp_i_ptr, tmp_d_ptr);
            if (report_index) {
                std::copy_n(tmp_i.begin(), k, out_i_ptr + out_offset); 
            }
            if (report_distance) {
                std::copy_n(tmp_d.begin(), k, out_d_ptr + out_offset); 
            }

#ifdef _OPENMP
        }
    }
#else
        }
    });
#endif

    return Rcpp::List::create(
        Rcpp::Named("index") = out_i,                
        Rcpp::Named("distance") = out_d
    );
} 

SEXP generic_query_knn(Rcpp::NumericMatrix query, const BiocNeighborsPrebuiltPointer& prebuilt_ptr, int k, int num_threads, bool report_index, bool report_distance) {
    const BiocNeighborsPrebuilt& prebuilt = *prebuilt_ptr;
    int nobs = prebuilt.num_observations();
    size_t ndim = prebuilt.num_dimensions();

    k = sanitize_k(k, nobs);

    int nquery = query.nrow();
    const double* query_ptr = query.begin();

    Rcpp::IntegerMatrix out_i;
    int* out_i_ptr = prepare_output<int>(out_i, report_index, k, nquery);
    Rcpp::NumericMatrix out_d;
    double* out_d_ptr = prepare_output<double>(out_d, report_distance, k, nquery);

#ifdef _OPENMP
    #pragma omp parallel num_threads(num_threads)
    {
#else
    generic_parallelize(nquery, num_threads, [&](int start, int length) {
#endif

        auto searcher = prebuilt.initialize();
        std::vector<int> tmp_i;
        auto tmp_i_ptr = prepare_buffer(tmp_i, report_index, k);
        std::vector<double> tmp_d;
        auto tmp_d_ptr = prepare_buffer(tmp_d, report_distance, k);

#ifdef _OPENMP
        #pragma omp for
        for (int o = 0; o < nquery; ++o) {
            size_t query_offset = static_cast<size_t>(o) * ndim; // using size_t to avoid overflow.
            size_t out_offset = static_cast<size_t>(o) * static_cast<size_t>(k);
#else
        size_t query_offset = static_cast<size_t>(start) * ndim; // using size_t to avoid overflow.
        size_t out_offset = static_cast<size_t>(start) * static_cast<size_t>(k);
        for (int o = start, end = start + length; o < end; ++o, query_offset += ndim, out_offset += k) {
#endif

            searcher->search(query_ptr + query_offset, k, tmp_i_ptr, tmp_d_ptr);
            if (report_index) {
                std::copy_n(tmp_i.begin(), k, out_i_ptr + out_offset); 
            }
            if (report_distance) {
                std::copy_n(tmp_d.begin(), k, out_d_ptr + out_offset); 
            }

#ifdef _OPENMP
        }
    }
#else
        }
    });
#endif

    return Rcpp::List::create(
        Rcpp::Named("index") = out_i,                
        Rcpp::Named("distance") = out_d
    );
} 


