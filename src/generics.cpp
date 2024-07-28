#include "generics.h"
#include "Rcpp.h"
#include "knncolle/knncolle.hpp"

#include "l2norm.h"

#include <algorithm>
#include <vector>

#ifdef _OPENMP
#include "omp.h"
#endif

BiocNeighbors::PrebuiltPointer generic_build(const BiocNeighborsBuilder& builder, Rcpp::NumericMatrix data) {
    auto out = BiocNeighbors::PrebuiltPointer(new BiocNeighbors::Prebuilt, true);
    out->index.reset(builder.build_raw(WrappedMatrix(data.rows(), data.cols(), data.begin())));
    return out;
}

/*********************************
 ********* KNN functions *********
 *********************************/

static int sanitize_k(int k, int nobs) {
    if (k < nobs) {
        return k;
    }

    Rcpp::warning("'k' capped at the number of observations minus 1");
    if (nobs >= 1) {
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

//[[Rcpp::export(rng=false)]]
SEXP generic_find_knn(SEXP prebuilt_ptr, int k, int num_threads, bool report_index, bool report_distance) {
    const auto& prebuilt = *(BiocNeighbors::PrebuiltPointer(prebuilt_ptr)->index);
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
                for (auto& i : tmp_i) { // getting back to 1-based indices on output.
                    ++i;
                }
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

//[[Rcpp::export(rng=false)]]
SEXP generic_find_knn_subset(SEXP prebuilt_ptr, Rcpp::IntegerVector chosen, int k, int num_threads, bool report_index, bool report_distance) {
    const auto& prebuilt = *(BiocNeighbors::PrebuiltPointer(prebuilt_ptr)->index);
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

            int i = chosen_ptr[o] - 1; // get to 0-based index.
            searcher->search(i, k, tmp_i_ptr, tmp_d_ptr);
            if (report_index) {
                for (auto& i : tmp_i) { // getting back to 1-based indices on output.
                    ++i;
                }
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

//[[Rcpp::export(rng=false)]]
SEXP generic_query_knn(SEXP prebuilt_ptr, Rcpp::NumericMatrix query, int k, int num_threads, bool report_index, bool report_distance) {
    const BiocNeighbors::Prebuilt& bnp = *(BiocNeighbors::PrebuiltPointer(prebuilt_ptr));
    const auto& prebuilt = *(bnp.index);
    int nobs = prebuilt.num_observations();
    size_t ndim = prebuilt.num_dimensions();
    bool do_cosine = bnp.cosine;

    if (k > nobs) {
        Rcpp::warning("'k' capped at the number of observations");
        k = nobs;
    }

    int nquery = query.ncol();
    const double* query_ptr = query.begin();
    if (static_cast<size_t>(query.nrow()) != ndim) {
        throw std::runtime_error("mismatch in dimensionality between index and 'query'");
    }

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
        std::vector<double> cosine_normalized(do_cosine ? ndim : 0);

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

            auto current_ptr = query_ptr + query_offset;
            if (do_cosine) {
                auto norm_ptr = cosine_normalized.data();
                std::copy_n(current_ptr, ndim, norm_ptr);
                l2norm(norm_ptr, ndim);
                current_ptr = norm_ptr;
            }

            searcher->search(current_ptr, k, tmp_i_ptr, tmp_d_ptr);
            if (report_index) {
                for (auto& i : tmp_i) { // getting back to 1-based indices on outpu.
                    ++i;
                }
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

/***********************************
 ********* Range functions *********
 ***********************************/

template<typename Vector_, typename Value_>
Rcpp::List format_range_output(const std::vector<std::vector<Value_> >& results) {
    Rcpp::List output(results.size());
    for (size_t r = 0, end = results.size(); r < end; ++r) {
        output[r] = Vector_(results[r].begin(), results[r].end());
    }
    return output;
}

//[[Rcpp::export(rng=false)]]
SEXP generic_find_all(SEXP prebuilt_ptr, Rcpp::NumericVector thresholds, int num_threads, bool report_index, bool report_distance) {
    const auto& prebuilt = *(BiocNeighbors::PrebuiltPointer(prebuilt_ptr)->index);
    int nobs = prebuilt.num_observations();

    std::vector<std::vector<double> > out_d(report_distance ? nobs : 0);
    std::vector<std::vector<int> > out_i(report_index ? nobs : 0);

    bool store_count = !report_distance && !report_index;
    Rcpp::IntegerVector counts(store_count ? nobs : 0);
    int* counts_ptr = counts.begin();

    int nthresholds = thresholds.size();
    bool multiple_thresholds = (nthresholds != 1);
    if (multiple_thresholds && nthresholds != nobs) {
        throw std::runtime_error("'threshold' should have length equal to the number of observations");
    }
    const double* threshold_ptr = thresholds.begin();

    bool no_support = false;

#ifdef _OPENMP
    #pragma omp parallel num_threads(num_threads)
    {
#else
    generic_parallelize(nobs, num_threads, [&](int start, int length) {
#endif

        auto searcher = prebuilt.initialize();
        if (!searcher->can_search_all()) {
            // Make sure only the first thread edits the failure variable.
#ifdef _OPENMP
            if (omp_get_thread_num() == 0) {
#else
            if (start == 0) {
#endif
                no_support = true;
            }

        } else {
#ifdef _OPENMP
            #pragma omp for
            for (int o = 0; o < nobs; ++o) {
#else
            for (int o = start, end = start + length; o < end; ++o) {
#endif

                auto count = searcher->search_all(
                    o,
                    threshold_ptr[multiple_thresholds ? o : 0],
                    (report_index ? &out_i[o] : NULL),
                    (report_distance ? &out_d[o] : NULL)
                ); 
                if (store_count) {
                    counts_ptr[o] = count;
                } else if (report_index) {
                    for (auto& i : out_i[o]) {
                        ++i; // get back to 1-based indexing.
                    }
                }
            }
        }

#ifdef _OPENMP
    }
#else
    });
#endif

    if (no_support) {
        throw std::runtime_error("algorithm does not support search by distance");
    }

    if (store_count) {
        return counts;
    } else {
        return Rcpp::List::create(
            Rcpp::Named("index") = format_range_output<Rcpp::IntegerVector>(out_i),
            Rcpp::Named("distance") = format_range_output<Rcpp::NumericVector>(out_d)
        );
    }
} 

//[[Rcpp::export(rng=false)]]
SEXP generic_find_all_subset(SEXP prebuilt_ptr, Rcpp::IntegerVector chosen, Rcpp::NumericVector thresholds, int num_threads, bool report_index, bool report_distance) {
    const auto& prebuilt = *(BiocNeighbors::PrebuiltPointer(prebuilt_ptr)->index);

    const int* chosen_ptr = chosen.begin();
    int nchosen = chosen.size();

    std::vector<std::vector<double> > out_d(report_distance ? nchosen : 0);
    std::vector<std::vector<int> > out_i(report_index ? nchosen : 0);

    bool store_count = !report_distance && !report_index;
    Rcpp::IntegerVector counts(store_count ? nchosen : 0);
    int* counts_ptr = counts.begin();

    int nthresholds = thresholds.size();
    bool multiple_thresholds = (nthresholds != 1);
    if (multiple_thresholds && nthresholds != nchosen) {
        throw std::runtime_error("'threshold' should have length equal to 'subset'");
    }
    const double* threshold_ptr = thresholds.begin();

    bool no_support = false;

#ifdef _OPENMP
    #pragma omp parallel num_threads(num_threads)
    {
#else
    generic_parallelize(nchosen, num_threads, [&](int start, int length) {
#endif

        auto searcher = prebuilt.initialize();
        if (!searcher->can_search_all()) {
            // Make sure only the first thread edits the failure variable.
#ifdef _OPENMP
            if (omp_get_thread_num() == 0) {
#else
            if (start == 0) {
#endif
                no_support = true;
            }

        } else {
#ifdef _OPENMP
            #pragma omp for
            for (int o = 0; o < nchosen; ++o) {
#else
            for (int o = start, end = start + length; o < end; ++o) {
#endif

                auto count = searcher->search_all(
                    chosen_ptr[o] - 1,
                    threshold_ptr[multiple_thresholds ? o : 0],
                    (report_index ? &out_i[o] : NULL),
                    (report_distance ? &out_d[o] : NULL)
                ); 
                if (store_count) {
                    counts_ptr[o] = count;
                } else if (report_index) {
                    for (auto& i : out_i[o]) {
                        ++i; // get back to 1-based indexing.
                    }
                }
            }
        }

#ifdef _OPENMP
    }
#else
    });
#endif

    if (no_support) {
        throw std::runtime_error("algorithm does not support search by distance");
    }

    if (store_count) {
        return counts;
    } else {
        return Rcpp::List::create(
            Rcpp::Named("index") = format_range_output<Rcpp::IntegerVector>(out_i),
            Rcpp::Named("distance") = format_range_output<Rcpp::NumericVector>(out_d)
        );
    }
} 

//[[Rcpp::export(rng=false)]]
SEXP generic_query_all(SEXP prebuilt_ptr, Rcpp::NumericMatrix query, Rcpp::NumericVector thresholds, int num_threads, bool report_index, bool report_distance) {
    const BiocNeighbors::Prebuilt& bnp = *(BiocNeighbors::PrebuiltPointer(prebuilt_ptr));
    const auto& prebuilt = *(bnp.index);
    size_t ndim = prebuilt.num_dimensions();
    bool do_cosine = bnp.cosine;

    int nquery = query.ncol();
    const double* query_ptr = query.begin();
    if (static_cast<size_t>(query.nrow()) != ndim) {
        throw std::runtime_error("mismatch in dimensionality between index and 'query'");
    }

    std::vector<std::vector<double> > out_d(report_distance ? nquery : 0);
    std::vector<std::vector<int> > out_i(report_index ? nquery : 0);

    bool store_count = !report_distance && !report_index;
    Rcpp::IntegerVector counts(store_count ? nquery : 0);
    int* counts_ptr = counts.begin();

    int nthresholds = thresholds.size();
    bool multiple_thresholds = (nthresholds != 1);
    if (multiple_thresholds && nthresholds != nquery) {
        throw std::runtime_error("'threshold' should have length equal to 'subset'");
    }
    const double* threshold_ptr = thresholds.begin();

    bool no_support = false;

#ifdef _OPENMP
    #pragma omp parallel num_threads(num_threads)
    {
#else
    generic_parallelize(nquery, num_threads, [&](int start, int length) {
#endif

        auto searcher = prebuilt.initialize();
        if (!searcher->can_search_all()) {
            // Make sure only the first thread edits the failure variable.
#ifdef _OPENMP
            if (omp_get_thread_num() == 0) {
#else
            if (start == 0) {
#endif
                no_support = true;
            }

        } else {
            std::vector<double> cosine_normalized(do_cosine ? ndim : 0);

#ifdef _OPENMP
            #pragma omp for
            for (int o = 0; o < nquery; ++o) {
                size_t query_offset = static_cast<size_t>(o) * ndim; // using size_t to avoid overflow.
#else
            size_t query_offset = static_cast<size_t>(start) * ndim; // using size_t to avoid overflow.
            for (int o = start, end = start + length; o < end; ++o, query_offset += ndim) {
#endif

                auto current_ptr = query_ptr + query_offset;
                if (do_cosine) {
                    auto norm_ptr = cosine_normalized.data();
                    std::copy_n(current_ptr, ndim, norm_ptr);
                    l2norm(norm_ptr, ndim);
                    current_ptr = norm_ptr;
                }

                auto count = searcher->search_all(
                    current_ptr,
                    threshold_ptr[multiple_thresholds ? o : 0],
                    (report_index ? &out_i[o] : NULL),
                    (report_distance ? &out_d[o] : NULL)
                ); 

                if (store_count) {
                    counts_ptr[o] = count;
                } else if (report_index) {
                    for (auto& i : out_i[o]) {
                        ++i; // get back to 1-based indexing.
                    }
                }
            }
        }

#ifdef _OPENMP
    }
#else
    });
#endif

    if (no_support) {
        throw std::runtime_error("algorithm does not support search by distance");
    }

    if (store_count) {
        return counts;
    } else {
        return Rcpp::List::create(
            Rcpp::Named("index") = format_range_output<Rcpp::IntegerVector>(out_i),
            Rcpp::Named("distance") = format_range_output<Rcpp::NumericVector>(out_d)
        );
    }
} 
