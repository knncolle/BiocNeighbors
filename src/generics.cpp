#include "Rcpp.h"
#include "BiocNeighbors.h"
#include "knncolle/knncolle.hpp"

#include "parallel.h"

#include <algorithm>
#include <vector>

#ifdef _OPENMP
#include "omp.h"
#endif

//[[Rcpp::export(rng=false)]]
SEXP generic_build(SEXP builder, Rcpp::NumericMatrix data) {
    auto out = BiocNeighbors::BuilderPointer(builder);
    return BiocNeighbors::PrebuiltPointer(out->build_raw(BiocNeighbors::SimpleMatrix(data.rows(), data.cols(), data.begin())));
}

//[[Rcpp::export(rng=false)]]
int generic_num_obs(SEXP prebuilt_ptr) {
    BiocNeighbors::PrebuiltPointer cast(prebuilt_ptr); 
    if (!R_ExternalPtrAddr(SEXP(cast))) {
        throw std::runtime_error("null pointer to a prebuilt index");
    }
    const auto& prebuilt = *cast;
    return prebuilt.num_observations();
}

/*********************************
 ********* KNN functions *********
 *********************************/

template<typename Value_, typename Matrix_>
Value_* prepare_output(Matrix_& mat, bool report, int k, int nobs) {
    if (report) {
        mat = Matrix_(k, nobs);
        return mat.begin();
    } else {
        return NULL;
    }
}

template<typename Vector_, typename Value_>
Rcpp::List format_range_output(const std::vector<std::vector<Value_> >& results) {
    Rcpp::List output(results.size());
    for (size_t r = 0, end = results.size(); r < end; ++r) {
        output[r] = Vector_(results[r].begin(), results[r].end());
    }
    return output;
}

//[[Rcpp::export(rng=false)]]
SEXP generic_find_knn(
    SEXP prebuilt_ptr,
    Rcpp::IntegerVector num_neighbors,
    bool force_variable_neighbors,
    Rcpp::Nullable<Rcpp::IntegerVector> chosen,
    int num_threads,
    bool last_distance_only,
    bool report_index,
    bool report_distance)
{
    BiocNeighbors::PrebuiltPointer cast(prebuilt_ptr); 
    if (!R_ExternalPtrAddr(SEXP(cast))) {
        throw std::runtime_error("null pointer to a prebuilt index");
    }
    const auto& prebuilt = *cast;
    int nobs = prebuilt.num_observations();

    // Checking if we have to handle subsets.
    int num_output = nobs;
    const int* subset_ptr = NULL;
    Rcpp::IntegerVector subset;
    if (!chosen.isNull()) {
        subset = Rcpp::IntegerVector(chosen);
        for (auto s : subset) {
            if (s <= 0 || s > nobs) {
                throw std::runtime_error("'subset' contains out-of-range indices");
            } 
        }
        subset_ptr = subset.begin();
        num_output = subset.size();
    }

    // Checking that the 'k' is valid.
    auto sanitize_k = [&](int k) -> int {
        if (k < nobs) {
            return k;
        }

        Rcpp::warning("'k' capped at the number of observations minus 1");
        if (nobs >= 1) {
            return nobs - 1;
        } else {
            return 0;
        }
    };

    bool is_k_variable = false;
    int const_k = 0;
    std::vector<int> variable_k;
    if (num_neighbors.size() != 1 || force_variable_neighbors) {
        is_k_variable = true;
        if (static_cast<int>(num_neighbors.size()) != num_output) {
            throw std::runtime_error("length of 'k' must be equal to the number of points in the index or 'subset'");
        }
        variable_k.resize(num_output);
        for (int o = 0; o < num_output; ++o) {
            variable_k[o] = sanitize_k(num_neighbors[o]);
        }
    } else {
        const_k = sanitize_k(num_neighbors[0]);
    }

    // Formatting all the possible output containers.
    Rcpp::IntegerMatrix const_i;
    Rcpp::NumericMatrix const_d;
    Rcpp::NumericVector last_d;
    int* out_i_ptr; 
    double* out_d_ptr; 
    std::vector<std::vector<int> > var_i;
    std::vector<std::vector<double> > var_d;

    if (last_distance_only) {
        last_d = Rcpp::NumericVector(num_output);
        out_d_ptr = last_d.begin();
        report_index = false;
        report_distance = true;

    } else if (is_k_variable) {
        if (report_index) {
            var_i.resize(num_output);
        }
        if (report_distance) {
            var_d.resize(num_output);
        }

    } else {
        out_i_ptr = prepare_output<int>(const_i, report_index, const_k, num_output);
        out_d_ptr = prepare_output<double>(const_d, report_distance, const_k, num_output);
    }

#ifdef _OPENMP
    #pragma omp parallel num_threads(num_threads)
    {
#else
    generic_parallelize(num_output, num_threads, [&](int start, int length) {
#endif

        auto searcher = prebuilt.initialize();
        std::vector<int> tmp_i;
        std::vector<double> tmp_d;

#ifdef _OPENMP
        #pragma omp for
        for (int o = 0; o < num_output; ++o) {
#else
        for (int o = start, end = start + length; o < end; ++o) {
#endif

            searcher->search(
                (subset_ptr != NULL ? subset_ptr[o] - 1 : o), // get subsets to 0-based indexing
                (is_k_variable ? variable_k[o] : const_k),
                (report_index ? &tmp_i : NULL),
                (report_distance ? &tmp_d : NULL)
            );

            if (report_index) {
                for (auto& i : tmp_i) { // getting back to 1-based indices on output.
                    ++i;
                }
                if (is_k_variable) {
                    var_i[o].swap(tmp_i);
                } else {
                    size_t out_offset = static_cast<size_t>(o) * static_cast<size_t>(const_k); // using size_t to avoid overflow.
                    std::copy_n(tmp_i.begin(), const_k, out_i_ptr + out_offset); 
                }
            }

            if (report_distance) {
                if (last_distance_only) {
                    out_d_ptr[o] = (tmp_d.empty() ? 0 : tmp_d.back());
                } else if (is_k_variable) {
                    var_d[o].swap(tmp_d);
                } else {
                    size_t out_offset = static_cast<size_t>(o) * static_cast<size_t>(const_k); // using size_t to avoid overflow.
                    std::copy_n(tmp_d.begin(), const_k, out_d_ptr + out_offset); 
                }
            }

#ifdef _OPENMP
        }
    }
#else
        }
    });
#endif

    if (last_distance_only) {
        return last_d;

    } else if (is_k_variable) {
        return Rcpp::List::create(
            Rcpp::Named("index") = format_range_output<Rcpp::IntegerVector>(var_i),
            Rcpp::Named("distance") = format_range_output<Rcpp::NumericVector>(var_d)
        );

    } else {
        return Rcpp::List::create(
            Rcpp::Named("index") = const_i,
            Rcpp::Named("distance") = const_d
        );
    }
} 

//[[Rcpp::export(rng=false)]]
SEXP generic_query_knn(
    SEXP prebuilt_ptr,
    Rcpp::NumericMatrix query,
    Rcpp::IntegerVector num_neighbors,
    bool force_variable_neighbors,
    int num_threads,
    bool last_distance_only,
    bool report_index,
    bool report_distance)
{
    BiocNeighbors::PrebuiltPointer cast(prebuilt_ptr); 
    if (!R_ExternalPtrAddr(SEXP(cast))) {
        throw std::runtime_error("null pointer to a prebuilt index");
    }
    const auto& prebuilt = *cast;
    int nobs = prebuilt.num_observations();
    size_t ndim = prebuilt.num_dimensions();

    int nquery = query.ncol();
    const double* query_ptr = query.begin();
    if (static_cast<size_t>(query.nrow()) != ndim) {
        throw std::runtime_error("mismatch in dimensionality between index and 'query'");
    }

    // Checking that 'k' is valid.
    auto sanitize_k = [&](int k) -> int {
        if (k <= nobs) {
            return k;
        }
        Rcpp::warning("'k' capped at the number of observations");
        return nobs;
    };

    bool is_k_variable = false;
    int const_k = 0;
    std::vector<int> variable_k;
    if (num_neighbors.size() != 1 || force_variable_neighbors) {
        is_k_variable = true;
        if (static_cast<int>(num_neighbors.size()) != nquery) {
            throw std::runtime_error("length of 'k' must be equal to the number of points in the index or 'subset'");
        }
        variable_k.resize(nquery);
        for (int o = 0; o < nquery; ++o) {
            variable_k[o] = sanitize_k(num_neighbors[o]);
        }
    } else {
        const_k = sanitize_k(num_neighbors[0]);
    }

    // Formatting all the possible output containers.
    Rcpp::IntegerMatrix const_i;
    Rcpp::NumericMatrix const_d;
    Rcpp::NumericVector last_d;
    int* out_i_ptr = NULL; 
    double* out_d_ptr = NULL; 
    std::vector<std::vector<int> > var_i;
    std::vector<std::vector<double> > var_d;

    if (last_distance_only) {
        last_d = Rcpp::NumericVector(nquery);
        out_d_ptr = last_d.begin();
        report_index = false;
        report_distance = true;

    } else if (is_k_variable) {
        if (report_index) {
            var_i.resize(nquery);
        }
        if (report_distance) {
            var_d.resize(nquery);
        }

    } else {
        out_i_ptr = prepare_output<int>(const_i, report_index, const_k, nquery);
        out_d_ptr = prepare_output<double>(const_d, report_distance, const_k, nquery);
    }

#ifdef _OPENMP
    #pragma omp parallel num_threads(num_threads)
    {
#else
    generic_parallelize(nquery, num_threads, [&](int start, int length) {
#endif

        auto searcher = prebuilt.initialize();
        std::vector<int> tmp_i;
        std::vector<double> tmp_d;

#ifdef _OPENMP
        #pragma omp for
        for (int o = 0; o < nquery; ++o) {
            size_t query_offset = static_cast<size_t>(o) * ndim; // using size_t to avoid overflow.
#else
        size_t query_offset = static_cast<size_t>(start) * ndim; // using size_t to avoid overflow.
        for (int o = start, end = start + length; o < end; ++o, query_offset += ndim) {
#endif

            searcher->search(
                query_ptr + query_offset,
                (is_k_variable ? variable_k[o] : const_k),
                (report_index ? &tmp_i : NULL),
                (report_distance ? &tmp_d : NULL)
            );

            if (report_index) {
                for (auto& i : tmp_i) { // getting back to 1-based indices on output.
                    ++i;
                }
                if (is_k_variable) {
                    var_i[o].swap(tmp_i);
                } else {
                    size_t out_offset = static_cast<size_t>(o) * static_cast<size_t>(const_k); // using size_t to avoid overflow.
                    std::copy_n(tmp_i.begin(), const_k, out_i_ptr + out_offset); 
                }
            }

            if (report_distance) {
                if (last_distance_only) {
                    out_d_ptr[o] = (tmp_d.empty() ? 0 : tmp_d.back());
                } else if (is_k_variable) {
                    var_d[o].swap(tmp_d);
                } else {
                    size_t out_offset = static_cast<size_t>(o) * static_cast<size_t>(const_k); // using size_t to avoid overflow.
                    std::copy_n(tmp_d.begin(), const_k, out_d_ptr + out_offset); 
                }
            }

#ifdef _OPENMP
        }
    }
#else
        }
    });
#endif

    if (last_distance_only) {
        return last_d;

    } else if (is_k_variable) {
        return Rcpp::List::create(
            Rcpp::Named("index") = format_range_output<Rcpp::IntegerVector>(var_i),
            Rcpp::Named("distance") = format_range_output<Rcpp::NumericVector>(var_d)
        );

    } else {
        return Rcpp::List::create(
            Rcpp::Named("index") = const_i,
            Rcpp::Named("distance") = const_d
        );
    }
}

/***********************************
 ********* Range functions *********
 ***********************************/

//[[Rcpp::export(rng=false)]]
SEXP generic_find_all(SEXP prebuilt_ptr, Rcpp::Nullable<Rcpp::IntegerVector> chosen, Rcpp::NumericVector thresholds, int num_threads, bool report_index, bool report_distance) {
    BiocNeighbors::PrebuiltPointer cast(prebuilt_ptr); 
    if (!R_ExternalPtrAddr(SEXP(cast))) {
        throw std::runtime_error("null pointer to a prebuilt index");
    }
    const auto& prebuilt = *cast;
    int nobs = prebuilt.num_observations();

    int num_output = nobs;
    const int* subset_ptr = NULL;
    Rcpp::IntegerVector subset;
    if (!chosen.isNull()) {
        subset = Rcpp::IntegerVector(chosen);
        for (auto s : subset) {
            if (s <= 0 || s > nobs) {
                throw std::runtime_error("'subset' contains out-of-range indices");
            } 
        }
        subset_ptr = subset.begin();
        num_output = subset.size();
    }

    std::vector<std::vector<double> > out_d(report_distance ? num_output : 0);
    std::vector<std::vector<int> > out_i(report_index ? num_output : 0);

    bool store_count = !report_distance && !report_index;
    Rcpp::IntegerVector counts(store_count ? num_output : 0);
    int* counts_ptr = counts.begin();

    int nthresholds = thresholds.size();
    bool multiple_thresholds = (nthresholds != 1);
    if (multiple_thresholds && nthresholds != num_output) {
        throw std::runtime_error("'threshold' should have length equal to the number of observations or 'subset'");
    }
    const double* threshold_ptr = thresholds.begin();

    bool no_support = false;

#ifdef _OPENMP
    #pragma omp parallel num_threads(num_threads)
    {
#else
    generic_parallelize(num_output, num_threads, [&](int start, int length) {
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
            for (int o = 0; o < num_output; ++o) {
#else
            for (int o = start, end = start + length; o < end; ++o) {
#endif

                auto count = searcher->search_all(
                    (subset_ptr != NULL ? subset_ptr[o] - 1 : o), // get subsets to 0-based indexing.
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
    BiocNeighbors::PrebuiltPointer cast(prebuilt_ptr); 
    if (!R_ExternalPtrAddr(SEXP(cast))) {
        throw std::runtime_error("null pointer to a prebuilt index");
    }
    const auto& prebuilt = *cast;
    size_t ndim = prebuilt.num_dimensions();

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
#ifdef _OPENMP
            #pragma omp for
            for (int o = 0; o < nquery; ++o) {
                size_t query_offset = static_cast<size_t>(o) * ndim; // using size_t to avoid overflow.
#else
            size_t query_offset = static_cast<size_t>(start) * ndim; // using size_t to avoid overflow.
            for (int o = start, end = start + length; o < end; ++o, query_offset += ndim) {
#endif

                auto current_ptr = query_ptr + query_offset;
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
