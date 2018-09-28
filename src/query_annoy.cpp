#include "init.h"
#include "annoy.h"

SEXP query_annoy (SEXP to_check, SEXP query, SEXP ndims, SEXP fname, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    const int Ndim=check_integer_scalar(ndims, "number of dimensions");
    annoyance obj(Ndim);
    auto Fname=check_string(fname, "index file name");
    obj.load(Fname.c_str());

    Rcpp::NumericMatrix Query(query);
    if (size_t(Query.nrow())!=Ndim) {
        throw std::runtime_error("'query' and 'X' have different dimensionality");
    }
    const Rcpp::IntegerVector points=check_indices(to_check, Query.ncol());
    const size_t nobs=points.size();
    const size_t K=check_k(nn);
    
    // Choosing the output mode.
    const bool store_neighbors=check_logical_scalar(get_index, "'get.index'");
    const bool store_distances=check_logical_scalar(get_distance, "'get.distance'");
    Rcpp::NumericMatrix out_dist;
    if (store_distances) { 
        out_dist=Rcpp::NumericMatrix(K, nobs);
    }
    auto odIt=out_dist.begin();
    
    Rcpp::IntegerMatrix out_idx;
    if (store_neighbors) {
        out_idx=Rcpp::IntegerMatrix(K, nobs);
    }
    auto oiIt=out_idx.begin();
                                
    std::vector<int> kept_index;
    kept_index.reserve(K);
    std::vector<ANNOYTYPE> kept_dist;
    kept_dist.reserve(K);

    auto iptr=&kept_index;
    auto dptr=(store_distances ? &kept_dist : NULL);

    // Running through all points.
    std::vector<ANNOYTYPE> tmp(Ndim);
    for (auto c : points) {
        auto qIt=Query.begin() + c * Ndim;
        std::copy(qIt, qIt+Ndim, tmp.begin());

        obj.get_nns_by_vector(tmp.data(), K, -1, iptr, dptr);
        const size_t limit=std::min(K, kept_index.size()); // as the API can yield < K elements.

        if (store_neighbors) {
            std::copy(kept_index.begin(), kept_index.begin() + limit, oiIt);
            iptr->clear();
            oiIt+=K;
        }
        if (store_distances) {
            std::copy(kept_dist.begin(), kept_dist.end() + limit, odIt);
            dptr->clear();
            odIt+=K;
        }
    }

    Rcpp::List output(2, R_NilValue);
    if (store_neighbors) {
        output[0]=out_idx;
    }   
    if (store_distances) {
        output[1]=out_dist;
    }
    return output;
    END_RCPP
}
