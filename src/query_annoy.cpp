#include "init.h"
#include "annoy.h"

SEXP find_annoy (SEXP to_check, SEXP query, SEXP ndims, SEXP fname, SEXP nn, SEXP get_index, SEXP get_distance) {
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
    std::vector<double> kept_dist;
    kept_dist.reserve(K);

    auto iptr=(store_neighbors ? &kept_index : NULL);
    auto dptr=(store_distances ? &kept_dist : NULL);

    // Running through all points.
    for (auto c : points) {
        auto qIt=Query.begin() + c * Ndim;
        obj.get_nns_by_vector(qIt, K, -1, iptr, dptr);
        if (store_neighbors) {
            std::copy(iptr->begin(), iptr->end(), oiIt);
            iptr->clear();
            oiIt+=K;
        }
        if (store_distances) {
            std::copy(dptr->begin(), dptr->end(), odIt);
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
