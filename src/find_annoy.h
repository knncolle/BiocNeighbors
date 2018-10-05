#include "init.h"
#include "annoy.h"

SEXP find_annoy (SEXP to_check, SEXP ndims, SEXP fname, SEXP nn, SEXP get_index, SEXP get_distance) {
    BEGIN_RCPP
    const int Ndim=check_integer_scalar(ndims, "number of dimensions");
    annoyance obj(Ndim);
    auto Fname=check_string(fname, "index file name");
    obj.load(Fname.c_str());

    auto chosen=check_indices(to_check, obj.get_n_items());
    const size_t nobs=chosen.size();
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
    for (auto c : chosen) { 
        obj.get_nns_by_item(c, K + 1, -1, iptr, dptr); // +1, as it forgets to discard 'self'.

        size_t counter=0;
        for (size_t idx=0; idx<kept_index.size() && counter < K; ++idx) { // protect against API returning more/less NNs.
            if (kept_index[idx]!=c) {
                if (store_neighbors) {
                    *(oiIt+counter)=kept_index[idx]+1; // getting back to 1-based indexing.
                }
                if (store_distances) {
                    *(odIt+counter)=kept_dist[idx];
                }
                ++counter;
            }
        }

        kept_index.clear();
        if (store_neighbors) {
            oiIt+=K;
        }
        if (store_distances) {
            kept_dist.clear();
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
