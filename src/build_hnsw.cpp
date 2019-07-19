#include "hnsw.h"

template<class Space>
Rcpp::RObject build_hnsw_internal (Rcpp::NumericMatrix Mat, int nlinks, int ef_construct, const std::string& Fname) {
    const int ndim=Mat.nrow();
    const int ncells=Mat.ncol();

    Space space(ndim);
    typename Hnsw<Space>::_index obj(&space, ncells, nlinks, ef_construct);

    std::vector<typename Hnsw<Space>::Data_t> tmp(ndim);
    auto mIt=Mat.begin();
    for (int i=0; i<ncells; ++i, mIt+=ndim) {
        std::copy(mIt, mIt+ndim, tmp.begin());
        obj.addPoint(tmp.data(), i);
    }

    obj.saveIndex(Fname);

    return R_NilValue;
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject build_hnsw(Rcpp::NumericMatrix mat, int nlinks, int ef_construct, std::string fname, std::string dtype) {
    if (dtype=="Manhattan") {
        return build_hnsw_internal<L1Space>(mat, nlinks, ef_construct, fname);
    } else {
        return build_hnsw_internal<hnswlib::L2Space>(mat, nlinks, ef_construct, fname);
    }
}
