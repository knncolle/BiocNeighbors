#ifdef USE_ANNOY
#include "annoy.h"

template<class Distance>
Rcpp::RObject build_annoy_internal (Rcpp::NumericMatrix Mat, int Ntrees, const std::string& Fname) {
    const int ndim=Mat.nrow();
    const int ncells=Mat.ncol();

    typename Annoy<Distance>::_index obj(ndim);
    std::vector<typename Annoy<Distance>::Data_t> tmp(ndim);
    auto mIt=Mat.begin();
    for (int i=0; i<ncells; ++i, mIt+=ndim) {
        std::copy(mIt, mIt+ndim, tmp.begin());
        obj.add_item(i, tmp.data());
    }

    obj.build(Ntrees);
    obj.save(Fname.c_str());

    return R_NilValue;
}

// [[Rcpp::export(rng=false)]]
Rcpp::RObject build_annoy(Rcpp::NumericMatrix mat, int ntrees, std::string fname, std::string dtype) {
    if (dtype=="Manhattan") {
        return build_annoy_internal<Manhattan>(mat, ntrees, fname);
    } else {
        return build_annoy_internal<Euclidean>(mat, ntrees, fname);
    }
}

#endif
