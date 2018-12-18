#include "init.h"
#include "annoy.h"

template<class Distance>
SEXP build_annoy_internal (SEXP mat, SEXP ntrees, SEXP fname) {
    Rcpp::NumericMatrix Mat(mat);
    const int ndim=Mat.nrow();
    const int ncells=Mat.ncol();

    typename Annoy<Distance>::_index obj(ndim);
    std::vector<typename Annoy<Distance>::Data_t> tmp(ndim);
    auto mIt=Mat.begin();
    for (int i=0; i<ncells; ++i, mIt+=ndim) {
        std::copy(mIt, mIt+ndim, tmp.begin());
        obj.add_item(i, tmp.data());
    }

    const int Ntrees=check_integer_scalar(ntrees, "number of trees");
    obj.build(Ntrees);

    auto Fname=check_string(fname, "index file name");
    obj.save(Fname.c_str());

    return R_NilValue;
}

SEXP build_annoy(SEXP mat, SEXP ntrees, SEXP fname) {
    BEGIN_RCPP
    return build_annoy_internal<Euclidean>(mat, ntrees, fname);
    END_RCPP
}
