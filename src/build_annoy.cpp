#include "init.h"
#include "annoy.h"

SEXP build_annoy (SEXP mat, SEXP ntrees, SEXP fname) {
    BEGIN_RCPP
    Rcpp::NumericMatrix Mat(mat);
    const int ndim=Mat.nrow();
    const int ncells=Mat.ncol();

    Annoy::_index obj(ndim);
    std::vector<Annoy::Data_t> tmp(ndim);
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
    END_RCPP
}
