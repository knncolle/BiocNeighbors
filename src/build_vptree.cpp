#include "init.h"
#include "vptree.h"

SEXP build_vptree (SEXP mat) {
    BEGIN_RCPP
    Rcpp::NumericMatrix Mat(mat);
    VpTree vp(Mat);
    return vp.save();
    END_RCPP
}
