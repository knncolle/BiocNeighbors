#include "utils.h"

template<typename T, class V>
T check_scalar_value (Rcpp::RObject val, const char* type, const char* thing) {
    V x(val);
    if (x.size()!=1) {
        std::stringstream err;
        err << "expected " << type << " for the " << thing;
        throw std::runtime_error(err.str().c_str());
    }
    return x[0];
}

bool check_logical_scalar(Rcpp::RObject x, const char* thing) {
    return check_scalar_value<bool, Rcpp::LogicalVector>(x, "logical scalar", thing);
}

int check_integer_scalar(Rcpp::RObject x, const char* thing) {
    return check_scalar_value<int, Rcpp::IntegerVector>(x, "integer scalar", thing);
}

double check_numeric_scalar(Rcpp::RObject x, const char* thing) {
    return check_scalar_value<double, Rcpp::NumericVector>(x, "double-precision scalar", thing);
}

const char* check_string(Rcpp::RObject x, const char* thing) {
    Rcpp::String current=check_scalar_value<Rcpp::String, Rcpp::StringVector>(x, "string", thing);
    return CHAR(current.get_sexp());
}

