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

std::string check_string(Rcpp::RObject x, const char* thing) {
    check_scalar_value<Rcpp::String, Rcpp::StringVector>(x, "string", thing);
    return Rcpp::as<std::string>(SEXP(x));
}

Rcpp::IntegerVector check_indices(Rcpp::IntegerVector incoming, size_t total_obs) {
    for (auto h : incoming) {
        if (h==NA_INTEGER || h < 0 || size_t(h) >= total_obs) {
            throw std::runtime_error("job indices out of range");
        }
    }
    return incoming;
}

size_t check_k(Rcpp::RObject incoming) {
    const int NN=check_integer_scalar(incoming, "'k'");
    if (NN<1){
        throw std::runtime_error("'k' must be positive");
    }
    return NN;
}

Rcpp::NumericVector check_distances(Rcpp::NumericVector incoming, size_t N) {
    if (incoming.size()!=N) {
        throw std::runtime_error("length of distance vector should be equal to number of points");
    }
    for (auto threshold : incoming) {
        if (threshold <= 0) {
            throw std::runtime_error("threshold should be positive");
        }
    }
    return incoming;
}

void check_ties(bool& diagnose_ties, std::deque<size_t>& neighbors, std::deque<double>& distances, size_t nn) {
    if (!diagnose_ties) { return; }

    constexpr double TOLERANCE=1.00000001;
    for (size_t d=1; d<distances.size(); ++d) {
        if (distances[d-1] * TOLERANCE >= distances[d]) {
            // Setting diagnose_ties to false, as we've found the first instance.
            diagnose_ties=false;
            Rcpp::warning("tied distances detected in nearest-neighbor calculation");
            break;
        }
    }

    // We assume that the NN search was conducted with an extra neighbor if diagnose_ties=true upon entry.
    // This is necessary to allow the above code to check for whether there is a tie at the boundary of the set.
    // It is now time to remove this extra neighbor which should lie at the end of the set.
    if (neighbors.size() > nn) {
        neighbors.pop_back();
    }
    if (distances.size() > nn) {
        distances.pop_back();
    }
    return;
}
