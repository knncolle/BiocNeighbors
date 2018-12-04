#ifndef KMKNN_H
#define KMKNN_H

#include <stdexcept>
#include <algorithm>
#include <deque>
#include <cmath>
#include "Rcpp.h"
#include "neighbor_queue.h"

struct Kmknn {
public:
    Kmknn(SEXP, SEXP, SEXP);

    void find_neighbors(size_t, double, const bool, const bool);
    void find_neighbors(const double*, double, const bool, const bool);
    void find_nearest_neighbors(size_t, size_t, const bool, const bool);
    void find_nearest_neighbors(const double*, size_t, const bool, const bool);
    
    size_t get_nobs() const;
    size_t get_ndims() const;

    std::deque<size_t>& get_neighbors ();
    std::deque<double>& get_distances ();
protected:  
    const Rcpp::NumericMatrix exprs;
    double compute_sqdist(const double*, const double*) const;
    
    // Data members to store output.
    std::deque<size_t> neighbors;
    std::deque<double> distances;
    void search_all(const double*, double, const bool, const bool);

    neighbor_queue nearest;
    void search_nn (const double*);

    // Cluster-related data members.
    const Rcpp::NumericMatrix centers;
    std::deque<int> clust_start;
    std::deque<int> clust_nobs;
    std::deque<Rcpp::NumericVector> clust_dist;
};

#endif
