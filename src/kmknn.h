#ifndef KMKNN_H
#define KMKNN_H

#include <stdexcept>
#include <algorithm>
#include <deque>
#include <queue>
#include "Rcpp.h"

struct searcher {
public:
    searcher(SEXP, SEXP, SEXP);

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
    void search_nn (const double*, size_t);

    // Cluster-related data members.
    const Rcpp::NumericMatrix centers;
    std::deque<int> clust_start;
    std::deque<int> clust_nobs;
    std::deque<Rcpp::NumericVector> clust_dist;

    // Nearest-neighbor-related data members.
    typedef std::priority_queue<std::pair<double, int> > nearest;
    nearest current_nearest;
    void pqueue2deque(const bool, const bool, bool=false, size_t=0);

    // Data members to deal with ties. 
    double last_distance2;
    bool tie_warned;
};

#endif
