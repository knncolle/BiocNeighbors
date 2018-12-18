#ifndef KMKNN_H
#define KMKNN_H

#include <stdexcept>
#include <algorithm>
#include <deque>

#include "Rcpp.h"
#include "utils.h"
#include "neighbor_queue.h"

template<class Distance>
class Kmknn {
public:
    Kmknn(SEXP, SEXP, SEXP);

    void find_neighbors(CellIndex_t, double, const bool, const bool);
    void find_neighbors(const double*, double, const bool, const bool);
    void find_nearest_neighbors(CellIndex_t, NumNeighbors_t, const bool, const bool);
    void find_nearest_neighbors(const double*, NumNeighbors_t, const bool, const bool);
    
    MatDim_t get_nobs() const;
    MatDim_t get_ndims() const;

    std::deque<CellIndex_t>& get_neighbors ();
    std::deque<double>& get_distances ();
protected:  
    const Rcpp::NumericMatrix exprs;

    // Data members to store output.
    std::deque<CellIndex_t> neighbors;
    std::deque<double> distances;
    void search_all(const double*, double, const bool, const bool);

    neighbor_queue<Distance> nearest;
    void search_nn(const double*, neighbor_queue<Distance>&);

    // Cluster-related data members.
    const Rcpp::NumericMatrix centers;
    std::deque<CellIndex_t> clust_start, clust_nobs;
    std::deque<Rcpp::NumericVector> clust_dist;
};

#endif
