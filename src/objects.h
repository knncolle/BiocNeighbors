#ifndef OBJECTS_H
#define OBJECTS_H

#include "kmknn.h"

struct naive_holder {
public:
    naive_holder(SEXP);
    virtual ~naive_holder();
   
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
    std::deque<size_t> neighbors;
    std::deque<double> distances;

    typedef std::priority_queue<std::pair<double, int> > nearest;
    nearest current_nearest;
    void pqueue2deque(const bool, const bool, bool=false, size_t=0);

    double compute_sqdist(const double*, const double*) const;
    virtual void search_all(const double*, double, const bool, const bool);
    virtual void search_nn (const double*, size_t);

    double last_distance2;
    bool tie_warned;
};

struct convex_holder : public naive_holder {
public:
    convex_holder(SEXP, SEXP, SEXP);
    ~convex_holder();
protected:
    const Rcpp::NumericMatrix centers;
    std::deque<int> clust_start;
    std::deque<int> clust_nobs;
    std::deque<Rcpp::NumericVector> clust_dist;

    void search_all(const double*, double, const bool, const bool);
    void search_nn (const double*, size_t);
};

std::unique_ptr<naive_holder> generate_holder(SEXP, SEXP, SEXP); 

#endif
