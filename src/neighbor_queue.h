#ifndef NEIGHBOR_QUEUE_H
#define NEIGHBOR_QUEUE_H

#include <queue>
#include <deque>
#include <cmath>
#include "Rcpp.h"

class neighbor_queue {
public:
    void setup(size_t, bool);
    void add(size_t, double);
    bool is_full() const;
    double limit() const;
    void report(std::deque<size_t>&, std::deque<double>&, bool, bool, bool=false, size_t=0);
private:
    bool self=false, ties=true;
    size_t n_neighbors=0, check_k=1;
    bool full=false;

    typedef std::pair<double, size_t> NeighborPoint;
    std::priority_queue<NeighborPoint> nearest;
};

#endif
