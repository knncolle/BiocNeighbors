#ifndef NEIGHBOR_QUEUE_H
#define NEIGHBOR_QUEUE_H

#include <queue>
#include <deque>
#include <cmath>
#include "Rcpp.h"
#include "utils.h"

class neighbor_queue {
public:
    void setup(NumNeighbors_t, bool);
    void add(CellIndex_t, double);
    bool is_full() const;
    double limit() const;
    void report(std::deque<CellIndex_t>&, std::deque<double>&, bool, bool, bool=false, CellIndex_t=0);
private:
    bool self=false, ties=true;
    NumNeighbors_t n_neighbors=0, check_k=1;
    bool full=false;

    typedef std::pair<double, CellIndex_t> NeighborPoint;
    std::priority_queue<NeighborPoint> nearest;
};

#endif
