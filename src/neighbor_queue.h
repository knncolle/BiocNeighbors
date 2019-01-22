#ifndef NEIGHBOR_QUEUE_H
#define NEIGHBOR_QUEUE_H

#include <queue>
#include <deque>
#include "Rcpp.h"
#include "utils.h"

template<class Distance>
class neighbor_queue {
public:
    void setup(NumNeighbors_t);
    void setup(NumNeighbors_t, CellIndex_t);

    void add(CellIndex_t, double);
    bool is_full() const;
    double limit() const;

    void report(std::deque<CellIndex_t>&, std::deque<double>&, bool, bool, bool=false);
private:
    bool self=false, ties=true;
    CellIndex_t self_dex=0;
    NumNeighbors_t n_neighbors=0, check_k=1;
    bool full=false;

    void base_setup(NumNeighbors_t);

    typedef std::pair<double, CellIndex_t> NeighborPoint;
    std::priority_queue<NeighborPoint> nearest;
};

#endif
