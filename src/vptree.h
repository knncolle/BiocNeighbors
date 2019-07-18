#ifndef VPTREE
#define VPTREE

#include "Rcpp.h"
#include "utils.h"
#include "neighbor_queue.h"

#include <deque>
#include <vector>

/* Adapted from http://stevehanov.ca/blog/index.php?id=130 */

template<class Distance>
class VpTree {
public:    
    VpTree(Rcpp::NumericMatrix); 
    VpTree(Rcpp::NumericMatrix, Rcpp::List, bool=true);
    Rcpp::List save();

    void find_neighbors(CellIndex_t, double, const bool, const bool);
    void find_neighbors(const double*, double, const bool, const bool);
    void find_nearest_neighbors(CellIndex_t, NumNeighbors_t, const bool, const bool);
    void find_nearest_neighbors(const double*, NumNeighbors_t, const bool, const bool);

    MatDim_t get_nobs() const;
    MatDim_t get_ndims() const;

    std::deque<CellIndex_t>& get_neighbors ();
    std::deque<double>& get_distances ();
private:
    Rcpp::NumericMatrix reference;
    MatDim_t ndim;

    typedef std::pair<CellIndex_t, const double*> DataPoint;
    std::vector<DataPoint> items;

    typedef int NodeIndex_t;
    static const NodeIndex_t LEAF_MARKER=-1;

    // Single node of a VP tree (has a point and radius; left children are closer to point than the radius)
    struct Node
    {
        double threshold;       // radius(?)
        NodeIndex_t index;              // index of point in node
        NodeIndex_t left;               // node: points closer by than threshold
        NodeIndex_t right;              // node: points farther away than threshold
        Node(NodeIndex_t i=0) : threshold(0), index(i), left(LEAF_MARKER), right(LEAF_MARKER) {}
    };
    std::deque<Node> nodes;

    struct DistanceComparator;
    NodeIndex_t buildFromPoints(NodeIndex_t, NodeIndex_t);
private:
    std::deque<CellIndex_t> neighbors;
    std::deque<double> distances;
    double tau;

    neighbor_queue nearest;
    void search_nn(NodeIndex_t, const double*, neighbor_queue&);
    void search_all(NodeIndex_t, const double*, double, bool, bool);
};

#endif
