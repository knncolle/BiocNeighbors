#ifndef VPTREE
#define VPTREE

#include <stdexcept>
#include <algorithm>
#include <deque>
#include <vector>
#include <queue>
#include "Rcpp.h"

/* Adapted from http://stevehanov.ca/blog/index.php?id=130 */

struct DataPoint {
    DataPoint();
    DataPoint(int, const double*);

    const double* ptr;
    int index;
};

class VpTree {
public:    
    VpTree(Rcpp::List);
    VpTree(Rcpp::NumericMatrix); 
    Rcpp::List save();
private:
    int ndim;
    Rcpp::NumericMatrix reference;
    std::vector<DataPoint> items;
    
    // Single node of a VP tree (has a point and radius; left children are closer to point than the radius)
    struct Node
    {
        double threshold;       // radius(?)
        int index;              // index of point in node
        int left;               // node: points closer by than threshold
        int right;              // node: points farther away than threshold
        Node(int i=0) : index(i), threshold(0.), left(-1), right(-1) {}
    };
    std::deque<Node> nodes;

    int buildFromPoints(int, int);
};

#endif
