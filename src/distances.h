#ifndef DISTANCES_H
#define DISTANCES_H

#include "utils.h"

struct BNEuclidean {
    static double distance (const double*, const double*, MatDim_t);
    static double raw_distance (const double*, const double*, MatDim_t);
    static double normalize(double);
    static double unnormalize(double);
};

struct BNManhattan {
    static double distance (const double*, const double*, MatDim_t);
    static double raw_distance (const double*, const double*, MatDim_t);
    static double normalize(double);
    static double unnormalize(double);
};

#endif
