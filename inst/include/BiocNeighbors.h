#ifndef BIOCNEIGHBORS_H
#define BIOCNEIGHBORS_H

#include <memory>
#include <stdexcept>
#include "knncolle/knncolle.hpp"
#include "Rcpp.h"

/**
 * @file BiocNeighbors.h
 * @brief Type definitions for **BiocNeighbors**.
 */

/**
 * @namespace BiocNeighbors
 * @brief Type definitions for **BiocNeighbors**.
 */
namespace BiocNeighbors {

/**
 * Type alias for a `knncolle::Matrix` instance with typical R types.
 */
typedef knncolle::Matrix<int, double> Matrix;

/**
 * Type alias for a `knncolle::Builder` instance with typical R types.
 */
typedef knncolle::Builder<int, double, double> Builder;

/**
 * Type alias for an external pointer to a `knncolle::Builder` instance,
 * as returned by **BiocNeighbors**'s `defineBuilder()` function.
 */
typedef Rcpp::XPtr<Builder> BuilderPointer;

/**
 * Type alias for a `knncolle::Prebuilt` instance with typical R types.
 */
typedef knncolle::Prebuilt<int, double, double> Prebuilt;

/**
 * Type alias for the external pointer to a `knncolle::Prebuilt` instance,
 * as returned by **BiocNeighbors**'s `buildIndex()` function.
 */
typedef Rcpp::XPtr<Prebuilt> PrebuiltPointer;

}

#endif
