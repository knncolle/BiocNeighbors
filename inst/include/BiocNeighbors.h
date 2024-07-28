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
 * @brief Wrapper structure around a prebuilt index.
 */
struct Prebuilt {
    /**
     * Pointer to a prebuilt index.
     * This should be set to a non-NULL value before returning an external pointer from **BiocNeighbors**'s `buildIndex()` function.
     */
    std::unique_ptr<knncolle::Prebuilt<int, int, double> > index;

    /**
     * Whether cosine normalization was used to construct the index in `index`.
     * This will be used to apply L2 normalization to the query data before `Searcher::search()` or `Searcher::search_all()`.
     */
    bool cosine = false;
};

/**
 * Type alias for the external pointer to be returned by **BiocNeighbors**'s `buildIndex()` function.
 */
typedef Rcpp::XPtr<Prebuilt> PrebuiltPointer;

}

#endif
