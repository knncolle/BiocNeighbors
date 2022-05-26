#ifndef ANNOY_H
#define ANNOY_H
#include "utils.h"

// turn off AVX always, to avoid small inconsistencies in distance calculations.
#define NO_MANUAL_VECTORIZATION 1

#include "annoy/RcppAnnoy.h"

#if RCPPANNOY_VERSION < RcppAnnoyVersion(0,1,17,3)
  typedef AnnoyIndexSingleThreadedBuildPolicy RcppAnnoyIndexThreadPolicy;
#endif

template<class Distance>
class Annoy {
public:
    Annoy(int, const std::string&, double);

    void find_nearest_neighbors(CellIndex_t, NumNeighbors_t, const bool, const bool);
    void find_nearest_neighbors(const double*, NumNeighbors_t, const bool, const bool);

    MatDim_t get_nobs() const;
    MatDim_t get_ndims() const;

    typedef int32_t Index_t;
    typedef float Data_t;
    typedef AnnoyIndex<Index_t, Data_t, Distance, Kiss64Random, RcppAnnoyIndexThreadPolicy> _index;
    const std::vector<Index_t>& get_neighbors () const;
    const std::vector<Data_t>& get_distances () const;
private:
    MatDim_t NDims;
    _index obj;

    std::vector<Index_t> kept_idx;
    std::vector<Data_t> kept_dist, holding;

    const double search_mult;
    NumNeighbors_t get_search_k(NumNeighbors_t);
};

#endif
