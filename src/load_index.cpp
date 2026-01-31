#include "BiocNeighbors.h"
#include "knncolle/knncolle.hpp"
#include "knncolle_kmknn/knncolle_kmknn.hpp"
#include "knncolle_hnsw/knncolle_hnsw.hpp"
#include "annoy.h"

//[[Rcpp::export(rng=false)]]
SEXP load_index(std::string prefix) {
    return BiocNeighbors::PrebuiltPointer(knncolle::load_prebuilt_raw<int, double, double>(prefix));
}

//[[Rcpp::export(rng=false)]]
SEXP initialize_load_index_registry() {
    knncolle::register_load_euclidean_distance<double, double>();
    knncolle::register_load_manhattan_distance<double, double>();

    auto& reg = knncolle::load_prebuilt_registry<int, double, double>();

    reg[knncolle::bruteforce_prebuilt_save_name] = [](const std::filesystem::path& dir) -> knncolle::Prebuilt<int, double, double>* {
        return knncolle::load_bruteforce_prebuilt<int, double, double>(dir);
    };

    reg[knncolle::vptree_prebuilt_save_name] = [](const std::filesystem::path& dir) -> knncolle::Prebuilt<int, double, double>* {
        return knncolle::load_vptree_prebuilt<int, double, double>(dir);
    };

    reg[knncolle::l2normalized_prebuilt_save_name] = [](const std::filesystem::path& dir) -> knncolle::Prebuilt<int, double, double>* {
        auto config = knncolle::load_l2normalized_prebuilt_types(dir);
        if (config.normalized != knncolle::get_numeric_type<double>()) {
            throw std::runtime_error("unexpected type for the L2Normalized index");
        }
        return knncolle::load_l2normalized_prebuilt<int, double, double, double>(dir);
    };

    reg[knncolle_kmknn::kmknn_prebuilt_save_name] = [](const std::filesystem::path& dir) -> knncolle::Prebuilt<int, double, double>* {
        auto config = knncolle_kmknn::load_kmknn_prebuilt_types(dir);
        if (config.kmeansfloat != knncolle::get_numeric_type<double>()) {
            throw std::runtime_error("unexpected type for the k-means centroids");
        }
        return knncolle_kmknn::load_kmknn_prebuilt<int, double, double>(dir);
    };

    reg[knncolle_annoy::annoy_prebuilt_save_name] = [](const std::filesystem::path& dir) -> knncolle::Prebuilt<int, double, double>* {
        auto config = knncolle_annoy::load_annoy_prebuilt_types(dir);
        if (config.index != knncolle::get_numeric_type<int>()) {
            throw std::runtime_error("unexpected type for the Annoy index");
        }
        if (config.data != knncolle::get_numeric_type<float>()) {
            throw std::runtime_error("unexpected type for the Annoy data");
        }

        if (config.distance == "euclidean") {
            return knncolle_annoy::load_annoy_prebuilt<int, double, double, Annoy::Euclidean>(dir);
        } else if (config.distance != "manhattan") {
            throw std::runtime_error("unknown Annoy distance");
        }
        return knncolle_annoy::load_annoy_prebuilt<int, double, double, Annoy::Manhattan>(dir);
    };

    reg[knncolle_hnsw::hnsw_prebuilt_save_name] = [](const std::filesystem::path& dir) -> knncolle::Prebuilt<int, double, double>* {
        auto config = knncolle_hnsw::load_hnsw_prebuilt_types(dir);
        if (config.data != knncolle::get_numeric_type<float>()) {
            throw std::runtime_error("unexpected type for the HNSW data");
        }
        return knncolle_hnsw::load_hnsw_prebuilt<int, double, double>(dir);
    };

    return R_NilValue;
}

//[[Rcpp::export(rng=false)]]
SEXP get_load_index_registry() {
    auto& reg = knncolle::load_prebuilt_registry<int, double, double>();
    return BiocNeighbors::LoadPrebuiltRegistryPointer(&reg, false); // no finalizer as we don't own it so we can't delete it.
}
