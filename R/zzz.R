.onLoad <- function(libname, pkgname) {
    initialize_load_index_registry()
    registerLoadGenericIndexClass("knncolle_annoy::Annoy", AnnoyIndex)
    registerLoadGenericIndexClass("knncolle::Bruteforce", ExhaustiveIndex)
    registerLoadGenericIndexClass("knncolle_hnsw::Hnsw", HnswIndex)
    registerLoadGenericIndexClass("knncolle_kmknn::Kmknn", KmknnIndex)
    registerLoadGenericIndexClass("knncolle::Vptree", VptreeIndex)
}
