#include "init.h"
#include "R_ext/Rdynload.h"
#include "R_ext/Visibility.h"

#define REGISTER(x, i) {#x, (DL_FUNC) &x, i}

extern "C" {

static const R_CallMethodDef all_call_entries[] = {
    REGISTER(find_kmknn, 8),
    REGISTER(query_kmknn, 9),
    REGISTER(range_find_kmknn, 8),
    REGISTER(range_query_kmknn, 9),

    REGISTER(build_annoy, 4),
    REGISTER(find_annoy, 7),
    REGISTER(query_annoy, 8),

    REGISTER(build_hnsw, 4),
    REGISTER(find_hnsw, 6),
    REGISTER(query_hnsw, 7),

    REGISTER(build_vptree, 2),
    REGISTER(find_vptree, 7),
    REGISTER(query_vptree, 8),
    REGISTER(range_find_vptree, 7),
    REGISTER(range_query_vptree, 8),
    {NULL, NULL, 0}
};

void attribute_visible R_init_BiocNeighbors(DllInfo *dll) {
    R_registerRoutines(dll, NULL, all_call_entries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}

}

