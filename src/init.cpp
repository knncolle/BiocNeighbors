#include "init.h"
#include "R_ext/Rdynload.h"
#include "R_ext/Visibility.h"

#define REGISTER(x, i) {#x, (DL_FUNC) &x, i}

extern "C" {

static const R_CallMethodDef all_call_entries[] = {
    REGISTER(find_kmknn, 7),
    REGISTER(query_kmknn, 8),
    REGISTER(find_neighbors, 7),
    REGISTER(query_neighbors, 8),

    REGISTER(build_annoy, 3),
    REGISTER(find_annoy, 6),
    {NULL, NULL, 0}
};

void attribute_visible R_init_kmknn(DllInfo *dll) {
    R_registerRoutines(dll, NULL, all_call_entries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}

}

