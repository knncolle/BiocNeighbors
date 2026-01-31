#ifndef ANTI_ANNOY_H
#define ANTI_ANNOY_H

// Copied from RcppAnnoy's RcppAnnoy.h:
#if defined(__MINGW32__)
#undef Realloc
#undef Free
#endif
#define __ERROR_PRINTER_OVERRIDE__  REprintf

#include "knncolle_annoy/knncolle_annoy.hpp"

#endif
