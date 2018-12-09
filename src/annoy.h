#ifndef ANNOY_H
#define ANNOY_H

// Stolen from RcppAnnoy's annoy.cpp, to get it to compile on Win32.
#if defined(__MINGW32__)
#undef Realloc
#undef Free
#endif

// define R's REprintf as the 'local' error print method for Annoy
#define __ERROR_PRINTER_OVERRIDE__  REprintf

// turn off AVX always, to avoid small inconsistencies in distance calculations.
#define NO_MANUAL_VECTORIZATION 1

#include "annoylib.h"
#include "kissrandom.h"
#include "utils.h"

typedef float ANNOYTYPE;

typedef AnnoyIndex<int32_t, ANNOYTYPE, Euclidean, Kiss64Random> annoyance;

#endif
