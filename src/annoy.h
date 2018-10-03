#ifndef ANNOY_H
#define ANNOY_H

// Stolen from RcppAnnoy's annoy.cpp, to get it to compile on Win32.
#if defined(__MINGW32__)
#undef Realloc
#undef Free
#endif

// define R's REprintf as the 'local' error print method for Annoy
#define __ERROR_PRINTER_OVERRIDE__  REprintf

#include "annoylib.h"
#include "kissrandom.h"
#include "utils.h"

typedef float ANNOYTYPE;

typedef AnnoyIndex<int, ANNOYTYPE, Euclidean, Kiss64Random> annoyance;

#endif
