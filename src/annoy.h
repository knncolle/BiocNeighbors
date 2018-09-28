#ifndef ANNOY_H
#define ANNOY_H

#include "annoylib.h"
#include "kissrandom.h"
#include "utils.h"

typedef float ANNOYTYPE;

typedef AnnoyIndex<int, ANNOYTYPE, Euclidean, Kiss64Random> annoyance;

#endif
