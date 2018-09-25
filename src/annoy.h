#ifndef ANNOY_H
#define ANNOY_H

#include "annoylib.h"
#include "kissrandom.h"
#include "utils.h"

typedef AnnoyIndex<int, double, Euclidean, Kiss64Random> annoyance;

#endif
