#include "init.h"
#include "annoy.h"
#include "build_annoy.h"
#include "find_annoy.h"
#include "query_annoy.h"

/* Consolidating this into a single object file to avoid multiple definition
 * errors that seem to occur when on Windows 32 only. 
 */
