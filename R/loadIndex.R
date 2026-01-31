#' Load a prebuilt search index from disk
#'
#' Load a \linkS4class{BiocNeighborIndex} object from its on-disk representation back into the current R session. 
#'
#' @param dir String containing the path to a directory in which the original index was saved.
#' This should be the same as the argument passed to \code{\link{saveIndex}}.
#' @param ... Additional arguments passed to specific methods.
#'
#' @details
#' As discussed in \code{\link{saveIndex}}, it is expected that the on-disk representation is loaded in the same R environment that was used to save it.
#'
#' Developers are directed to \code{\link{getLoadGenericIndexRegistry}} to add loading functions for their own search algorithms.
#'
#' @return A \linkS4class{BiocNeighborIndex} object, created from files inside \code{dir}.
#'
#' @author Aaron Lun
#'
#' @examples
#' Y <- matrix(rnorm(100000), ncol=20)
#' k.out <- buildIndex(Y)
#' k.nn <- findKNN(k.out, k=5)
#'
#' tmp <- tempfile()
#' dir.create(tmp)
#' saveIndex(k.out, tmp)
#'
#' reloaded <- loadIndex(tmp)
#' re.nn <- findKNN(reloaded, k=5)
#' identical(k.nn, re.nn)
#'
#' @export
loadIndex <- function(dir, ...) {
    algo <- readLines(file.path(dir, "ALGORITHM"), warn=FALSE)
    otherfun <- loading.globals$other[[algo]]
    if (!is.null(otherfun)) {
        return(otherfun(dir, ...))
    }

    out <- load_index(dir)
    names <- NULL
    names.path <- paste0(dir, "r_names")
    if (file.exists(names.path)) {
        names <- readLines(names.path)
    }

    # Just in case we're dealing with a cosine-normalized thingy.
    if (algo == "knncolle::L2Normalized") {
        algo <- readLines(file.path(dir, "INDEX", "ALGORITHM"), warn=FALSE)
    }

    loading.globals$generic[[algo]](out, names)
}

#' Load indices from extension packages
#'
#' Extend \code{\link{loadIndex}} to work with algorithms from \pkg{BiocNeighbors} extension packages.
#' 
#' @param name String containing the name of the neighbor search algorithm, e.g., \code{"knncolle::Vptree"}.
#' @param class Function that accepts a single argument (the external pointer to a \code{knncolle::Prebuilt} instance)
#' and returns an instance of the appropriate \linkS4class{BiocNeighborGenericIndex} subclass.
#' @param fun Function that accepts a directory path (see \code{\link{loadIndex}}) plus any number of additional arguments via \code{...},
#' and returns an instance of the \linkS4class{BiocNeighborIndex} subclass corresponding to \code{name}.
#'
#' @return For \code{getLoadGenericIndexRegistry}, an external pointer to the global \code{knncolle::load_prebuilt_registry()} object in C++.
#'
#' For \code{registerLoadGenericIndexClass}, the name and class are registered with \code{loadIndex}.
#'
#' For \code{registerLoadIndexFunction}, the name and function are registered with \code{loadIndex}.
#'
#' @details
#' If the extension developer implements their search algorithm in the form of C++ subclasses of the various \pkg{knncolle} interfaces,
#' they can call \code{getLoadGenericIndexRegistry} in their package's \code{\link{.onLoad}} to access the registry and add a loading method for their algorithm.
#' They should also call \code{registerLoadGenericIndexClass} to specify a constructor function for the corresponding \linkS4class{BiocNeighborGenericIndex} subclass.
#'
#' If the extension developer implements their search algorithm by subclassing \pkg{BiocNeighbors} generics at the R level,
#' they should call \code{registerLoadIndexFunction} in their package's \code{\link{.onLoad}}.
#' This function is responsible for reading the on-disk contents from the specified directory and using them to construct a \linkS4class{BiocNeighborIndex} instance.
#'
#' @author Aaron Lun
#' @examples
#' getLoadGenericIndexRegistry()
#' registerLoadGenericIndexClass("knncolle_annoy::Annoy", AnnoyIndex)
#' 
#' @export
getLoadGenericIndexRegistry <- function() {
    get_load_index_registry()
}

loading.globals <- new.env()
loading.globals$generic <- list()
loading.globals$other <- list()

#' @export
#' @rdname getLoadGenericIndexRegistry 
registerLoadGenericIndexClass <- function(name, class) {
    loading.globals$generic[[name]] <- class
}

#' @export
#' @rdname getLoadGenericIndexRegistry 
registerLoadIndexFunction <- function(name, fun) {
    loading.globals$other[[name]] <- fun 
}
