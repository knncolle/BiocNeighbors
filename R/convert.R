#' Convert neighbor search results to alternative representations 
#' 
#' Convert the results of various neighbor search functions to alternative representations for more convenient downstream use.
#' 
#' @param results 
#' For \code{findResultsToMatrix} and \code{findResultsToHits}, a named list containing the output from \code{\link{findKNN}}, \code{\link{findNeighbors}} or related functions. 
#'
#' For \code{queryResultsToMatrix} and \code{queryResultsToHits}, a named list containing the output from \code{\link{queryKNN}}, \code{\link{queryNeighbors}} or related functions. 
#'
#' In both cases, the list should contain an \code{index} entry, and may optionally contain a \code{distance} entry. 
#' @param num.pts Integer specifying the number of data points in the \code{X} dataset (see the argument of the same name in \code{\link{findKNN}}, \code{\link{queryKNN}} and friends).
#'
#' For \code{findResultsToMatrix} and \code{findResultsToHits}, this can be \code{NULL},
#' in which case the number of data points is assumed to be the same as the number of query points in \code{results}.
#' Note that users should specify \code{num.pts} if \code{subset=} is used in \code{\link{findKNN}} or related functions.
#'
#' For \code{queryResultsToMatrix} and \code{queryResultsToHits}, this is a required argument.
#' @param index.transposed Boolean indicating whether \code{results$index} is a transposed matrix, i.e., each column corresponds to a query point.
#' This should be set to \code{TRUE} if \code{get.index="transposed"} in \code{\link{findKNN}} or \code{\link{queryKNN}}. 
#' Only relevant if \code{results$index} is a matrix.
#' @param distance.transposed Boolean indicating whether \code{results$distance} is a transposed matrix, i.e., each column corresponds to a query point.
#' This should be set to \code{TRUE} if \code{get.distance="transposed"} in \code{\link{findKNN}} or \code{\link{queryKNN}}. 
#' Only relevant if \code{results$distance} is present and a matrix.
#' @param query.rows Boolean indicating whether query points should be represented as rows in the returned matrix. 
#' @param query.from Boolean indicating whether query points should be stored as \code{from} in the returned \link[S4Vectors]{Hits} object.
#' @param repr String specifing the type of sparse matrix, see \code{?\link[Matrix]{sparseMatrix}} for more details.
#' 
#' @return
#' For \code{findResultsToMatrix} and \code{queryResultsToMatrix}, a sparse matrix from the \pkg{Matrix} package is returned.
#' \itemize{
#' \item If \code{query.rows=TRUE}, each row corresponds to a query point and the columns with non-zero entries represent the nearest neighbors.
#' Otherwise, each column corresponds to a query point.
#' \item If \code{results} contains \code{distance}, the value of each non-zero entry contains the distance to that neighbor.
#' Otherwise, a pattern matrix is returned indicating whether each entry is a nearest neighbor.
#' }
#'
#' For \code{findResultsToHits} and \code{queryResultsToHits}, a \link[S4Vectors]{Hits} object is returned.
#' \itemize{
#' \item If \code{query.from=TRUE}, \code{from} contains the observations and \code{to} contains the identities of the neighbors.
#' Otherwise, the query points are stored in \code{to}.
#' \item If \code{results} contains \code{distance}, a \code{distance} field will be present in \code{\link[S4Vectors]{mcols}},
#' containing the distance from each observation to each of its neighbors.
#' }
#' The output will be a \link[S4Vectors]{SelfHits} instance if \code{num.obs=NULL} for \code{findResultsToHits}.
#' 
#' @examples
#' X <- matrix(rnorm(10000), ncol=20)
#' out <- findKNN(X, k=5)
#' sm <- findResultsToMatrix(out)
#' str(sm)
#' sh <- findResultsToHits(out)
#' sh
#'
#' Y <- matrix(rnorm(1000), ncol=20)
#' out2 <- queryKNN(query=Y, X=X, k=5)
#' sm2 <- queryResultsToMatrix(out2, num.pts = nrow(X))
#' str(sm2)
#' h2 <- queryResultsToHits(out2, num.pts = nrow(X))
#' h2
#'
#' @seealso
#' The \code{colPairs} slot of the SingleCellExperiment class, from the \pkg{SingleCellExperiment} package.
#' 
#' @export
#' @rdname convert
findResultsToMatrix <- function(results, num.pts = NULL, index.transposed = FALSE, distance.transposed = FALSE, query.rows = TRUE, repr = "C") {
    .convert_nn_results(
        results,
        n.subject = num.pts,
        index.transposed = index.transposed,
        distance.transposed = distance.transposed, 
        as.hits = FALSE,
        output.transposed = !query.rows,
        matrix.repr = repr
    )
}

.convert_nn_results <- function(results, n.subject, index.transposed, distance.transposed, as.hits, matrix.repr, output.transposed) {
    is.mat <- is.matrix(results$index)
    has.dist <- !is.null(results$distance)

    if (is.mat) {
        if (index.transposed) {
            n.query <- ncol(results$index)
            query <- rep(seq_len(n.query), each = nrow(results$index))
        } else {
            n.query <- nrow(results$index)
            query <- rep(seq_len(n.query), ncol(results$index))
        }

        subject <- as.vector(results$index)

        if (has.dist) {
            if (distance.transposed != index.transposed) {
                val <- as.vector(t(results$distance))
            } else {
                val <- as.vector(results$distance)
            }
        } else {
            val <- NULL
        }

    } else {
        n.query <- length(results$index)
        query <- rep(seq_along(results$index), lengths(results$index))
        subject <- unlist(results$index) 
        if (has.dist) {
            val <- unlist(results$distance)
        } else {
            val <- NULL
        }
    }

    if (is.null(n.subject)) {
        if (as.hits) {
            if (!output.transposed) {
                args <- list(from = query, to = subject)
            } else {
                args <- list(from = subject, to = query)
            }
            args$nnode <- n.query
            output <- do.call(S4Vectors::SelfHits, args)
            if (!is.null(val)) {
                S4Vectors::mcols(output)$distance <- val
            }
            return(output)
        }

        n.subject <- n.query
    }

    if (!as.hits) {
        args <- list(repr = matrix.repr)
        if (!output.transposed) {
            args <- c(args, list(i = query, j = subject, dims = c(n.query, n.subject)))
        } else {
            args <- c(args, list(i = subject, j = query, dims = c(n.subject, n.query)))
        }
        if (!is.null(val)) {
            args$x <- val
        }
        return(do.call(Matrix::sparseMatrix, args))

    } else {
        if (!output.transposed) {
            args <- list(from = query, to = subject, nLnode = n.query, nRnode = n.subject)
        } else {
            args <- list(from = subject, to = query, nLnode = n.subject, nRnode = n.query)
        }
        output <- do.call(S4Vectors::Hits, args)
        if (!is.null(val)) {
            S4Vectors::mcols(output)$distance <- val
        }
        return(output)
    }
}

#' @export
#' @rdname convert
findResultsToHits <- function(results, num.pts = NULL, index.transposed = FALSE, distance.transposed = FALSE, query.from = TRUE) {
    .convert_nn_results(
        results,
        n.subject = num.pts,
        index.transposed = index.transposed,
        distance.transposed = distance.transposed, 
        as.hits = TRUE,
        output.transposed = !query.from,
        matrix.repr = NULL
    )
}

#' @export
#' @rdname convert
queryResultsToMatrix <- function(results, num.pts, index.transposed = FALSE, distance.transposed = FALSE, query.rows = TRUE, repr = "C") {
    .convert_nn_results(
        results,
        n.subject = num.pts,
        index.transposed = index.transposed,
        distance.transposed = distance.transposed, 
        as.hits = FALSE,
        output.transposed = !query.rows,
        matrix.repr = repr
    )
}

#' @export
#' @rdname convert
queryResultsToHits <- function(results, num.pts, index.transposed = FALSE, distance.transposed = FALSE, query.from = TRUE) {
    .convert_nn_results(
        results,
        n.subject = num.pts,
        index.transposed = index.transposed,
        distance.transposed = distance.transposed, 
        as.hits = TRUE,
        output.transposed = !query.from,
        matrix.repr = NULL
    )
}
