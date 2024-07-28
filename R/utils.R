.subset_to_index <- function(subset, x, byrow=TRUE) 
# Converts an arbitary subset into an integer vector.
{
    if (byrow) {
        dummy <- seq_len(nrow(x))
        names(dummy) <- rownames(x)
    } else {
        dummy <- seq_len(ncol(x))
        names(dummy) <- colnames(x) 
    }

    out <- unname(dummy[subset])
    if (any(is.na(out))) {
        stop("'subset' indices out of range of 'x'")
    }

    out
}

.coerce_matrix_build <- function(X, transposed) {
    if (!transposed) {
        if (!is.matrix(X)) {
            X <- Matrix::t(X)
        } else {
            X <- t(X)
        }
    }

    if (!is.matrix(X)) {
        X <- as.matrix(X)
    }    

    if (!is.double(X)) {
        storage.mode(X) <- "double"
    }

    X
}

.format_output <- function(output, name, to.get) {
    if (isFALSE(to.get)) {
        output[[name]] <- NULL
    } else if (identical(to.get, "transposed")) {
        ; # no-op
    } else if (isTRUE(to.get) || identical(to.get, "normal")) {
        output[[name]] <- t(output[[name]])
    } else {
        stop("unsupported option '", to.get, "'")
    }
    output
}
