.integerize_subset <- function(index, subset) {
    if (is.null(subset)) {
        subset
    } else if (!is.numeric(subset)) {
        dummy <- seq_len(generic_num_obs(index@ptr))
        names(dummy) <- index@names
        dummy[subset]
    } else if (!is.integer(subset)) {
        as.integer(subset)
    } else {
        subset
    }
}

.format_output <- function(output, name, to.get) {
    if (isFALSE(to.get)) {
        output[[name]] <- NULL
    } else if (identical(to.get, "transposed")) {
        ; # no-op
    } else if (isTRUE(to.get) || identical(to.get, "normal")) {
        if (is.matrix(output[[name]])) {
            output[[name]] <- t(output[[name]])
        }
    } else {
        stop("unsupported option '", to.get, "'")
    }
    output
}

.transpose_and_subset <- function(x, transposed, subset) {
    if (is.matrix(x)) {
        # For back-compatibility, and to avoid loading beachmat and Matrix,
        # we just apply these operations directly if a matrix is provided.
        if (!transposed) {
            x <- t(x)
        }
    } else {
        x <- DelayedArray::DelayedArray(x)
        if (!transposed) {
            x <- Matrix::t(x)
        }
    }

    if (!is.null(subset)) {
        x <- x[,subset,drop=FALSE]
    }

    x
}
