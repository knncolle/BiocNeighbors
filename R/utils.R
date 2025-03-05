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

.coerce_matrix_build <- function(X, transposed) {
    if (!is.matrix(X)) {
        X <- as.matrix(X)
    }    

    if (!transposed) {
        X <- t(X)
    }

    if (!is.double(X)) {
        storage.mode(X) <- "double"
    }
    if (anyNA(X)) {
        stop("NA values are not supported")
    }

    X
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
