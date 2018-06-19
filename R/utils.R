#' @importFrom BiocParallel bpworkers
.assign_jobs <- function (njobs, BPPARAM) 
# Assigns a contiguous set of jobs to workers.
# Returns a list of job indices, one per worker.
#
# written by Aaron Lun
# created 19 June 2018
{
    ncores <- bpworkers(BPPARAM)
    starting <- as.integer(seq(1, njobs + 1, length.out = ncores + 1))
    jobsize <- diff(starting)
    starting <- starting[-length(starting)] - 1L
    return(mapply("+", starting, lapply(jobsize, seq_len), SIMPLIFY = FALSE))
}

