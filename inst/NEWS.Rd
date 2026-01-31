\name{BiocNeighborsnews}
\title{BiocNeighbors News}
\encoding{UTF-8}

\section{Version 2.6.0}{\itemize{
\item Enable manual vectorization for Annoy and HNSW, if supported by the compiler.
This improves speed at the cost of exact reproducibility across different toolchains/architectures.
We consider this trade-off to be acceptable given that many other floating-point calculations already have non-identical precision across machines.

\item Accept non-ordinary matrices in \code{X=} and \code{query=} arguments for all functions.
This requires \pkg{beachmat} to be installed.

\item Added \code{.check.nonfinite=} option to skip checks for non-finite values in all functions.
This is useful when the user can guarantee that missing or infinite values are not present.

\item Moved the \code{BNPARAM=} argument closer to the front of the argument list of \code{buildIndex()}.
This is more intuitive when examining the generic arguments.

\item Added \code{saveIndex()} and \code{loadIndex()} functions to save and load indices from disk.
}}

\section{Version 2.4.0}{\itemize{
\item Updated the C++ interfaces to match updates to the underlying \pkg{knncolle} libraries.
}}

\section{Version 2.2.0}{\itemize{
\item Throw an error when \code{NA} values are present in the input matrices.
}}

\section{Version 2.0.0}{\itemize{
\item Switched to the new \pkg{knncolle} C++ libaries for all implementations.
This greatly streamlines the internals and allows downstream packages to re-use the search indices in their own C++ code.

\item Parallelization is now performed using the standard \code{<thread>} library. 
This avoids the overhead of forking or starting new processes via \pkg{BiocParallel}.

\item All \code{*Index} classes have been removed. 
The output of \code{buildIndex()} is no longer guaranteed to be serializable, e.g., external pointers to C++-owned objects;
attempting to do so will raise a \dQuote{null pointer to prebuilt index} error for all methods implemented in this package.
The removal of this guarantee makes it easier to extend \pkg{BiocNeighbors} to new methods where the index structure does not have an obvious R representation.

\item All functions (\code{findKNN()}, \code{queryNeighbors()}, etc.) will no longer coerce \code{X} to a matrix, to avoid the headache of S4 dispatch ambiguity.
Users should coerce their data into matrix format before supplying it to these functions.

\item The \code{last=} option in \code{findKNN()} and \code{queryKNN()} has been replaced by the \code{findDistance()} and \code{queryDistance()} functions instead.
This provides a much more intuitive method for the typical use of \code{last=}, i.e., to obtain the distance to the k-th nearest neighbor.

\item \code{findNeighbors()} no longer reports each point as its own neighbor.
Also, all neighbors are now sorted by increasing distance in \code{findNeighbors()} and \code{queryNeighbors()}.
}}

\section{Version 1.10.0}{\itemize{
\item Migrated \code{findMutualNN()} from \pkg{batchelor}.

\item Vendored the \pkg{RcppAnnoy} headers for greater reproducibility.

\item Added a \code{distance="Cosine"} option for all algorithms.
}}

\section{Version 1.4.0}{\itemize{
\item Allow memory-efficient retrieval of the distance to the furthest neighbors.

\item Added a warn.ties= argument to turn off tie-related warnings in the KMKNN and VP tree algorithms.

\item Return neighbor counts in rangeFind*() and rangeQuery*() functions when get.index=FALSE and get.distance=FALSE.
}}

\section{Version 1.2.0}{\itemize{
\item findNeighbors() and queryNeighbors() now accept a vector of point-specific thresholds.

\item Added an VP tree implemention in findVptree(), queryVptree(), buildVptree().
Supported dispatch to these methods in the generics.

\item Added a HNSW implementation in findHnsw(), queryHnsw(), buildHnsw().
Supported dispatch to these methods in the generics.

\item Renamed buildNNIndex() to buildIndex().

\item Converted findNeighbors() and queryNeighbors() into S4 methods.
Created specific rangeFind*() and rangeQuery*() functions for KMKNN and VP tree algorithms.

\item Modified AnnoyIndex class to hold the original data matrix.
Created bnorder(), bndata() generics to obtain (possibly reordered) input matrices for all indexes.

\item Supported Manhattan distance searching in all algorithms.
}}

\section{Version 1.0.0}{\itemize{
\item
New package BiocNeighbors, for k-means-based k-nearest neighbor detection.
}}

