\name{kmknnnews}
\title{kmknn News}
\encoding{UTF-8}

\section{Version 1.2.0}{\itemize{
\item findNeighbors() and queryNeighbors() now accept a vector of point-specific thresholds.

\item Added an VP tree implemention in findVptree(), queryVptree(), buildVptree().
Supported dispatch to these methods in the generics.

\item Renamed buildNNIndex() to buildIndex().

\item Converted findNeighbors() and queryNeighbors() into S4 methods.
Created specific rangeFind*() and rangeQuery*() functions for KMKNN and VP tree algorithms.

\item Created bnorder(), bndata() generics to obtain reordered input matrices for VP tree, KMKNN indexes. 
}}

\section{Version 1.0.0}{\itemize{
\item
New package kmknn, for k-means-based k-nearest neighbor detection.
}}

