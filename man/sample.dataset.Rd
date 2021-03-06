% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/BN-methods.R,
%   R/InferenceEngine-methods.R
\name{sample.dataset}
\alias{sample.dataset}
\alias{sample.dataset,BN-method}
\alias{sample.dataset,BN}
\alias{sample.dataset,InferenceEngine-method}
\alias{sample.dataset,InferenceEngine}
\title{sample a \code{\link{BNDataset}} from a network of an inference engine.}
\usage{
sample.dataset(x, n = 100, mar = 0)

\S4method{sample.dataset}{BN}(x, n = 100, mar = 0)

\S4method{sample.dataset}{InferenceEngine}(x, n = 100)
}
\arguments{
\item{x}{a \code{\link{BN}} or \code{\link{InferenceEngine}} object.}

\item{n}{number of items to sample.}

\item{mar}{fraction [0,1]  of missing values in the sampled dataset 
(missing at random), default value is 0 (no missing values).}
}
\value{
a \code{\link{BNDataset}}
}
\description{
sample a \code{\link{BNDataset}} from a network of an inference engine.
}
