\name{ernm}
\alias{ernm}
\title{fits an ERNM model}
\usage{
  ernm(formula, modelType = FullErnmModel,
    modelArgs = list(), fullSampler = "NTDNBRMetropolis",
    missingSampler = "NTDNBRNonObservedMetropolis",
    nodeSamplingPercentage = 0.2, ...)
}
\arguments{
  \item{formula}{model formula}

  \item{modelType}{the likelihood model type to use}

  \item{modelArgs}{additional arguments for ModelType}

  \item{fullSampler}{the sampler for the unconditional
  simulations}

  \item{missingSampler}{sampler the sampler for the
  conditional simulations}

  \item{nodeSamplingPercentage}{how often are nodal
  variates toggled}

  \item{...}{additional parameters for ernmFit}
}
\description{
  fits an ERNM model
}

