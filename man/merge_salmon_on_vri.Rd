% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_salmon_on_vri.R
\name{merge_salmon_on_vri}
\alias{merge_salmon_on_vri}
\title{Union merge between CCB attributes and VRI features}
\usage{
merge_salmon_on_vri(vri_bem, salmon)
}
\arguments{
\item{vri_bem}{sf object that represent VRI (vegetation ressource inventory) features}

\item{salmon}{sf object that represent salmon features}
}
\value{
sf object that represent the intersections of salmon and vri
}
\description{
Performs a Union merge between CCB (Consolidated Cutblocks) attributes on VRI (vegetation ressource inventory) features
}
