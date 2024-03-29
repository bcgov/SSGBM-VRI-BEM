% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_ccb_on_vri.R
\name{merge_ccb_on_vri}
\alias{merge_ccb_on_vri}
\title{Union merge between CCB attributes and VRI features}
\usage{
merge_ccb_on_vri(vri_bem, ccb)
}
\arguments{
\item{vri_bem}{sf object that represent VRI (vegetation ressource inventory) features}

\item{ccb}{sf object that represent CCB (consolidated cutblock) features}
}
\value{
sf object that represent the intersections of ccb and vri
}
\description{
Performs a Union merge between CCB (Consolidated Cutblocks) attributes on VRI (vegetation ressource inventory) features
}
