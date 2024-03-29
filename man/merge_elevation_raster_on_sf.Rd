% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_elevation_raster_on_sf.R
\name{merge_elevation_raster_on_sf}
\alias{merge_elevation_raster_on_sf}
\title{Compute slope and aspect from elevation and their mean by polygon}
\usage{
merge_elevation_raster_on_sf(
  elev_raster,
  vri_bem,
  elevation_threshold = 1500,
  terrain_raster = NULL
)
}
\arguments{
\item{elev_raster}{SpatRaster object that represent the elevation}

\item{vri_bem}{sf object containing VRI-BEM}

\item{elevation_threshold}{numeric elevation threshold used to create above elevation indicator (\code{ABOVE_ELEV_THOLD})}

\item{terrain_raster}{SpatRaster that contains slope and aspect computed in radiants}
}
\value{
VRI-BEM augmented of the following variables : MEAN_ASP, MEAN_SLOPE, SLOPE_MOD and ABOVE_ELEV_THOLD.
}
\description{
This function overlays the user-specified polygon feature class with slope and aspect raster to
add mean aspect, mean slope and slope modifiers to the polygon attributes
}
\details{
Aspect and slope are calculated by extracting the information from the elevation raster.

Based on whether the the elevation is above the selected threshold then ABOLVE_ELEV_THOLD is created with either "Y" or "N".

The SLOPE_MOD is then defined using the following logic:
\itemize{
\item k - \emph{cool aspect}:  if aspect is 285-359 or 0-134 AND slope is 25\%-100\% (except in CWH and MH zones, it's 35\%-100\%)
\item q - \emph{very steep cool aspect}:  if aspect is 285-359 or 0-134 AND slope > 100\%
\item w - \emph{warm aspect}:  if aspect is 135-284 AND slope is 25\%-100\% (except in CWH and MH zones, it's 35\%-100\%)
\item z - \emph{very steep warm aspect}: if aspect is 135-284 AND slope > 100\%
}
}
