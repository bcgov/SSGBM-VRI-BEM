% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_rrm_on_vri.R
\name{merge_rating_moose}
\alias{merge_rating_moose}
\title{merge_rating_moose}
\usage{
merge_rating_moose(vri_bem, rrm_dt, rating_variables, rating_variables_expr)
}
\arguments{
\item{vri_bem}{sf object that represent VRI (vegetation ressource inventory) features}

\item{rrm_dt}{data.table object that contains the rrm}

\item{rating_variables}{list, vector for each rating variables to create}

\item{rating_variables_expr}{parse_expr , expression for variable to merge in data.table}
}
\value{
sf  vri-bem object with new columns for rating
}
\description{
merge suitability and capability rating from rrm onto vri-bem
and calculate highest value and weighted average for each scores
}
