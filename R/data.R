#' Monthly CO2 records taken at Mauna Loa, since March 1958
#'
#' Atmospheric Carbon Dioxide Dry Air Mole Fractions from the
#' NOAA GML Carbon Cycle Cooperative Global Air Sampling
#' Network. Monthly time series constructed from daily mean values,
#' see \url{www.esrl.noaa.gov/gmd/ccgg/trends/} for additional details.
#'
#' @format A data frame with 766 rows and 8 variables:
#' \describe{
#'   \item{date}{date of measurement}
#'   \item{year}{year of measurement}
#'   \item{month}{month of measurement}
#'   \item{co2_ppm}{CO2 concentration, in parts per million}
#'   \item{trend}{trend component}
#'   \item{decade}{decade of the measurement}
#'   \item{year_of_decade}{year into the decade of measurement}
#'   \item{date_scale}{A common scale, used for plotting}
#' }
#' @source Dr. Pieter Tans, NOAA/GML (gml.noaa.gov/ccgg/trends/) and Dr. Ralph Keeling, Scripps Institution of Oceanography (scrippsco2.ucsd.edu/).
#'
#' \url{https://gml.noaa.gov/ccgg/trends/data.html}
"co2_monthly"