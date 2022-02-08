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



#' Rates of COVID-19 Cases and Deaths by Vaccination Status
#'
#' Data on overall weekly rates of COVID-19 cases and deaths among
#' fully vaccinated and unvaccinated people aged 12 years and older,
#' according to COVID-19 positive specimen collection date. Data covers
#' the periods from April 4, to December 25, 2021.
#'
#' @format A data frame with 146 rows and 4 variables:
#' \describe{
#'   \item{date}{Week of data collection}
#'   \item{incidence}{COVID-19 cases and deaths, standardized by age}
#'   \item{status}{vaccination status}
#'   \item{outcome}{COVID-19 cases and deaths}
#' }
#' @source Centers for Disease Control and Prevention Rates of COVID-19 Cases and Deaths by Vaccination Status
#'
#' \url{https://covid.cdc.gov/covid-data-tracker/#rates-by-vaccine-status}
"covid_vac"
