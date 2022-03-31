#' Monthly CO2 records taken at Mauna Loa, since March 1958
#'
#' Atmospheric Carbon Dioxide Dry Air Mole Fractions from the
#' NOAA GML Carbon Cycle Cooperative Global Air Sampling
#' Network. Monthly time series constructed from daily mean values,
#' from March 1958 to January 2022.
#'
#' @format A data frame with 766 rows and 5 variables
#' \describe{
#'   \item{date}{date of measurement}
#'   \item{year}{year of measurement}
#'   \item{month}{month of measurement}
#'   \item{co2_ppm}{CO2 concentration, in parts per million}
#'   \item{decade}{decade of the measurement}
#' }
#' @source Dr. Pieter Tans, NOAA/GML (gml.noaa.gov/ccgg/trends/)
#' and Dr. Ralph Keeling, Scripps Institution of Oceanography
#' (scrippsco2.ucsd.edu/).
#'
#' \url{https://gml.noaa.gov/ccgg/trends/data.html}
"co2_ml"



#' Rates of COVID-19 Cases and Deaths by Vaccination Status
#'
#' Data on overall weekly rates of COVID-19 cases and deaths among
#' fully vaccinated and unvaccinated people aged 12 years and older,
#' according to COVID-19 positive specimen collection date. Data covers
#' the periods from April 4, to December 25, 2021.
#'
#' @format A data frame with 146 rows and 4 variables
#' \describe{
#'   \item{date}{Week of data collection}
#'   \item{incidence}{COVID-19 cases and deaths, standardized by age}
#'   \item{status}{vaccination status}
#'   \item{outcome}{COVID-19 cases and deaths}
#' }
#' @source Centers for Disease Control and Prevention,
#'  Rates of COVID-19 Cases and Deaths by Vaccination Status
#'
#' \url{https://covid.cdc.gov/covid-data-tracker/#rates-by-vaccine-status}
"covid_vac"



#' Female leaders of independent states.
#'
#' Data from Wikipedia on women who have been elected or appointed
#' head of state or government of their respective countries since
#' the interwar period (1918–1939).
#'
#' This list includes women who were appointed by a governing committee
#' or parliament where heads of state or government are not directly
#' elected by citizens. The list does not include women chosen by
#' a hereditary monarch.
#'
#' @format A data frame with 131 rows and 5 variables
#' \describe{
#'   \item{name}{Person}
#'   \item{startdate}{Start of tenure}
#'   \item{enddate}{End of tenure}
#'   \item{country}{Country}
#'   \item{power}{Executive or non-executive}
#' }
#' @source wikipedia.org
#'
#' \url{https://en.wikipedia.org/w/index.php?title=List_of_elected_and_appointed_female_heads_of_state_and_government&oldid=1078024588}
"female_leaders"
