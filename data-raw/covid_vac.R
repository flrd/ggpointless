# https://covid.cdc.gov/covid-data-tracker/#rates-by-vaccine-status

url <- "https://data.cdc.gov/api/views/3rge-nu2a/rows.csv?accessType=DOWNLOAD"
cdc_raw <- read.csv(url)


# subset data -------------------------------------------------------------
# don't distinguish between age groups and vaccine
cdc <- subset(cdc_raw,
  subset = Age.group == "all_ages_adj" & Vaccine.product == "all_types",
  select = c("outcome", "month", "MMWR.week", "Age.adjusted.vax.IR", "Age.adjusted.unvax.IR")
)

# clean col names ---------------------------------------------------------
vac_idx <- which(names(cdc) %in% c("Age.adjusted.vax.IR", "Age.adjusted.unvax.IR"))
names(cdc)[vac_idx] <- paste("value", c("fully.vaccinated", "unvaccinated"), sep = "_")


# reshape to long ---------------------------------------------------------
covid_vac <- reshape(cdc,
  direction = "long",
  idvar = c("outcome", "month", "MMWR.week"),
  varying = c("value_fully.vaccinated", "value_unvaccinated"),
  sep = "_"
)
row.names(covid_vac) <- NULL


# rename coloumns ---------------------------------------------------------
names(covid_vac) <- c("outcome", "month", "year_week", "status", "incidence")


# fully.vaccinated --> fully vaccinated -----------------------------------
covid_vac$status <- replace(covid_vac$status, covid_vac$status == "fully.vaccinated", "fully vaccinated")


# add a date column -------------------------------------------------------
covid_vac$date <- as.Date(paste0(covid_vac$year_week, "01"), "%Y%U%w")


# remove columns ----------------------------------------------------------
covid_vac <- covid_vac[, c("date", "incidence", "status", "outcome")]


# save as .csv ------------------------------------------------------------
write.csv(covid_vac, "data-raw/covid_vac.csv", row.names = FALSE)


# save as .rda in /data directory -----------------------------------------
usethis::use_data(covid_vac, overwrite = TRUE)
