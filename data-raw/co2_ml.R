# https://gml.noaa.gov/ccgg/trends/data.html

# download data -----------------------------------------------------------
url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.txt"
co2_full <- read.table(url, header = FALSE, na.strings = c("-9.99", "-0.99", "-1"))

# rename columns ----------------------------------------------------------
nms <- c(
  "year",
  "month",
  "year_decimal",
  "co2_ppm",
  "trend",
  "#days",
  "std_deviation",
  "uncertainty"
)

co2_full <- setNames(co2_full, nms)

# create a proper date column ---------------------------------------------
co2_full[["date"]] <- with(co2_full, as.Date(sprintf("%d/%d/01", year, month)))


# remove cols -------------------------------------------------------------
co2_ml <- co2_full[, c("date", "year", "month", "co2_ppm")]


# add decade --------------------------------------------------------------
co2_ml[["decade"]] <- ggpointless:::get_decades(co2_ml$year)


# save as .csv ------------------------------------------------------------
write.csv(co2_ml, "data-raw/co2_ml", row.names = FALSE)


# save as .rda in /data directory -----------------------------------------
usethis::use_data(co2_ml, overwrite = TRUE)
