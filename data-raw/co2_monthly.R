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
co2_monthly <- co2_full[, c("date", "year", "month", "co2_ppm", "trend")]


# add decade --------------------------------------------------------------
decades <- function(x) {
  tmp <- x %% 100 %/% 10 * 10
  century <- as.character(x %/% 100)
  sprintf("%s%02.0f's", century, tmp)
}

co2_monthly[["decade"]] <- decades(co2_monthly$year)
# unique(co2_monthly[["decade"]])


# common x-scale ----------------------------------------------------------
co2_monthly[["year_of_decade"]] <- co2_monthly$year %% 10

# 1958 was the first year in the data, using it as "base year" makes
# sure we won't miss a Feb 29, when we add year_of_decade
co2_monthly[["date_scale"]] <- with(co2_monthly, as.Date(sprintf("%d/%d/01", 1958 + year_of_decade, month)))

# save as .csv ------------------------------------------------------------
write.csv(co2_monthly, "co2_monthly.csv")


# save as .rda in /data directory -----------------------------------------
usethis::use_data(co2_monthly, overwrite = TRUE)
