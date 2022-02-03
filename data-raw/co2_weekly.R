# https://gml.noaa.gov/ccgg/trends/data.html

# download data -----------------------------------------------------------
url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_weekly_mlo.txt"
co2_full <- read.table(url, header = FALSE, na.strings = "-999.99")

# rename columns ----------------------------------------------------------
nms <- c(
  "year",
  "month",
  "day",
  "year_decimal",
  "co2_ppm",
  "#days",
  "1_yr_ago",
  "10_yr_ago",
  "delta_1800"
)
co2_full <- setNames(co2_full, nms)

# create a proper date column ---------------------------------------------
co2_full[["date"]] <- with(co2_full, as.Date(paste(year, month, day, sep = "/")))


# remove cols -------------------------------------------------------------
co2_weekly <- co2_full[, c("date", "year", "month", "day", "co2_ppm")]


# interpolate missing values ----------------------------------------------
idx <- which(is.na(co2_weekly$co2_ppm))
co2_weekly$co2_ppm[idx] <- spline(co2_weekly$co2_ppm, xout = idx)$y


# add decade --------------------------------------------------------------
decades <- function(year) {

  stopifnot(is.numeric(year))

  tmp <- year %% 100 %/% 10 * 10
  century <- as.character(year %/% 100)
  sprintf("%s%02.0f's", century, tmp)
}

co2_weekly[["decade"]] <- decades(co2_weekly$year)
# unique(co2[["decade"]])


# common x-scale ----------------------------------------------------------
co2_weekly[["year_of_decade"]] <- co2_weekly$year %% 10

# 1974 was the first year in the data, using it as "base year" makes
# sure we won't miss a Feb 29, when we add year_of_decade
co2_weekly[["date_scale"]] <- with(co2_weekly, as.Date(paste(1974 + year_of_decade, month, day, sep = "/")))


# save as .csv ------------------------------------------------------------
write.csv(co2_weekly, "co2_weekly.csv")


# save as .rda in /data directory -----------------------------------------
# usethis::use_data(co2_weekly, overwrite = TRUE)
