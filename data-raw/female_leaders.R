# download data -----------------------------------------------------------
library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_elected_and_appointed_female_heads_of_state_and_government"
xpath <- "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]"

female_leaders <- url |>
  read_html() |>
  html_node(xpath = xpath) |>
  html_table()


# subset and rename cols --------------------------------------------------
female_leaders <- female_leaders[, c("Name", "Mandate start", "Mandate end", "Country", "Executive ornon-executive")]
names(female_leaders) <- c("name", "startdate", "enddate", "country", "power")

# remove Katalin Novák because she is not yet in office, as of March 2022
female_leaders <- subset(female_leaders, enddate != "Elect")


# Executive or non-executive? ---------------------------------------------
female_leaders$power <- tolower(female_leaders$power)
female_leaders$power[female_leaders$power != ""] <- "executive"
female_leaders$power[female_leaders$power == ""] <- "non-executive"


# clean end dates ---------------------------------------------------------
female_leaders <- transform(female_leaders, enddate = replace(enddate, enddate == "Incumbent", NA))
# ;(
female_leaders$enddate <- gsub("(assassinated)", "", female_leaders$enddate, fixed = TRUE)
female_leaders$enddate <- gsub("Suspended.*$", "", female_leaders$enddate)


# create Date class -------------------------------------------------------
# overcome locale specific month names by setting the C locale, see
lct <- Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
female_leaders$startdate <- as.Date(female_leaders$startdate, "%d %B %Y")
female_leaders$enddate <- as.Date(female_leaders$enddate, "%d %B %Y")
Sys.setlocale("LC_TIME", lct)


# set NAs in end date to today, because ladies are still in office --------
female_leaders <- transform(female_leaders, enddate = replace(enddate, is.na(enddate), Sys.Date()))


# save as .csv ------------------------------------------------------------
write.csv(female_leaders, "data-raw/female_leaders.csv", row.names = FALSE)


# save as .rda in /data directory -----------------------------------------
usethis::use_data(female_leaders, overwrite = TRUE)
