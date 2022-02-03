# # Source: German Federal Statistical Office ("destatis")
# # Background: https://www.destatis.de/EN/Themes/Society-Environment/Population/Deaths-Life-Expectancy/mortality.html
#
# library(curl)
# library(readxl)
# url <- "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/Sterbefaelle-Lebenserwartung/Tabellen/sonderauswertung-sterbefaelle.xlsx?__blob=publicationFile"
# destfile <- "sonderauswertung_sterbefaelle.xlsx"
# curl::curl_download(url, destfile)
#
# # weekly data, gender data in different sheets
# sheets <- c(
#   "female" = "D_2016_2022_KW_AG_Weiblich",
#   "male" = "D_2016_2022_KW_AG_Männlich"
#   )
#
# mortality_weekly_lst <- lapply(sheets, function(sheet) {
#   readxl::read_xlsx(
#     path = destfile,
#     sheet = sheet,
#     skip = 8,
#     na = "X"
#   )
# })
#
# mortality_weekly_lst <- lapply(mortality_weekly_lst, function(sheet) {
#
#   # change names of second and third columns
#   names(sheet)[2:3] <- c("year", "age_group")
#
#   # remove observations for 2022 & keep only totals ("Insgesamt") per year, then remove 1st and 3rd columns
#   sheet <- subset(sheet, year != 2022 & age_group == "Insgesamt", select = -c(1, 3))
#
#   # reshape to long
#   reshape2::melt(sheet, id.var = "year", variable.name = "week", value.name = "deaths")
# })
#
# # add a gender column
# mortality_clean <- Map(
#   f = function(gender, data) {
#     data[["gender"]] <- gender
#     data
#   },
#   gender = names(mortality_weekly_lst),
#   data = mortality_weekly_lst
# )
#
# # rbind the list to single data frame
# mortality_DE <- do.call(rbind, c(mortality_clean, make.row.names = FALSE))
#
# # compare the ratio of deaths
# # Reduce(function(female, male) merge(female, male, by = c("year", "week")), mortality_clean)
#
# # add a proper date column
# mortality_DE <- transform(
#   mortality_DE,
#   date = ISOweek::ISOweek2date(sprintf("%d-W%02.0f-1", year, week))
#   )
#
# # reorder columns and sort by date
# mortality_DE <- mortality_DE[order(mortality_DE[["date"]], mortality_DE[["gender"]]), c("date", "deaths", "gender", "year", "week")]
#
# # make wide and comapre the "gender death ratio"
# mortality_DE_wide <- reshape2::dcast(mortality_DE, date + year + week ~ gender, value.var = 'deaths')
# mortality_DE_wide[["gender_ratio"]] <- mortality_DE_wide[["female"]] / mortality_DE_wide[["male"]]
#
#
# library(ggplot2)
# ggplot(mortality_DE_wide,
#        aes(ISOweek::ISOweek2date(sprintf("%d-W%02.0f-1", 2022, week)),
#            gender_ratio,
#            group = year)) +
#   geom_hline(yintercept = 1, linetype = "11", colour = "#333333") +
#   ggalt::geom_xspline(
#     size = 1,
#     colour = "#333333"
#     ) +
#   geom_pointless(
#     aes(colour = after_stat(location)),
#     location = c("min", "max"),
#     size = 3
#     ) +
#   scale_x_date(name = NULL, date_labels = "%b") +
#   scale_y_continuous(
#     name = NULL,
#     breaks = c(1),
#     labels = "1/1"
#     ) +
#   scale_color_manual(
#     values = c(
#       "Maximum" = "#FA9529",
#       "Minimum" = "#0CC792"
#       )
#     ) +
#   facet_grid(rows = vars(year)) +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   theme(panel.grid = element_blank()) +
#   theme(legend.title = element_blank()) +
#   NULL
#
# # ggplot(mortality_DE_wide,
# #        aes(date,
# #            gender_ratio)) +
# #   ggalt::geom_xspline() +
# #   scale_y_continuous(name = NULL, labels = function(x) paste0(round(x * 100), "%")) +
# #   scale_x_date(date_labels = "%Y") +
# #   facet_grid(rows = vars(year))
