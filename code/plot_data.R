# Load libraries ---------------------------------------------------------------

library(magrittr)

# Load data --------------------------------------------------------------------

source("code/process_data.R")
df <- read.csv("data/variant_data.csv",
               colClasses = c("Date","character","numeric","factor"))

# Make month markers -----------------------------------------------------------

months <- data.frame(day = 1,
                     month = c(rep(1:12, times = 2), 1),
                     year = c(rep(2020:2021, each = 12), 2022))

months$date <- lubridate::as_date(paste(months$year,
                                        months$month,
                                        months$day,
                                        sep = "-"))

months$label <- ifelse(months$month %in% c(1,4,7,10),
                       paste(format(as.Date(months$date), "%B"),months$year),"")

# Notable dates ----------------------------------------------------------------

dates <- data.frame(date = character(),
                    event = character())

dates[nrow(dates)+1,] <- c("2020-04-09", "mass_testing")
dates[nrow(dates)+1,] <- c("2020-12-08", "vax_rollout")
dates[nrow(dates)+1,] <- c("2020-01-01", "cohort_prevax_start")
dates[nrow(dates)+1,] <- c("2021-06-18", "cohort_prevax_exp_stop")
dates[nrow(dates)+1,] <- c("2021-12-14", "cohort_prevax_stop")
dates[nrow(dates)+1,] <- c("2021-06-01", "cohort_delta_start")
dates[nrow(dates)+1,] <- c("2021-12-14", "cohort_delta_stop")
dates$date <- as.Date(dates$date)

# Sort variants ----------------------------------------------------------------

df$variant <- factor(df$variant, levels = c("Wild type","Alpha","Delta","Omicron"))

# Plot data --------------------------------------------------------------------

max_cases <- 700000

ggplot2::ggplot(data = df[df$value>0 & df$date<="2021-12-31",], 
                mapping = ggplot2::aes(x = date, y = value, color = variant)) +
  ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = dates[dates$event=="mass_testing",]$date), linetype = "dotted") +
  ggplot2::geom_text(ggplot2::aes(x=(dates[dates$event=="mass_testing",]$date - 7), label="Mass testing available", y=700000), colour="black", angle=90, size=3.5, hjust = 1) +
  ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = dates[dates$event=="vax_rollout",]$date), linetype = "dotted") +
  ggplot2::geom_text(ggplot2::aes(x=(dates[dates$event=="vax_rollout",]$date - 7), label="Vaccination available", y=700000), colour="black", angle=90, size=3.5, hjust = 1) +
  ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = dates[dates$event=="cohort_prevax_start",]$date), linetype = "dashed") +
  ggplot2::geom_text(ggplot2::aes(x=(dates[dates$event=="cohort_prevax_start",]$date - 7), label="Start of follow-up for the pre-vaccination cohort", y=700000), colour="black", angle=90, size=3.5, hjust = 1) +
  ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = dates[dates$event=="cohort_delta_start",]$date), linetype = "dotdash") +
  ggplot2::geom_text(ggplot2::aes(x=(dates[dates$event=="cohort_delta_start",]$date - 7), label="Start of follow-up for the vaccinated and unvaccinated cohorts", y=700000), colour="black", angle=90, size=3.5, hjust = 1) +
  ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = dates[dates$event=="cohort_prevax_exp_stop",]$date), linetype = "dashed") +
  ggplot2::geom_text(ggplot2::aes(x=(dates[dates$event=="cohort_prevax_exp_stop",]$date - 7), label="End of ascertainment of COVID-19 diagnoses for the pre-vaccination cohort", y=700000), colour="black", angle=90, size=3.5, hjust = 1) +
  ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = dates[dates$event=="cohort_delta_stop",]$date), linetype = "solid") +
  ggplot2::geom_text(ggplot2::aes(x=(dates[dates$event=="cohort_delta_stop",]$date - 7), label="End of follow-up for all cohorts", y=700000), colour="black", angle=90, size=3.5, hjust = 1) +
  ggplot2::geom_line() +
  ggplot2::labs(x = "", y = "COVID-19 cases identified by community testing", color = "Variant") +
  ggplot2::scale_x_continuous(breaks = months$date, labels = months$label) +
  ggplot2::scale_y_continuous(breaks = seq(0, max_cases, 100000),
                              labels = format(seq(0, max_cases, 100000), scientific = FALSE),
                              lim = c(0, max_cases)) +
  ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_line(colour = "darkgrey"),
                 legend.key = ggplot2::element_rect(fill = "white"),
                 legend.position = "bottom")

ggplot2::ggsave("output/covid_variants.png",
                height = 210, width = 297, 
                unit = "mm", dpi = 600, scale = 1)