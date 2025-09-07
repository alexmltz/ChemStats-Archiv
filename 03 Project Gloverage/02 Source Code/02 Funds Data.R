########################################################################################################################
# Vorbereitung des Systems
########################################################################################################################
packages <- c("tidyverse", "tidyquant", "imputeTS", "plotly")
invisible(lapply(packages, require, character.only = TRUE)); rm(packages)
if(.Platform$OS.type == "windows"){Sys.setlocale("LC_ALL", "en_US.UTF-8")}

#---> Definition des Analysehorizonts:
start_date <- "1975-01-01"
stopp_date <- "2025-08-31"


msci_full_data <- list(
  read.csv("./03 Results Data/01 Equity Indicies/MSCI World USD.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    select(date, days, world = world_us_ndtr_l2),
  read.csv("./01 Source Data/02 EuroStat/Eurostat Exchange Rate EUCR.csv") %>%
    filter(Currency == "US dollar") %>%
    mutate(
      date = as.Date(TIME_PERIOD, format="%Y-%m-%d"),
      eucr = 1 / OBS_VALUE) %>%
    filter(date >= start_date & date <= stopp_date) %>%
    select(date, eucr)) %>%
  Reduce(function(x, y) left_join(x, y, by = c("date")), .) %>%
  mutate(eucr = round(na_interpolation(eucr, "stine"), digits = 5)) %>%
  filter(date >= start_date & date <= stopp_date) %>%
  mutate(
    world_ter = 0.6,
    world_return = world/lag(world),
    world_sim_return = world_return - ((world_ter/100 + 1)^(1/days) - 1),
    world_sim = Reduce(function(values, rates){values*rates},
                       lead(world_sim_return), init = 100, accumulate = TRUE)[-nrow(.)],
    world_sim = case_when(row_number() == n() ~ round(lag(world_sim)*world_sim_return, digits = 2), TRUE ~ world_sim),
    world_sim = world_sim * eucr) %>%
    mutate(across(-date, ~ .x / first(.x) * 100)) %>%
  select(date, world_sim)
write.csv(msci_full_data, "./Project Gloverage Funds.csv", row.names = FALSE)


msci_full_data %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~date, y = ~acwi_sim/100, line = list(width = 1),
    name = "MSCI All-Country World x2") %>%
  add_trace(
    x = ~date, y = ~world_sim/100, line = list(width = 1),
    name = "MSCI World x2") %>%
  add_trace(
    x = ~date, y = ~america_sim/100, line = list(width = 1),
    name = "MSCI North America x2") %>%
  add_trace(
    x = ~date, y = ~europe_sim/100, line = list(width = 1),
    name = "MSCI Europe x2") %>%
  add_trace(
    x = ~date, y = ~pacific_sim/100, line = list(width = 1),
    name = "MSCI Pacific x2") %>%
  add_trace(
    x = ~date, y = ~emerging_sim/100, line = list(width = 1),
    name = "MSCI Emerging x2") %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'),
    xaxis = list(title = "Time"),
    yaxis = list(title = "Market Value"),
    title = "Statistical Simulation of Leveraged Exchange Trade Funds",
    margin = list(t = 50))

msci_full_data %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~date, y = ~log(acwi_sim/100), line = list(width = 1),
    name = "MSCI All-Country World x2") %>%
  add_trace(
    x = ~date, y = ~log(world_sim/100), line = list(width = 1),
    name = "MSCI World x2") %>%
  add_trace(
    x = ~date, y = ~log(america_sim/100), line = list(width = 1),
    name = "MSCI North America x2") %>%
  add_trace(
    x = ~date, y = ~log(europe_sim/100), line = list(width = 1),
    name = "MSCI Europe x2") %>%
  add_trace(
    x = ~date, y = ~log(pacific_sim/100), line = list(width = 1),
    name = "MSCI Pacific x2") %>%
  add_trace(
    x = ~date, y = ~log(emerging_sim/100), line = list(width = 1),
    name = "MSCI Emerging x2") %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'),
    xaxis = list(title = "Time"),
    yaxis = list(title = "Market Value"),
    title = "Statistical Simulation of Leveraged Exchange Trade Funds",
    margin = list(t = 50))


########################################################################################################################
# Bereinigung des Workspace
########################################################################################################################
rm(list = ls())