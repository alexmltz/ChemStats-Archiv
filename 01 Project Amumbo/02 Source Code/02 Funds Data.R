########################################################################################################################
# Vorbereitung des Systems
########################################################################################################################
packages <- c("tidyverse", "tidyquant", "imputeTS", "plotly")
invisible(lapply(packages, require, character.only = TRUE)); rm(packages)

if(.Platform$OS.type == "windows"){Sys.setlocale("LC_ALL", "en_US.UTF-8")}

#---> Hilfsfunktion für die Berechnung der Grid Search-Metriken:
search_metrics <- function(search_data, search_grid){
  metrics <- sapply(
    list(
      median_absolute_error = function(x) median(abs(x), na.rm = TRUE),
      mean_absolute_error = function(x) mean(abs(x), na.rm = TRUE),
      mean_squared_error = function(x) mean(x^2, na.rm = TRUE),
      root_mean_squared_error = function(x) sqrt(mean(x^2, na.rm = TRUE)),
      median_absolute_percentage_error = function(x) median(abs(x / (x + 1e-8)) * 100, na.rm = TRUE),
      mean_absolute_percentage_error = function(x) mean(abs(x / (x + 1e-8) * 100), na.rm = TRUE),
      mean_squared_percentage_error = function(x) mean((x / (x + 1e-8))^2 * 100, na.rm = TRUE),
      root_mean_squared_percentage_error = function(x) sqrt(mean((x / (x + 1e-8))^2 * 100, na.rm = TRUE))),
    function(metric){search_grid[which.min(apply(search_data, 2, metric))]})
  return(metrics)}

#---> Definition des Analysehorizonts:
start_date <- "1974-07-01"
stopp_date <- "2025-01-03"


########################################################################################################################
# Vorbereitung der Handelsdaten
########################################################################################################################
# Auf der Grundlage unserer US- und EU-Indexzeitreihen ist es uns möglich, plausible Simulationen für UCITS-ETFs für das
# Hebelspektrum von -2 bis 2 zu entwickeln, wobei faktische Total Expense Ratios berücksichtigt werden:
msci_full_data <- list(
  #---> 1. US- und EU-Indexzeitreihen:
  read.csv("./Project Amumbo EUR Indices.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")),
  read.csv("./Project Amumbo USD Indices.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")),
  #---> 2. XETRA-Handelsdaten der UCITS-ETFs:
  read.csv("./01 Source Data/02 Data Services/Onvista Amundi USA EUR Xetra.csv", sep = ";") %>%
    mutate(
      date = as.Date(Datum, format = "%d.%m.%Y"),
      amundi_l1_us_in_eu = as.numeric(gsub(",", ".", Schluss))) %>%
    select(date, amundi_l1_us_in_eu),
  read.csv("./01 Source Data/02 Data Services/Onvista Amundi USA Leverage Daily EUR Xetra.csv", sep = ";") %>%
    mutate(
      date = as.Date(Datum, format = "%d.%m.%Y"),
      amundi_l2_eu_in_eu = as.numeric(gsub(",", ".", Schluss))) %>%
    select(date, amundi_l2_eu_in_eu),
  read.csv("./01 Source Data/02 Data Services/Onvista Amundi USA Daily Inverse EUR Xetra.csv", sep = ";") %>%
    mutate(
      date = as.Date(Datum, format = "%d.%m.%Y"),
      amundi_s1_us_in_eu = as.numeric(gsub(",", ".", Schluss))) %>%
    select(date, amundi_s1_us_in_eu)) %>%
  Reduce(function(x, y) left_join(x, y, by = "date"), .) %>%
  filter(date >= start_date & date <= stopp_date) %>%
  group_by(year = format(date, "%Y")) %>%
  mutate(
    days = ifelse(year != year(Sys.Date()), n(),
                  length(seq.Date(
                    as.Date(paste0(year(Sys.Date()), "-01-01")),
                    as.Date(paste0(year(Sys.Date()), "-12-31")),
                    by = "days"))),
    #---> 3. Berechnung Anbieterwechselkurses:
    ex_rate = eu_price/us_price,
    #---> 4. Umstellung Leitindex: 03. September 2024 / Festlegung TER: 0.03
    amundi_l1_us_in_eu = ifelse(date >= "2024-09-03", na_interpolation(amundi_l1_us_in_eu, "stine"), NA),
    amundi_l1_us_in_us = amundi_l1_us_in_eu/ex_rate,
    #---> 5. Umstellung Leitindex: 5. Juni 2024 / Festlegung TER: 0.60
    amundi_s1_us_in_eu = ifelse(date >= "2024-06-05", na_interpolation(amundi_s1_us_in_eu, "stine"), NA),
    amundi_s1_us_in_us = amundi_s1_us_in_eu/ex_rate,
    #---> 6. Anpassung TER: 0.35 auf 0.5 (2023-10-10)
    amundi_l2_eu_in_eu = ifelse(date >= "2010-02-24", na_interpolation(amundi_l2_eu_in_eu, "stine"), NA)) %>%
  ungroup()


########################################################################################################################
# Approximation von Adjustierungsfaktoren zur Abweichungsminimierung
########################################################################################################################
search_grid <- seq(-0.1, 0.1, by = 1e-4)
search_data <- sapply(search_grid, function(search_value){
  msci_full_data %>% 
    filter(date >= "2010-02-24") %>% # <- Hier: Erster Handelstag ETF
    mutate(
      expense = ifelse(date >= "2023-10-10", 0.5, 0.35), # <- Hier: Angabe TER
      msci_return = eu_ndtr_l2/lag(eu_ndtr_l2),  # <- Hier: Basisindex
      sims_return = (msci_return - (((expense/100 + 1)^(1/days) - 1) - (search_value/100))),
      sims_fonds = Reduce(function(values, rates){values * rates}, 
                         lead(sims_return), init = 100, accumulate = TRUE)[-nrow(.)],
      amundi_l2_eu_in_eu = amundi_l2_eu_in_eu/first(amundi_l2_eu_in_eu) * 100, # <- Hier: Angabe ETF
      error = (1 - sims_fonds/amundi_l2_eu_in_eu)) %>% # <- Hier: Angabe ETF
    pull(error)})

search_metrics(search_data, search_grid)
rm(search_data, search_grid)

msci_full_data %>% 
  filter(date >= "2010-02-24") %>%
  mutate(
    expense = ifelse(date >= "2023-10-10", 0.5, 0.35),
    msci_return = eu_ndtr_l2/lag(eu_ndtr_l2), 
    sims_return = (msci_return - ((expense/100 + 1)^(1/days) - 1) - 0.0002/100),
    sims_fonds = Reduce(function(values, rates){values * rates}, 
                       lead(sims_return), init = 100, accumulate = TRUE)[-nrow(.)],
    amundi_l2_eu_in_eu = amundi_l2_eu_in_eu/first(amundi_l2_eu_in_eu)*100) %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(x = ~date, y = ~(1 - sims_fonds/amundi_l2_eu_in_eu), line = list(width = 1)) %>%
  layout(
    showlegend = FALSE, legend = list(orientation = 'h'),
    xaxis = list(title = "Time"), yaxis = list(title = "Simulation vs. Real ETF"))
# Adjustierungsfaktoren: Long Leverage x2: 0.0029 / Long Leverage x1: -0.0136 / Short Leverage x-1: -0.0015


########################################################################################################################
# Simulation der Amumbo-Familie
########################################################################################################################
msci_sims_data <- msci_full_data %>%
  mutate(
    #---> 1. Simulation Long Leverage EU-ETF x2:
    ter_l2 = ifelse(date >= "2023-10-10", 0.5, 0.35),
    index_l2_return = eu_ndtr_l2/lag(eu_ndtr_l2),
    fonds_l2_return = amundi_l2_eu_in_eu/lag(amundi_l2_eu_in_eu),
    sim_l2_return = ifelse(date <= "2010-02-24",
                           index_l2_return - ((ter_l2/100 + 1)^(1/days) - 1) - 0.0002/100,
                           fonds_l2_return),
    sim_l2 = Reduce(function(values, rates){values*rates},
                    lead(sim_l2_return), init = 100, accumulate = TRUE)[-nrow(.)],
    sim_l2 = case_when(row_number() == n() ~ round(lag(sim_l2)*sim_l2_return, digits = 2), TRUE ~ sim_l2),
    #---> 2. Simulation Long Leveraged EU-ETF x1:
    ter_l1 = 0.03,
    index_l1_return = us_ndtr_l1/lag(us_ndtr_l1),
    fonds_l1_return = index_l1_return/lag(index_l1_return),
    sim_l1_return = ifelse(date <= "2024-09-03",
                           index_l1_return - ((ter_l1/100 + 1)^(1/days) - 1) - 0.0100/100,
                           fonds_l1_return),
    sim_l1 = Reduce(function(values, rates){values*rates},
                    lead(sim_l1_return), init = 100, accumulate = TRUE)[-nrow(.)],
    sim_l1 = case_when(row_number() == n() ~ round(lag(sim_l1)*sim_l1_return, digits = 2), TRUE ~ sim_l1),
    sim_l1 = sim_l1*ex_rate,
    #---> 3. Simulation Short Leveraged EU-ETF x2:
    ter_s2 = 0.9,
    index_s2_return = us_gdtr_s2/lag(us_gdtr_s2),
    sim_s2_return = index_s2_return - ((ter_s2/100 + 1)^(1/days) - 1),
    sim_s2 = Reduce(function(values, rates){values*rates},
                    lead(sim_s2_return), init = 100, accumulate = TRUE)[-nrow(.)],
    sim_s2 = case_when(row_number() == n() ~ round(lag(sim_s2)*sim_s2_return, digits = 2), TRUE ~ sim_s2),
    sim_s2 = sim_s2*ex_rate,
    #---> 4. Simulation Short Leveraged EU-ETF x1:
    ter_s1 = 0.6,
    index_s1_return = us_gdtr_s1/lag(us_gdtr_s1),
    fonds_s1_return = amundi_s1_us_in_us/lag(amundi_s1_us_in_us),
    sim_s1_return = ifelse(date <= "2024-06-05",
                           index_s1_return - ((ter_s1/100 + 1)^(1/days) - 1) - 0.0007/100,
                           fonds_s1_return),
    sim_s1 = Reduce(function(values, rates){values*rates},
                    lead(sim_s1_return), init = 100, accumulate = TRUE)[-nrow(.)],
    sim_s1 = case_when(row_number() == n() ~ round(lag(sim_s1)*sim_s1_return, digits = 2), TRUE ~ sim_s1),
    sim_s1 = sim_s1*ex_rate) %>%
  filter(date <= "2024-12-31") %>%
  select(date,
         sim_l2, sim_l1, sim_s2, sim_s1,
         eu_index_l2 = eu_ndtr_l2, eu_index_l1 = eu_ndtr_l1,
         us_index_l2 = us_ndtr_l2, us_index_l1 = us_ndtr_l1,
         eu_index_s2 = eu_gdtr_s2, eu_index_s1 = eu_gdtr_s1,
         us_index_s2 = us_gdtr_s2, us_index_s1 = us_gdtr_s1)
write.csv(msci_sims_data, "./Project Amumbo UCITS Funds.csv", row.names = FALSE)


msci_sims_data %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~date, y = ~sim_l1/100, line = list(width = 1),
    name = "Long Leverage x1", legendgroup = "long") %>%
  add_trace(
    x = ~date, y = ~sim_l2/100, line = list(width = 1),
    name = "Long Leverage x2", legendgroup = "long") %>%
  add_trace(
    x = ~date, y = ~sim_s1/100, line = list(width = 1),
    name = "Short Leverage x1", legendgroup = "short") %>%
  add_trace(
    x = ~date, y = ~sim_s2/100, line = list(width = 1),
    name = "Short Leverage x2", legendgroup = "short") %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'),
    xaxis = list(title = "Time"),
    yaxis = list(title = "Market Value (EUR)"),
    title = "Statistical Simulation of Exchange Trade Funds",
    margin = list(t = 50))

msci_sims_data %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~date, y = ~log(sim_l1/100), line = list(width = 1),
    name = "Long Leverage x1", legendgroup = "long") %>%
  add_trace(
    x = ~date, y = ~log(sim_l2/100), line = list(width = 1),
    name = "Long Leverage x2", legendgroup = "long") %>%
  add_trace(
    x = ~date, y = ~log(sim_s1/100), line = list(width = 1),
    name = "Short Leverage x1", legendgroup = "short") %>%
  add_trace(
    x = ~date, y = ~log(sim_s2/100), line = list(width = 1),
    name = "Short Leverage x2", legendgroup = "short") %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'),
    xaxis = list(title = "Time"),
    yaxis = list(title = "Log. Market Value (EUR)", range = c(-10, 10)),
    title = "Statistical Simulation of Exchange Trade Funds",
    margin = list(t = 50))


########################################################################################################################
# Bereinigung des Workspace
########################################################################################################################
rm(list = ls())