########################################################################################################################
# Vorbereitung des Systems
########################################################################################################################
packages <- c("tidyverse", "tidyquant", "imputeTS", "modelr", "plotly", "readxl", "mgcv", "httr", "jsonlite")
invisible(lapply(packages, require, character.only = TRUE)); rm(packages)

if(.Platform$OS.type == "windows"){Sys.setlocale("LC_ALL", "en_US.UTF-8")}

#---> Hilfsfunktion für den Abruf der Marktindizes:
scraping_index <- function(code, type, currency, start, stopp){
  data <- GET(
    paste0("https://app2.msci.com/products/service/index/indexmaster/getLevelDataForGraph?currency_symbol=",
           currency, "&index_variant=", type, "&start_date=", start, "&end_date=", stopp,
           "&data_frequency=DAILY&index_codes=", code))
  
  if(status_code(data) == 200){
    data <- fromJSON(content(data, "text", encoding = "utf-8"))$indexes$INDEX_LEVELS
  } else {
    print("Fehler beim Abrufen der Daten")
  }
  return(data)}

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
# Vorbereitung der Analysedaten für die US- und EU-Indexvarianten
########################################################################################################################
# Zur Vorbereitung der Analysedaten werden Stineman-Splines zur Interpolation fehlender Handelstage in den Zeitreihen der
# US- und EU-Indexvarianten sowie der Zeitreihen der Zinssätze eingesetzt, um einerseits die Komplexität der Modellierung
# zu reduzieren, andererseits die Interoperationalität der Zeitreihen zu erhöhen, was die Generierung fehlender Indizes
# deutlich vereinfacht. Aufgrund der Interpolation wird eine kleine Verzerrung in die Modellierung eingebracht, allerdings
# ist diese aufgrund der mathematischen Eigenschaften der Stineman-Splines deutlich geringer als es bei anderen Spline- 
# oder einer Linear-Interpolation der Fall wäre. Sofern eine Simulation lediglich tatsächliche Handelstage nutzen soll, 
# lassen sich diese über die Datumsspalte idenzifizieren.
us_msci_data <- list(
  #---> 1. Konstruktion des Kalendar-Zeitindex und Berechnung der Kalendartage pro Jahr:
  data.frame(date = seq.Date(from = as.Date(start_date),
                             to = as.Date(paste0(year(Sys.Date()), "-12-31")),
                             by = "days")) %>%
    group_by(year = format(date, "%Y")) %>%
    mutate(days = ifelse(year != year(Sys.Date()), n(),
                               length(seq.Date(
                                 as.Date(paste0(year(Sys.Date()), "-01-01")),
                                 as.Date(paste0(year(Sys.Date()), "-12-31")),
                                 by = "days")))) %>%
    ungroup() %>%
    select(date, days),
  #---> 2. Integration der US-Interbanken-Zinssätze (Effective Federal Funds Rate und Secured Overnight Financing Rate):
  read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=DFF", na.strings = ".") %>%
    rename(date = 1, effr = 2) %>%
    mutate(date = as.Date(date)),
  read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=SOFR", na.strings = ".") %>%
    rename(date = 1, sofr = 2) %>%
    mutate(date = as.Date(date)),
  #---> 3. Integration der S&P 500-Indexdaten für die USD-Indexvariante Price Return:
  tq_get("^GSPC", from = start_date, to = stopp_date) %>%
    select(date, us_sp500 = adjusted),
  #---> 4. Integration der MSCI USA-Indexdaten für die USD-Indexvarianten Price, Gross Total und Net Total Return:
  scraping_index(code = "984000", type = "STRD", currency = "USD", start = "19990101", stopp = gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      us_price = level_eod) %>%
    select(date, us_price),
  scraping_index(code = "984000", type = "GRTR", currency = "USD", start = "19990101", stopp = gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      us_gdtr_l1 = level_eod) %>%
    select(date, us_gdtr_l1),
  scraping_index(code = "984000", type = "NETR", currency = "USD", start = "19990101", stopp = gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      us_ndtr_l1 = level_eod) %>%
    select(date, us_ndtr_l1),
  #---> 5. Integration der MSCI USA-Indexdaten für die USD-Indexvarianten Long Leverage und Short Gross Total Return:
  read.table("https://app2.msci.com/eqb/short/performance/90491.49.all.xls", sep = ",", skip = 4) %>%
    filter(grepl("\t", V1)) %>%
    separate_wider_delim(V1, delim = "\t", names = c("date", "us_gdtr_l2"), too_few = "align_start") %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      us_gdtr_l2 = as.numeric(us_gdtr_l2)),
  read.table("https://app2.msci.com/eqb/short/performance/90490.49.all.xls", sep = ",", skip = 4) %>%
    filter(grepl("\t", V1)) %>%
    separate_wider_delim(V1, delim = "\t", names = c("date", "us_gdtr_s1"), too_few = "align_start") %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      us_gdtr_s1 = as.numeric(us_gdtr_s1))) %>%
  Reduce(function(x, y) left_join(x, y, by = "date"), .) %>%
  filter(date <= stopp_date) %>%
  #---> 6. Stineman-Spline-Interpolierung fehlender Werte und Berechnung der Daily Returns:
  mutate(
    across(-c(date, days, effr, sofr), ~ if_else(is.na(us_sp500), NA, .)),
    us_sp500 = round(na_interpolation(us_sp500, "stine"), digits = 5),
    us_sp500_return = us_sp500/lag(us_sp500),
    us_price = ifelse(date >= "1999-01-04", round(na_interpolation(us_price, "stine"), digits = 5), NA),
    us_price_return = us_price/lag(us_price),
    us_gdtr_l1 = ifelse(date >= "1999-01-04", round(na_interpolation(us_gdtr_l1, "stine"), digits = 5), NA),
    us_gdtr_l1_return = us_gdtr_l1/lag(us_gdtr_l1),
    us_ndtr_l1 = ifelse(date >= "1999-01-04", round(na_interpolation(us_ndtr_l1, "stine"), digits = 5), NA),
    us_ndtr_l1_return = us_ndtr_l1/lag(us_ndtr_l1),
    us_gdtr_l2 = ifelse(date >= "2000-12-29", round(na_interpolation(us_gdtr_l2, "stine"), digits = 5), NA),
    us_gdtr_l2_return = us_gdtr_l2/lag(us_gdtr_l2),
    us_gdtr_s1 = ifelse(date >= "2000-12-29", round(na_interpolation(us_gdtr_s1, "stine"), digits = 5), NA),
    us_gdtr_s1_return = us_gdtr_s1/lag(us_gdtr_s1),
    us_rate = na_interpolation(ifelse(date <= "2021-07-31", effr, sofr), "stine"),
    us_rate = round(us_rate, digits = 3)) %>%
  select(date, days, us_rate,
         us_sp500, us_sp500_return, us_price, us_price_return,
         us_gdtr_l1, us_gdtr_l1_return, us_gdtr_l2, us_gdtr_l2_return,
         us_ndtr_l1, us_ndtr_l1_return,
         us_gdtr_s1, us_gdtr_s1_return)

eu_msci_data <- list(
  #---> 1. Konstruktion des Kalendar-Zeitindex und Berechnung der Kalendartage pro Jahr:
  data.frame(date = seq.Date(from = as.Date(start_date),
                             to = as.Date(paste0(year(Sys.Date()), "-12-31")),
                             by = "days")) %>%
    group_by(year = format(date, "%Y")) %>%
    mutate(days = ifelse(year != year(Sys.Date()), n(),
                               length(seq.Date(
                                 as.Date(paste0(year(Sys.Date()), "-01-01")),
                                 as.Date(paste0(year(Sys.Date()), "-12-31")),
                                 by = "days")))) %>%
    ungroup() %>%
    select(date, days),
  #---> 2. Integration der EU-Interbanken-Zinssätze (Frankfurter Tagesgeldsatz, EONIA und ESTER):
  read.csv("./01 Source Data/01 Bundesbank/Overnight Borrowing/Bundesbank ST0101.csv", skip = 8, sep = ";") %>%
    select(date = 1, ftgr = 2) %>%
    mutate(
      date = as.Date(date, format = "%Y-%m-%d"),
      ftgr = as.numeric(gsub(",", ".", na_if(ftgr, ".")))),
  read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=EONIARATE", na.strings = ".") %>%
    rename(date = 1, eonia = 2) %>%
    mutate(date = as.Date(date)),
  read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=ECBESTRVOLWGTTRMDMNRT", na.strings = ".") %>%
    rename(date = 1, ester = 2) %>%
    mutate(date = as.Date(date)),
  #---> 3. Integration der S&P 500-Indexdaten für die USD- und EUR-Indexvariante Price Return:
  tq_get("^GSPC", from = start_date, to = stopp_date) %>%
    select(date, us_sp500 = adjusted),
  read_xlsx("./01 Source Data/02 Data Services/S&P Global S&P 500 EUR.xlsx") %>%
    mutate(
      Date = str_replace_all(Date, c(
        "Jan" = "January", "Feb" = "February", "Mar" = "March",
        "Apr" = "April", "May" = "May", "Jun" = "June",
        "Jul" = "July", "Aug" = "August", "Sep" = "September",
        "Oct" = "October", "Nov" = "November", "Dec" = "December")),
      Date = as.Date(Date, format = "%B %d, %Y")) %>%
    select(date = Date, eu_sp500 = GSPC_EUR),
  #---> 4. Integration der MSCI USA-Indexdaten für die EUR-Indexvarianten Price, Gross Total und Net Total Return:
  scraping_index(code = "984000", type = "STRD", currency = "EUR", start = "19990101", gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      eu_price = level_eod) %>%
    select(date, eu_price),
  scraping_index(code = "984000", type = "GRTR", currency = "EUR", start = "19990101", gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      eu_gdtr_l1 = level_eod) %>%
    select(date, eu_gdtr_l1),
  scraping_index(code = "984000", type = "NETR", currency = "EUR", start = "19990101", gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      eu_ndtr_l1 = level_eod) %>%
    select(date, eu_ndtr_l1),
  #---> 5. Integration der MSCI USA-Indexdaten für die EUR-Indexvariante Leverage Net Total Return:
  read_xlsx("./01 Source Data/02 Data Services/Investing MSCI USA Leveraged Daily Net TR EUR.xlsx") %>%
    mutate(
      date = as.Date(Date, format = "%Y-%m-%d"),
      eu_ndtr_l2 = as.numeric(gsub(",",".", Close))) %>%
    select(date, eu_ndtr_l2)) %>%
  Reduce(function(x, y) left_join(x, y, by = "date"), .) %>%
  filter(date <= stopp_date) %>%
  #---> 6. Stineman-Spline-Interpolierung fehlender Werte und Berechnung der Daily Returns:
  mutate(
    across(-c(date, days, ftgr, eonia, ester, us_sp500), ~ if_else(is.na(eu_sp500), NA, .)),
    us_sp500 = round(na_interpolation(us_sp500, "stine"), digits = 5),
    us_sp500_return = us_sp500/lag(us_sp500),
    eu_sp500 = ifelse(date >= "1989-12-29", round(na_interpolation(eu_sp500, "stine"), digits = 5), NA),
    eu_sp500_return = eu_sp500/lag(eu_sp500),
    eu_price = ifelse(date >= "1999-01-04", round(na_interpolation(eu_price, "stine"), digits = 5), NA),
    eu_price_return = eu_price/lag(eu_price),
    eu_gdtr_l1 = ifelse(date >= "2000-12-29", round(na_interpolation(eu_gdtr_l1, "stine"), digits = 5), NA),
    eu_gdtr_l1_return = eu_gdtr_l1/lag(eu_gdtr_l1),
    eu_ndtr_l1 = ifelse(date >= "2000-12-29", round(na_interpolation(eu_ndtr_l1, "stine"), digits = 5), NA),
    eu_ndtr_l1_return = eu_ndtr_l1/lag(eu_ndtr_l1),
    eu_ndtr_l2 = ifelse(date >= "2012-12-26", round(na_interpolation(eu_ndtr_l2, "stine"), digits = 5), NA),
    eu_ndtr_l2_return = eu_ndtr_l2/lag(eu_ndtr_l2),
    eu_rate = na_interpolation(ifelse(date <= "2021-07-31", ifelse(date <= "1999-01-01", ftgr, eonia), ester), "stine"),
    eu_rate = round(eu_rate, digits = 3)) %>%
  select(date, days, eu_rate, 
         us_sp500, us_sp500_return,
         eu_sp500, eu_sp500_return, eu_price, eu_price_return,
         eu_gdtr_l1, eu_gdtr_l1_return, 
         eu_ndtr_l1, eu_ndtr_l1_return, eu_ndtr_l2, eu_ndtr_l2_return)


########################################################################################################################
# Modellierung der US- und EU-Indexvarianten
########################################################################################################################
# Leider reichen die offiziellen Daten der MSCI Group lediglich ins Jahr 1999 zurück, weshalb wir uns einiger statistischer
# Modelle bedienen, um die US- und EU-Indexzeitreihen unter Verwendung adäquater Proxy-Zeitreihen zurückzurechnen. Hierbei
# ist zu beachten, dass wir keine Möglichkeit haben, die "Korrektheit" der Berechnung zu prüfen, weshalb wir ein Verfahren
# nutzen, dass einerseits relativ robust und leicht zu realisieren ist, andererseits genug Flexibilität besitzt, um latente
# Divergenzen zwischen den Index- und den Proxy-Zeitreihen "abzufangen" -- diesbezüglich bietet uns die Verwendung verallge-
# meinerter linearer Modelle bzw. generalisierter additiver Modelle uns einen sehr guten Trade-Off, wenn wir diesen Ansatz
# auf Return-Zeitreihen anwenden (z.B. Return-Zeitreihen des S&P 500 als Proxy für den MSCI USA).
# Im Hinblick auf die Modellierung der EU-Zeitreihen ist ein weiterer Schritt nötig, um Wechselkurse und Währungsumstellung
# abzubilden. Hierfür wird die beschriebene Modellierung eingesetzt, um den "Anbieterwechselkurs" (Quotient US-Index/EU-
# Index) für den S&P 500 anhand des faktischen Wechselkurses von US-Dollar zur Europäischen Währungseinheit EUC vor 1999 
# bzw. Euro ab 1999 zu bestimmen.
us_msci_model <- us_msci_data %>%
  add_predictions(gam(us_price_return ~ s(us_sp500_return, bs = "cr"), data = .), var = "us_price_model") %>%
  mutate(
    us_price_return = ifelse(date < "1999-01-04" | is.na(us_price_return), us_price_model, us_price_return),
    us_price = ifelse(date < "1999-01-04",
                      rev(Reduce(function(values, rates){values/rates},
                                 rev(us_price_return), init = last(us_price), accumulate = TRUE)[-nrow(.)]),
                      us_price),
    us_price = ifelse(row_number() == 1, round(lead(us_price)/lead(us_price_return), digits = 5), round(us_price, digits = 5))) %>%
  #---> 1. US Gross Total Return:
  add_predictions(gam(us_gdtr_l1_return ~ s(us_price_return), data = .), var = "us_gdtr_l1_model") %>%
  mutate(
    us_gdtr_l1_return = ifelse(date < "1999-01-04" | is.na(us_gdtr_l1_return), us_gdtr_l1_model, us_gdtr_l1_return),
    us_gdtr_l1 = ifelse(date < "1999-01-04",
                        rev(Reduce(function(values, rates){values/rates},
                                   rev(us_gdtr_l1_return), init = last(us_gdtr_l1), accumulate = TRUE)[-nrow(.)]),
                        us_gdtr_l1),
    us_gdtr_l1 = ifelse(row_number() == 1, round(lead(us_gdtr_l1)/lead(us_gdtr_l1_return), digits = 5), round(us_gdtr_l1, digits = 5))) %>%
  #---> 2. US Net Total Return:
  add_predictions(gam(us_ndtr_l1_return ~ s(us_price_return), data = .), var = "us_ndtr_l1_model") %>%
  mutate(
    us_ndtr_l1_return = ifelse(date < "1999-01-04" | is.na(us_ndtr_l1_return), us_ndtr_l1_model, us_ndtr_l1_return),
    us_ndtr_l1 = ifelse(date < "1999-01-04",
                        rev(Reduce(function(values, rates){values/rates},
                                   rev(us_ndtr_l1_model), init = last(us_ndtr_l1), accumulate = TRUE)[-nrow(.)]),
                        us_ndtr_l1),
    us_ndtr_l1 = ifelse(row_number() == 1, round(lead(us_ndtr_l1)/lead(us_ndtr_l1_return), digits = 5), round(us_ndtr_l1, digits = 5))) %>%
  select(date, days, us_rate,
         us_price, us_price_return, 
         us_gdtr_l1, us_gdtr_l1_return, us_gdtr_l2, us_gdtr_l2_return, 
         us_ndtr_l1, us_ndtr_l1_return,
         us_gdtr_s1, us_gdtr_s1_return) %>%
  filter(date >= "1975-01-01")


eu_msci_model <- left_join(
  eu_msci_data,
  read.csv("./01 Source Data/03 EuroStat/Eurostat Exchange Rate EUCR.csv") %>%
    filter(currency == "US dollar") %>%
    mutate(
      date = as.Date(TIME_PERIOD, format="%Y-%m-%d"),
      eucr = 1 / OBS_VALUE) %>%
    filter(date >= start_date & date <= stopp_date) %>%
    select(date, eucr),
  by = "date") %>%
  #---> 1. Umrechnungskurs US/EUCR:
  mutate(eucr = round(na_interpolation(eucr, "stine"), digits = 5)) %>%
  add_predictions(gam(eu_sp500/us_sp500 ~ s(eucr, bs = "cr"), data = .), var = "index_model") %>%
  mutate(
    eu_sp500 = ifelse(date <= "1989-12-29", index_model * us_sp500, eu_sp500),
    eu_sp500_return = eu_sp500/lag(eu_sp500)) %>%
  add_predictions(gam(eu_price_return ~ s(eu_sp500_return, bs = "cr"), data = .), var = "eu_price_model") %>%
  mutate(
    eu_price_return = ifelse(date < "1999-01-04" | is.na(eu_price_return), eu_price_model, eu_price_return),
    eu_price = ifelse(date < "1999-01-04",
                      rev(Reduce(function(values, rates){values/rates},
                                 rev(eu_price_return), init = last(eu_price), accumulate = TRUE)[-nrow(.)]),
                      eu_price),
    eu_price = ifelse(row_number() == 1, round(lead(eu_price)/lead(eu_price_return), digits = 5), round(eu_price, digits = 5))) %>%
  #---> 2. EU Gross Total Return:
  add_predictions(gam(eu_gdtr_l1_return ~ s(eu_price_return, bs = "cr"), data = .), var = "eu_gdtr_l1_model") %>%
  mutate(
    eu_gdtr_l1_return = ifelse(date < "2000-12-29" | is.na(eu_gdtr_l1_return), eu_gdtr_l1_model, eu_gdtr_l1_return),
    eu_gdtr_l1 = ifelse(date < "2000-12-29",
                        rev(Reduce(function(values, rates){values/rates},
                                   rev(eu_gdtr_l1_return), init = last(eu_gdtr_l1), accumulate = TRUE)[-nrow(.)]),
                        eu_gdtr_l1),
    eu_gdtr_l1 = ifelse(row_number() == 1, round(lead(eu_gdtr_l1)/lead(eu_gdtr_l1_return), digits = 5), round(eu_gdtr_l1, digits = 5))) %>%
  #---> 3. EU Net Total Return:
  add_predictions(gam(eu_ndtr_l1_return ~ s(eu_price_return, bs = "cr"), data = .), var = "eu_ndtr_l1_model") %>%
  mutate(
    eu_ndtr_l1_return = ifelse(date < "2000-12-29" | is.na(eu_ndtr_l1_return), eu_ndtr_l1_model, eu_ndtr_l1_return),
    eu_ndtr_l1 = ifelse(date < "2000-12-29",
                        rev(Reduce(function(values, rates){values/rates},
                                   rev(eu_ndtr_l1_model), init = last(eu_ndtr_l1), accumulate = TRUE)[-nrow(.)]),
                        eu_ndtr_l1),
    eu_ndtr_l1 = ifelse(row_number() == 1, round(lead(eu_ndtr_l1)/lead(eu_ndtr_l1_return), digits = 5), round(eu_ndtr_l1, digits = 5))) %>%
  select(date, days, eu_rate, eu_price, eu_price_return, 
         eu_gdtr_l1, eu_gdtr_l1_return, 
         eu_ndtr_l1, eu_ndtr_l1_return, eu_ndtr_l2, eu_ndtr_l2_return) %>%
  filter(date >= "1975-01-01")


########################################################################################################################
# Visualisierung der Indexvarianten
########################################################################################################################
us_msci_model %>%
  select(date, us_price, us_gdtr_l1, us_ndtr_l1) %>%
  mutate(across(where(is.numeric), function(x){x/first(x)})) %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(x = ~date, y = ~log(us_price), name = "Price Return", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(us_gdtr_l1), name = "Gross Total Return", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(us_ndtr_l1), name = "Net Total Return", line = list(width = 1)) %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'), 
    xaxis = list(title = "Time"),
    yaxis = list(title = "Log. Index Value (USD)", range = c(0, 6)),
    title = "MSCI USA",
    margin = list(t = 50))

eu_msci_model %>%
  select(date, eu_price, eu_gdtr_l1, eu_ndtr_l1) %>%
  mutate(across(where(is.numeric), function(x){x/first(x)})) %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(x = ~date, y = ~log(eu_price), name = "Price Return", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_gdtr_l1), name = "Gross Total Return", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_ndtr_l1), name = "Net Total Return", line = list(width = 1)) %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'),
    xaxis = list(title = "Time"),
    yaxis = list(title = "Log. Index Value (EUR)", range = c(0, 6)),
    title = "MSCI USA",
    margin = list(t = 50))


########################################################################################################################
# Approximation von Adjustierungsfaktoren zur Abweichungsminimierung
########################################################################################################################
eu_search_grid <- seq(-0.1, 0.1, by = 1e-4)
eu_search_data <- eu_msci_model %>%
  filter(date >= "2012-12-26" & date <= stopp_date)

search_eu_ndtr_l2 <- sapply(eu_search_grid, function(search_value){
  eu_search_data %>%
    mutate(
      eu_ndtr_l2_real = Reduce(function(values, rates){values*rates},
                               lead(eu_ndtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)],
      eu_ndtr_l2_return_model = (2*eu_ndtr_l1_return + (1-2)*(eu_rate/100)*(1/days) - 1) + (search_value/100),
      eu_ndtr_l2_stat = Reduce(function(values, rates){values*rates},
                               lead(eu_ndtr_l2_return_model), init = 100, accumulate = TRUE)[-nrow(.)],
      error = 1 - (eu_ndtr_l2_stat/eu_ndtr_l2_real)) %>%
    pull(error)})

search_metrics(search_eu_ndtr_l2, eu_search_grid)
rm(search_eu_ndtr_l2, eu_search_data, eu_search_grid)

us_search_grid <- seq(-0.1, 0.1, by = 1e-4)
us_search_data <- us_msci_model %>%
  filter(date >= "2000-12-29" & date <= stopp_date)

search_us_gdtr_s1 <- sapply(us_search_grid, function(search_value){
  us_search_data %>%
    mutate(
      us_gdtr_s1_real = Reduce(function(values, rates){values*rates},
                               lead(us_gdtr_s1_return), init = 100, accumulate = TRUE)[-nrow(.)],
      us_gdtr_s1_return_model = ((1 - us_gdtr_l1_return) + 1) + (2*(us_rate/100)*(1/days)) - (search_value/100),
      us_gdtr_s1_stat = Reduce(function(values, rates){values*rates},
                               lead(us_gdtr_s1_return_model), init = 100, accumulate = TRUE)[-nrow(.)],
      error = 1 - (us_gdtr_s1_stat/us_gdtr_s1_real)) %>%
    pull(error)})

search_metrics(search_us_gdtr_s1, us_search_grid)
rm(search_us_gdtr_s1, us_search_data, us_search_grid)

########################################################################################################################
# Vervollständigung der US- und EU-Indexvarianten
########################################################################################################################
# Zur Vervollständigung der Indexvarianten gehen wir davon aus, dass die Adjustierung zwischen den Währungsräumen identisch
# oder mindestens ähnlich ist, was eine relativ starke Annahme ist, die geprüft werden müsste -- allerdings gibt es derzeit
# keine offiziellen Daten der MSCI Group, weshalb dies erst zu einem späteren Zeitpunkt möglich sein wird.
us_msci_final <- us_msci_model %>%
  mutate(
    us_gdtr_l2_return = ifelse(date <= "2000-12-29", 
                               ((2*us_gdtr_l1_return + (1-2)*(us_rate/100)*(1/days)) - 1) - 0.0015/100,
                               us_gdtr_l2_return),
    us_gdtr_l2 = round(Reduce(function(values, rates){values*rates},
                              lead(us_gdtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    us_gdtr_l2 = case_when(row_number() == n() ~ round(lag(us_gdtr_l2)*us_gdtr_l2_return, digits = 2), TRUE ~ us_gdtr_l2),
    us_ndtr_l2_return = ((2*us_ndtr_l1_return + (1-2)*(us_rate/100)*(1/days)) - 1) - 0.0015/100,
    us_ndtr_l2 = round(Reduce(function(values, rates){values*rates},
                              lead(us_ndtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    us_ndtr_l2 = case_when(row_number() == n() ~ round(lag(us_ndtr_l2)*us_ndtr_l2_return, digits = 2), TRUE ~ us_ndtr_l2),
    us_gdtr_s1_return = ifelse(date <= "2000-12-29", ((1 - us_gdtr_l1_return) + 1) + (2*(us_rate/100)*(1/days)) - 0.0025/100,
                               us_gdtr_s1_return),
    us_gdtr_s1 = round(Reduce(function(values, rates){values*rates},
                              lead(us_gdtr_s1_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    us_gdtr_s1 = case_when(row_number() == n() ~ round(lag(us_gdtr_s1)*us_gdtr_s1_return, digits = 2), TRUE ~ us_gdtr_s1),
    us_ndtr_s1_return = ((1 - us_ndtr_l1_return) + 1) + (2*(us_rate/100)*(1/days)) - 0.0025/100,
    us_ndtr_s1 = round(Reduce(function(values, rates){values*rates},
                              lead(us_ndtr_s1_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    us_ndtr_s1 = case_when(row_number() == n() ~ round(lag(us_ndtr_s1)*us_ndtr_s1_return, digits = 2), TRUE ~ us_ndtr_s1),
    us_gdtr_s2_return = ((1 - us_gdtr_l1_return)*2 + 1) + (3*(us_rate/100)*(1/days)) - 2*(0.0025/100),
    us_gdtr_s2 = round(Reduce(function(values, rates){values*rates},
                              lead(us_gdtr_s2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    us_gdtr_s2 = case_when(row_number() == n() ~ round(lag(us_gdtr_s2)*us_gdtr_s2_return, digits = 2), TRUE ~ us_gdtr_s2),
    us_ndtr_s2_return = ((1 - us_ndtr_l1_return)*2 + 1) + (3*(us_rate/100)*(1/days)) - 2*(0.0025/100),
    us_ndtr_s2 = round(Reduce(function(values, rates){values*rates},
                              lead(us_ndtr_s2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    us_ndtr_s2 = case_when(row_number() == n() ~ round(lag(us_ndtr_s2)*us_ndtr_s2_return, digits = 2), TRUE ~ us_ndtr_s2),
    us_price = round(us_price/first(us_price)*100, digits = 5),
    us_gdtr_l1 = round(us_gdtr_l1/first(us_gdtr_l1)*100, digits = 5),
    us_ndtr_l1 = round(us_ndtr_l1/first(us_ndtr_l1)*100, digits = 5)) %>%
  select("date", "us_price", 
         "us_gdtr_l1", "us_gdtr_l2", "us_ndtr_l1", "us_ndtr_l2",
         "us_gdtr_s1", "us_gdtr_s2", "us_ndtr_s1", "us_ndtr_s2")
write.csv(us_msci_final, "./Project Amumbo USD Indices.csv", row.names = FALSE)

us_msci_final %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~date, y = ~log(us_price/100), line = list(width = 1),
    name = "Price Return", legendgroup = "price") %>%
  add_trace(
    x = ~date, y = ~log(us_gdtr_l1/100), line = list(width = 1),
    name = "Gross Total Return", legendgroup = "long1") %>%
  add_trace(
    x = ~date, y = ~log(us_ndtr_l1/100), line = list(width = 1),
    name = "Net Total Return", legendgroup = "long1") %>%
  add_trace(
    x = ~date, y = ~log(us_gdtr_l2/100), line = list(width = 1),
    name = "Gross Total Return x2", legendgroup = "long2") %>%
  add_trace(
    x = ~date, y = ~log(us_ndtr_l2/100), line = list(width = 1),
    name = "Net Total Return x2", legendgroup = "long2") %>%
  add_trace(
    x = ~date, y = ~log(us_gdtr_s1/100), line = list(width = 1),
    name = "Short Gross Total Return", legendgroup = "short1") %>%
  add_trace(
    x = ~date, y = ~log(us_ndtr_s1/100), line = list(width = 1),
    name = "Short Net Total Return", legendgroup = "short1") %>%
  add_trace(
    x = ~date, y = ~log(us_gdtr_s2/100), line = list(width = 1),
    name = "Short Gross Total Return x2", legendgroup = "short2") %>%
  add_trace(
    x = ~date, y = ~log(us_ndtr_s2/100), line = list(width = 1),
    name = "Short Net Total Return x2", legendgroup = "short2") %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'),
    xaxis = list(title = "Time"),
    yaxis = list(title = "Log. Index Value (USD)", range = c(-12, 12)),
    title = "MSCI USA Indices",
    margin = list(t = 50))


eu_msci_final <- eu_msci_model %>%
  mutate(
    eu_gdtr_l2_return = ((2*eu_gdtr_l1_return + (1-2)*(eu_rate/100)*(1/days)) - 1) - 0.0015/100,
    eu_gdtr_l2 = round(Reduce(function(values, rates){values*rates},
                              lead(eu_gdtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    eu_gdtr_l2 = case_when(row_number() == n() ~ round(lag(eu_gdtr_l2)*eu_gdtr_l2_return, digits = 2), TRUE ~ eu_gdtr_l2),
    eu_ndtr_l2_return = ifelse(date <= "2012-12-26", ((2*eu_ndtr_l1_return + (1-2)*(eu_rate/100)*(1/days)) - 1) - 0.0015/100,
                               eu_ndtr_l2_return),
    eu_ndtr_l2 = round(Reduce(function(values, rates){values*rates},
                              lead(eu_ndtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    eu_ndtr_l2 = case_when(row_number() == n() ~ round(lag(eu_ndtr_l2)*eu_ndtr_l2_return, digits = 2), TRUE ~ eu_ndtr_l2),
    eu_gdtr_s1_return = ((1 - eu_gdtr_l1_return) + 1) + (2*(eu_rate/100)*(1/days)) - 0.0025/100,
    eu_gdtr_s1 = round(Reduce(function(values, rates){values*rates},
                              lead(eu_gdtr_s1_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    eu_gdtr_s1 = case_when(row_number() == n() ~ round(lag(eu_gdtr_s1)*eu_gdtr_s1_return, digits = 2), TRUE ~ eu_gdtr_s1),
    eu_ndtr_s1_return = ((1 - eu_ndtr_l1_return) + 1) + (2*(eu_rate/100)*(1/days)) - 0.0025/100,
    eu_ndtr_s1 = round(Reduce(function(values, rates){values*rates},
                              lead(eu_ndtr_s1_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    eu_ndtr_s1 = case_when(row_number() == n() ~ round(lag(eu_ndtr_s1)*eu_ndtr_s1_return, digits = 2), TRUE ~ eu_ndtr_s1),
    eu_gdtr_s2_return = ((1 - eu_gdtr_l1_return)*2 + 1) + (3*(eu_rate/100)*(1/days)) - 2*(0.0025/100),
    eu_gdtr_s2 = round(Reduce(function(values, rates){values*rates},
                              lead(eu_gdtr_s2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    eu_gdtr_s2 = case_when(row_number() == n() ~ round(lag(eu_gdtr_s2)*eu_gdtr_s2_return, digits = 2), TRUE ~ eu_gdtr_s2),
    eu_ndtr_s2_return = ((1 - eu_ndtr_l1_return)*2 + 1) + (3*(eu_rate/100)*(1/days)) - 2*(0.0025/100),
    eu_ndtr_s2 = round(Reduce(function(values, rates){values*rates},
                              lead(eu_ndtr_s2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    eu_ndtr_s2 = case_when(row_number() == n() ~ round(lag(eu_ndtr_s2)*eu_ndtr_s2_return, digits = 2), TRUE ~ eu_ndtr_s2),
    eu_price = round(eu_price/first(eu_price)*100, digits = 5),
    eu_gdtr_l1 = round(eu_gdtr_l1/first(eu_gdtr_l1)*100, digits = 5),
    eu_ndtr_l1 = round(eu_ndtr_l1/first(eu_ndtr_l1)*100, digits = 5)) %>%
  select("date", "eu_price", 
         "eu_gdtr_l1", "eu_gdtr_l2", "eu_ndtr_l1", "eu_ndtr_l2",
         "eu_gdtr_s1", "eu_gdtr_s2", "eu_ndtr_s1", "eu_ndtr_s2")
write.csv(eu_msci_final, "./Project Amumbo EUR Indices.csv", row.names = FALSE)

eu_msci_final %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(
    x = ~date, y = ~log(eu_price/100), line = list(width = 1),
    name = "Price Return", legendgroup = "price") %>%
  add_trace(
    x = ~date, y = ~log(eu_gdtr_l1/100), line = list(width = 1),
    name = "Gross Total Return", legendgroup = "long1") %>%
  add_trace(
    x = ~date, y = ~log(eu_ndtr_l1/100), line = list(width = 1),
    name = "Net Total Return", legendgroup = "long1") %>%
  add_trace(
    x = ~date, y = ~log(eu_gdtr_l2/100), line = list(width = 1),
    name = "Gross Total Return x2", legendgroup = "long2") %>%
  add_trace(
    x = ~date, y = ~log(eu_ndtr_l2/100), line = list(width = 1),
    name = "Net Total Return x2", legendgroup = "long2") %>%
  add_trace(
    x = ~date, y = ~log(eu_gdtr_s1/100), line = list(width = 1),
    name = "Short Gross Total Return", legendgroup = "short1") %>%
  add_trace(
    x = ~date, y = ~log(eu_ndtr_s1/100), line = list(width = 1),
    name = "Short Net Total Return", legendgroup = "short1") %>%
  add_trace(
    x = ~date, y = ~log(eu_gdtr_s2/100), line = list(width = 1),
    name = "Short Gross Total Return x2", legendgroup = "short2") %>%
  add_trace(
    x = ~date, y = ~log(eu_ndtr_s2/100), line = list(width = 1),
    name = "Short Net Total Return x2", legendgroup = "short2") %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'),
    xaxis = list(title = "Time"),
    yaxis = list(title = "Log. Index Value (EUR)", range = c(-12, 12)),
    title = "MSCI USA Indices",
    margin = list(t = 50))


########################################################################################################################
# Bereinigung des Workspace
########################################################################################################################
rm(list = ls())