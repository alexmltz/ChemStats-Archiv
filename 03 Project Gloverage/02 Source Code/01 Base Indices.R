########################################################################################################################
# Vorbereitung des Systems
########################################################################################################################
packages <- c("data.table","tidyverse", "tidyquant", "imputeTS", "modelr", "plotly", "readxl", "mgcv", "httr", "jsonlite")
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
start_date <- "1974-12-31"
stopp_date <- "2025-09-01"


#---> Initialisierung Index-Zeitreihen:
msci_data <- list(
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
  #---> 2. Integration der ECB-Wechselkurswerte:
  read.csv("./01 Source Data/02 EuroStat/Eurostat Exchange Rate EUCR.csv") %>%
    filter(Currency == "US dollar") %>%
    mutate(
      date = as.Date(TIME_PERIOD, format="%Y-%m-%d"),
      eucr = 1 / OBS_VALUE) %>%
    filter(date >= start_date & date <= stopp_date) %>%
    select(date, eucr),
  #---> 3. Integration der US-Interbanken-Zinssätze (Effective Federal Funds Rate und Secured Overnight Financing Rate):
  read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=DFF", na.strings = ".") %>%
    rename(date = 1, effr = 2) %>%
    mutate(date = as.Date(date)),
  read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?id=SOFR", na.strings = ".") %>%
    rename(date = 1, sofr = 2) %>%
    mutate(date = as.Date(date)),
  #---> 4. Integration der EU-Interbanken-Zinssätze (EONIA und ESTER):
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
  #---> 5. Integration der MSCI-Indexdaten für die USD-Indexvarianten Price, Gross Total und Net Total Return:
  tq_get("^990100-USD-STRD", from = start_date) %>%
    select(date, us_price_yahoo = adjusted),
  scraping_index(code = "990100", type = "STRD", currency = "USD", start = gsub("-", "", start_date), stopp = gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      us_price_msci = level_eod) %>%
    select(date, us_price_msci),
  scraping_index(code = "990100", type = "GRTR", currency = "USD", start = gsub("-", "", start_date), stopp = gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      us_gdtr_l1 = level_eod) %>%
    select(date, us_gdtr_l1),
  scraping_index(code = "990100", type = "NETR", currency = "USD", start = gsub("-", "", start_date), stopp = gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      us_ndtr_l1 = level_eod) %>%
    select(date, us_ndtr_l1),
  #---> 6. Integration der MSCI-Indexdaten für die EUR-Indexvarianten Price, Gross Total und Net Total Return:
  scraping_index(code = "990100", type = "STRD", currency = "EUR", start = gsub("-", "", start_date), stopp = gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      eu_price = level_eod) %>%
    select(date, eu_price),
  scraping_index(code = "990100", type = "GRTR", currency = "EUR", start = gsub("-", "", start_date), stopp = gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      eu_gdtr_l1 = level_eod) %>%
    select(date, eu_gdtr_l1),
  scraping_index(code = "990100", type = "NETR", currency = "EUR", start = gsub("-", "", start_date), stopp = gsub("-", "", stopp_date)) %>%
    mutate(
      date = as.Date(as.character(calc_date), format = "%Y%m%d"),
      eu_ndtr_l1 = level_eod) %>%
    select(date, eu_ndtr_l1),
  #---> 7. Integration der Leverage MSCI-Indexdaten für die USD-Indexvarianten Gross Total Return:
  read.table("https://app2.msci.com/eqb/short/performance/90479.49.all.xls", sep = ",", skip = 4) %>%
    filter(grepl("\t", V1)) %>%
    separate_wider_delim(V1, delim = "\t", names = c("date", "us_gdtr_l2"), too_few = "align_start") %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
      us_gdtr_l2 = as.numeric(us_gdtr_l2))) %>%
  Reduce(function(x, y) left_join(x, y, by = "date"), .) %>%
  filter(date <= stopp_date) %>%
  #---> 8. Stineman-Spline-Interpolierung fehlender Werte und Berechnung der Daily Returns:
  mutate(
    #---> 8a. USD-Indizes:
    us_price = ifelse(date >= "1996-12-31", us_price_msci, us_price_yahoo),
    us_price = round(na_interpolation(us_price, "stine"), digits = 5),
    us_price_return = us_price/lag(us_price),
    us_gdtr_l1 = ifelse(date >= "1998-12-31", round(na_interpolation(us_gdtr_l1, "stine"), digits = 5), NA),
    us_gdtr_l1_return = us_gdtr_l1/lag(us_gdtr_l1),
    us_ndtr_l1 = ifelse(date >= "1998-12-31", round(na_interpolation(us_ndtr_l1, "stine"), digits = 5), NA),
    us_ndtr_l1_return = us_ndtr_l1/lag(us_ndtr_l1),
    us_gdtr_l2 = ifelse(date >= "2000-12-29", round(na_interpolation(us_gdtr_l2, "stine"), digits = 5), NA),
    us_gdtr_l2_return = us_gdtr_l2/lag(us_gdtr_l2),
    #---> 8b. EUR-Indizes:
    eu_price = ifelse(date >= "1999-01-01", round(na_interpolation(eu_price, "stine"), digits = 5), NA),
    eu_price_return = eu_price/lag(eu_price),
    eu_gdtr_l1 = ifelse(date >= "2000-12-29", round(na_interpolation(eu_gdtr_l1, "stine"), digits = 5), NA),
    eu_gdtr_l1_return = eu_gdtr_l1/lag(eu_gdtr_l1),
    eu_ndtr_l1 = ifelse(date >= "2000-12-29", round(na_interpolation(eu_ndtr_l1, "stine"), digits = 5), NA),
    eu_ndtr_l1_return = eu_ndtr_l1/lag(eu_ndtr_l1),
    #---> 8c. Zins- und Wechselkurse:
    us_rate = na_interpolation(ifelse(date <= "2021-07-31", effr, sofr), "stine"),
    us_rate = round(us_rate, digits = 3),
    eu_rate = na_interpolation(ifelse(date <= "2021-07-31", ifelse(date <= "1999-01-01", ftgr, eonia), ester), "stine"),
    eu_rate = round(eu_rate, digits = 3),
    eucr = round(na_interpolation(eucr, "stine"), digits = 5))


#---> Vervollständigung der Index-Zeitreihen:
msci_model <- msci_data %>%
  #---> 1. MSCI World USD Gross Total Return:
  add_predictions(gam(us_gdtr_l1_return ~ s(us_price_return, bs = "cr"), data = .), var = "us_gdtr_l1_model") %>%
  mutate(
    us_gdtr_l1_return = ifelse(date < "1998-12-31" | is.na(us_gdtr_l1_return), us_gdtr_l1_model, us_gdtr_l1_return),
    us_gdtr_l1 = ifelse(date < "1998-12-31",
                        rev(Reduce(function(values, rates){values/rates},
                                   rev(us_gdtr_l1_return), init = last(us_gdtr_l1), accumulate = TRUE)[-nrow(.)]),
                        us_gdtr_l1),
    us_gdtr_l1 = ifelse(row_number() == 1, round(lead(us_gdtr_l1)/lead(us_gdtr_l1_return), digits = 5), round(us_gdtr_l1, digits = 5))) %>%
  #---> 2. MSCI World USD Net Total Return:
  add_predictions(gam(us_ndtr_l1_return ~ s(us_price_return, bs = "cr"), data = .), var = "us_ndtr_l1_model") %>%
  mutate(
    us_ndtr_l1_return = ifelse(date < "1998-12-31" | is.na(us_ndtr_l1_return), us_ndtr_l1_model, us_ndtr_l1_return),
    us_ndtr_l1 = ifelse(date < "1998-12-31",
                        rev(Reduce(function(values, rates){values/rates},
                                   rev(us_ndtr_l1_model), init = last(us_ndtr_l1), accumulate = TRUE)[-nrow(.)]),
                        us_ndtr_l1),
    us_ndtr_l1 = ifelse(row_number() == 1, round(lead(us_ndtr_l1)/lead(us_ndtr_l1_return), digits = 5), round(us_ndtr_l1, digits = 5))) %>%
  #---> 3. MSCI World EUR Price Return:
  add_predictions(gam(eu_price/us_price ~ s(eucr, bs = "cr"), data = .), var = "scale_factor") %>%
  mutate(
    eu_price = ifelse(date <= "1999-01-01", scale_factor * us_price, eu_price),
    eu_price_return = eu_price/lag(eu_price),
    eu_price = ifelse(row_number() == 1, round(lead(eu_price)/lead(eu_price_return), digits = 5), round(eu_price, digits = 5))) %>%
  #---> 4. MSCI World EUR Gross Total Return:
  add_predictions(gam(eu_gdtr_l1_return ~ s(eu_price_return, bs = "cr"), data = .), var = "eu_gdtr_l1_model") %>%
  mutate(
    eu_gdtr_l1_return = ifelse(date < "2000-12-29" | is.na(eu_gdtr_l1_return), eu_gdtr_l1_model, eu_gdtr_l1_return),
    eu_gdtr_l1 = ifelse(date < "2000-12-29",
                          rev(Reduce(function(values, rates){values/rates},
                                     rev(eu_gdtr_l1_return), init = last(eu_gdtr_l1), accumulate = TRUE)[-nrow(.)]),
                          eu_gdtr_l1),
      eu_gdtr_l1 = ifelse(row_number() == 1, round(lead(eu_gdtr_l1)/lead(eu_gdtr_l1_return), digits = 5), round(eu_gdtr_l1, digits = 5))) %>%
  #---> 5. MSCI World EUR Net Total Return:
    add_predictions(gam(eu_ndtr_l1_return ~ s(eu_price_return, bs = "cr"), data = .), var = "eu_ndtr_l1_model") %>%
    mutate(
      eu_ndtr_l1_return = ifelse(date < "2000-12-29" | is.na(eu_ndtr_l1_return), eu_ndtr_l1_model, eu_ndtr_l1_return),
      eu_ndtr_l1 = ifelse(date < "2000-12-29",
                          rev(Reduce(function(values, rates){values/rates},
                                     rev(eu_ndtr_l1_model), init = last(eu_ndtr_l1), accumulate = TRUE)[-nrow(.)]),
                          eu_ndtr_l1),
      eu_ndtr_l1 = ifelse(row_number() == 1, round(lead(eu_ndtr_l1)/lead(eu_ndtr_l1_return), digits = 5), round(eu_ndtr_l1, digits = 5))) %>%
  filter(date >= "1975-01-01")


#---> Visualisierung der Index-Zeitreihen:
msci_model %>%
  select(date, us_price, us_gdtr_l1, us_ndtr_l1, eu_price, eu_gdtr_l1, eu_ndtr_l1) %>%
  mutate(across(where(is.numeric), function(x){x/first(x)})) %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(x = ~date, y = ~us_price, name = "Price Return  (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~us_gdtr_l1, name = "Gross Total Return (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~us_ndtr_l1, name = "Net Total Return (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~eu_price, name = "Price Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~eu_gdtr_l1, name = "Gross Total Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~eu_ndtr_l1, name = "Net Total Return (EUR)", line = list(width = 1)) %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'), 
    xaxis = list(title = "Time"),
    yaxis = list(title = "Index Value"),
    title = "MSCI World Indices",
    margin = list(t = 50))

msci_model %>%
  select(date,us_price, us_gdtr_l1, us_ndtr_l1, eu_price, eu_gdtr_l1, eu_ndtr_l1) %>%
  mutate(across(where(is.numeric), function(x){x/first(x)})) %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(x = ~date, y = ~log(us_price), name = "Price Return  (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(us_gdtr_l1), name = "Gross Total Return (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(us_ndtr_l1), name = "Net Total Return (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_price), name = "Price Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_gdtr_l1), name = "Gross Total Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_ndtr_l1), name = "Net Total Return (EUR)", line = list(width = 1)) %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'), 
    xaxis = list(title = "Time"),
    yaxis = list(title = "Log. Index Value"),
    title = "MSCI World Indices",
    margin = list(t = 50))



#---> Initialisierung des Grid-Search-Algorithmus:
search_grid <- seq(-0.1, 0.1, by = 1e-4)
search_data <- msci_data %>%
  filter(date >= "2000-12-29" & date <= stopp_date)

search_results <- sapply(search_grid, function(search_value){
  search_data %>%
    mutate(
      us_gdtr_l2_real = Reduce(function(values, rates){values*rates},
                               lead(us_gdtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)],
      us_gdtr_l2_return_model = (2*us_gdtr_l1_return + (1-2)*(us_rate/100)*(1/days) - 1) + (search_value/100),
      us_gdtr_l2_stat = Reduce(function(values, rates){values*rates},
                               lead(us_gdtr_l2_return_model), init = 100, accumulate = TRUE)[-nrow(.)],
      error = 1 - (us_gdtr_l2_stat/us_gdtr_l2_real)) %>%
    pull(error)})

search_metrics(search_results, search_grid)
rm(search_results, search_data, search_grid)


#---> Finalisierung der Index-Zeitreihen:
msci_final <- msci_model %>%
  mutate(
    us_gdtr_l2_return = ifelse(date <= "2000-12-29",
                               ((2*us_gdtr_l1_return + (1-2)*(us_rate/100)*(1/days)) - 1) - (0.0015/100),
                               us_gdtr_l2_return),
    us_gdtr_l2 = round(Reduce(function(values, rates){values*rates},
                              lead(us_gdtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    us_gdtr_l2 = case_when(row_number() == n() ~ round(lag(us_gdtr_l2)*us_gdtr_l2_return, digits = 2), TRUE ~ us_gdtr_l2),
    us_ndtr_l2_return = us_gdtr_l2_return - 2*(us_gdtr_l1_return - us_ndtr_l1_return),
    
    us_ndtr_l2 = round(Reduce(function(values, rates){values*rates},
                              lead(us_ndtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    us_ndtr_l2 = case_when(row_number() == n() ~ round(lag(us_ndtr_l2)*us_ndtr_l2_return, digits = 2), TRUE ~ us_ndtr_l2),
    us_price = round(us_price/first(us_price), digits = 5),

    eu_gdtr_l2_return = (2*eu_gdtr_l1_return + (1-2)*(eu_rate/100)*(1/days)) - 1 - (0.0015/100),
    eu_gdtr_l2 = round(Reduce(function(values, rates){values*rates},
                              lead(eu_gdtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    eu_gdtr_l2 = case_when(row_number() == n() ~ round(lag(eu_gdtr_l2)*eu_gdtr_l2_return, digits = 2), TRUE ~ eu_gdtr_l2),
    eu_ndtr_l2_return = eu_gdtr_l2_return - 2*(eu_gdtr_l1_return - eu_ndtr_l1_return),
    
    eu_ndtr_l2 = round(Reduce(function(values, rates){values*rates},
                              lead(eu_ndtr_l2_return), init = 100, accumulate = TRUE)[-nrow(.)], digits = 5),
    eu_ndtr_l2 = case_when(row_number() == n() ~ round(lag(eu_ndtr_l2)*eu_ndtr_l2_return, digits = 2), TRUE ~ eu_ndtr_l2),
    eu_price = round(eu_price/first(eu_price), digits = 5)) %>%
    select(date, days, eucr, us_rate, eu_rate,
           us_price, eu_price,
           us_gdtr_l1, eu_gdtr_l1, us_ndtr_l1, eu_ndtr_l1,
           us_gdtr_l2, eu_gdtr_l2, us_ndtr_l2, eu_ndtr_l2) %>%
  mutate(across(-c(date, days, eucr, us_rate, eu_rate), ~ round(.x/first(.x), digits = 5)*100))

msci_final %>%
  select(date, us_price, eu_price,
         us_gdtr_l1, us_ndtr_l1, us_gdtr_l2, us_ndtr_l2,
         eu_gdtr_l1, eu_ndtr_l1, eu_gdtr_l2, eu_ndtr_l2) %>%
  mutate(across(where(is.numeric), function(x){x/first(x)})) %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(x = ~date, y = ~us_price/100, name = "Price Return  (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~us_gdtr_l1/100, name = "Gross Total Return (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~us_ndtr_l1/100, name = "Net Total Return (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~us_gdtr_l2/100, name = "Gross Total Return x2 (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~us_ndtr_l2/100, name = "Net Total Return x2 (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~eu_price/100, name = "Price Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~eu_gdtr_l1/100, name = "Gross Total Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~eu_ndtr_l1/100, name = "Net Total Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~eu_gdtr_l2/100, name = "Gross Total Return x2 (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~eu_ndtr_l2/100, name = "Net Total Return x2 (EUR)", line = list(width = 1)) %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'), 
    xaxis = list(title = "Time"),
    yaxis = list(title = "Index Value"),
    title = "MSCI World Indices",
    margin = list(t = 50))

msci_final %>%
  select(date, us_price, eu_price,
         us_gdtr_l1, us_ndtr_l1, us_gdtr_l2, us_ndtr_l2,
         eu_gdtr_l1, eu_ndtr_l1, eu_gdtr_l2, eu_ndtr_l2) %>%
  mutate(across(where(is.numeric), function(x){x/first(x)})) %>%
  plot_ly(type = "scatter", mode = "lines") %>%
  add_trace(x = ~date, y = ~log(us_price), name = "Price Return  (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(us_gdtr_l1), name = "Gross Total Return (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(us_ndtr_l1), name = "Net Total Return (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(us_gdtr_l2), name = "Gross Total Return x2 (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(us_ndtr_l2), name = "Net Total Return x2 (USD)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_price), name = "Price Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_gdtr_l1), name = "Gross Total Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_ndtr_l1), name = "Net Total Return (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_gdtr_l2), name = "Gross Total Return x2 (EUR)", line = list(width = 1)) %>%
  add_trace(x = ~date, y = ~log(eu_ndtr_l2), name = "Net Total Return x2 (EUR)", line = list(width = 1)) %>%
  layout(
    showlegend = TRUE, legend = list(orientation = 'h'), 
    xaxis = list(title = "Time"),
    yaxis = list(title = "Log. Index Value"),
    title = "MSCI World Indices",
    margin = list(t = 50))


#---> Separierter Export von USD- und EUR-Indexreihen:
msci_final %>%
  select(
    date, days, world_us_price = us_price, 
    world_us_ndtr_l1 = us_ndtr_l1, world_us_gdtr_l1 = us_gdtr_l1,
    world_us_ndtr_l2 = us_ndtr_l2, world_us_gdtr_l2 = us_gdtr_l2) %>%
  fwrite(., "./03 Results Data/01 Equity Indicies/MSCI World USD.csv", row.names = FALSE)

msci_final %>%
  select(
    date, days, world_eu_price = eu_price,
    world_eu_ndtr_l1 = eu_ndtr_l1, world_eu_gdtr_l1 = eu_gdtr_l1, 
    world_eu_ndtr_l2 = eu_ndtr_l2, world_eu_gdtr_l2 = eu_gdtr_l2) %>%
  fwrite(., "./03 Results Data/01 Equity Indicies/MSCI World EUR.csv", row.names = FALSE)

########################################################################################################################

#---> Berechnung ungehebelter Referenz:
msci_full_data <- list(
  read.csv("./03 Results Data/01 Equity Indicies/MSCI World USD.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    select(date, days, world_us_ndtr_l1),
  read.csv("./01 Source Data/03 Data Services/Onvista iShares Core MSCI World.csv", sep = ";") %>%
    mutate(
      date = as.Date(Datum, format = "%d.%m.%Y"),
      ishares_l1_us_in_eu = as.numeric(gsub(",", ".", Schluss))) %>%
    select(date, ishares_l1_us_in_eu),
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
    ex_rate = eucr/first(eucr),
    ishares_l1_us_in_eu = ifelse(date >= "2009-10-20", na_interpolation(ishares_l1_us_in_eu, "stine"), NA),
    ishares_l1_us_in_us = ishares_l1_us_in_eu/ex_rate)
  

search_grid <- seq(-0.1, 0.1, by = 1e-4)
search_data <- sapply(search_grid, function(search_value){
  msci_full_data %>% 
    filter(date >= "2009-10-20") %>% # <- Erster Handelstag realer ETF
    mutate(
      expense = 0.2, # <- Hier: Angabe TER
      msci_return = world_us_ndtr_l1/lag(world_us_ndtr_l1),  # <- Basisindex
      sims_return = (msci_return - (((expense/100 + 1)^(1/days) - 1) + (search_value/100))),
      sims_fonds = Reduce(function(values, rates){values * rates}, 
                          lead(sims_return), init = 100, accumulate = TRUE)[-nrow(.)],
      ishares_l1_us_in_us = ishares_l1_us_in_us/first(ishares_l1_us_in_us) * 100,
      error = (1 - sims_fonds/ishares_l1_us_in_us)) %>%
    pull(error)})

search_metrics(search_data, search_grid)
rm(search_data, search_grid)


#---> Simulation ungehebelter Referenz:
msci_sims_data <- msci_full_data %>%
  mutate(
    ter_l1 = 0.2,
    index_l1_return = world_us_ndtr_l1/lag(world_us_ndtr_l1),
    fonds_l1_return = ishares_l1_us_in_us/lag(ishares_l1_us_in_us),
    sim_l1_return = ifelse(date <= "2009-10-20",
                           index_l1_return - ((ter_l1/100 + 1)^(1/days) - 1) - 0.0003/100,
                           fonds_l1_return),
    sim_l1 = Reduce(function(values, rates){values*rates},
                    lead(sim_l1_return), init = 100, accumulate = TRUE)[-nrow(.)],
    sim_l1 = case_when(row_number() == n() ~ round(lag(sim_l1)*sim_l1_return, digits = 2), TRUE ~ sim_l1),
    world_sim_l1 = sim_l1*ex_rate) %>%
  select(date, world_sim_l1) %>%
  filter(date <= stopp_date)

write.csv(msci_sims_data, "./03 Results Data/01 Equity Indicies/Reference World.csv", row.names =FALSE)
########################################################################################################################
# Bereinigung des Workspace
########################################################################################################################
rm(list = ls())
