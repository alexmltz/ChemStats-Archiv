########################################################################################################################
# Vorbereitung des Systems
########################################################################################################################
packages <- c("tidyverse", "tidyquant", "imputeTS", "foreach", "parallel", "doParallel", "doFuture", "data.table", "Rcpp", "myLeverage")
invisible(lapply(packages, library, character.only = TRUE)); rm(packages)

if(.Platform$OS.type == "windows"){Sys.setlocale("LC_ALL", "en_US.UTF-8")}

#---> Definition des Analysehorizonts:
start_date <- "1975-01-01"
stopp_date <- "2024-12-31"

#---> Vorbereitung der Analysedaten:
msci_letf <- list(
  read.csv("./Project Amumbo UCITS Funds.csv") %>%
    mutate(date = as.Date(date)),
  read.csv("./Project Amumbo Bond Yields.csv") %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    filter(!weekdays(date) %in% c("Saturday", "Sunday"),
           month(date) == 1,
           day(date) != 1) %>% 
    group_by(year = year(date)) %>%
    slice_min(date) %>%
    ungroup() %>%
    filter(date >= start_date & date <= stopp_date) %>%
    select(date, rate = m15)) %>%
  Reduce(function(x, y) left_join(x, y, by = "date"), .) %>%
  mutate(rate = na_locf(rate)) %>%
  filter(date >= start_date & date <= stopp_date)

configs <- list(
#  list(name = "No Taxes", tax_mode = "none", spread = 0.5),
#  list(name = "German Taxes", tax_mode = "person", spread = 0.5),
  list(name = "Fund Fees", tax_mode = "funds", spread = 0)
)

no_cores <- detectCores() - 1


########################################################################################################################
# Long Lump Sum Buy and Hold-Strategien
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "bnh", action_dcas = "hold",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- seq(10, 40, by = 10)

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Long x2
for(row in seq_along(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(across(-c(date, rate), ~ . / first(.) * 100)) %>%
                           mutate(letf = sim_l2, buy = TRUE, sell = FALSE) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = FALSE, dca_value = NULL, dca_span = NULL, dca_day = NULL, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                        c(date_ranges[n, 1], date_ranges[n, 2], sim_results$ttwror, sim_results$buys, sim_results$sells,
                          sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                          sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                          quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Buy and Hold/Lump Sum - Long x2 - ", config$name, " - ", params[row], " Years.csv"), row.names = FALSE)
  }
}

#---> Long x1
for(row in seq_along(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(across(-c(date, rate), ~ . / first(.) * 100)) %>%
                           mutate(letf = sim_l1, buy = TRUE, sell = FALSE) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = FALSE, dca_value = NULL, dca_span = NULL, dca_day = NULL, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Buy and Hold/Lump Sum - Long x1 - ", config$name, " - ", params[row], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Short Lump Sum Buy and Hold-Strategien
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "bnh", action_dcas = "hold",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- seq(10, 40, by = 10)

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Short x2
for(row in seq_along(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(across(-c(date, rate), ~ . / first(.) * 100)) %>%
                           mutate(letf = sim_s2, buy = TRUE, sell = FALSE) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = FALSE, dca_value = NULL, dca_span = NULL, dca_day = NULL, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Buy and Hold/Lump Sum - Short x2 - ", config$name, " - ", params[row], " Years.csv"), row.names = FALSE)
  }
}

#---> Short x1
for(row in seq_along(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(across(-c(date, rate), ~ . / first(.) * 100)) %>%
                           mutate(letf = sim_s1, buy = TRUE, sell = FALSE) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = FALSE, dca_value = NULL, dca_span = NULL, dca_day = NULL, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Buy and Hold/Lump Sum - Short x1 - ", config$name, " - ", params[row], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Long Dollar Cost Averaging Buy and Hold-Strategien
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "bnh", action_dcas = "trade",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- seq(10, 40, by = 10)

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Long x2
for(row in seq_along(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(across(-c(date, rate), ~ . / first(.) * 100)) %>%
                           mutate(letf = sim_l2, buy = TRUE, sell = FALSE) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Buy and Hold/DCA Trade - Long x2 - ", config$name, " - ", params[row], " Years.csv"), row.names = FALSE)
  }
}

#---> Long x1
for(row in seq_along(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(across(-c(date, rate), ~ . / first(.) * 100)) %>%
                           mutate(letf = sim_l1, buy = TRUE, sell = FALSE) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}


    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Buy and Hold/DCA Trade - Long x1 - ", config$name, " - ", params[row], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Short Dollar Cost Averaging Buy and Hold-Strategien
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "bnh", action_dcas = "trade",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- seq(10, 40, by = 10)

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Short x2
for(row in seq_along(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(across(-c(date, rate), ~ . / first(.) * 100)) %>%
                           mutate(letf = sim_s2, buy = TRUE, sell = FALSE) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Buy and Hold/DCA Trade - Short x2 - ", config$name, " - ", params[row], " Years.csv"), row.names = FALSE)
  }
}

#---> Short x1
for(row in seq_along(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(across(-c(date, rate), ~ . / first(.) * 100)) %>%
                           mutate(letf = sim_s1, buy = TRUE, sell = FALSE) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Buy and Hold/DCA Trade - Short x1 - ", config$name, " - ", params[row], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Long Lump Sum SMA-Strategien
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "sma", action_dcas = "hold",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- expand.grid(sma = seq(10, 600, by = 10), span = seq(10, 40, by = 10))
params <- params[order(params$span, params$sma), ]

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Long x2
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(eu_index_l1, params[row, "sma"]),
                             ma_exit = SMA(eu_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(eu_index_l1) * 100,
                             ma_exit = ma_exit / first(eu_index_l1) * 100,
                             ma_index = eu_index_l1 / first(eu_index_l1) * 100,
                             letf = sim_l2,
                             buy = ma_index >= ma_entry,
                             sell = ma_index < ma_exit) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = FALSE, dca_value = NULL, dca_span = NULL, dca_day = NULL, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Moving Average/Lump Sum - Long x2 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}

#---> Long x1
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(us_index_l1, params[row, "sma"]),
                             ma_exit = SMA(us_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(us_index_l1) * 100,
                             ma_exit = ma_exit / first(us_index_l1) * 100,
                             ma_index = us_index_l1 / first(us_index_l1) * 100,
                             letf = sim_l1,
                             buy = ma_index >= ma_entry,
                             sell = ma_index < ma_exit) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = FALSE, dca_value = NULL, dca_span = NULL, dca_day = NULL, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Moving Average/Lump Sum - Long x1 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Short Lump Sum SMA-Strategien
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "sma", action_dcas = "hold",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- expand.grid(sma = seq(10, 600, by = 10), span = seq(10, 40, by = 10))
params <- params[order(params$span, params$sma), ]

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Short x2
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(us_index_l1, params[row, "sma"]),
                             ma_exit = SMA(us_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(us_index_l1) * 100,
                             ma_exit = ma_exit / first(us_index_l1) * 100,
                             ma_index = us_index_l1 / first(us_index_l1) * 100,
                             letf = sim_s2,
                             buy = ma_index < ma_entry,
                             sell = ma_index >= ma_exit) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = FALSE, dca_value = NULL, dca_span = NULL, dca_day = NULL, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Moving Average/Lump Sum - Short x2 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}

#---> Short x1
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind,
                       .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(us_index_l1, params[row, "sma"]),
                             ma_exit = SMA(us_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(us_index_l1) * 100,
                             ma_exit = ma_exit / first(us_index_l1) * 100,
                             ma_index = us_index_l1 / first(us_index_l1) * 100,
                             letf = sim_s1,
                             buy = ma_index < ma_entry,
                             sell = ma_index >= ma_exit) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = FALSE, dca_value = NULL, dca_span = NULL, dca_day = NULL, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Moving Average/Lump Sum - Short x1 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Long Dollar Cost Averaging SMA-Strategien mit einem Holding-Element
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "sma", action_dcas = "hold",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- expand.grid(sma = seq(10, 600, by = 10), span = seq(10, 40, by = 10))
params <- params[order(params$span, params$sma), ]

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Long x2
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(eu_index_l1, params[row, "sma"]),
                             ma_exit = SMA(eu_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(eu_index_l1) * 100,
                             ma_exit = ma_exit / first(eu_index_l1) * 100,
                             ma_index = eu_index_l1 / first(eu_index_l1) * 100,
                             letf = sim_l2,
                             buy = ma_index >= ma_entry,
                             sell = ma_index < ma_exit) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Moving Average/DCA Hold - Long x2 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}

#---> Long x1
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(us_index_l1, params[row, "sma"]),
                             ma_exit = SMA(us_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(us_index_l1) * 100,
                             ma_exit = ma_exit / first(us_index_l1) * 100,
                             ma_index = us_index_l1 / first(us_index_l1) * 100,
                             letf = sim_l1,
                             buy = ma_index >= ma_entry,
                             sell = ma_index < ma_exit) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Moving Average/DCA Hold - Long x1 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Short DCA SMA-Strategien mit einem Holding-Element
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "sma", action_dcas = "hold",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- expand.grid(sma = seq(10, 600, by = 10), span = seq(10, 40, by = 10))
params <- params[order(params$span, params$sma), ]

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Short x2
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(us_index_l1, params[row, "sma"]),
                             ma_exit = SMA(us_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(us_index_l1) * 100,
                             ma_exit = ma_exit / first(us_index_l1) * 100,
                             ma_index = us_index_l1 / first(us_index_l1) * 100,
                             letf = sim_s2,
                             buy = ma_index < ma_entry,
                             sell = ma_index >= ma_exit) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Moving Average/DCA Hold - Short x2 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}

#---> Short x1
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])

  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(us_index_l1, params[row, "sma"]),
                             ma_exit = SMA(us_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(us_index_l1) * 100,
                             ma_exit = ma_exit / first(us_index_l1) * 100,
                             ma_index = us_index_l1 / first(us_index_l1) * 100,
                             letf = sim_s1,
                             buy = ma_index < ma_entry,
                             sell = ma_index >= ma_exit) %>%
                           select(date, letf, buy, sell, rate)

                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)

                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}

    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))

    fwrite(results, paste0("./03 Results Data/Moving Average/DCA Hold - Short x1 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Long DCA SMA-Strategien mit einem Trading-Element
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "sma", action_dcas = "trade",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- expand.grid(sma = seq(10, 600, by = 10), span = seq(10, 40, by = 10))
params <- params[order(params$span, params$sma), ]

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Long x2
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])
  
  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(eu_index_l1, params[row, "sma"]),
                             ma_exit = SMA(eu_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(eu_index_l1) * 100,
                             ma_exit = ma_exit / first(eu_index_l1) * 100,
                             ma_index = eu_index_l1 / first(eu_index_l1) * 100,
                             letf = sim_l2,
                             buy = ma_index >= ma_entry,
                             sell = ma_index < ma_exit) %>%
                           select(date, letf, buy, sell, rate)
                         
                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)
                         
                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}
    
    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))
    
    fwrite(results, paste0("./03 Results Data/Moving Average/DCA Trade - Long x2 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}

#---> Long x1
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])
  
  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(us_index_l1, params[row, "sma"]),
                             ma_exit = SMA(us_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(us_index_l1) * 100,
                             ma_exit = ma_exit / first(us_index_l1) * 100,
                             ma_index = us_index_l1 / first(us_index_l1) * 100,
                             letf = sim_l1,
                             buy = ma_index >= ma_entry,
                             sell = ma_index < ma_exit) %>%
                           select(date, letf, buy, sell, rate)
                         
                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)
                         
                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}
    
    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))
    
    fwrite(results, paste0("./03 Results Data/Moving Average/DCA Trade - Long x1 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Short DCA SMA-Strategien mit einem Trading-Element
########################################################################################################################
strategy <- list(
  letf = list(
    asset_share = 1,
    asset_class = "etf", asset_bonus = 0.3,
    action_type = "sma", action_dcas = "trade",
    signal_buy = "buy", signal_sell = "sell"
  )
)

params <- expand.grid(sma = seq(10, 600, by = 10), span = seq(10, 40, by = 10))
params <- params[order(params$span, params$sma), ]

sim_clust <- makeCluster(no_cores)
registerDoParallel(sim_clust)
#---> Short x2
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])
  
  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(us_index_l1, params[row, "sma"]),
                             ma_exit = SMA(us_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(us_index_l1) * 100,
                             ma_exit = ma_exit / first(us_index_l1) * 100,
                             ma_index = us_index_l1 / first(us_index_l1) * 100,
                             letf = sim_s2,
                             buy = ma_index < ma_entry,
                             sell = ma_index >= ma_exit) %>%
                           select(date, letf, buy, sell, rate)
                         
                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)
                         
                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}
    
    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))
    
    fwrite(results, paste0("./03 Results Data/Moving Average/DCA Trade - Short x2 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}

#---> Short x1
for(row in 1:nrow(params)){
  date_ranges <- create_sequences(as.character(msci_letf$date), span = params[row, "span"], signal_a = params[row, "sma"])
  
  for(config in configs){
    results <- foreach(n = 1:nrow(date_ranges), .combine = rbind, .packages = c("tidyverse", "tidyquant", "imputeTS", "myLeverage"),
                       .options.future = list(chunk.size = ceiling(nrow(date_ranges)/no_cores))) %dopar% {
                         data <- msci_letf %>%
                           mutate(
                             ma_entry = SMA(us_index_l1, params[row, "sma"]),
                             ma_exit = SMA(us_index_l1, params[row, "sma"])) %>%
                           filter(date >= date_ranges[n, 1] & date <= date_ranges[n, 2]) %>%
                           mutate(
                             ma_entry = ma_entry / first(us_index_l1) * 100,
                             ma_exit = ma_exit / first(us_index_l1) * 100,
                             ma_index = us_index_l1 / first(us_index_l1) * 100,
                             letf = sim_s1,
                             buy = ma_index < ma_entry,
                             sell = ma_index >= ma_exit) %>%
                           select(date, letf, buy, sell, rate)
                         
                         sim_results <- simulation(data, strategy,
                                                   start_value = 1000, spread = config$spread, details = FALSE, liquidate = FALSE,
                                                   dca_mode = TRUE, dca_value = 1000, dca_span = 1, dca_day = 1, dca_unit = "month",
                                                   balance_mode = FALSE, balance_span = NULL, balance_day = NULL, balance_unit = "month",
                                                   balance_dca = FALSE, tax_mode = config$tax_mode, tax_rate = 0.26375,
                                                   base_rate = NULL, base_rate_flex = TRUE, base_rate_data = "rate",
                                                   fractions = TRUE, split_mode = FALSE, split_tresh = NULL, funds_fee = 1, bonus_fee = 5)
                         
                         c(date_ranges[n, 1], date_ranges[n, 2], params[row, "sma"], sim_results$ttwror, sim_results$buys, sim_results$sells,
                           sim_results$statistics$mean, sim_results$statistics$sd, sim_results$statistics$cv,
                           sim_results$statistics$skewness, sim_results$statistics$kurtosis, sim_results$statistics$lpm, sim_results$statistics$hpm,
                           quantile(sim_results$drawdowns, probs = seq(0, 1, by = 0.1)))}
    
    results <- as.data.frame(results)
    colnames(results) <- c("Start_Date", "Stopp_Date", "SMA", "TTWROR", "Buy Trades", "Sell Trades", "Return_Mean", "Return_Std", "Return_CV",
                           "Return_Skewness", "Return_Kurtosis", "Return_LPM", "Return_HPM",
                           paste0(c("Min", seq(10, 90, by = 10), "Max"), "_DD"))
    results <- transform(results, Start_Date = as.Date(Start_Date), Stopp_Date = as.Date(Stopp_Date))
    
    fwrite(results, paste0("./03 Results Data/Moving Average/DCA Trade - Short x1 - SMA ", params[row, "sma"], " - ", config$name, " - ", params[row, "span"], " Years.csv"), row.names = FALSE)
  }
}
stopCluster(sim_clust)


########################################################################################################################
# Bereinigung des Workspace
########################################################################################################################
rm(list = ls())