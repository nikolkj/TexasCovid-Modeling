#!/usr/bin/Rscript
# Create model-objects 

rm(list = ls());
require(tidyverse)
require(magrittr)
require(forecast)
#imputeTS

dat = readr::read_csv(
  file = "/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/Texas-County-Main.csv",
  col_names = TRUE,
  col_types = cols(
    County = readr::col_factor(),
    Date = readr::col_date(),
    DailyCount_cases = readr::col_integer(),
    DailyDelta_cases = readr::col_integer(),
    DailyCount_tests = readr::col_integer(),
    DailyDelta_tests = readr::col_integer(),
    DailyCount_deaths = readr::col_integer(),
    DailyDelta_deaths = readr::col_integer(),
    LastUpdateDate = readr::col_date()
  ),
  na = ""
) %>% select(-LastUpdateDate)

# Pre-ts-modeling Data Processing ----
# Create tibble nested by County, focusing on [DailyCount]
# ... with ts() objects on target field
dat = dat %>% 
  select(County, Date, DailyCount_tests) %>%
  group_by(County) %>%
  arrange(Date) %>% 
  nest() %>%
  ungroup() 

# Create entries for missing dates
for(i in seq_along(dat$data)){
  dat$data[[i]] = dat$data[[i]] %>% complete(Date = seq.Date(min(Date), max(Date), by="day"))
}

# Ts modeling of [DailyCount_tests]
dat = dat %>%
  mutate(data_ts = map(
    .x = data,
    .f = function(df) {
      df %$%
        # Create ts() objects
        ts(
          data = DailyCount_tests,
          start = lubridate::decimal_date(min(Date)),
          frequency = 365
        )
    }
  ))

# Averaging ETS & ARIMA models (ALL COUNTIES) with Train/Test Divisions, 35-day range ----
# Use last 35-days to train & test
# ... 30-days training
# ... 5-days testing
#
# Forecast next (7)-days with ensemble model
# ... displaying both mean point forecasts and Hi/Lo 80/95% confidence intervals

# Pre-allocate output fields on "dat"
dat$forecast = list(rep(vector(mode = "list", length = 0L)))
dat$models = list(rep(vector(mode = "list", length = 0L)))
dat$opera = list(rep(vector(mode = "list", length = 0L)))

# Key Parameters
param_traindays = 30
param_testdays = 5
param_forecastdays = 7

# Set Progress Bar
for_short = 10000
pb = progress::progress_bar$new(total = min(for_short, nrow(dat)))

for (i in seq(from = 1, to = min(for_short, nrow(dat)), by = 1)) {
  # Grab and trim sequence to (test+train) period
  # MODIFIED FOR TESTING DATA
  dts = dat$data_ts[[i]] %>% # isolate time series
    subset(x = ., end = length(.) - 1) %>% # !!DEVNOTE: drop last observation b/c testing data is one-day late, therefore last observation is always NA
    subset(x = ., start = (length(.) - (param_traindays + param_testdays - 1)))
  
  dts = dts %>% 
    imputeTS::na_locf(x = ., option = "locf", na_remaining = "rev")
  
  # Create seperate testing & training sets
  dts.train = subset(dts, 
                     start = 1, 
                     end = length(dts) - param_testdays)
  
  dts.test = subset(dts,
                    start = (length(dts) - param_testdays) + 1,
                    end = length(dts))
  
  # Skip if more that 10% training data = 0
  if( ((dts.train == 0) %>% sum(., na.rm = TRUE))/param_traindays > 0.1 ){
    
    # No models fitted, assign nulls
    dat$forecast[[i]] = NA
    dat$models[[i]] = NA
    dat$opera[[i]] = NA
    
    pb$tick()
    next 
    
  }
  
  # Create testing models
  dts.models = list(
    "arima" = tryCatch(expr = {forecast::auto.arima(y = dts.train, lambda = NULL)}, 
                       error = function(e){return(NULL)}),
    
    "arima_bc" = tryCatch(expr = {forecast::auto.arima(y = dts.train, lambda = "auto", biasadj = TRUE)},
                          error = function(e){return(NULL)}),
    
    "ets" = tryCatch(expr = {forecast::ets(y = dts.train, model = "ZZZ", lambda = NULL)},
                     error = function(e){return(NULL)}),
    
    "ets_bc" = tryCatch(expr = {forecast::ets(y = dts.train, model = "ZZZ", lambda = "auto", biasadj = TRUE)},
                        error = function(e){return(NULL)})
  )
  
  # Drop Failed Models
  dts.models = dts.models[which(!sapply(dts.models, is_null))]
  dts.models = dts.models[which(!sapply(dts.models, function(mod){is.infinite(mod$bic)}))]
  
  # Continue evaluation if models were fitted
  if(length(dts.models) < 1){
    # No models fitted, assign nulls
    dat$forecast[[i]] = NA
    dat$models[[i]] = NA
    dat$opera[[i]] = NA
    
    pb$tick()
    next 
    
  }else{
    
    # Generate Forecasts using
    dts.forecasts = lapply(dts.models, function(mod) {
      forecast::forecast(object = mod, h = param_testdays)
    })
    
    # Extract Point-forecasts for ensemble test-error evaluation
    dts.forecasts_means = lapply(dts.forecasts, function(fc) {
      fc$mean %>% as.ts(
        x = .,
        start = tsp(dts.test)[1],
        end = tsp(dts.test)[2],
        frequency = tsp(dts.test)[3]
      )
    }) %>%
      do.call(what = "cbind", args = .) %>%
      cbind(dts.test, .)
    
    # p = dts.forecasts_means %>% 
    #   forecast::autoplot() + 
    #   ggtitle(label = "Spot Visualization\nForecasts vs Test Observations")
    # 
    # plotly::ggplotly(p)
    
    
    # Create Opera Ensemble
    opera_model = opera::mixture(model = "MLpol", loss.type = "percentage")
    opera_summary = predict(object = opera_model, newexperts = dts.forecasts_means[,-1],
                            newY = dts.test, type = "all")
    
    # # ... Testing ...
    # plot(opera_summary$model)
    # dev.off()
    
    # Forecasts T+h, h = param_forecastdays
    # ... T = end of test data 
    # ... Weights determined by coef(opera_summary$model)
    dts.forecasts = lapply(dts.models, function(mod) {
      forecast::forecast(object = mod, h = (param_testdays + param_forecastdays) )
    })
    
    # Scale forecasts by ensemble weight recommendations
    for(j in 1:length(dts.forecasts)){
      # Multiply each forecast result against recommended weight
      dts.forecasts[[j]] = as.ts(dts.forecasts[[j]])  * coef(opera_summary$model)[j]
    }
    
    # ... sum weigthed forecasts using matrix operations
    dts.forecasts = apply(X = simplify2array(dts.forecasts), MARGIN = c(1,2), FUN = sum)
    
    # ... convert back from matrix to ts object
    dts.forecasts =  ts(data = dts.forecasts, start = (tsp(dts.test)[1]), 
                        deltat = (1/tsp(dts.test)[3])) 
    
    # ... drop observations from testing period
    dts.forecasts = subset(x = dts.forecasts, start = param_testdays + 1)
    
    # Create Forecast object
    dts.final = cbind(dts, dts.forecasts)
    # ... name repair
    colnames(dts.final) = gsub(pattern = "\\s+", replacement = "_", x = colnames(dts.final))
    # ... coerce into tibble with normalized date
    dts.final = timetk::tk_tbl(data = dts.final, preserve_index = TRUE,
                               rename_index = "Date", silent = TRUE) %>%
      mutate(Date = format(lubridate::date_decimal(Date), "%Y-%m-%d"),
             Date = as.Date(Date)) %>% 
      pivot_longer(data = ., cols = -Date, names_to = "forecast", values_to = "value") %>%
      filter(!is.na(value))
    
    # Write Output
    dat$forecast[[i]] = dts.final
    dat$models[[i]] = dts.models
    dat$opera[[i]] = opera_summary
    
  }
  
  
  pb$tick()
  
}

dat = dat %>%
  select(-data_ts, -data)

# Save Output ----
# Save Local 
saveRDS(object = dat, file = "/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/mod_DailyCount-tests.RDS")

# Save to Dropbox
drop_token = readRDS(file = "dropbox_token.rds")
rdrop2::drop_upload(file = "/home/niko/Documents/R-projects/TexasCovid-modeling/shiny-server_files/mod_DailyCount-tests.RDS", path = "Texas-Covid/", dtoken = drop_token)


