source('../00_functions.R')
library(xts)
library(quantstrat)
library(quantmod)
library(RPostgreSQL)



options(scipen=999)

if( Sys.info()['sysname'] == "Windows") {
  library(doParallel)
  registerDoParallel(cores=detectCores())
} else {
  library(doMC)
  registerDoMC(cores=detectCores())
}

get_symbols<-function(ticker, year_begin){
  prices<-getSymbols(ticker,auto.assign = F)
  prices<-prices[paste0(as.character(year_begin),'/')]
  prices<-merge(prices, rollmedian(Vo(prices), k = 15, align = 'right'))
  #prices[,colnames(Vo(prices))[1]]<-if_else(coalesce(prices[,colnames(Vo(prices))[1]],)>=10*coalesce(coalesce(prices[,colnames(Vo(prices))[2]],0),0), 
  #                                          as.numeric(coalesce(coalesce(prices[,colnames(Vo(prices))[2]],0),0)),
  #                                          as.numeric(coalesce(coalesce(prices[,colnames(Vo(prices))[1]],0),0)))
  prices<-prices[,colnames(prices)!=colnames(Vo(prices))[2]]
  prices
}


Diver<-function(price, width = 15, rsi_width = 14, threshold = .001) {
  price$rsi<-RSI(Cl(price), rsi_width)
  price$close<-Cl(price)
  price$rsi<-na.locf(price$rsi, fromLast = T)
  
  price$ret<-1+dailyReturn(price$close)
  price$roll_ret<-rollapply(price$ret, 
                            width = width, 
                            function(x){
                              prod(x)
                            })
  price$roll_ret<-na.locf(price$roll_ret, fromLast = T)
  
  price$diver<-rollapply(price, width = width, by.column = F, FUN = function(x) {
    diver_fin(x, threshold)
  })
  return(price$diver)
}



floor_trade_pivots<-function(SPY){
  cl_lag = lag(Cl(SPY), n = 1)
  hi_lag = lag(Hi(SPY), n = 1)
  op_lag = lag(Op(SPY), n = 1)
  lo_lag = lag(Lo(SPY), n = 1)
  center_pivot = (hi_lag+lo_lag+cl_lag)/3
  lst = list(r1 = 2*center_pivot-lo_lag,
             r2 = center_pivot + hi_lag - lo_lag,
             r3 = center_pivot + 2*(hi_lag - lo_lag),
             s1 = 2*center_pivot-hi_lag,
             s2 = center_pivot - (hi_lag - lo_lag),
             s3 = center_pivot - 2*(hi_lag - lo_lag))
  for (name in names(lst)) {
    names(lst[[name]]) = name
  }
  r1 = lst[['r1']]
  r2 = lst[['r2']]
  r3 = lst[['r3']]
  s1 = lst[['s1']]
  s2 = lst[['s2']]
  s3 = lst[['s3']]
  fin = merge(r1,r2,r3,s1,s2,s3, Cl(SPY))
  fin$pivots = case_when(Cl(fin)>r1 ~ 0.25, 
                         Cl(fin)>r2 ~ .5,
                         Cl(fin)>r3 ~ 1,
                         Cl(fin)<s1 ~ -.25,
                         Cl(fin)<s2 ~ -.5,
                         Cl(fin)<s3 ~ -.1,
                         T ~ 0)
  return(fin$pivots)
}


woodies_pivots<-function(SPY){
  cl_lag = lag(Cl(SPY), n = 1)
  hi_lag = lag(Hi(SPY), n = 1)
  op_lag = lag(Op(SPY), n = 1)
  lo_lag = lag(Lo(SPY), n = 1)
  center_pivot = (hi_lag+lo_lag+2*op_lag)/4
  lst = list(r1 = 2*center_pivot-lo_lag,
             r2 = center_pivot + hi_lag - lo_lag,
             r3 = 2*center_pivot + (hi_lag - 2*lo_lag),
             s1 = 2*center_pivot-hi_lag,
             s2 = center_pivot - (hi_lag - lo_lag),
             s3 = 2*center_pivot - (2*hi_lag - lo_lag))
  for (name in names(lst)) {
    names(lst[[name]]) = name
  }
  r1 = lst[['r1']]
  r2 = lst[['r2']]
  r3 = lst[['r3']]
  s1 = lst[['s1']]
  s2 = lst[['s2']]
  s3 = lst[['s3']]
  fin = merge(r1,r2,r3,s1,s2,s3, Cl(SPY))
  fin$pivots = case_when(Cl(fin)>r1 ~ 0.25, 
                         Cl(fin)>r2 ~ .5,
                         Cl(fin)>r3 ~ 1,
                         Cl(fin)<s1 ~ -.25,
                         Cl(fin)<s2 ~ -.5,
                         Cl(fin)<s3 ~ -.1,
                         T ~ 0)
  return(fin)
}


levels_ticker<-function(ticker_, threshold_, touches_, r_finances, max_ratio=1.5, min_ratio=.5){
  last_price<-r_finances %>% 
    filter(ticker==ticker_) %>% 
    arrange(desc(date_)) %>% 
    select(close, volume) %>% 
    slice(1)
  
  l_t<-r_finances %>% 
    filter(ticker==ticker_) %>% 
    arrange(close) %>% 
    mutate(ret = coalesce(close/lag(close),0),
           flag = if_else(abs(ret-1)<threshold_,0,1),
           group = cumsum(flag)) %>% 
    group_by(group) %>% 
    summarise(cl = mean(close), 
              cnt = n(),
              total_vol = sum(volume)) %>% 
    filter(cnt>=touches_) %>% 
    select(cl, cnt, total_vol) %>% 
    mutate(delta = cl-last_price$close,
           current_price = last_price$close,
           current_vol = last_price$volume,
           ticker = ticker_) %>% 
    filter(between(x = cl/current_price, left = min_ratio, right = max_ratio))
  return(l_t)
}





option_chain<-function(ticker, date_) {
  ts <- as.numeric(as.POSIXct(as.Date(date_)+hours(6), tz = 'Asia/Almaty'))
  url_ <- paste0('https://query1.finance.yahoo.com/v7/finance/options/',ticker,'?date=',ts)
  content <- httr::content(httr::GET(url_))
  calls <- content$optionChain$result[[1]]$options[[1]]$calls
  puts <- content$optionChain$result[[1]]$options[[1]]$puts
  fin_df<-data.frame()
  for (i in 1:length(calls)) {
    tmp_df<-as.data.frame(calls[i]) %>% 
      mutate(option = 'call')
    fin_df %<>% 
      bind_rows(tmp_df)
  } 
  for (i in 1:length(puts)) {
    tmp_df<-as.data.frame(puts[i]) %>% 
      mutate(option = 'put')
    fin_df %<>% 
      bind_rows(tmp_df)
  } 
  fin_df %<>% 
    mutate(expiration = as.Date(as.POSIXct(expiration, origin='1970-01-01')),
           lastTradeDate = as.Date(as.POSIXct(lastTradeDate, origin='1970-01-01')))
  return(fin_df)
}




option_dates<-function(ticker){
  url <- paste0('https://query1.finance.yahoo.com/v7/finance/options/',ticker)
  content <- httr::content(httr::GET(url))
  vec_ts <- unlist(content$optionChain$result[[1]]$expirationDates)
  if(length(vec_ts)==0){
    return(NULL)
  }
  dates <- as.Date(as.POSIXct(vec_ts, origin='1970-01-01'))
  return(dates)
}

BlackScholes <- function(S, K, r, T, sig, type){
  
  if(type=="C"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <- S*pnorm(d1) - K*exp(-r*T)*pnorm(d2)
    return(value)}
  
  if(type=="P"){
    d1 <- (log(S/K) + (r + sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    
    value <-  (K*exp(-r*T)*pnorm(-d2) - S*pnorm(-d1))
    return(value)}
}


get_sigma<-function(ticker){
  ticker<-ticker['2019-05-13/']
  ticker$return<- dailyReturn(Ad(ticker))
  return(sd(as.numeric(ticker$return),na.rm=TRUE) * sqrt(250))
}


get_option_value<-function(ticker_, date_, level_, type_, r=0.006, sigma = 0, sigma_custom=F){
  spot<-r_finances %>% 
    filter(ticker==ticker_) %>% 
    arrange(desc(date_)) %>% 
    slice(1) %>% 
    select(close)
  t_days = as.numeric(as.Date(date_)-as.Date(now()))
  if(sigma_custom){
    sigma<-sigma
  } else {
    sigma <- (stds %>% 
                filter(ticker==ticker_))$std    
  }
  main_val = BlackScholes(S = spot$close, K = level_, r = r, T = t_days/250, sig = sigma, type = type_)
  return(main_val)
}

get_info_ticker<-function(ticker_){
  to_scrape<-httr::content(
    httr::GET(paste0('https://finance.yahoo.com/quote/',ticker_)), as = 'text')
  part1 = stri_split_fixed(str = to_scrape, pattern = 'root.App.main = ', simplify = T)
  part2 = stri_split_fixed(str = part1[1, 2], pattern = '(this)', simplify = T)
  part3 = stri_split_fixed(str = part2[1, 1], pattern = ';\n}', simplify = T)
  part4 = trimws(part3[1,1])
  part5 = jsonlite::fromJSON(part4)
  part6 = part5[['context']][['dispatcher']][['stores']][['QuoteSummaryStore']]
  
  summary<-part6[['summaryProfile']]
  keystat<-part6[['defaultKeyStatistics']]
  
  rm(to_scrape, part1, part2, part3, part4, part5, part6)
  
  df_summary<-as.data.frame(lapply(summary,function(x) { if(length(x)>0) {x}  else 'none'}), stringsAsFactors = F) %>% 
    mutate(ticker = ticker_)
  keystat_columns<-c('profitMargins', 'forwardEps', '52WeekChange', 'sharesShort', 'heldPercentInstitutions',
                     'heldPercentInsiders', 'priceToBook', 'shortRatio', 'beta', 'enterpriseValue',
                     'pegRatio', 'forwardPE')
  df_keystat<-as.data.frame(lapply(keystat[keystat_columns], function(x) 
  { if ('raw'%in% names(x)){x['raw']} else {'none'} }), stringsAsFactors = F)
  colnames(df_keystat)<-keystat_columns
  return(cbind(df_keystat, df_summary))
}


yfinance<-function(ticker, range, interval = '1d') {
  js_aapl<-httr::GET(paste0('https://query1.finance.yahoo.com/v8/finance/chart/',
                            ticker,
                            '?range=',
                            range,
                            '&interval=',
                            interval))
  js_aapl<-httr::content(js_aapl)
  appl<-as_tibble(list(ts = unlist(js_aapl$chart$result[[1]]$timestamp),
                       open = unlist(as.numeric(as.character(js_aapl$chart$result[[1]]$indicators$quote[[1]]$open))),
                       high = unlist(as.numeric(as.character(js_aapl$chart$result[[1]]$indicators$quote[[1]]$high))),
                       low = unlist(as.numeric(as.character(js_aapl$chart$result[[1]]$indicators$quote[[1]]$low))),
                       close = unlist(as.numeric(as.character(js_aapl$chart$result[[1]]$indicators$quote[[1]]$close))),
                       volume = unlist(as.numeric(as.character(js_aapl$chart$result[[1]]$indicators$quote[[1]]$volume))),
                       ticker = ticker)) %>% 
    as.data.frame() %>% 
    mutate(ts2 = as.POSIXct(ts, origin = '1970-01-01'))
  return(appl)  
}


