#### Import libs ####
source("functions/functions.R")
Rcpp::sourceCpp("functions/divergence_fin.cpp")

#### Get some stock price #### 
ticker <- "AAPL"
year_begin<-2021
ticker_price <- get_symbols(ticker, year_begin = year_begin)
ticker_price <- na.locf(ticker_price)
price<-ticker_price[, 4]
colnames(price)<-c('close')

#### Get divergence points ####
price$diver<-Diver(price)
(ups<-price[which(price$diver>.25)])
(downs<-price[which(price$diver<(-.45))])

#### Get current trend ####
price_trend<-get_symbols(ticker, year_begin)
price_trend$close<-Cl(price_trend)
bottoms <- findValleysCPP(price_trend$close, 0.03)[['valleys']]
peaks <- findPeaksCPP(price_trend$close, 0.03)[['peaks']]

#### Visualizations ####
library(ggplot2)
library(gtable)
library(gridExtra)
library(grid)

p1<-ggplot(data = price, mapping = aes(x=Index, y = close))+
  geom_line()+
  theme_minimal()+
  theme(legend.position = 'none')+
  geom_point(data = ups, aes(x = index(ups), y = ups$close),colour='green', size = 1)+
  geom_point(data = downs, aes(x = index(downs), y = downs$close), colour = 'red', size = 1)


p2<-ggplot()+
  geom_line(data = price_trend, mapping = aes(x=Index, y = close))+
  geom_point(data = bottoms, mapping = aes(x=Index, y = close), color = 'red')+
  stat_smooth(data = bottoms,#[bottoms$close<282], 
              mapping = aes(x=Index, y = close), 
              color = 'red', 
              method = MASS::rlm, 
              fullrange=TRUE,
              se = F)+
  geom_point(data = ups, mapping = aes(x=Index, y = close), color = 'green')+
  stat_smooth(data = ups,#[ups$close>286.5], 
              mapping = aes(x=Index, y = close), 
              color = 'green', 
              method = MASS::rlm, 
              fullrange=TRUE,
              se = F)+
  theme_minimal()

g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)
g <- rbind(g1, g2)
grid.newpage()
grid.draw(g)

