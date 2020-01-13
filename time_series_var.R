# install these packages
install.packages("WDI")
install.packages("reshape2")
install.packages("wbstats")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("scales")
install.packages("useful")
install.packages("numDiffs")
install.packages("forecast")
install.packages("vars")
# load the required library

library("WDI")
library(ggplot2)
library(scales)
library(useful)
library(reshape2)
library(forecast)
library(vars)
library(VAR)
# fetch the data from API world bank
# coutry code means:- us <- unite dstate
# ca <- canada
# aus <- australia
#ind <- india
#----------------------------------------------------

# for the indicator and more details -"?WDI
gdp <- WDI(country = c("US","CA","AUS","CHN","IND","PAK","SAU"),
          indicator = c("NY.GDP.PCAP.CD","NY.GDP.MKTP.CD"),
          start = 1960, end = 2011)
# view gdp of listed country
View(gdp)

#give them a good name OF All variable
names(gdp) <- c("iso2c","Country","Year","PerCapGDP","GDP")
# view head of gdp of loaded data
head(gdp)

# plot the year wise hgdp of bove mentioned country 
ggplot(gdp,aes(Year,PerCapGDP,color=Country,linetype=Country))+
  geom_line()+scale_y_continuous(label=dollar)

# plot them with lables and multiple format percapital growth
ggplot(gdp,aes(Year,PerCapGDP,color=Country,linetype=Country))+
  geom_line()+scale_y_continuous(label=multiple_format(extra=dollar,multiple="M"))


# plot them with lables and multiple format gdp of each country
ggplot(gdp,aes(Year,GDP ,color=Country,linetype=Country))+
  geom_line()+scale_y_continuous(label=multiple_format(extra=dollar,multiple="M"))
# plot a individual country
us <- gdp$PerCapGDP[gdp$Country == "United States"]
#change this country to time serires
us <- ts(us, start = min(gdp$Year, end=max(gdp$Year)))
us
# plot the individual contry to visualize
plot(us, ylab="Per Capita GDP of US",xlab="Year")
?plot
# convert to time series
gdpcast <- dcast(Year ~ Country,
                 data = gdp[, c("Country","Year","PerCapGDP")],
                 value.var = "PerCapGDP")
head(gdpcast)


#convert first 10 rows since saudi arbia has not
gdpTs <- ts(data = gdpcast[, -1], start = min(gdpcast$Year),
            end = max(gdpcast$Year))

# build a plot and legend using base graphics
plot(gdpTs, plot.type="single", col=1:8)
# add legend
legend("topleft", legend=colnames(gdpTs), ncol=2, lty=2,col=1:8, cex=.9)


ndiffs(x=us)
plot(diff(us,2))
numDiffs <- ndiffs(gdpTs)
numDiffs

gdpDiffed <- diff(gdpTs, differences = numDiffs)
plot(gdpDiffed, plot="single",col=1:7)
legend("bottomleft", legend = colnames(gdpDiffed), ncol = 2, lty=1, col = 1:7, cex = .9)

##  fit the model
x <- c(1 , 4 , 8 , 2 , 6 , 6 , 5, 3) > 
  # one diff
  
diff(x, differences=1)


# two iterative diffs 
diff(x, differences=2)
# equivalent to one diff 
diff(x, lag=1)
# diff elements that are two indices apart 
diff(x, lag=2)
?var

gdpVar <- var(gdpDiffed, lag.max = 12,type= "const", ic = "AIC", exogen = exog)
gdpVar$p

library(vars)

# fit the model
  
gdpVar <- VAR(gdpDiffed, lag.max=12,type = "none") 
# chosen order
gdpVar$p

AIC(n) 6
