#install.packages("lubridate")
#install.packages("zoo")                              # Install & load zoo
library("zoo")
library("dplyr")
library("ggplot2")
library("plotly")
library("tidyverse")
library("stringr")
library("lubridate")

data_frame <- read.csv( "owid-covid-data.csv", header = TRUE, sep = ",")
data_frame[is.na(data_frame)] <- 0 # replace NA with 0
# list of countries we are looking for
countries <- c("Australia", "China", "India","New Zealand","Sweden","Ukraine","United Kingdom","United States")

# total number of cases 
plot_ly(
  data = data_frame[data_frame$location %in% countries,],
  x = ~location,
  y = ~new_cases,
  color = ~location,
  type = "bar"
)


# cases line graph 
plot_ly(
  data = data_frame[data_frame$location %in% countries,],
  x = ~date, 
  y = ~total_cases,
  color = ~location,
  type = "scatter",
  mode = "lines+markers",
  line = list(width = 4)) %>% layout(title= list(text = "Total numbers of COVID-19 cases",font = "Helvetica"), font=t, 
                                    legend=list(title=list(text='Countries',font = "Helvetica")), 
                                    xaxis = list(title = list(text ='Date', font = "Helvetica")),
                                    yaxis = list(title = list(text ='Number of Cases', font = "Helvetica"), type = "log"),
                                    plot_bgcolor='#e5ecf6')



# QUESTION 3
countryGdp <- data_frame[data_frame$gdp_per_capita > 0,] %>% select(gdp_per_capita,total_cases_per_million,total_deaths_per_million, date, cardiovasc_death_rate) 
countryGdp$date <- strftime(countryGdp$date, "%Y-%m") # get month only

countryGdpQuantile <- unname(quantile(countryGdp$gdp_per_capita)) # get quantile
cat("Poor Country: ",countryGdpQuantile[1],"\nAverage Country: ",countryGdpQuantile[2], "-",countryGdpQuantile[3],"\nRich Country: ",countryGdpQuantile[5],"+")

countryGdp$wealth <-  cut(countryGdp$gdp_per_capita, breaks=c(countryGdpQuantile[1],countryGdpQuantile[2],countryGdpQuantile[4],countryGdpQuantile[5]),labels=c("low", "average", "high"))
countryGdp <- drop_na(countryGdp)

# get cases by country wealth
lowCases <- countryGdp[countryGdp$wealth == 'low',] %>% group_by(date) %>% summarise(lowCases = max(total_cases_per_million)) %>% ungroup()
averageCases <- countryGdp[countryGdp$wealth == 'average',] %>% group_by(date) %>% summarise(averageCases = max(total_cases_per_million)) %>% ungroup()
highCases <- countryGdp[countryGdp$wealth == 'high',] %>% group_by(date) %>% summarise(highCases = max(total_cases_per_million)) %>% ungroup()

# join cases into one dataframe 
countryWealth <- full_join(lowCases,averageCases)
countryWealth <- full_join(countryWealth, highCases)

# plot case wealth dataframe
plot_ly(countryWealth, x = ~date, y = ~countryWealth$lowCases, type = 'scatter', mode = "lines+markers", name = 'low') %>% 
  add_trace(y = ~averageCases, name = 'average', mode = 'lines+markers')%>% 
  add_trace(y = ~countryWealth$highCases, name = 'high', mode = 'lines+markers')%>% layout(title= list(text = "covid cases per-country by wealth",font = "Helvetica"), font=t, 
                                                                                           legend=list(title=list(text='country wealth',font = "Helvetica")), 
                                                                                           xaxis = list(title = list(text ='Date', font = "Helvetica")),
                                                                                           yaxis = list(title = list(text ='total cases per million', font = "Helvetica")),
                                                                                           plot_bgcolor='#e5ecf6') 

# get death by country wealth
lowDeath <- countryGdp[countryGdp$wealth == 'low',] %>% group_by(date) %>% summarise(lowDeath = max(total_deaths_per_million),cardiovasc = max(cardiovasc_death_rate)) %>% mutate(deaths_per_hundred_K = lowDeath / 10) %>% ungroup()
averageDeath <- countryGdp[countryGdp$wealth == 'average',] %>% group_by(date) %>% summarise(averageDeath = max(total_deaths_per_million),cardiovasc = max(cardiovasc_death_rate)) %>% mutate(deaths_per_hundred_K = averageDeath / 10) %>% ungroup()
highDeath <- countryGdp[countryGdp$wealth == 'high',] %>% group_by(date) %>% summarise(highDeath = max(total_deaths_per_million),cardiovasc = max(cardiovasc_death_rate))%>% mutate(deaths_per_hundred_K = highDeath / 10) %>% ungroup()

# join cases into one dataframe 
countryWealthDeath <- full_join(lowDeath,averageDeath)
countryWealthDeath <- full_join(countryWealthDeath, highDeath)

# plot case wealth dataframe
plot_ly(countryWealthDeath, x = ~date, y = ~lowDeath, type = 'scatter', mode = "lines+markers", name = 'low') %>% 
  add_trace(y = ~averageDeath, name = 'average', mode = 'lines+markers') %>% 
  add_trace(y = ~highDeath, name = 'high', mode = 'lines+markers')%>% layout(title= list(text = "covid deaths per-country by wealth",font = "Helvetica"), font=t, 
                                                                             legend=list(title=list(text='country wealth',font = "Helvetica")), 
                                                                             xaxis = list(title = list(text ='Date', font = "Helvetica")),
                                                                             yaxis = list(title = list(text ='total cases per million', font = "Helvetica")),
                                                                             plot_bgcolor='#e5ecf6') 



# GDP countries: Cardiovasc v Covid 19 Death Rate

plot_ly(lowDeath, x = ~date, y = ~lowDeath, type = 'scatter', mode = "lines+markers", name = 'Covid 19') %>% 
  add_trace(y = ~cardiovasc, name = 'Cardiovasc', mode = 'lines+markers')%>% layout(title= list(text = "Low GDP countries: Cardiovasc v Covid 19 Death Rate",font = "Helvetica"), font=t, 
                                                                                 legend=list(title=list(text='Death rate',font = "Helvetica")), 
                                                                                 xaxis = list(title = list(text ='Date', font = "Helvetica")),
                                                                                 yaxis = list(title = list(text ='Deaths per 100k', font = "Helvetica")),
                                                                                 plot_bgcolor='#e5ecf6')

plot_ly(averageDeath, x = ~date, y = ~averageDeath, type = 'scatter', mode = "lines+markers", name = 'Covid 19') %>% 
  add_trace(y = ~cardiovasc, name = 'Cardiovasc', mode = 'lines+markers')%>% layout(title= list(text = "Average GDP countries: Cardiovasc v Covid 19 Death Rate",font = "Helvetica"), font=t, 
                                                                                          legend=list(title=list(text='Death rate',font = "Helvetica")), 
                                                                                          xaxis = list(title = list(text ='Date', font = "Helvetica")),
                                                                                          yaxis = list(title = list(text ='Deaths per 100k', font = "Helvetica")),
                                                                                          plot_bgcolor='#e5ecf6')

plot_ly(highDeath, x = ~date, y = ~highDeath, type = 'scatter', mode = "lines+markers", name = 'Covid 19') %>% 
  add_trace(y = ~cardiovasc, name = 'Cardiovasc', mode = 'lines+markers')%>% layout(title= list(text = "High GDP countries: Cardiovasc v Covid 19 Death Rate",font = "Helvetica"), font=t, 
                                                                                          legend=list(title=list(text='Death rate',font = "Helvetica")), 
                                                                                          xaxis = list(title = list(text ='Date', font = "Helvetica")),
                                                                                          yaxis = list(title = list(text ='Deaths per 100k', font = "Helvetica")),
                                                                                          plot_bgcolor='#e5ecf6')

#############################################################################################################################


# QUESTION 5

# compare gdp to daily covid scores

excessMortality <- data_frame[, c("date", "excess_mortality")] # get date and mortality 
excessMortality$date <- strftime(excessMortality$date, "%Y-%m") # remove day

monthlyExcessMortality <- excessMortality %>% group_by(date) %>% summarise(excess_mortality = sum(excess_mortality))

#wow <- data_frame %>% group_by(date) %>% summarise(total_sales = sum(excess_mortality)) %>% arrange(desc(total_sales))

vline <- function(x = 0, color = "lightgreen") {
  list(
    type = "line", 
    y0 = 0, 
    y1 = 1, 
    yref = "paper",
    x0 = x, 
    x1 = x, 
    line = list(color = color)
  )
}

plot_ly(
  data = monthlyExcessMortality,
  x = ~date, 
  y = ~excess_mortality,
  color = ~excess_mortality,
  type = "scatter",
  mode = "lines+markers")%>% layout(shapes = list(vline("2020-06")),title = list(text = "Excess Mortality Rates",font = "Helvetica"), 
                                    legend=list(title=list(text='Excess Mortality',font = "Helvetica")), 
                                    xaxis = list(title = list(text ='Date', font = "Helvetica")),
                                    yaxis = list(title = list(text ='total cases per million', font = "Helvetica")),
                                    plot_bgcolor='#e5ecf6')


