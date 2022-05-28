#install.packages("lubridate")
library("dplyr")
library("ggplot2")
library("plotly")
library("tidyverse")


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
) %>%layout(title= list(text = "Total numbers of COVID-19 cases",font = "Helvetica"), font=t, 
            legend=list(title=list(text='Countries',font = "Helvetica")), 
            xaxis = list(title = list(text ='Countries', font = "Helvetica")),
            yaxis = list(title = list(text ='Number of Cases', font = "Helvetica")),
            plot_bgcolor='#e5ecf6')


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




# compare gdp to daily covid scores

excessMortality <- data_frame[, c("date", "excess_mortality")] # get date and mortality 
excessMortality$date <- strftime(excessMortality$date, "%Y-%m") # remove day

monthlyExcessMortality <- excessMortality %>% group_by(date) %>% summarise(excess_mortality = sum(excess_mortality))

#wow <- data_frame %>% group_by(date) %>% summarise(total_sales = sum(excess_mortality)) %>% arrange(desc(total_sales))

plot_ly(
  data = monthlyExcessMortality,
  x = ~date, 
  y = ~excess_mortality,
  color = ~excess_mortality,
  type = "scatter",
  mode = "lines+markers")%>% layout(shapes = list(list(type = "rect",line = list(color = "black"),
                                      x0 = "2020-10", x1 = "2021-10", y0 = 0, y1= 9000), plot_bgcolor = "#e5ecf6") # get max value 

 






































#split split countries 
rich <- 0
average <- 0
poor <- 0