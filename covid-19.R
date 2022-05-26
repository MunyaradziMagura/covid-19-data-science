library("dplyr")
library("ggplot2")
library("plotly")
data_frame <- read.csv( "owid-covid-data.csv", header = TRUE, sep = ",")

data_frame[is.na(data_frame)] <- 0 # replace NA with 0

# list of countries we are looking for
countries <- c("Australia", "China", "India","New Zealand","Sweden","Ukraine","United Kingdom","United States")

plot_ly(
  data = data_frame[data_frame$location %in% countries,],
  x = ~date, 
  y = ~total_cases,
  color = ~location,
  type = "scatter",
  mode = "lines+markers",
  line = list(width = 4))%>% layout(title= list(text = "Total numbers of COVID-19 cases",font = "Helvetica"), font=t, 
                                    legend=list(title=list(text='Countries',font = "Helvetica")), 
                                    xaxis = list(title = list(text ='Date', font = "Helvetica")),
                                    yaxis = list(title = list(text ='Number of Cases', font = "Helvetica")),
                                    plot_bgcolor='#e5ecf6')


