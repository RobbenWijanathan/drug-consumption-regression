library(ggplot2)
library(readr)

url_quantified <- "https://raw.githubusercontent.com/RobbenWijanathan/drug-consumption-regression/main/drug_consumption_quantified.csv"
data_quantified <- read_csv(url_quantified)

head(data_quantified)