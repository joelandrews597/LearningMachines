export_data = read.csv(file.choose())
import_data = read.csv(file.choose())

library(tidyverse)
library(dplyr)
library(DT)
library(tidyr)
library(reshape2)
library(scales)
library(plotly)

#### UNDERSTANDING THE DATA ####

summary(export_data[,c(1,3,5)])
#Large no.of NA's in value col
#Large no.of outliers with 75% of data below 3.77 - Value col

summary(import_data[,c(1,3,5)])
#Large no.of NA's in value col
#Large no.of outliers with 75% of data below 4.91 - Value col


#### DATA CLEANING ####

levels(export_data$country) #Contains "UNSPECIFIED" as country
export_data %>% filter(country == "UNSPECIFIED") %>% count() #726 rows

levels(import_data$country)
import_data %>% filter(country == "UNSPECIFIED") %>% count() #979 rows

#Setting country UNSPECIFIED to NaN
export_data$country = as.character(export_data$country)
export_data$country = ifelse(export_data$country == "UNSPECIFIED","NaN",export_data$country)
export_data$country = as.factor(export_data$country)

import_data$country = as.character(import_data$country)
import_data$country = ifelse(import_data$country == "UNSPECIFIED","NaN",import_data$country)
import_data$country = as.factor(import_data$country)

#Identifying and removing duplicates
export_data %>%  group_by_all() %>% add_tally() %>% ungroup() %>% filter(n>1) #No duplicates
import_data %>% group_by_all() %>% add_tally() %>% ungroup() %>% filter(n>1) #75093 duplicates

#Removing duplicates
import_data = import_data %>% distinct()

#Removings NA's and rows containing 0 in Value 
View(export_data[!complete.cases(export_data),]) 
export_data = export_data %>% drop_na(value)

export_data = export_data %>% filter(!value == 0)
import_data = import_data %>% filter(!value == 0)

##Analysing Commodities
export_data %>% distinct(HSCode) %>% count() #98 commodities
import_data %>% distinct(HSCode) %>% count() #98 commodities

#There exists 98 commodities when there exists 99 chapters i.e a HSCode is missing
#HSCode 77 missing
#HSCode 77 is actually reserved for Possible Future Use.


#### DATA vISUALIZATION ####

#Most popular export commodity 
export_data %>% 
  group_by(HSCode) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>% 
  head(5) %>% 
  ggplot(aes(reorder(HSCode, Total),Total)) + geom_col(fill = "#F5B041") +
  ggtitle("Top 5 most exported commodity by HSCode") + labs(x = "HSCode", y = "Total")

#Most popular import commodity 
import_data %>% 
  group_by(HSCode) %>% 
  summarise(Total = n()) %>% 
  arrange(desc(Total)) %>% 
  head(5) %>% 
  ggplot(aes(reorder(HSCode,Total),Total)) + geom_col(fill = "#F5A065") +
  ggtitle("Top 5 most imported commodity by HSCode") + labs(x = "HSCode", y = "Total")

#Top 10 trading partners by perc of export/import
export_data %>% 
  group_by(country) %>% 
  summarise(Percent = (sum(value)/sum(export_data$value))*100) %>%
  arrange(desc(Percent)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(country, Percent),Percent)) + geom_col(fill = "Lightgreen") + coord_flip()
  ggtitle("Top 10 trading partner by perc. of export") + labs(y = "Country")

import_data %>% 
  group_by(country) %>% 
  summarise(Percent = (sum(value)/sum(import_data$value))*100) %>%
  arrange(desc(Percent)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(country, Percent),Percent)) + geom_col(fill = "Skyblue") + coord_flip()
  ggtitle("Top 10 trading partner by perc. of import") + labs(y = "Country")

#Top 10 trading partners by value  
export_data %>% group_by(country) %>% summarise(TotalImp = sum(value)) %>%
    arrange(desc(TotalImp)) %>% 
    head(10) %>% 
    ggplot(aes(reorder(country, TotalImp),TotalImp)) + geom_col(fill = "Skyblue") + coord_flip() +
    ggtitle("Top 10 trading partner by perc. of import") + labs(y = "Country") 
    
import_data %>% group_by(country) %>% summarise(TotalImp = sum(value)) %>%
    arrange(desc(TotalImp)) %>% 
    head(10) %>% 
    ggplot(aes(reorder(country, TotalImp),TotalImp)) + geom_col(fill = "Skyblue") + coord_flip() +
    ggtitle("Top 10 trading partner by perc. of import") + labs(y = "Country") 
    
#Year-wise performance of Import and Export transactions
year_export = export_data %>% 
                group_by(year) %>% 
                summarise(Total = sum(value))

year_import = import_data %>% 
                group_by(year) %>% 
                summarise(Total = sum(value)) 

year_data = merge(year_export,year_import, by.x = "year", by.y = "year")
year_data = setNames(year_data, c("Year","Export","Import"))

year_data_melt = melt(year_data, id.vars ="Year")
year_data_melt %>% ggplot(aes(Year, value, fill = factor(variable))) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_discrete(name = "Trade Info", labels = c("Export","Import")) + 
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03, prefix = "$"))

#Year-wise trade deficit
year_deficit_data = year_data %>% mutate(Deficit = Export - Import)

year_deficit_data_melt = melt(year_deficit_data, id.vars = "Year")

year_deficit_data_melt %>% 
  ggplot(aes(Year, value, fill = factor(variable))) +
  geom_bar(position = "dodge", stat = "identity") +
  ggtitle("Yearly Trade Deficit") +
  scale_fill_discrete(name = "Trade Info") +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03, prefix = "$")) + 
  labs(y = "Value in Millions")


