jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")
earnings_female <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/earnings_female.csv") 
employed_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/employed_gender.csv") 

library(tidyverse)
library(scales)
library(plotly)

#Creating a summarise fn
#Taking weighted avereage of the median earnings 
summarise_jobs_gender = function(tbl){
  tbl %>% summarise(total_earnings = sum(total_earnings * total_workers)/ sum(total_workers),
                    total_earnings_male = sum(total_earnings_male * workers_male, na.rm = TRUE)/ sum(workers_male[!is.na(total_earnings_male)]),
                    total_earnings_female = sum(total_earnings_female * workers_female, na.rm = TRUE)/ sum(workers_female[!is.na(total_earnings_female)]),
                    total_workers = sum(total_workers),
                    workers_male = sum(workers_male),
                    workers_female = sum(workers_female)) %>%
    mutate(wage_percent_of_male = total_earnings_female/total_earnings_male)
  
}

year_major_category_group = jobs_gender %>% 
  group_by(year,major_category) %>% 
  summarise_jobs_gender

#Plot of major category group and total earnings
year_major_category_group %>% 
  mutate(major_category = fct_reorder(major_category,-total_earnings)) %>%
  ggplot(aes(year,total_earnings, color = major_category)) + 
  geom_line( size = 1)

##Health care practitioners/CS Engineers have higher pay and with a steady increase over the years.
##Service industry pays the least with a slight increase in pay over the years.

#Plot of major category group and wage percentage of male
year_major_category_group %>% 
  mutate(major_category = fct_reorder(major_category,-wage_percent_of_male)) %>%
  ggplot(aes(year,wage_percent_of_male, color = major_category)) + 
  geom_line( size = 1) 

##There's a large disparity in pay among men and women in the Healtcare field 
##The least disparity in pay is seen in the Computer, Engineering and Science field.

#Looking at minor categories, specifically in the year 2016
minor_category_group_2016 = jobs_gender %>%
  filter(year == 2016) %>%
  group_by(major_category, minor_category) %>%
  summarise_jobs_gender() %>%
  ungroup()

#Plot of minor category vs female wage percent of male
minor_category_group_2016 %>% 
  mutate(minor_category = fct_reorder(minor_category, wage_percent_of_male)) %>%
  ggplot(aes(minor_category,wage_percent_of_male, fill = major_category)) + geom_col() + coord_flip() 

##Healthcare Practitioners and Legal minor categories have a larger pay disparity
##Community and social service minor category with the least pay disparity


#Understanding the major category with largest pay disparity - Healthcare Practitioners and Technical
p1 = jobs_gender %>% 
  filter(year == 2016) %>% 
  filter(major_category == "Healthcare Practitioners and Technical") %>% 
  arrange(desc(wage_percent_of_male)) %>% 
  ggplot(aes(workers_female/total_workers,
             total_earnings_female/total_earnings_male, 
             size = total_workers,
             label = occupation)) + 
  geom_point(alpha = 0.6) + 
  scale_size_continuous(range = c(1,10)) +
  labs(x = "% of workforce reported as female",
       y = "Median salary in the occupation") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = dollar_format())
ggplotly(p1) #Interactive Plot


#Understanding the major category with least pay disparity - Computer, Engineering, and Science
#total_workers >= 15000 to remove outliers
p2 = jobs_gender %>% 
  filter(year == 2016, total_workers >= 15000) %>% 
  filter(major_category == "Computer, Engineering, and Science") %>% 
  arrange(desc(wage_percent_of_male)) %>% 
  ggplot(aes(workers_female / total_workers,
             total_earnings_female / total_earnings_male,
             color = minor_category,
             size = total_workers,
             label = occupation)) +
  geom_point(alpha = 0.6) +
  scale_size_continuous(range = c(1, 10)) +
  labs(size = "Total # of workers",
       x = "% of workforce reported as female",
       y = "% of median female salary / median male") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = percent_format()) 
ggplotly(p2) #Interactive Plot
  
  
  
  
  
  

