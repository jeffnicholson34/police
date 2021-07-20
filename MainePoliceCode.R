library(tidyverse)
library(ggplot2)
library(readxl)
library(ggthemes)
library(ggrepel)
library(directlabels)
options(scipen = 100)
police_budgeting <- read_excel("~/projects/police/police-budgeting-2.xlsx")

####### load in Excel file

data <- subset(police_budgeting, select = -c(...3))
####### erase empty column

data$`Approved General Fund Budget` <- as.numeric(data$`Approved General Fund Budget`)
data$`Police Budget` <- as.numeric(data$`Police Budget`)
data$`% on Police` <- as.numeric(data$`% on Police`)
data$`Population (est.)` <- as.numeric(data$`Population (est.)`)
data$`% Change 2017-2021` <- as.numeric(data$`% Change 2017-2021`)
data$`% Change 2012-2021` <- as.numeric(data$`% Change 2012-2021`)
data$`Median Income` <- as.numeric(data$`Median Income`)
data$`Police $ per income` <-as.numeric(data$`Police $ per income`)
data$`Police$ per Citizen` <- as.numeric(data$`Police$ per Citizen`)
####### convert character columns into numeric

Lewiston_data <- data %>% filter(City == 'Lewiston')
###### create Lewiston dataset

years <- c (2010:2022)
###### create vector of years
 
Lewiston_data %>%
  ggplot(aes(`Fiscal Year`, `% on Police`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years) +
  scale_y_continuous("% of Budget spent on Police", limits = c(0.05, 0.20)) +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_point() +
  ggtitle("Police Spending in Lewiston") +
  theme_solarized_2() +
  scale_colour_manual(values = c("#dc322f"))
###### work in progress - Lewiston Police Budget 

Portland_Lewiston_Data <- data %>% filter(City == 'Lewiston' | City == 'Portland')
###### create Portland/Lewiston dataset

Portland_Lewiston_Data %>%
  ggplot(aes(`Fiscal Year`, `% on Police`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years) +
  scale_y_continuous("% of Budget spent on Police", limits = c(0.05, 0.20)) +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_point() +
  ggtitle("Police Spending in Lewiston/Portland") +
  theme_solarized_2() +
  scale_colour_manual(values = c("#dc322f", "#268bd2"))
##### work in progress - Lewiston/Portland Police Budget

Portland_Lewiston_Data %>%
  filter('Fiscal Year' >= 2017) %>%
  ggplot(aes(`Fiscal Year`, `% on Police`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years, limits = c(2017,2022)) +
  scale_y_continuous("% of Budget spent on Police", limits = c(0.05, 0.20)) +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_point() +
  ggtitle("Police Spending in Lewiston/Portland, last 5 years") +
  theme_solarized_2() +
  scale_colour_manual(values = c("#dc322f", "#268bd2"))
###### work in progress -- L/P budget last 5 years

data_new <- data
data_new$City <- factor(data_new$City,
                        levels = c("Brunswick", "Lewiston", "Auburn", "Portland"))
###### create a vector to properly order towns

data_new %>%
  filter(City == 'Lewiston'| City == 'Portland'| City == 'Auburn') %>%
  ggplot(aes(`Fiscal Year`, `% on Police`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years) +
  scale_y_continuous("% of Budget spent on Police", limits = c(0.05, 0.20)) +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_point() +
  ggtitle("Police Spending in Lewiston/Portland/Auburn") +
  theme_solarized_2() +
  scale_colour_manual(values = c("#dc322f", "#859900", "#268bd2"))
###### work in progress - Lewiston/Auburn/Portland Police Budget

data_new %>%
  filter(City == 'Lewiston'| City == 'Portland'| City == 'Auburn') %>%
  filter(`Fiscal Year` >= 2017) %>%
  ggplot(aes(`Fiscal Year`, `% on Police`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years, limits = c(2017, 2022)) +
  scale_y_continuous("% of Budget spent on Police", limits = c(0.05, 0.20)) +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_point() +
  ggtitle("Police Spending in Lewiston/Portland/Auburn, last 5 years") +
  theme_solarized_2() +
  scale_colour_manual(values = c("#dc322f", "#859900", "#268bd2"))
###### work in progress - L/A/P/ Budget last 5 years

data_new %>%
  filter(City == 'Brunswick'|City == 'Lewiston'| City == 'Portland'| City == 'Auburn') %>%
  ggplot(aes(`Fiscal Year`, `% on Police`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years) +
  scale_y_continuous("% of Budget spent on Police", limits = c(0.05, 0.20)) +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_point(alpha = 0.5) +
  ggtitle("Police Spending in Lewiston/Portland/Auburn/Brunswick") +
  theme_solarized_2() +
  scale_colour_manual(values = c("#6c71c4", "#dc322f", "#859900", "#268bd2"))
###### work in progress Lewiston/Portland/Auburn/Brunswick

data_new %>%
  filter(City == 'Brunswick'|City == 'Lewiston'| City == 'Portland'| City == 'Auburn') %>%
  filter(`Fiscal Year` >= 2017) %>%
  ggplot(aes(`Fiscal Year`, `% on Police`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years, limits = c(2017, 2022)) +
  scale_y_continuous("% of Budget spent on Police", limits = c(0.05, 0.20)) +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_point(alpha = 0.5) +
  ggtitle("Police Spending in Lewiston/Portland/Auburn/Brunswick, last 5 years") +
  theme_solarized_2() +
  scale_colour_manual(values = c("#6c71c4", "#dc322f", "#859900", "#268bd2"))
###### work in progress Lewiston/Portland/Auburn/Brunswick last 5 years

colors <- c ("#586e75", "#586e75", "#dc322f", "#586e75", 
             "#586e75", "#dc322f", "#586e75","#586e75","#dc322f")
###### color coded for increasing cities in the last 10 years (workaround)

allcities <- subset(data, select = c('City', 'Fiscal Year', '% on Police'))
###### dataset with just City, Fiscal Year, and % on Police

allcities %>%
  filter(City != 'Saco') %>%
  ggplot(aes(`Fiscal Year`, `% on Police`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years) +
  scale_y_continuous("% of Budget spent on Police", limits = c(0.05, 0.20)) +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_label(aes(x = 2016.5, y = 0.185, label = "Bangor")) +
  geom_label(aes(x = 2019, y = 0.1475, label = "Lewiston")) +
  geom_label(aes(x = 2014, y = 0.115, label = "Westbrook")) +
  ggtitle("Upward Police Spending Trajectories in Maine") +
  theme_solarized_2() +
  theme(legend.position = "none") +
  scale_colour_manual(values = colors)
###### work in progress Police Spending Trajectories red/black 

colors2 <- c("#586e75","#dc322f", "#dc322f", "#586e75", "#586e75", "#dc322f",
             "#586e75", "#dc322f", "#dc322f" )
allcities %>%
  filter(City != 'Saco') %>%
  filter('Fiscal Year' >= 2017) %>%
  ggplot(aes(`Fiscal Year`, `% on Police`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years, limits = c(2017, 2022)) +
  scale_y_continuous("% of Budget spent on Police", limits = c(0.05, 0.20)) +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_label(aes(x = 2017.40, y = .181, label = "Bangor")) +
  geom_label(aes(x = 2018.4, y = .159, label = "Augusta")) +
  geom_label(aes(x = 2019.2, y = .144, label = "Lewiston")) +
  geom_label(aes(x = 2020, y = .13, label = "South Portland")) +
  geom_label(aes(x = 2021, y = 0.107, label = "Westbrook")) +
  ggtitle("Upward Police Spending Trajectories in Maine") +
  theme_solarized_2() +
  theme(legend.position = "none") +
  scale_colour_manual(values = colors2)
###### work in progress Police Spending Trajectories, last 5 years

compare_percentage <- na.omit(subset(data, select = c('City', '% Change 2012-2021'))) %>%
  filter(City != 'Saco')
###### create data set for comparing percentages for spending trajectories

compare_percentage %>%
  mutate(city = fct_reorder(City, compare_percentage$`% Change 2012-2021`)) %>%
  ggplot(aes(x = city, y = compare_percentage$`% Change 2012-2021`, label = City)) +
  geom_bar(stat = "identity", alpha = 0.6, width = 0.3, fill = "#dc322f") +
  scale_y_continuous("% Change", limits = c(-0.05, 0.05)) +
  ggtitle("Difference in Police Spending by Percentage 2012-2021") +
  scale_x_discrete("City") +
  theme_solarized_2() 
###### work in progress Difference in Police Spending by Percentage

compare_percentage2 <- na.omit(subset(data, select = c('City', '% Change 2017-2021'))) %>%
  filter(City != 'Saco')
##### create dataset for comparing percentages from previous graph 2017-2021

compare_percentage2 %>%
  mutate(city = fct_reorder(City, compare_percentage2$'% Change 2017-2021')) %>%
  ggplot(aes(x = city, y = compare_percentage2$`% Change 2017-2021`, label = City)) +
  geom_bar(stat = "identity", alpha = 0.6, width = 0.3, fill = "#dc322f") +
  scale_y_continuous("% Change", limits = c(-0.05, 0.05)) +
  ggtitle("Difference in Police Spending by Percentage 2017-2021") +
  scale_x_discrete("City") +
  theme_solarized_2() 
###### work in progress Difference in Police Spending by Percentage last 5 years

Lewiston_data %>%
  ggplot(aes(`Fiscal Year`, `Police$ per Citizen`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years, limits = c(2010, 2020)) +
  scale_y_continuous("$ per Citizen") +
  stat_smooth(method = "lm", geom = "line", alpha = 0.75, size = 1, se = FALSE) +
  geom_point() +
  ggtitle("Police Spending Per Citizen") +
  theme_solarized_2() +
  scale_colour_manual(values = c("#dc322f"))
##### Lewiston $ per citizen graph

data_new %>%
  filter(City == 'Brunswick'|City == 'Lewiston'| City == 'Portland'| City == 'Auburn') %>%
  ggplot(aes(`Fiscal Year`, `Police$ per Citizen`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years, limits = c(2010, 2020)) +
  scale_y_continuous("$ per Citizen") +
  geom_smooth(method = "lm", alpha = 0.7, size = 1, se = FALSE) +
  geom_point(alpha = 0.5) +
  ggtitle("Police Spending Per Citizen in Lewiston/Portland/Auburn/Brunswick") +
  theme_solarized_2() +
  scale_colour_manual(values = c("#6c71c4", "#dc322f", "#859900", "#268bd2"))
##### Lewiston/Auburn/Brunswick/Portland Police $ per Citizen graph

allcities2 <- na.omit(subset(data, select = c('City', 'Fiscal Year', 'Police $ per income')))
##### data set for all cites police/income comparison

colors3 <- c("#586e75","#586e75", "#586e75", "#586e75", "#586e75", "#dc322f",
             "#586e75", "#586e75", "#586e75" )
##### color set to highlight Lewiston

allcities2 %>%
  filter(City != 'Saco') %>%
  ggplot(aes(`Fiscal Year`, `Police $ per income`, color = City)) +
  scale_x_continuous("Fiscal Year", breaks = years) +
  scale_y_continuous("Police $ per Citizen, divided by Median Income") +
  geom_smooth(method = "lm", size = 1, se = FALSE) +
  ggtitle("Police Spending Per Citizen Divided by Median Income") +
  geom_label(aes(x = 2010.3, y = .0043, label = "Lewiston")) +
  geom_label(aes(x = 2012.1, y = 0.0062, label = "Augusta")) +
  geom_label(aes(x =  2014.2, y = 0.00735, label = "Bangor")) +
  geom_label(aes(x = 2015.8, y = 0.0026, label = "South Portland")) +
  geom_label(aes(x = 2017.8, y = 0.00335, label = "Westbrook")) +
  geom_label(aes(x = 2014.3, y = 0.00548, label = "Brunswick")) +
  theme_solarized_2() +
  theme(legend.position = "none") +
  scale_colour_manual(values = colors3)
##### comparison of all cities while accounting for median income