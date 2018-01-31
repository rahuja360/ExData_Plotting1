library(tidyverse)
library(dplyr)
library(ggplot2)
library (lubridate)

#Set working directory

setwd("~/Desktop/Practice/Coursera")

#Read in txt file data

data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?")

#Convert to tibble

datas <- as_data_frame(data)

#Reformat dates and filter by datetime

plot2 <- datas %>%
  mutate(Date = dmy(paste(datas$Date))) %>%
  mutate(Day = weekdays(Date)) %>% 
  mutate(Date_Time = dmy_hms(paste(datas$Date, datas$Time))) %>% 
  filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>% 
  select(-one_of(c("Date", "Time"))) %>% 
  
  ggplot(aes(x = Date_Time, y = Global_active_power)) +
  geom_line () +
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(
    y = "Global Active Power (kilowatts)",
    x = "")

ggsave("plot2.png")