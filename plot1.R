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

plot1 <- datas %>%
  mutate(Date = dmy(paste(datas$Date))) %>%
  mutate(Date_Time = dmy_hms(paste(datas$Date, datas$Time))) %>% 
  filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>% 
  select(-one_of(c("Date", "Time"))) %>% 
  
  #Make graph
  
  ggplot(aes(x = Global_active_power)) +
  geom_histogram(breaks = seq(0,6,by=.5), na.rm=TRUE, color = "black", fill = "red") +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  
  theme(plot.title = element_text(hjust=.5, face="bold")) +
  labs(
    title = "Global Active Power",
    y = "Frequency",
    x = "Global Active Power (kilowatts)")

ggsave("plot1.png")