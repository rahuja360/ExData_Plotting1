library(tidyverse)
library(dplyr)
library(ggplot2)
library (lubridate)
library(grid)
library(gridExtra)

#Set working directory

setwd("~/Desktop/Practice/Coursera")

#Read in txt file data

data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?")

#Convert to tibble

datas <- as_data_frame(data)

#Reformat dates and filter by datetime

plot1_grid <- datas %>%
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

#Next plot

plot3_grid <- datas %>%
  mutate(Date = dmy(paste(datas$Date))) %>%
  mutate(Day = weekdays(Date)) %>% 
  mutate(Date_Time = dmy_hms(paste(datas$Date, datas$Time))) %>% 
  filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>% 
  select(-one_of(c("Date", "Time"))) %>% 
  
  ggplot(aes(x = Date_Time)) +
  geom_line (aes(y = Sub_metering_1, col = "Sub_metering_1")) +
  geom_line (aes(y = Sub_metering_2, col = "Sub_metering_2")) +
  geom_line (aes(y = Sub_metering_3, col = "Sub_metering_3")) +
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  scale_colour_manual(values = c("black", "red", "blue")) +
  theme_bw()+
  theme(panel.border = element_rect(colour = "black"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = c(.9, .9),
        legend.title=element_blank(),
        legend.background = element_rect(size = .5, linetype = "solid", colour = "black")) +
  labs(
    y = "Energy sub metering",
    x = "")

#New plot

plot2_grid <- datas %>%
  mutate(Date = dmy(paste(datas$Date))) %>%
  mutate(Day = weekdays(Date)) %>% 
  mutate(Date_Time = dmy_hms(paste(datas$Date, datas$Time))) %>% 
  filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>% 
  select(-one_of(c("Date", "Time"))) %>% 
  
  ggplot(aes(x = Date_Time, y = Voltage)) +
  geom_line () +
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(
    y = "Voltage",
    x = "")

#New plot 2

plot4_grid <- datas %>%
  mutate(Date = dmy(paste(datas$Date))) %>%
  mutate(Day = weekdays(Date)) %>% 
  mutate(Date_Time = dmy_hms(paste(datas$Date, datas$Time))) %>% 
  filter(Date >= "2007-02-01" & Date <= "2007-02-02") %>% 
  select(-one_of(c("Date", "Time"))) %>% 
  
  ggplot(aes(x = Date_Time, y = Global_reactive_power)) +
  geom_line () +
  scale_x_datetime(date_labels = "%a", date_breaks = "1 day") +
  theme_bw()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs(
    y = "Global_reactive_power",
    x = "")

#Assemble graphs

grid <- grid.arrange(plot1_grid, plot2_grid, plot3_grid, plot4_grid)

ggsave(file="plot4.png", grid)
