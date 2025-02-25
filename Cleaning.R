install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("C50")
install.packages("gmodels")
library(readr)
library(dplyr)
library(ggplot2)
library(C50)
library(gmodels)

install.packages("OneR")
install.packages("caret")
install.packages("rJava")
install.packages("RWeka")
library(OneR)
library(caret)
library(rJava)
library(RWeka)

## Part 0 - Background
hotel <- read.csv("hotel.csv", stringsAsFactors = TRUE)
glimpse(hotel)

prop.table(table(hotel$is_canceled))
  ## Not Cancelled - 0.62959
  ## Cancelled - 0.370401


## Part 1 - Data Preparation
  ## Step 1 - children
hotel <- hotel %>%
  mutate(children = children + babies)

hotel <- hotel %>%
  select(-babies)

  ## Step 2 - nights
hotel <- hotel %>%
  mutate(nights = stays_in_week_nights + stays_in_weekend_nights)

hotel <- hotel %>%
  select(-stays_in_weekend_nights, -stays_in_week_nights)

  ## Step 3 - repeated guests
hotel$is_repeated_guest <- factor(hotel$is_repeated_guest,
                                  levels = c("0", "1"),
                                  labels = c("No", "Yes"))
  ## Step 4 - categorical bins
summary(hotel$lead_time)
hist(hotel$lead_time, xlim = c(0,160))
hotel$lead_time_bin <- cut(hotel$lead_time, 
                           breaks = c(0, 50, 100, 150, 200, max(hotel$lead_time)), 
                           labels = c("0-50", "50-100", "100-150", "150-200", ">200"), 
                           include.lowest = TRUE)

summary(hotel$nights)
hist(hotel$nights, xlim = c(0,10))
hotel$nights_bin <- cut(hotel$nights, 
                       breaks = c(0, 0.5, 1.5, 2.5, 3.5, max(hotel$nights)), 
                       labels = c(">1", "1", "2", "3", ">3"), 
                       include.lowest = TRUE)

summary(hotel$previous_cancellations)
ggplot(hotel, aes(x = previous_cancellations)) +
  geom_bar()

hotel <- hotel %>%
  mutate(previous_cancellations_factor = 
           if_else(previous_cancellations > 0, "Yes", "No"))

summary(hotel$booking_changes)
ggplot(hotel, aes(x = booking_changes)) +
  geom_bar()

hotel <- hotel %>%
  mutate(booking_changes_factor = 
           if_else(booking_changes > 0, "Yes", "No"))

summary(hotel$adr)
ggplot(hotel, aes(x=adr))+
  geom_bar() +
  xlim(0,150)

hist(hotel$adr, xlim = c(0,1000))
hotel$adr_bin <- cut(hotel$adr, 
                      breaks = c(0, 50, 100, 150, 200, max(hotel$adr)), 
                      labels = c("0-50", "50-100", "100-150", "150-200", ">200"), 
                      include.lowest = TRUE)

  ## Step 5 - canceled
hotel$is_canceled <- factor(hotel$is_canceled,
                      levels = c("0","1"),
                      labels = c("Not Cancelled", "Cancelled"))

## Part 2 - Exploratory Data Analysis
  ## Creating df for Decision Trees
hotel_dt <- hotel %>%
  select(nights, booking_changes_factor, is_repeated_guest,
         previous_cancellations_factor, deposit_type, adr_bin, 
         lead_time_bin, arrival_date_month, is_canceled)

glimpse(hotel_dt)
hotel_dt$previous_cancellations <- as.factor(hotel_dt$previous_cancellations)
hotel_dt$booking_changes_factor <- as.factor(hotel_dt$booking_changes_factor)

  ## Creating df for Decision Rules
hotel_dr <- hotel %>%
  select(nights_bin, booking_changes_factor, is_repeated_guest,
         previous_cancellations_factor, deposit_type, adr_bin, 
         lead_time_bin, arrival_date_month, is_canceled)

glimpse(hotel_dr)
hotel_dr$previous_cancellations <- as.factor(hotel_dr$previous_cancellations)
hotel_dr$booking_changes_factor <- as.factor(hotel_dt$booking_changes_factor)

  ## Investigate chosen features
    ## Nights
summary(hotel_dt$nights)
ggplot(hotel_dt, aes(x=nights, fill = is_canceled)) +
  geom_bar(position = "dodge")+
  labs(title = "Number of Nights",
       x = "Nights", 
       y = "Count") +
      xlim(0,20)

    ## Booking changes
summary(hotel_dt$booking_changes_factor)
  ## 101309 - no
  ## 18075 - yes 
prop.table(table(hotel_dt$booking_changes_factor))

ggplot(hotel_dt, aes(x = booking_changes_factor, fill = is_canceled)) +
  geom_bar(position = "dodge")+
  labs(title = "Cancellations By Booking Changes",
       x = "Booking Changes", 
       y = "Count")


    ## Repeated guest 
summary(hotel_dt$is_repeated_guest)
  ## 115575 - no
  ## 3809 - yes
prop.table(table(hotel_dt$is_repeated_guest))

ggplot(hotel_dt, aes(x = is_repeated_guest, fill = is_canceled)) +
  geom_bar(position = "dodge") +
  labs(title = "Repeated Guests",
       x = "Repeated",
       y = "Count")

  ## Previous cancellations
summary(hotel_dt$previous_cancellations_factor)
prop.table(table(hotel_dt$previous_cancellations_factor))

ggplot(hotel_dt, aes(x=previous_cancellations, fill = is_canceled)) +
  geom_bar(position = "dodge") +
  labs(title = "Previous Cancellations",
       x = "Previous Cancellations",
       y = "Count")

  ## Deposit type
summary(hotel_dt$deposit_type)
prop.table(table(hotel_dt$deposit_type))

ggplot(hotel_dt, aes(x = deposit_type, fill = is_canceled)) +
  geom_bar(position = "dodge") +
  labs(title = "Deposit Type",
       x = "Deposit Type",
       y = "Count")
  
  ## Adr
summary(hotel_dt$adr_bin)

ggplot(hotel_dt, aes(x = adr_bin, fill = is_canceled)) +
  geom_bar(position = "dodge") +
  labs(title = "Average Daily Rate by Cancellations", 
       x = "Average Daily Rate", 
       y = "Count")
  
  ## Lead time
summary(hotel_dt$lead_time_bin)

ggplot(hotel_dt, aes(x = lead_time_bin, fill = is_canceled)) +
  geom_bar(position = "dodge") +
  labs(title = "Lead Time by Cancellations", 
       x = "Lead Time", 
       y = "Count")

  ## Arrival month
summary(hotel_dt$arrival_date_month)

ggplot(hotel_dt, aes(x=arrival_date_month, fill = is_canceled)) +
  geom_bar(position = "dodge") +
  labs(title = "Arrival Month by Cancellations", 
       x = "Arrival Month", 
       y = "Count")
  
  ## Nights_bin
summary(hotel_dr$nights_bin)

ggplot(hotel_dr, aes(x=nights_bin, fill = is_canceled)) +
geom_bar(position = "dodge") +
  labs(title = "Number of Nights by Cancellations by Bins", 
       x = "Number of Nights", 
       y = "Count")



## Part 3 - Test and Training Datasets
set.seed(4836)
train_size <- floor(0.9 * 119384)
train_sample <- sample(119384, train_size)

hotel_dt_train <- hotel_dt[train_sample,]
hotel_dt_test <- hotel_dt[-train_sample,]

prop.table(table(hotel_dt_train$is_canceled))
  ## Not cancelled - 0.6298
  ## Cancelled - 0.3702
prop.table(table(hotel_dt_test$is_canceled))
  ## Not cancelled - 0.6277
  ## Cancelled - 0.3722
