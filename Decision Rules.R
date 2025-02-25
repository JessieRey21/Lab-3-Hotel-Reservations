## Part 5 - Decision Rules Algorithm
## 1R Classifier
install.packages("OneR")
library(OneR)

hotel_1R <- OneR(is_canceled ~ ., data = hotel_dr)
hotel_1R

## RIPPER Algorithm
install.packages("rJava")
install.packages("RWeka")
library(rJava)
library(RWeka)

hotel_JRip <- JRip(is_canceled ~ ., data = hotel_dr)
hotel_JRip

hotel_JRip_pred <- predict(hotel_JRip, hotel_dr)
table(actual = hotel_dr$is_canceled, predicted = hotel_JRip_pred)
