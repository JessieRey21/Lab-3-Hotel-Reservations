## Part 4 - Use the Decision Tree C5.0 Algorithm 
hotel_dt_model <- C5.0(is_canceled ~ ., data = hotel_dt_train)
hotel_dt_model

summary(hotel_dt_model)

plot(hotel_dt_model)

hotel_predict <- predict(hotel_dt_model, hotel_dt_test)

CrossTable(hotel_dt_test$is_canceled, hotel_predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default','predicted default'))

## Improve the model
hotel_dt_boost10 <- C5.0(is_canceled ~ ., data = hotel_dt_train, trials = 10)
hotel_dt_boost10

hotel_dt_boost_pred10 <- predict(hotel_dt_boost10, hotel_dt_test)
CrossTable(hotel_dt_test$is_canceled, hotel_dt_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default','predicted default'))

