library(data.table)
library(dplyr)
library(caret)
library(ie2misc)

# Model 1 -----------------------------------------------------------------

####### product 138936951
prod1 <- read.csv("C:/Users/Joyce/OneDrive - Emory University/Desktop/Emory MSBA/Marketing Analytics/Project4/prod1.csv")
prod1 = data.table(prod1)

### normalize, standardize features
# z score = (x-mean)/std
# season index
prod1$seas_index<- (prod1$seas_index-mean(prod1$seas_index))/sd (prod1$seas_index)

# paid search
prod1$Paid_Search<- (prod1$Paid_Search-mean(prod1$Paid_Search))/sd (prod1$Paid_Search)

# web display
prod1$Web_Display<- (prod1$Web_Display-mean(prod1$Web_Display))/sd(prod1$Web_Display)

# email

prod1$Email<- (prod1$Email-mean(prod1$Email))/sd(prod1$Email)

### log transform target variable

# max sales quantity
max(prod1$weekly_sale_qty) # 336

# assume max sales quantity to be 10% up the current max

prod1[, max_sales_qty := max(weekly_sale_qty) * 1.1] 
prod1[, sales_tran := log(weekly_sale_qty / (max_sales_qty - weekly_sale_qty))] 

### build regression, no Store Display for this product
formula <- sales_tran ~  weekly_shelf_price + weekly_discount_rate+ as.factor(Flyer) +  Web_Display + Paid_Search + Email+ TV + Radio + seas_index + as.factor(holiday)
lm1 <- lm(sales_tran ~  weekly_shelf_price + weekly_discount_rate+ as.factor(Flyer) +  Web_Display + Paid_Search + Email+ TV + Radio + seas_index + as.factor(holiday), data = prod1)
summary(lm1)

## use logit to bound the prediction
prod1$pred <- predict(lm1,newdata=prod1)

prod1[, pred := max_sales_qty * exp(pred) / (exp(pred) + 1)]

### Decompose DueTo
## base
model1_base <- copy(prod1)

# price of earliest week
model1_base[, weekly_shelf_price := prod1$weekly_shelf_price[prod1$tran_wk == min(prod1$tran_wk)]]

model1_base[, c('weekly_discount_rate', 'Email', 'Flyer', 'Paid_Search', 'Web_Display', 'TV', 'Radio')] <- 0
prod1$base <- predict(lm1, newdata = model1_base)

# logit transformation
prod1[, base := max_sales_qty * exp(base) / (exp(base) + 1)]

# dueto shelf price
model1_base <- copy(prod1)
model1_base[, weekly_shelf_price := prod1$weekly_shelf_price[prod1$tran_wk == min(prod1$tran_wk)]]
prod1$due_to_base_price <- predict(lm1, newdata = model1_base)
prod1[, due_to_base_price := max_sales_qty * exp(due_to_base_price) / (exp(due_to_base_price) + 1)]
prod1[, due_to_Price := pred - due_to_base_price]
prod1[, due_to_base_price := NULL]

# dueto weekly_discount
model1_base <- copy(prod1)

# set to 0 
model1_base[, weekly_discount_rate := 0]
prod1$due_to_weekly_discount <- predict(lm1, newdata = model1_base)

# logit transform
prod1[, due_to_weekly_discount := max_sales_qty * exp(due_to_weekly_discount) / (exp(due_to_weekly_discount) + 1)]

# difference from prediction is the due to volume increase from discount rate 
prod1[, due_to_weekly_discount := pred - due_to_weekly_discount]

# dueto vehicles
for (media in c('Flyer', 'Email', 'Web_Display', 'Paid_Search', 'TV', 'Radio')) {
  model1_base <- copy(prod1)
  model1_base[, c(media) := 0] # set to 0
  col_name = paste('due_to', media, sep = '_')
  prod1[, c(col_name) := predict(lm1, newdata = model1_base)]
  prod1[, c(col_name)] <- prod1$max_sales_qty * exp(prod1[, .SD, .SDcols = c(col_name)]) / (exp(prod1[, .SD, .SDcols = c(col_name)]) + 1)
  prod1[, c(col_name)] <- prod1$pred - prod1[, .SD, .SDcols = c(col_name)]
}


#write.csv(prod1, 'prod1_dueto.csv', row.names = FALSE)


# Model 2 -----------------------------------------------------------------

####### product 138936952
prod2 <- read.csv("C:/Users/Joyce/OneDrive - Emory University/Desktop/Emory MSBA/Marketing Analytics/Project4/prod2.csv")
prod2 = data.table(prod2)

### normalize, standardize features
# z score = (x-mean)/std
# season index
prod2$seas_index<- (prod2$seas_index-mean(prod2$seas_index))/sd (prod2$seas_index)

# paid search
prod2$Paid_Search<- (prod1$Paid_Search-mean(prod2$Paid_Search))/sd (prod2$Paid_Search)

# web display
prod2$Web_Display<- (prod2$Web_Display-mean(prod2$Web_Display))/sd(prod2$Web_Display)

# email

prod2$Email<- (prod2$Email-mean(prod2$Email))/sd(prod2$Email)

### log transform target variable

# max sales quantity
max(prod2$weekly_sale_qty) # 216

# assume max sales quantity to be 10% up the current max

prod2[, max_sales_qty := max(weekly_sale_qty) * 1.1] 
prod2[, sales_tran := log(weekly_sale_qty / (max_sales_qty - weekly_sale_qty))] 

### build regression, no Store Display for this product
formula <- sales_tran ~  weekly_shelf_price + weekly_discount_rate+ as.factor(Flyer) +  Web_Display + Paid_Search + Email+ as.factor(Store_Display) + TV + Radio + seas_index + as.factor(holiday)
lm2 <- lm(sales_tran ~  weekly_shelf_price + weekly_discount_rate+ as.factor(Flyer) +  Web_Display + Paid_Search + Email+ as.factor(Store_Display) + TV + Radio + seas_index + as.factor(holiday), data = prod2)
summary(lm2)

## use logit to bound the prediction
prod2$pred <- predict(lm2,newdata=prod2)

prod2[, pred := max_sales_qty * exp(pred) / (exp(pred) + 1)]

### Decompose due tos
## base
model2_base <- copy(prod2)

model2_base[, weekly_shelf_price := prod2$weekly_shelf_price[prod2$tran_wk == min(prod2$tran_wk)]]

model2_base[, c('weekly_discount_rate','Store_Display', 'Email', 'Flyer', 'Paid_Search', 'Web_Display', 'TV', 'Radio')] <- 0
prod2$base <- predict(lm2, newdata = model2_base)

# logit transformation
prod2[, base := max_sales_qty * exp(base) / (exp(base) + 1)]

# dueto shelf price
model2_base <- copy(prod2)
model2_base[, weekly_shelf_price := prod2$weekly_shelf_price[prod2$tran_wk == min(prod2$tran_wk)]]
prod2$due_to_base_price <- predict(lm2, newdata = model2_base)
prod2[, due_to_base_price := max_sales_qty * exp(due_to_base_price) / (exp(due_to_base_price) + 1)]
prod2[, due_to_Price := pred - due_to_base_price]
prod2[, due_to_base_price := NULL]

# dueto weekly_discount
model2_base <- copy(prod2)
model2_base[, weekly_discount_rate := 0]
prod2$due_to_weekly_discount <- predict(lm2, newdata = model2_base)
prod2[, due_to_weekly_discount := max_sales_qty * exp(due_to_weekly_discount) / (exp(due_to_weekly_discount) + 1)]
prod2[, due_to_weekly_discount := pred - due_to_weekly_discount]

# dueto vehicles
for (media in c('Flyer', 'Email', 'Store_Display','Web_Display', 'Paid_Search', 'TV', 'Radio')) {
  model2_base <- copy(prod2)
  model2_base[, c(media) := 0] # set to 0
  col_name = paste('due_to', media, sep = '_')
  prod2[, c(col_name) := predict(lm2, newdata = model2_base)]
  prod2[, c(col_name)] <- prod2$max_sales_qty * exp(prod2[, .SD, .SDcols = c(col_name)]) / (exp(prod2[, .SD, .SDcols = c(col_name)]) + 1)
  prod2[, c(col_name)] <- prod2$pred - prod2[, .SD, .SDcols = c(col_name)]
}


#write.csv(prod2, 'prod2_dueto.csv', row.names = FALSE)



# Model 3 -----------------------------------------------------------------

####### product 138936953
prod3 <- read.csv("C:/Users/Joyce/OneDrive - Emory University/Desktop/Emory MSBA/Marketing Analytics/Project4/prod3.csv")
prod3 = data.table(prod3)

### normalize, standardize features
# z score = (x-mean)/std
# season index
prod3$seas_index<- (prod3$seas_index-mean(prod3$seas_index))/sd (prod3$seas_index)

# paid search
prod3$Paid_Search<- (prod3$Paid_Search-mean(prod3$Paid_Search))/sd (prod3$Paid_Search)

# web display
prod3$Web_Display<- (prod3$Web_Display-mean(prod3$Web_Display))/sd(prod3$Web_Display)

# email

prod3$Email<- (prod3$Email-mean(prod3$Email))/sd(prod3$Email)

### log transform target variable

# max sales quantity
max(prod3$weekly_sale_qty) # 81

# assume max sales quantity to be 10% up the current max

prod3[, max_sales_qty := max(weekly_sale_qty) * 1.1] 
prod3[, sales_tran := log(weekly_sale_qty / (max_sales_qty - weekly_sale_qty))] 

### build regression, no Store Display for this product
formula <- sales_tran ~  weekly_shelf_price + weekly_discount_rate+ as.factor(Flyer) +  Web_Display + Paid_Search + Email+ as.factor(Store_Display) + TV + Radio + seas_index + as.factor(holiday)
lm3 <- lm(sales_tran ~  weekly_shelf_price + weekly_discount_rate+ as.factor(Flyer) +  Web_Display + Paid_Search + Email+ as.factor(Store_Display) + TV + Radio + seas_index + as.factor(holiday), data = prod3)
summary(lm3)

## use logit to bound the prediction
prod3$pred <- predict(lm3,newdata=prod3)

prod3[, pred := max_sales_qty * exp(pred) / (exp(pred) + 1)]

### Decompose DueTo
## base
model3_base <- copy(prod3)

model3_base[, weekly_shelf_price := prod3$weekly_shelf_price[prod3$tran_wk == min(prod3$tran_wk)]]

model3_base[, c('weekly_discount_rate','Store_Display', 'Email', 'Flyer', 'Paid_Search', 'Web_Display', 'TV', 'Radio')] <- 0
prod3$base <- predict(lm3, newdata = model3_base)

# logit transformation
prod3[, base := max_sales_qty * exp(base) / (exp(base) + 1)]

# dueto shelf price
model3_base <- copy(prod3)
model3_base[, weekly_shelf_price := prod3$weekly_shelf_price[prod3$tran_wk == min(prod3$tran_wk)]]
prod3$due_to_base_price <- predict(lm3, newdata = model3_base)
prod3[, due_to_base_price := max_sales_qty * exp(due_to_base_price) / (exp(due_to_base_price) + 1)]
prod3[, due_to_Price := pred - due_to_base_price]
prod3[, due_to_base_price := NULL]

# dueto weekly_discount
model3_base <- copy(prod3)
model3_base[, weekly_discount_rate := 0]
prod3$due_to_weekly_discount <- predict(lm3, newdata = model3_base)
prod3[, due_to_weekly_discount := max_sales_qty * exp(due_to_weekly_discount) / (exp(due_to_weekly_discount) + 1)]
prod3[, due_to_weekly_discount := pred - due_to_weekly_discount]

# dueto vehicles
for (media in c('Flyer', 'Email', 'Store_Display','Web_Display', 'Paid_Search', 'TV', 'Radio')) {
  model3_base <- copy(prod3)
  model3_base[, c(media) := 0] # set to 0
  col_name = paste('due_to', media, sep = '_')
  prod3[, c(col_name) := predict(lm3, newdata = model3_base)]
  prod3[, c(col_name)] <- prod3$max_sales_qty * exp(prod3[, .SD, .SDcols = c(col_name)]) / (exp(prod3[, .SD, .SDcols = c(col_name)]) + 1)
  prod3[, c(col_name)] <- prod3$pred - prod3[, .SD, .SDcols = c(col_name)]
}


#write.csv(prod3, 'prod3_dueto.csv', row.names = FALSE)

# Model Evaluation ------------------------------------------------------

### Model 1

# R^2
summary(lm1)$r.squared

# mae
mae(prod1$weekly_sale_qty,prod1$pred)

# mape
mape(prod1$weekly_sale_qty,prod1$pred)

# rmse
rmse(prod1$weekly_sale_qty,prod1$pred)


# vif
car::vif(lm1)

### Model 2

# R^2
summary(lm2)$r.squared

# mae
mae(prod2$weekly_sale_qty,prod2$pred)

# mape
mape(prod2$weekly_sale_qty,prod2$pred)

# rmse
rmse(prod2$weekly_sale_qty,prod2$pred)

# vif
car::vif(lm2)

### Model 3

# R^2
summary(lm3)$r.squared

# mae
mae(prod3$weekly_sale_qty,prod3$pred)

# mape
mape(prod3$weekly_sale_qty,prod3$pred)

# rmse
rmse(prod3$weekly_sale_qty,prod3$pred)


# vif
car::vif(lm3)


