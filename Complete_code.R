# Required libraries
library(dplyr)
library(car)
library(ggplot2)
library(GGally)
library(vcd)
library(leaps)
library(MASS)
library(nortest)
library(mgcv)
library(viridis)
library(lmtest)
library(glmnet)
library(readxl)
library(openxlsx)


#Data Preprocessing-------------------------------------------------------------
# read the dataset
data_train <- read.csv2("data_train.csv", comment.char="#")

summary(data_train)

# Convert all the character variables into factors
data_train <- data_train %>%
  mutate(across(where(is.character), as.factor))

# Binary variables as factors
data_train <- data_train %>%
  mutate(across(c(comercial, casco.historico, M.30), as.factor))

# Integers

# cod_distrito y distrito son lo mismo
data_train$cod_distrito <- as.factor(data_train$cod_distrito)
# cod_barrio y barrio lo mismo
data_train$cod_barrio <- as.factor(data_train$cod_barrio)



data_train$dorm <- as.factor(data_train$dorm)
data_train$banos <- as.factor(data_train$banos)


# sup.const vs sup.util
which(data_train$sup.const< data_train$sup.util)
data_train <- data_train[-which(data_train$sup.const< data_train$sup.util),]

# Recode categorical variables
data_train$dorm <- recode_factor(data_train$dorm, "0"="0-1", "1"="0-1", "4"= "4≤", "5"= "4≤",
                                 "6"= "4≤", "7"= "4≤")

data_train$banos <- recode_factor(data_train$banos, "3"= "3≤",  "4"= "3≤", "5"= "3≤",
                                  "6"= "3≤", "7"= "3≤")

data_train$tipo.casa <- recode_factor(data_train$tipo.casa, "chalet"="grande", "duplex"="grande",
                                      "estudio"="piso", "Otros"="piso")
data_train$comercial <- recode_factor(data_train$comercial, "0"="no", "1"="si")
data_train$casco.historico <- recode_factor(data_train$casco.historico, "0"="no", "1"="si")
data_train$M.30 <- recode_factor(data_train$M.30, "0"="no", "1"="si")

data_train$estado <- recode_factor(data_train$estado, "excelente"="bien", "buen_estado"="bien",
                                   "reformado"="bien", "nuevo-semin,"="bien", "segunda_mano"= "bien",
                                   "reg,-mal"="mal", "a_reformar"="mal")


# Response variable

plot(density(data_train$precio.house.m2))

data_train$precio.house.m2_log <- log(data_train$precio.house.m2)

plot(density(data_train$precio.house.m2_log))

## Simple linear models --------------------------------------------------------

# cod_barrio

cod_barrio_slm <- lm(precio.house.m2_log ~ cod_barrio, data = data_train)
summary(cod_barrio_slm)
# p-value: < 2.2e-16 

# cod_distrito

cod_distrito_slm <- lm(precio.house.m2_log ~ cod_distrito, data = data_train)
summary(cod_distrito_slm)
# p-value: < 2.2e-16 

# longitud

longitud_slm <- lm(precio.house.m2_log ~ longitud, data = data_train)
summary(longitud_slm)
# p-value: 0.03928

# latitud

latitud_slm <- lm(precio.house.m2_log ~ latitud, data = data_train)
summary(latitud_slm)
# p-value: < 2.2e-16

# sup.const

sup.const_slm <- lm(precio.house.m2_log ~ sup.const, data = data_train)
summary(sup.const_slm)
# p-value: 0.0004824

# sup.util

sup.util_slm <- lm(precio.house.m2_log ~ sup.util, data = data_train)
summary(sup.util_slm)
# p-value: 0.0003111

# ref.hip.zona

ref.hip.zona_slm <- lm(precio.house.m2_log ~ ref.hip.zona, data = data_train)
summary(ref.hip.zona_slm)
# p-value: < 2.2e-16

# dorm
dorm_slm <- lm(precio.house.m2_log ~ dorm, data = data_train)
summary(dorm_slm)
# p-value: 4.584e-16

# banos
banos_slm <- lm(precio.house.m2_log ~ banos, data = data_train)
summary(banos_slm)
# p-value: 7.39e-09

# tipo.casa
tipo.casa_slm <- lm(precio.house.m2_log ~ tipo.casa, data = data_train)
summary(tipo.casa_slm)
# p-value: 0.0006578

# inter.exter
inter.exter_slm <- lm(precio.house.m2_log ~ inter.exter, data = data_train)
summary(inter.exter_slm)
# p-value: 0.0003997

# ascensor
ascensor_slm <- lm(precio.house.m2_log ~ ascensor, data = data_train)
summary(ascensor_slm)
# p-value: < 2.2e-16

# estado
estado_slm <- lm(precio.house.m2_log ~ estado, data = data_train)
summary(estado_slm)
# p-value: 0.002425

# antig
antig_slm <- lm(precio.house.m2_log ~ antig, data = data_train)
summary(antig_slm)
# p-value: 6.242e-05

# comercial
comercial_slm <- lm(precio.house.m2_log ~ comercial, data = data_train)
summary(comercial_slm)
# p-value: < 2.2e-16

# casco.historico
casco.historico_slm <- lm(precio.house.m2_log ~ casco.historico, data = data_train)
summary(casco.historico_slm)
# p-value: < 2.2e-16

# Ruidos_ext
Ruidos_ext_slm <- lm(precio.house.m2_log ~ Ruidos_ext, data = data_train)
summary(Ruidos_ext_slm)
# p-value: 0.0001237

# Mal_olor
Mal_olor_slm <- lm(precio.house.m2_log ~ Mal_olor, data = data_train)
summary(Mal_olor_slm)
# p-value: 0.002516

# Poca_limp
Poca_limp_slm <- lm(precio.house.m2_log ~ Poca_limp, data = data_train)
summary(Poca_limp_slm)
# p-value: 1.253e-12

# Malas_comunic
Malas_comunic_slm <- lm(precio.house.m2_log ~ Malas_comunic, data = data_train)
summary(Malas_comunic_slm)
# p-value: 5.235e-13

# Pocas_zonas
Pocas_zonas_slm <- lm(precio.house.m2_log ~ Pocas_zonas, data = data_train)
summary(Pocas_zonas_slm)
# p-value: 0.003859

# Delincuencia
Delincuencia_slm <- lm(precio.house.m2_log ~ Delincuencia, data = data_train)
summary(Delincuencia_slm)
# p-value: 1.313e-14

# M.30
M.30_slm <- lm(precio.house.m2_log ~ M.30, data = data_train)
summary(M.30_slm)
# p-value: < 2.2e-16

# CO
CO_slm <- lm(precio.house.m2_log ~ CO, data = data_train)
summary(CO_slm)
# p-value: < 2.2e-16

# NO2
NO2_slm <- lm(precio.house.m2_log ~ NO2, data = data_train)
summary(NO2_slm)
# p-value: 0.3542

# Nox
Nox_slm <- lm(precio.house.m2_log ~ Nox, data = data_train)
summary(Nox_slm)
# p-value: 0.483

# O3
O3_slm <- lm(precio.house.m2_log ~ O3, data = data_train)
summary(O3_slm)
# p-value: 0.001052

# SO2
SO2_slm <- lm(precio.house.m2_log ~ SO2, data = data_train)
summary(SO2_slm)
# p-value: < 2.2e-16

# PM10
PM10_slm <- lm(precio.house.m2_log ~ PM10, data = data_train)
summary(PM10_slm)
# p-value: 8.544e-15

# Pobl.0_14_div_Poblac.Total
Pobl.0_14_div_Poblac.Total_slm <- lm(precio.house.m2_log ~ Pobl.0_14_div_Poblac.Total, data = data_train)
summary(Pobl.0_14_div_Poblac.Total_slm)
# p-value: < 2.2e-16

# PoblJubilada_div_Poblac.Total
PoblJubilada_div_Poblac.Total_slm <- lm(precio.house.m2_log ~ PoblJubilada_div_Poblac.Total, data = data_train)
summary(PoblJubilada_div_Poblac.Total_slm)
# p-value: 3.688e-11

# Inmigrantes.porc
Inmigrantes.porc_slm <- lm(precio.house.m2_log ~ Inmigrantes.porc, data = data_train)
summary(Inmigrantes.porc_slm)
# p-value: 8.24e-09

## Correlation -----------------------------------------------------------------
#correlation matrix 
vbles_cont <-subset(data_train, dplyr::select = c("longitud", "latitud", "sup.const","sup.util","ref.hip.zona", "antig","Ruidos_ext","Mal_olor","Poca_limp","Malas_comunic","Pocas_zonas","Delincuencia","CO","NO2","Nox","O3","SO2","PM10","Pobl.0_14_div_Poblac.Total","PoblJubilada_div_Poblac.Total","Inmigrantes.porc"))
print(vbles_cont)

matriz_corr <- cor(vbles_cont)
print(matriz_corr)

x <- ggpairs(vbles_cont)
x
# Asscociation measures
## Medidas de asociacion entre categóricas

categorical_vars <- names(data_train_1 %>% select_if(is.factor))

cramers_v_matrix <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars),
                           dimnames = list(categorical_vars, categorical_vars))
for (i in 1:length(categorical_vars)) {
  for (j in i:length(categorical_vars)) {
    if (i == j) {
      cramers_v_matrix[i, j] <- 1  # Asociación perfecta consigo mismo
    } else {
      tabla <- table(data_train_1[[categorical_vars[i]]], data_train_1[[categorical_vars[j]]])
      cramers_v_matrix[i, j] <- assocstats(tabla)$cramer
      cramers_v_matrix[j, i] <- cramers_v_matrix[i, j]  # Matriz simétrica
    }
  }
}


## Select variables for the model----------------------------------------------

data_train_1 <- data_train %>%
  dplyr::select(-train_indices, -barrio, -cod_barrio, -distrito, -cod_distrito,
                -sup.const, -PM10, -Nox, -NO2, -Poca_limp, -SO2, -precio.house.m2,
                -inter.exter, -Inmigrantes.porc,-casco.historico)
numeric_vars <- names(data_train_1 %>% select_if(is.numeric) %>% dplyr::select(-precio.house.m2_log))
categorical_vars <- names(data_train_1 %>% select_if(is.factor))


## VIF -------------------------------------------------------------------------

numeric_vars <- names(data_train_1 %>% select_if(is.numeric) %>% dplyr::select(-precio.house.m2_log))

formula <- as.formula(paste("precio.house.m2_log", "~", paste(numeric_vars, collapse = " + ")))

model <- lm(formula = formula, data = data_train_1)

vif_values<-vif(model)

## CN --------------------------------------------------------------------------
data_train_CN <- data_train_1 %>% select_if(is.numeric) %>% select(-precio.house.m2_log)

MCN <- as.matrix(data_train_CN)

eigenvalues <- eigen(t(MCN)%*%MCN)$values


# Compute the Condition Number (C.N.)
condition_number <- sqrt(max(eigenvalues) / min(eigenvalues))

condition_number

# CN deleting the j variable each time

#Initialize a vector to store results
variables <- names(data_train_CN)
results <- data.frame(Variable_Removed = character(), Condition_Number = numeric())

# Step 3: Loop through each variable
for (var in variables) {
  # Create a new dataset excluding the current variable
  temp_data <- data_train_CN %>%
    dplyr::select(-all_of(var))
  
  # Convert to matrix
  MCN <- as.matrix(temp_data)
  
  # Calculate eigenvalues
  eigenvalues <- eigen(t(MCN) %*% MCN)$values
  
  # Compute the Condition Number
  condition_number <- sqrt(max(eigenvalues) / min(eigenvalues))
  
  # Store the result
  results <- rbind(results, data.frame(Variable_Removed = var, Condition_Number = condition_number))
}

# Step 4: Print results
print(results)

## Models without interaction --------------------------------------------------

full_model <- lm(precio.house.m2_log ~., data = data_train_1) #R^2=0.6064, n_param=26

#AIC
model_aic <-stepAIC(full_model, direction="both") 

summary(model_aic)

# BIC
model_bic <-stepAIC(full_model, direction="both", k = log(nrow(data_train_1))) #R^2=0.6002, n_param=13

summary(model_bic)

## MODELS WITH INTERACTIONS ---------------------------------------------------

# all possible interactions
interaction_terms <- c()
for (cat_var in categorical_vars) {
  for (num_var in numeric_vars) {
    interaction_terms <- c(interaction_terms, paste(cat_var, num_var, sep = "*"))
  }
}

formula <- as.formula(paste("precio.house.m2_log ~", paste(interaction_terms, collapse = " + ")))
model_inter <- lm(formula, data = data_train_1) #R^2=0.6436, n_param=180

#AIC 
model_aic_inter_total <- stepAIC(model_inter, direction = "both")

summary(model_bic_inter_total)

#BIC
model_bic_inter_total <- stepAIC(model_inter, direction = "both", k = log(nrow(data_train_1)))
#R^2=0.622, n_param=21

summary(model_bic_inter_total)
#sup.util isn't significative fo all its coeffs, let's evaluate if the model is better without it

#Let's try now to recode bathrooms

data_train_2<-data_train_1

data_train_2$banos<-recode_factor(data_train_2$banos, "2"= "2≤","3≤"="2≤")

model_inter_2<-lm(formula, data = data_train_2)
model_bic_inter_2<-stepAIC(model_inter_2, direction = "both", k = log(nrow(data_train_2)))
summary(model_bic_inter_2) #R^2=0.6247, n_param=21

#Pocas zonas not significative

formula_2 <- as.formula(paste("precio.house.m2_log ~", 'latitud + sup.util + ref.hip.zona + 
  antig + O3 + Pobl.0_14_div_Poblac.Total + PoblJubilada_div_Poblac.Total + 
  banos + tipo.casa + ascensor + estado + comercial + M.30 + 
  sup.util:banos + PoblJubilada_div_Poblac.Total:tipo.casa + 
 O3:ascensor'))

model_bic_inter_total_3 <- lm(formula_2, data = data_train_2) #R^2=0.6212, n_param=19
#all are significative now
summary(model_bic_inter_total_3)

model_bic_inter_final<-model_bic_inter_total_3 #R^2=0.6212, n_param=19

# Ridge regression

data_ridge <- data_train %>%
  dplyr::select(-barrio, -distrito, -cod_barrio, -cod_distrito,
                -sup.const, -Nox, -NO2, -PM10, -train_indices)
X <- model.matrix(precio.house.m2_log~.-1,data=data_ridge)

fit.ridge=glmnet(X,data_ridge$precio.house.m2_log,alpha=0)

cv.out = cv.glmnet(X, data_ridge$precio.house.m2_log, alpha = 0) #alpha=0 means Ridge Regression
opt_lambda <- cv.out$lambda.min


## RMSE calculation ------------------------------------------------------------
R <- 500
set.seed(68)
seeds <- round(runif(R)*10000)
ntrain <- 736
ntest <- 247
ntot <- ntrain+ntest
prop_train <- ntrain/ntot

data_train_rmse <- data_train %>%
  dplyr::select(-barrio, -cod_barrio, -distrito, -cod_distrito,
                -sup.const, -PM10, -Nox, -NO2, -Poca_limp, -SO2, -precio.house.m2,
                -inter.exter, -Inmigrantes.porc, -casco.historico)

data_train_rmse_2<-data_train_rmse

data_train_rmse_2$banos<-recode_factor(data_train_rmse_2$banos, "2"= "2≤","3≤"="2≤")

formula_2 <- as.formula(paste("precio.house.m2_log ~", 'latitud + sup.util + ref.hip.zona + 
  antig + O3 + Pobl.0_14_div_Poblac.Total + PoblJubilada_div_Poblac.Total + 
  banos + tipo.casa + ascensor + estado + comercial + M.30 + 
  sup.util:banos + PoblJubilada_div_Poblac.Total:tipo.casa + 
 O3:ascensor'))


rmse_full_model <- numeric(R)
rmse_full_model_inter <- numeric(R)
rmse_bic_model <- numeric(R)
rmse_bic_model_inter <- numeric(R)
rmse_ridge_model <- numeric(R)

for (r in 1:R){
  set.seed(seeds[r])
  
  id_boot <- sample(data_train_rmse$train_indices, size = round(prop_train*ntrain,0), replace = F)
  
  train_rmse <- data_train_rmse %>% filter(train_indices %in% id_boot)
  test_rmse <- data_train_rmse %>% filter(!train_indices %in% id_boot)
  
  train_rmse_2 <- data_train_rmse_2 %>% filter(train_indices %in% id_boot)
  test_rmse_2 <- data_train_rmse_2 %>% filter(!train_indices %in% id_boot)
  
  # Train the model
  model_full_train <- lm(formula = precio.house.m2_log ~ .-train_indices, data = train_rmse)
  
  model_inter_full <- lm(formula=as.formula(paste("precio.house.m2_log ~", paste(interaction_terms, collapse = " + "))), data = train_rmse)
  
  model_bic <- lm(formula = precio.house.m2_log ~ latitud + ref.hip.zona + dorm + 
                    banos + ascensor + estado + comercial + M.30 + Pobl.0_14_div_Poblac.Total, data = train_rmse)
  model_bic_inter <- lm(formula_2, data = train_rmse_2)
  
  
  
  #Predict the response variable
  pred_full <- predict(model_full_train, newdata = test_rmse, type = "response")
  pred_inter_full <- predict(model_inter_full, newdata = test_rmse, type = "response")
  pred_bic <- predict(model_bic, newdata = test_rmse, type = "response")
  pred_bic_inter <- predict(model_bic_inter, newdata = test_rmse_2, type = "response")
  
  
  # Calculate RMSE and store it
  rmse <- sqrt(mean((exp(test_rmse$precio.house.m2_log) - exp(pred_full))^2))
  rmse_full_model[r] <- rmse
  
  rmse_2 <- sqrt(mean((exp(test_rmse$precio.house.m2_log) - exp(pred_inter_full))^2))
  rmse_full_model_inter[r] <- rmse_2
  
  rmse_3 <- sqrt(mean((exp(test_rmse$precio.house.m2_log) - exp(pred_bic))^2))
  rmse_bic_model[r] <- rmse_3
  
  rmse_4 <- sqrt(mean((exp(test_rmse_2$precio.house.m2_log) - exp(pred_bic_inter))^2))
  rmse_bic_model_inter[r] <- rmse_4
}


data_train_rmse_ridge <- data_train %>%
  dplyr::select(-barrio, -distrito, -cod_barrio, -cod_distrito,
                -sup.const, -Nox, -NO2, -PM10)

for (r in 1:R){
  set.seed(seeds[r])
  
  id_boot <- sample(data_train_rmse_ridge$train_indices, size = round(prop_train*ntrain,0), replace = F)
  
  train_rmse <- data_train_rmse_ridge %>% filter(train_indices %in% id_boot)
  test_rmse <- data_train_rmse_ridge %>% filter(!train_indices %in% id_boot)
  
  train_rmse <- train_rmse%>%
    dplyr::select(-train_indices)
  
  test_rmse <- test_rmse%>%
    dplyr::select(-train_indices)
  
  # Train the model
  X <- model.matrix(precio.house.m2_log~.-1,data=train_rmse)
  
  fit.ridge=glmnet(X,train_rmse$precio.house.m2_log,alpha=0)
  
  cv.out = cv.glmnet(X, train_rmse$precio.house.m2_log, alpha = 0) #alpha=0 means Ridge Regression
  opt_lambda <- cv.out$lambda.min
  
  X_new <- model.matrix(precio.house.m2_log~.-1,data=test_rmse)
  
  #Predict the response variable
  predictions <- predict(fit.ridge, type = "response", newx = X_new, s = opt_lambda)
  
  # Calculate RMSE and store it
  rmse_5 <- sqrt(mean((exp(test_rmse$precio.house.m2_log) - exp(predictions))^2))
  rmse_ridge_model[r] <- rmse_5
}




data_list <- list(
  rmse_full_model, 
  rmse_full_model_inter, 
  rmse_bic_model,
  rmse_bic_model_inter,
  rmse_ridge_model
)

names(data_list) <- c(
  expression(omega[1]), 
  expression(omega[2]), 
  expression(omega[3]), 
  expression(omega[4]), 
  expression(omega[5])
)

# Create the boxplot with parsed names
boxplot(data_list, 
        main = "Boxplot of RMSE values, original scale", 
        ylab = "RMSE", 
        col = viridis(5), 
        names = sapply(names(data_list), function(x) parse(text = x)))

summary(rmse_full_model)
summary(rmse_full_model_inter)
summary(rmse_bic_model)
summary(rmse_bic_model_inter)
summary(rmse_ridge_model)

# Logarithmic scale

rmse_full_model_2 <- numeric(R)
rmse_full_model_inter_2 <- numeric(R)
rmse_bic_model_2 <- numeric(R)
rmse_bic_model_inter_2 <- numeric(R)
rmse_ridge_model_2 <- numeric(R)

for (r in 1:R){
  set.seed(seeds[r])
  
  id_boot <- sample(data_train_rmse$train_indices, size = round(prop_train*ntrain,0), replace = F)
  
  train_rmse <- data_train_rmse %>% filter(train_indices %in% id_boot)
  test_rmse <- data_train_rmse %>% filter(!train_indices %in% id_boot)
  
  train_rmse_2 <- data_train_rmse_2 %>% filter(train_indices %in% id_boot)
  test_rmse_2 <- data_train_rmse_2 %>% filter(!train_indices %in% id_boot)
  
  # Train the model
  model_full_train <- lm(formula = precio.house.m2_log ~ .-train_indices, data = train_rmse)
  
  model_inter_full <- lm(formula=as.formula(paste("precio.house.m2_log ~", paste(interaction_terms, collapse = " + "))), data = train_rmse)
  
  model_bic <- lm(formula = precio.house.m2_log ~ latitud + ref.hip.zona + dorm + 
                    banos + ascensor + estado + comercial + M.30 + Pobl.0_14_div_Poblac.Total, data = train_rmse)
  model_bic_inter <- lm(formula_2, data = train_rmse_2)
  
  
  
  #Predict the response variable
  pred_full <- predict(model_full_train, newdata = test_rmse, type = "response")
  pred_inter_full <- predict(model_inter_full, newdata = test_rmse, type = "response")
  pred_bic <- predict(model_bic, newdata = test_rmse, type = "response")
  pred_bic_inter <- predict(model_bic_inter, newdata = test_rmse_2, type = "response")
  
  
  # Calculate RMSE and store it
  rmse <- sqrt(mean((test_rmse$precio.house.m2_log - pred_full)^2))
  rmse_full_model_2[r] <- rmse
  
  rmse_2 <- sqrt(mean((test_rmse$precio.house.m2_log - pred_inter_full)^2))
  rmse_full_model_inter_2[r] <- rmse_2
  
  rmse_3 <- sqrt(mean((test_rmse$precio.house.m2_log - pred_bic)^2))
  rmse_bic_model_2[r] <- rmse_3
  
  rmse_4 <- sqrt(mean((test_rmse_2$precio.house.m2_log - pred_bic_inter)^2))
  rmse_bic_model_inter_2[r] <- rmse_4
}



for (r in 1:R){
  set.seed(seeds[r])
  
  id_boot <- sample(data_train_rmse_ridge$train_indices, size = round(prop_train*ntrain,0), replace = F)
  
  train_rmse <- data_train_rmse_ridge %>% filter(train_indices %in% id_boot)
  test_rmse <- data_train_rmse_ridge %>% filter(!train_indices %in% id_boot)
  
  train_rmse <- train_rmse%>%
    dplyr::select(-train_indices)
  
  test_rmse <- test_rmse%>%
    dplyr::select(-train_indices)
  
  # Train the model
  X <- model.matrix(precio.house.m2_log~.-1,data=train_rmse)
  
  fit.ridge=glmnet(X,train_rmse$precio.house.m2_log,alpha=0)
  
  cv.out = cv.glmnet(X, train_rmse$precio.house.m2_log, alpha = 0) #alpha=0 means Ridge Regression
  opt_lambda <- cv.out$lambda.min
  
  X_new <- model.matrix(precio.house.m2_log~.-1,data=test_rmse)
  
  #Predict the response variable
  predictions <- predict(fit.ridge, type = "response", newx = X_new, s = opt_lambda)
  
  # Calculate RMSE and store it
  rmse_5 <- sqrt(mean((test_rmse$precio.house.m2_log - predictions)^2))
  rmse_ridge_model_2[r] <- rmse_5
}




data_list_2 <- list(
  rmse_full_model_2, 
  rmse_full_model_inter_2, 
  rmse_bic_model_2,
  rmse_bic_model_inter_2,
  rmse_ridge_model_2
)

names(data_list_2) <- c(
  expression(omega[1]), 
  expression(omega[2]), 
  expression(omega[3]), 
  expression(omega[4]), 
  expression(omega[5])
)

# Create the boxplot with parsed names
boxplot(data_list_2, 
        main = "Boxplot of RMSE values, logarithmic scale", 
        ylab = "RMSE", 
        col = viridis(5), 
        names = sapply(names(data_list), function(x) parse(text = x)))

summary(rmse_full_model_2)
summary(rmse_full_model_inter_2)
summary(rmse_bic_model_2)
summary(rmse_bic_model_inter_2)
summary(rmse_ridge_model_2)

## Diagnosis of the selected model ---------------------------------------------
# Normality
residuals <- rstandard(model_bic_inter_final)
plot(density((residuals)),ylab= "Density", xlab= "Residuals", main = "Density plot of the residuals")
lillie.test(residuals) #we don't consider the model to follow a normal distribution
qqnorm(y=residuals)
qqline(y=residuals)
mean(residuals)
#very low mean of the residuals


# Constant variance
plot(model_bic_inter_final$fitted,residuals, xlab= "Fitted values", ylab= "Residuals", main="Standarized residuals vs Fitted values")

# Residuals

# Normality
residuals <- residuals(model_bic_inter_final)
plot(density((residuals)),ylab= "Density", xlab= "Residuals", main = "Density plot of the residuals")
lillie.test(residuals)
qqnorm(y=residuals)
qqline(y=residuals)
mean(residuals)
#very low mean of the residuals


# Constant variance
plot(model_bic_inter_final$fitted,residuals, ylab= "residuals")

# Linearity
plot(data_train_1$latitud, residuals)
plot(data_train_1$ref.hip.zona, residuals, xlab= "ref.hip.zona", ylab= "residuals")
plot(data_train_1$dorm, residuals)
plot(data_train_1$banos, residuals)
plot(data_train_1$tipo.casa, residuals)
plot(data_train_1$ascensor, residuals)
plot(data_train_1$estado, residuals)
plot(data_train_1$antig, residuals)
plot(data_train_1$comercial, residuals)
plot(data_train_1$M.30, residuals)
plot(data_train_1$CO, residuals)
plot(data_train_1$Pobl.0_14_div_Poblac.Total, residuals)
plot(data_train_1$PoblJubilada_div_Poblac.Total, residuals)


for (variable in numeric_vars) {
  formula <- as.formula(paste("precio.house.m2_log ~ s(", variable, ", bs='ps')"))
  fit.gam1 <- gam(formula, data = data_train_1, method = "REML")
  plot(fit.gam1, xlab = variable, ylab = "precio.house.m2_log")
}



#Cook's distance
c_dist<-cooks.distance(model_bic_inter_final)
max(c_dist)  
which(c_dist>(4/model_bic_inter_final$df.residual))


# leverage
hii<-influence(model_bic_inter_final)$hat
which(hii>(2*sum(hii)/736))
max(hii)
plot(hii)
abline(h=(2*sum(hii)/736), col='red')


#outliers
res_stand <- rstandard(model_bic_inter_final)
max(res_stand)
min(res_stand)
which(abs(res_stand)>3)

res_stud <- rstudent(model_bic_inter_final)
max(res_stud)
min(res_stud)
which(abs(res_stud)>3)



## Test predictions ------------------------------------------------------------
data_test <- read_excel("data_test.xlsx")

data_test <- data_test %>%
  mutate(across(where(is.character), as.factor))

# Binary variables as factors
data_test <- data_test %>%
  mutate(across(c(comercial, casco.historico, M.30), as.factor))

data_test$cod_distrito <- as.factor(data_test$cod_distrito)
data_test$cod_barrio <- as.factor(data_test$cod_barrio)



data_test$dorm <- as.factor(data_test$dorm)
data_test$banos <- as.factor(data_test$banos)


data_test$dorm <- recode_factor(data_test$dorm, "0"="0-1", "1"="0-1", "4"= "4≤", "5"= "4≤",
                                "6"= "4≤", "7"= "4≤")

#data_test$banos <- recode_factor(data_test$banos, "3"= "3≤",  "4"= "3≤", "5"= "3≤", "6"= "3≤", "7"= "3≤")

data_test$banos <- recode_factor(data_test$banos, "2" = "2≤", "3"= "2≤",  "4"= "2≤", "5"= "2≤",
                                 "6"= "2≤", "7"= "2≤")

data_test$tipo.casa <- recode_factor(data_test$tipo.casa, "chalet"="grande", "duplex"="grande",
                                     "estudio"="piso", "Otros"="piso")
data_test$comercial <- recode_factor(data_test$comercial, "0"="no", "1"="si")
data_test$casco.historico <- recode_factor(data_test$casco.historico, "0"="no", "1"="si")
data_test$M.30 <- recode_factor(data_test$M.30, "0"="no", "1"="si")

data_test$estado <- recode_factor(data_test$estado, "excelente"="bien", "buen_estado"="bien",
                                  "reformado"="bien", "nuevo-semin,"="bien", "segunda_mano"= "bien",
                                  "reg,-mal"="mal", "a_reformar"="mal")



data_train_1 <- data_train %>%
  dplyr::select(-train_indices, -barrio, -cod_barrio, -distrito, -cod_distrito,
                -sup.const, -PM10, -Nox, -NO2, -Poca_limp, -SO2, -precio.house.m2,
                -inter.exter, -Inmigrantes.porc, -casco.historico)

data_train_2<-data_train_1

data_train_2$banos<-recode_factor(data_train_2$banos, "2"= "2≤","3≤"="2≤")

formula_2 <- as.formula(paste("precio.house.m2_log ~", 'latitud + sup.util + ref.hip.zona + 
  antig + O3 + Pobl.0_14_div_Poblac.Total + PoblJubilada_div_Poblac.Total + 
  banos + tipo.casa + ascensor + estado + comercial + M.30 + 
  sup.util:banos + PoblJubilada_div_Poblac.Total:tipo.casa + 
 O3:ascensor'))

model_bic_inter_total_3 <-lm(formula_2, data = data_train_2)

# Assuming the test dataframe is named 'data_test'
predictions <- predict(model_bic_inter_total_3, newdata = data_test)


predicted_values <- data_test
predicted_values$predicted.price.m2 <- exp(predictions)

final_pred <- predicted_values[,c(1,36)]

# Assuming the test dataframe is named 'data_test'
predictions <- predict(model_bic_inter_total_3, newdata = data_test)

write.xlsx(final_pred, "predicted_prices.xlsx")

## Mean predictions (log)
mean_pred <- predict(model_bic_inter_total_3, data_test, interval="confidence",level = 0.95)
mean_pred <- as.data.frame(mean_pred)
indices<-1:nrow(mean_pred)
mean_pred$indices<-indices

## Mean predictions
ggplot(mean_pred, aes(x = indices, y = exp(fit))) +
  geom_point(color = "blue", size = 2) +                     
  
  geom_errorbar(aes(ymin =  exp(lwr), ymax = exp(upr)),  mean_pred,
                width = 1, color = "red") +                
  labs(title = "Predictions for the mean", y = expression("Predicted Price/"*m^2)) +
  theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())


## Predictions for individuals (log) 
pred <- predict(model_bic_inter_total_3, data_test, interval="prediction",level = 0.95)
pred <- as.data.frame(pred)
pred$indices <- indices


#  Predictions for individuals
ggplot(pred, aes(x = indices, y = exp(fit))) +
  geom_point(color = "blue", size = 2) + 
  
  geom_errorbar(aes(ymin =  exp(lwr), ymax = exp(upr)),  pred,
                width = 1, color = "red") + 
  labs(title = "Predictions for the individuals", y = expression("Predicted Price/"*m^2)) +
  theme(plot.title = element_text(hjust = 0.5,size = 15, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
