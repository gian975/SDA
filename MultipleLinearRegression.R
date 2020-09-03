setwd("~/GitHub/SDA")

# ==============================================================
# INSTALLATION: 
# ==============================================================
install.packages("vctrs")
install.packages("tidyverse")
install.packages("corrplot")
install.packages('caTools')
install.packages('caret', dependencies = TRUE)
install.packages("leaps")
install.packages("glmnet")
install.packages("pls")


# ==============================================================
# IMPORT: 
# ==============================================================
library(corrplot)
library(caTools)
library(tidyverse)
library(caret)
library(ISLR)
library(MASS)
library(boot)
library(stringr)
library(car)
library(leaps)

data_complete <- read.csv("dataset/data_complete.csv", header=TRUE)

head(data_complete)
names(data_complete)

# PREPREOCCESSING FOR ENCODING CATEGORICAL DATA
# data_complete$Colonna = factor(data_complete$Colonna, levels = c('', '', ''),
#                               labels = c(1,2,3))


# library(MASS)
# step.model <- stepAIC(model, direction = "both", trace = FALSE)
# summary(step.model)
# y_pred = predict(model, newdata = t_s)


attach(data_complete)
data_complete_1 <- data_complete[,c(3,7,8,9,10,11,12,13,14,15,16)]

plot(co2_emission, year)
abline(lm(co2_emission~year), col="red") 
lines(lowess(co2_emission,year), col="blue") 

plot(co2_emission, euro_standard)
abline(lm(co2_emission~euro_standard), col="red") 
lines(lowess(co2_emission,euro_standard), col="blue") 

plot(co2_emission, transmission_type)
abline(lm(co2_emission~transmission_type), col="red") 
lines(lowess(co2_emission,transmission_type), col="blue") 

plot(co2_emission, engine_capacity)
abline(lm(co2_emission~engine_capacity), col="red") 
lines(lowess(co2_emission,engine_capacity), col="blue") 

plot(co2_emission, fuel_type)
abline(lm(co2_emission~fuel_type), col="red") 
lines(lowess(co2_emission,fuel_type), col="blue") 

plot(co2_emission, urban_metric)
abline(lm(co2_emission~urban_metric), col="red") 
lines(lowess(co2_emission,urban_metric), col="blue") 

plot(co2_emission, extra_urban_metric)
abline(lm(co2_emission~extra_urban_metric), col="red") 
lines(lowess(co2_emission,extra_urban_metric), col="blue") 

plot(co2_emission, combined_metric)
abline(lm(co2_emission~combined_metric), col="red") 
lines(lowess(co2_emission,combined_metric), col="blue") 

plot(co2_emission, noise_level)
abline(lm(co2_emission~noise_level), col="red") 
lines(lowess(co2_emission,noise_level), col="blue") 

plot(co2_emission, fuel_cost_6000_miles)
abline(lm(co2_emission~fuel_cost_6000_miles), col="red") 
lines(lowess(co2_emission,fuel_cost_6000_miles), col="blue") 

set.seed(1)
model <- lm(co2_emission ~ . -engine_capacity, data = data_complete_1)
summary(model)
confint(model, level=.95)

set.seed(5)
step.model <- stepAIC(model, direction = "both", scope = formula(model), trace = FALSE)
step.model$anova
confint(step.model, level=.95)
summary(step.model)


# Dall'analisi degli intervalli di confidenza si intuisce: 
# 1) Tutti i regressori sono statisticamente significativi in quanto il loro intervallo di confidenza non comprende il valore 0
# 2) Sono da considerare maggiornmente signficativi i regressori con intervallo di confidenza stretto e lontano dallo zero

# ==============================================================
# MODEL ASSUMPTION
# ==============================================================
# I residui devono avere distribuzione gaussiana: 
resid <- model$residuals
hist(resid, breaks= 100, xlim =c(-100, 100))
# Altrimenti si guarda il QQ-plot: se i residui seguono una linea retta allora essi sono normalmente distribuiti. 


# Analisi dei potenziali problemi del fit del modello di regressione lineare sul dataset: 

# 1) Potrebbe esserci linearità tra x e y? 
# ==============================================================
# STUDIO LINEARITA' attraverso i residui 
# ==============================================================
dev.new()
plot(model)

# Nel summary guardare il t value per il test di ipotesi.
# F_test: determina se esiste una relazione tra i regressori e l'uscita. In particolare, il p-value è molto piccoll
# quindi si può concludere che esiste una relazione tra almeno un regressore e l'uscita. 

# ==============================================================
# RESIDUAL VS FITTED : Osservando il grafico dei residui e dei valori fittati, si osserva un certo pattern lineare. Quindi, si può concludere che il modello è
# lineare. 
# ==============================================================


# 2) Correlazione tra i termini di errore? 
# ==============================================================
# CORRELAZIONE TRA I TERMINI DI ERRORE
# ==============================================================
# Si assume per ipotesi che i dati abbiano errore non correlato, in quanto la raccolta dei dati non è stata effettuata, ma si è utilizzato un dataset preesistente, di cui
# si ignorano le modalità di raccolta. 


# 3) Varianza non costante del termine di errore: 
# ==============================================================
# OMOSCHEDASTICITà: varianza costante nei dati
# ==============================================================

# Analisi: ipotesi di omoschedasticità è soddisfatta se dal grafico non si ha evidenza di diversi livelli di varianza e i punti sembrano essere
# distributi in modo causale, in contrasto con la visualizzazione di un certo pattern. 
set.seed(2)
yfit<-fitted(model)
plot(yfit, resid, ylab="Residui", xlab="Fitted", main="Residui vs fitted")


# ==============================================================
# QQ-Plot: I quantili del modello seguono una distribuzione normale poichè non vi è molta deviazione dal normal QQ-plot. 
# ==============================================================


# 4) Outliers: 
# ==============================================================
# OUTLIERS: Analisi ed Eliminazione 
# ==============================================================
# L'analisi si effettua mediante l'utilizzo dei boxplot, i punti al di fuori del box sono considerati outliers (sotto determinate condizioni -> CODICE LUCA)
plot.new()
boxplot(data_complete)$co2_emission

data_complete_2 <- data_complete_1[, c(1,2,3,5,6,7,8,9,10,11)]
#set.seed(2)
LargeResiduals <- abs(rstudent(model)) > 3
tr_s_outliers <- data_complete_2[!LargeResiduals,]
set.seed(4)

model_outliers<-lm(co2_emission ~ ., data = tr_s_outliers)
summary(model_outliers)

boxplot(tr_s_outliers)$co2_emission 



# 5) High Leverage Point -> DA VEDERE MEGLIO
# ==============================================================
# 
# ==============================================================
# Gli High Leverage Point si osservano dall'ultimo grafico di plot(model), ovvero, dal grafico Residual vs Leverage si può vedere che alcuni punti dati sono raggruppati 
# insieme, mentre si preferisce una loro distribuzione uniforme. 

# Plot dei 5 punti a maggior influenza
plot(model_without_outliers, 4, id.n = 5)

# HighLeverage <- cooks.distance(model_without_outliers) > (11/(nrow(tr_s_outliers)))

boxplot(tr_s_high_leverage)$co2_emission 
# ==============================================================
# Confronto con e senza high leverage point: 
# ==============================================================

summary(model_outliers)
summary(model_high_leverage)

# si osserva un RSE di due punti più basso, quindi si procede con il modello senza punti high leverage

# 6) Collinearità dei dati
# ==============================================================
# COLLINEARITA' e CORRELAZIONE per eliminare qualche regressore: 
# ==============================================================

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")

car::vif(model_outliers)

# DOPO LE RIFLESSIONI: sono stati eliminati i regressori che presentano un VIF oltre i 10 e che sono in correlazione con altri regressori con VIF minore di 10 (year). 
# SI sono analizzati 3 possibili scenari: Fuel_cost6000, Combine Metric ed Extra Urban Metric: 


set.seed(1)
model_year <- lm(co2_emission ~ . - year, data = tr_s_outliers)
summary(model_year)
confint(model_year, level=.95)
car::vif(model_year)

# ==============================================================
# COMBINE METRIC: 
# ==============================================================
set.seed(2)
model_reduced_collinearity_CM <- lm(co2_emission ~ euro_standard + transmission_type +
             fuel_type + combined_metric + noise_level, data = tr_s_outliers)
summary(model_reduced_collinearity_CM)
confint(model_reduced_collinearity_CM, level=.95)
car::vif(model_reduced_collinearity_CM)

MSE_training_CM = mean((model_reduced_collinearity_CM$residuals)^2)

# ==============================================================
# FUEL COST 6000: 
# ==============================================================
model_reduced_collinearity_FC6000 <- lm(co2_emission ~ euro_standard + transmission_type +
                                      fuel_type + fuel_cost_6000_miles  + noise_level, data = tr_s_outliers)

model_reduced_collinearity_FC6000 <- lm(co2_emission ~  fuel_type + combined_metric, data = tr_s_outliers)
summary(model_reduced_collinearity_FC6000 )
confint(model_reduced_collinearity_FC6000 , level=.95)
car::vif(model_reduced_collinearity_FC6000 )


anova(model_reduced_collinearity_CM, model_reduced_collinearity_FC6000)

# ==============================================================
# URBAN METRIC: L'analisi si ferma qua perchè fa schifo
# ==============================================================
model_reduced_collinearity_EUM <- lm(co2_emission ~ euro_standard + transmission_type  +
                               noise_level + urban_metric + extra_urban_metric, data = tr_s_outliers)
summary(model_reduced_collinearity_EUM)
confint(model_reduced_collinearity_EUM, level=.95)
car::vif(model_reduced_collinearity_EUM )

# ==============================================================
# STEP-WISE SELECTION 
# ==============================================================
set.seed(5)
model_reduced_collinearity_CM <- lm(co2_emission ~ euro_standard + transmission_type +
                                      fuel_type + combined_metric + noise_level, data = tr_s_outliers)
step.model_CM <- stepAIC(model_reduced_collinearity_CM, direction = "both", scope = formula(model_reduced_collinearity_CM), trace = FALSE)
step.model_CM$anova
confint(step.model_CM, level=.95)
summary(step.model_CM)
MSE_training_CM_Stepwise = mean((step.model_CM$residuals)^2)


set.seed(5)
step.model_FC6000 <- stepAIC(model_reduced_collinearity_FC6000, direction = "both", scope = formula(model_reduced_collinearity_FC6000), trace = FALSE)
step.model_FC6000$anova
confint(step.model, level=.95)
summary(step.model_FC6000)

anova(model_reduced_collinearity_FC6000, step.model_FC6000)
anova(model_reduced_collinearity_CM, step.model_CM)

# ==============================================================
# BEST MODEL SELECTION CM:  
# ==============================================================

x <-regsubsets(co2_emission~euro_standard + transmission_type +
                 fuel_type + combined_metric  + noise_level, data=tr_s_outliers, nvmax = 6, method = "forward")
summary(x)

set.seed(100)
model_reduced_collinearity_CM <- lm(co2_emission ~ euro_standard + transmission_type +
                                      fuel_type + combined_metric + noise_level, data = tr_s_outliers)
summary(model_reduced_collinearity_CM)
confint(model_reduced_collinearity_CM, level=.95)

car::vif(model_reduced_collinearity_CM)

model_reduced_collinearity_CM_2 <- lm(co2_emission ~ fuel_type + combined_metric, data = tr_s_outliers)
summary(model_reduced_collinearity_CM_2)
confint(model_reduced_collinearity_CM_2, level=.95)
MSE_training_CM_2 = mean((model_reduced_collinearity_CM_2$residuals)^2)

# ==============================================================
# BEST MODEL SELECTION FC6000:  
# ==============================================================
set.seed(100)
x <-regsubsets(co2_emission~yco2_emission ~ euro_standard + transmission_type +
                 fuel_type + fuel_cost_6000_miles + noise_level, data=tr_s_outliers, nvmax = 7, method = "seqrep")
summary(x)


model_reduced_collinearity_FC6000 <- lm(co2_emission ~ euro_standard + transmission_type +
                                          fuel_type + fuel_cost_6000_miles + noise_level, data = tr_s_high_leverage)
summary(model_reduced_collinearity_FC6000 )
confint(model_reduced_collinearity_FC6000 , level=.95)
car::vif(model_reduced_collinearity_FC6000 )


# Engine Capacity ha un intervallo di confidenza stretto quindi lo si può eliminare
set.seed(100)
model_reduced_collinearity_FC6000 <- lm(co2_emission ~ euro_standard + transmission_type +
                                          fuel_type + fuel_cost_6000_miles, data = tr_s_high_leverage)
summary(model_reduced_collinearity_FC6000 )
confint(model_reduced_collinearity_FC6000 , level=.95)
car::vif(model_reduced_collinearity_FC6000 )


anova(x, model_reduced_collinearity_FC6000)

# ==============================================================
# K-Fold Cross Validation
# ==============================================================

library(boot)
model_validation_CM_5 <- lm(co2_emission ~ euro_standard + transmission_type +
                              fuel_type + combined_metric + noise_level, data = tr_s_outliers)

glm.fit=glm(model_validation_CM_5 ,data=tr_s_outliers)

cv.err=cv.glm(tr_s_outliers,glm.fit, K = 10)
cv.err$delta # The K-Fold Cross validation estimate for the test error is approximately 1.102361 (seed=1).

# K-Fold Cross validation for polynomial regressions with orders i=1,2,...,4.

cv.error=rep(0,1)
for (i in 1:1){
  glm.fit=glm(co2_emission~euro_standard + transmission_type +
                fuel_type + combined_metric + noise_level, data = tr_s_outliers)
  cv.error[i]=cv.glm(tr_s_outliers,glm.fit, K=10)$delta[1]
}
cv.error
# We still see little evidence that using cubic or higher-order polynomial terms leads to lower test error than simply



library(boot)
model_validation_CM_2 <- lm(co2_emission ~ combined_metric + fuel_type, data = tr_s_outliers)
glm.fit=glm(model_validation_CM_2 ,data=tr_s_outliers)

cv.err=cv.glm(tr_s_outliers,glm.fit, K = 10)
cv.err$delta # The K-Fold Cross validation estimate for the test error is approximately 1.102361 (seed=1).

# K-Fold Cross validation for polynomial regressions with orders i=1,2,...,4.

cv.error=rep(0,1)
for (i in 1:1){
  glm.fit=glm(co2_emission ~ combined_metric + fuel_type, data = tr_s_outliers)
  cv.error[i]=cv.glm(tr_s_outliers,glm.fit, K=10)$delta[1]
}
cv.error
# We still see little evidence that using cubic or higher-order polynomial terms leads to lower test error than simply

# ==============================================================
# TEST PREDICTION
# ==============================================================
set.seed(8)
y_pred = predict(model_validation_CM_5$finalModel, newdata = t_s, interval = 'predict')
y_pred

set.seed(7)
y_pred_step_model = predict(model_reduced_collinearity_CM_min, newdata = t_s, interval = 'predict')
y_pred
mean((t_s$co2_emission - y_pred)^2)




