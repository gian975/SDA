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
my_data <- data_complete[,c(3,7,8,9,10,11,12,13,14,15,16)]
split = sample.split(my_data$co2_emission, SplitRatio = 0.8)
set.seed(10)
tr_s = subset(my_data, split == TRUE)
t_s = subset(my_data, split == FALSE)



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
model <- lm(co2_emission ~ ., data = tr_s)
summary(model)
confint(model, level=.95)
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
boxplot(tr_s)$co2_emission 

##metodo IQR per trovare gli outlier:

## restituzione del 25 esimo e 75 esimo percentile del set di dati 
Q <- quantile(tr_s$engine_capacity, probs=c(.25, .75), na.rm = FALSE)

##differenza del 75 esimo e del 25esimo percentile 
iqr <- IQR(tr_s$engine_capacity)

## ci calcoliamo gli intervalli oltre i quali tutti i punti sono outlier

up <-  Q[2]+1.75*iqr # Upper Range  
low<- Q[1]-1.75*iqr # Lower Range

# ==============================================================
#ELIMINAZIONE OUTLIER 
# ==============================================================

set.seed(15)
tr_s_outliers<- subset(tr_s, tr_s$engine_capacity> low & tr_s$engine_capacity< up)

boxplot(tr_s_outliers)$co2_emission

Q <- quantile(tr_s_outliers$fuel_cost_6000_miles, probs=c(.25, .75), na.rm = FALSE)

##differenza del 75 esimo e del 25esimo percentile 
iqr <- IQR(tr_s_outliers$fuel_cost_6000_miles)
up <-  Q[2]+1.75*iqr # Upper Range  
low<- Q[1]-1.75*iqr # Lower Range

tr_s_outliers<- subset(tr_s_outliers, tr_s_outliers$fuel_cost_6000_miles> low & tr_s_outliers$fuel_cost_6000_miles< up)
boxplot(tr_s_outliers)$co2_emission

Q <- quantile(tr_s_outliers$noise_level, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(tr_s_outliers$noise_level)

up <-  Q[2]+1.75*iqr # Upper Range  
low <- Q[1]-1.75*iqr # Lower Range

tr_s_outliers<- subset(tr_s_outliers, tr_s_outliers$noise_level> low & tr_s_outliers$noise_level< up)
boxplot(tr_s_outliers)$co2_emission

set.seed(3)
model_without_outliers <- lm(co2_emission ~ ., data = tr_s_outliers)
summary(model_without_outliers)


# ==============================================================
# Confronto con e senza outliers: 
# ==============================================================

summary(model)
summary(model_without_outliers)

# si osserva un RSE di due punti più basso, quindi si procede con il modello senza outliers

# 5) High Leverage Point -> DA VEDERE MEGLIO
# ==============================================================
# 
# ==============================================================
# Gli High Leverage Point si osservano dall'ultimo grafico di plot(model), ovvero, dal grafico Residual vs Leverage si può vedere che alcuni punti dati sono raggruppati 
# insieme, mentre si preferisce una loro distribuzione uniforme. 

# Plot dei 5 punti a maggior influenza
plot(model_without_outliers, 4, id.n = 5)

HighLeverage <- cooks.distance(model_without_outliers) > (11/(nrow(tr_s_outliers)))
LargeResiduals <- rstudent(model_without_outliers) > 3
tr_s_high_leverage <- tr_s_outliers[!HighLeverage & !LargeResiduals,]
set.seed(4)
model_high_leverage<-lm(co2_emission ~ ., data = tr_s_high_leverage)

# ==============================================================
# Confronto con e senza high leverage point: 
# ==============================================================

summary(model_without_outliers)
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

car::vif(model_high_leverage)

# DOPO LE RIFLESSIONI: sono stati eliminati i regressori che presentano un VIF oltre i 10 e che sono in correlazione con altri regressori con VIF minore di 10 (year). 
# SI sono analizzati 3 possibili scenari: Fuel_cost6000, Combine Metric ed Extra Urban Metric: 

# ==============================================================
# COMBINE METRIC: 
# ==============================================================
set.seed(2)
model_reduced_collinearity_CM <- lm(co2_emission ~ euro_standard + transmission_type + engine_capacity +
             fuel_type + combined_metric  + noise_level, data = tr_s_high_leverage)
summary(model_reduced_collinearity_CM)
confint(model_reduced_collinearity_CM, level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_CM)

# Engine Capacity osservando gli intervalli di confidenza viene eliminato per semplificare il modello: 
set.seed(2)
model_reduced_collinearity_CM <- lm(co2_emission ~ euro_standard + transmission_type +
                                      fuel_type + combined_metric  + noise_level, data = tr_s_high_leverage)
summary(model_reduced_collinearity_CM)
confint(model_reduced_collinearity_CM, level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_CM)


# ==============================================================
# FUEL COST 6000: 
# ==============================================================
model_reduced_collinearity_FC6000 <- lm(co2_emission ~ euro_standard + transmission_type + engine_capacity +
                                      fuel_type + fuel_cost_6000_miles   + noise_level, data = tr_s_high_leverage)
summary(model_reduced_collinearity_FC6000 )
confint(model_reduced_collinearity_FC6000 , level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_FC6000 )


# Engine Capacity viene eliminato per il suo intervallo di confidenza: 
model_reduced_collinearity_FC6000 <- lm(co2_emission ~ euro_standard + transmission_type +
                                          fuel_type + fuel_cost_6000_miles   + noise_level, data = tr_s_high_leverage)
summary(model_reduced_collinearity_FC6000 )
confint(model_reduced_collinearity_FC6000 , level=.95)
summary(model_reduced_collinearity_CM)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_FC6000 )


# ==============================================================
# EXTRA URBAN METRIC: L'analisi si ferma qua perchè fa schifo
# ==============================================================
model_reduced_collinearity_EUM <- lm(co2_emission ~ euro_standard + transmission_type + engine_capacity +
                                          fuel_type + extra_urban_metric + noise_level, data = tr_s_high_leverage)
summary(model_reduced_collinearity_EUM )
confint(model_reduced_collinearity_EUM , level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_EUM )


# Engine capacity viene eliminato: 
model_reduced_collinearity_EUM <- lm(co2_emission ~ euro_standard + transmission_type +
                                       fuel_type + extra_urban_metric + noise_level, data = tr_s_high_leverage)
summary(model_reduced_collinearity_EUM )
confint(model_reduced_collinearity_EUM , level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_EUM)


# ==============================================================
# STEP-WISE SELECTION 
# ==============================================================
set.seed(5)
step.model_CM <- stepAIC(model_reduced_collinearity_CM, direction = "both", scope = formula(model_reduced_collinearity_CM), trace = FALSE)
step.model_CM$anova
confint(step.model, level=.95)
summary(step.model_CM)

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

x <-regsubsets(co2_emission~euro_standard + transmission_type + engine_capacity +
                 fuel_type + combined_metric  + noise_level, data=tr_s_high_leverage, nvmax = 6, method = "seqrep")
summary(x)

set.seed(100)
model_reduced_collinearity_CM <- lm(co2_emission ~ euro_standard + transmission_type +
                                      fuel_type + combined_metric + engine_capacity, data = tr_s_high_leverage)
summary(model_reduced_collinearity_CM)
confint(model_reduced_collinearity_CM, level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_CM)

# Si è proceduto ad eliminare noise_level visto dal bestSubset Selection per poi procedere all'eliminazione
# di engine_capacity visto dall'intervallo di confidenza e year visto sia dall'intervallo di confidenza che dal p-value

set.seed(100)
model_reduced_collinearity_CM_1 <- lm(co2_emission ~ euro_standard + transmission_type +
                                      fuel_type + combined_metric, data = tr_s_high_leverage)
summary(model_reduced_collinearity_CM_1)
confint(model_reduced_collinearity_CM_1, level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_CM)

anova(model_reduced_collinearity_CM_1, model_reduced_collinearity_CM)
# ==============================================================
# BEST MODEL SELECTION FC6000:  
# ==============================================================
set.seed(100)
x <-regsubsets(co2_emission~yco2_emission ~ euro_standard + transmission_type +
                 fuel_type + fuel_cost_6000_miles   + noise_level + engine_capacity, data=tr_s_high_leverage, nvmax = 7, method = "seqrep")
summary(x)


model_reduced_collinearity_FC6000 <- lm(co2_emission ~ euro_standard + transmission_type +
                                          fuel_type + fuel_cost_6000_miles + engine_capacity, data = tr_s_high_leverage)
summary(model_reduced_collinearity_FC6000 )
confint(model_reduced_collinearity_FC6000 , level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_FC6000 )


# Engine Capacity ha un intervallo di confidenza stretto quindi lo si può eliminare
set.seed(100)
model_reduced_collinearity_FC6000 <- lm(co2_emission ~ euro_standard + transmission_type +
                                          fuel_type + fuel_cost_6000_miles, data = tr_s_high_leverage)
summary(model_reduced_collinearity_FC6000 )
confint(model_reduced_collinearity_FC6000 , level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_reduced_collinearity_FC6000 )


anova(x, model_reduced_collinearity_FC6000)

# ==============================================================
# k-FOLD CROSS VALIDATION 
# ==============================================================

train.control <- trainControl(method = "cv", number = 10)
set.seed(6)
model_validation <- train(co2_emission ~ euro_standard + transmission_type +
                            fuel_type + combined_metric, data = tr_s_high_leverage, method = "lm",
               trControl = train.control)
summary(model_validation)
confint(model_validation, level=.95)
mean((model_reduced_collinearity_CM_1$residuals)^2)

# ==============================================================
# TEST PREDICTION
# ==============================================================
set.seed(7)
y_pred_step_model = predict(step.model, newdata = t_s, interval = 'confidence')
plot(y_pred_step_model)

set.seed(8)
y_pred_validation = predict(model_reduced_collinearity_CM, newdata = t_s, interval = 'confidence')
plot(y_pred_validation)


# ==============================================================
# BOOTSTRAP
# ==============================================================


boot.fn=function(data,index){
  return(coef(lm(co2_emission~year + euro_standard + transmission_type + engine_capacity +
                   fuel_type + fuel_cost_6000_miles + noise_level, data = data,subset=index)))
}
n = nrow(tr_s_outliers)
qrt(sum((model$residuals)^2)/21)
boot.fn(tr_s_outliers, 1:n)

# Boot estimate is not deterministic
boot.fn(tr_s_outliers,sample(1:n, 79576,replace=T))
boot.fn(tr_s_outliers,sample(1:n, 79576,replace=T))
# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.
b = boot(tr_s_outliers ,boot.fn ,1000)

s = summary(lm(model_validation, data = tr_s_outliers))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the linear model
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between no-Transformation Std.errors:\n",c - se,"\n")

mean((model_reduced_collinearity_CM$residuals)^2)
