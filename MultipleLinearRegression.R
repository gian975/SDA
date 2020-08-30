setwd("~/GitHub/SDA")

# ==============================================================
# INSTALLATION: 
# ==============================================================
install.packages("vctrs")
install.packages("tidyverse")
install.packages("corrplot")
install.packages('caTools')
install.packages('caret', dependencies = TRUE)


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
tr_s = subset(my_data, split == TRUE)
t_s = subset(my_data, split == FALSE)



plot(co2_emission, year)
#abline(lm(co2_emission~year), col="red") 
#lines(lowess(co2_emission,year), col="blue") 

plot(co2_emission, euro_standard)
#abline(lm(co2_emission~euro_standard), col="red") 
#lines(lowess(co2_emission,euro_standard), col="blue") 

plot(co2_emission, transmission_type)
#abline(lm(co2_emission~transmission_type), col="red") 
#lines(lowess(co2_emission,transmission_type), col="blue") 

plot(co2_emission, engine_capacity)
#abline(lm(co2_emission~engine_capacity), col="red") 
#lines(lowess(co2_emission,engine_capacity), col="blue") 

plot(co2_emission, fuel_type)
#abline(lm(co2_emission~fuel_type), col="red") 
#lines(lowess(co2_emission,fuel_type), col="blue") 

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
hist(resid)
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

yfit<-fitted(model)
plot(yfit, abs(resid), ylab="Residui", xlab="Fitted", main="Residui in valore assoluto vs fitted")


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

up <-  Q[2]+1*iqr # Upper Range  
low<- Q[1]-1*iqr # Lower Range

# ==============================================================
#ELIMINAZIONE OUTLIER 
# ==============================================================

tr_s_outliers<- subset(tr_s, tr_s$engine_capacity> low & tr_s$engine_capacity< up)

boxplot(tr_s_outliers)$co2_emission

Q <- quantile(tr_s_outliers$fuel_cost_6000_miles, probs=c(.25, .75), na.rm = FALSE)

##differenza del 75 esimo e del 25esimo percentile 
iqr <- IQR(tr_s_outliers$fuel_cost_6000_miles)
up <-  Q[2]+iqr # Upper Range  
low<- Q[1]-iqr # Lower Range

tr_s_outliers<- subset(tr_s_outliers, tr_s_outliers$fuel_cost_6000_miles> low & tr_s_outliers$fuel_cost_6000_miles< up)
boxplot(tr_s_outliers)$co2_emission

Q <- quantile(tr_s_outliers$noise_level, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(tr_s_outliers$noise_level)

up <-  Q[2]+iqr # Upper Range  
low <- Q[1]-iqr # Lower Range

tr_s_outliers<- subset(tr_s_outliers, tr_s_outliers$noise_level> low & tr_s_outliers$noise_level< up)
boxplot(tr_s_outliers)$co2_emission

model_without_outliers <- lm(co2_emission ~ ., data = tr_s_outliers)
summary(model_without_outliers)


# 5) High Leverage Point -> DA VEDERE MEGLIO
# ==============================================================
# 
# ==============================================================
# Gli High Leverage Point si osservano dall'ultimo grafico di plot(model), ovvero, dal grafico Residual vs Leverage si può vedere che alcuni punti dati sono raggruppati 
# insieme, mentre si preferisce una loro distribuzione uniforme. 


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
         tl.col = "black", tl.srt = 45)

car::vif(model_without_outliers)

# DOPO LE RIFLESSIONI: sono stati eliminati i regressori che presentano un VIF oltre i 10 e che sono in correlazione con altri regressori con VIF minore di 10. 
model_reduced_collinearity <- lm(co2_emission ~ year + euro_standard + transmission_type + engine_capacity +
             fuel_type + fuel_cost_6000_miles  + noise_level, data = tr_s_outliers)
summary(model_reduced_collinearity)
confint(model_reduced_collinearity, level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# ==============================================================
# STEP-WISE SELECTION 
# ==============================================================

step.model <- stepAIC(model_reduced_collinearity, direction = "both", scope = formula(model_reduced_collinearity), trace = FALSE)
summary(step.model)
step.model$anova
confint(step.model, level=.95)

# ==============================================================
# k-FOLD CROSS VALIDATION 
# ==============================================================

train.control <- trainControl(method = "cv", number = 10)
model_validation <- train(co2_emission ~ year + euro_standard + transmission_type + engine_capacity +
                            fuel_type + fuel_cost_6000_miles + noise_level, data = tr_s_outliers, method = "lm",
               trControl = train.control)
summary(model_validation)
confint(model_validation, level=.95)

# ==============================================================
# TEST PREDICTION
# ==============================================================

y_pred_step_model = predict(step.model, newdata = t_s, interval = 'confidence')
plot(y_pred_step_model)


y_pred_validation = predict(model_validation, newdata = t_s, interval = 'confidence')
plot(y_pred_validation)


# ==============================================================
# BOOTSTRAP
# ==============================================================


boot.fn=function(data,index){
  return(coef(lm(co2_emission~year + euro_standard + transmission_type + engine_capacity +
                   fuel_type + fuel_cost_6000_miles + noise_level, data = data,subset=index)))
}
n = nrow(tr_s_outliers)

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


