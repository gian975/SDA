setwd("~/GitHub/SDA")
# ==============================================================
# INSTALLATION: 
# ==============================================================
install.packages(c("caret", "tidyverse"))
install.packages(corrplot)
install.packages('caTools')

# ==============================================================
# IMPORT: 
# ==============================================================
library(corrplot)
library(caTools)
library(tidyverse)
library(caret)


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




my_data <- data_complete[,c(3,7,8,9,10,11,12,13,14,15,16)]
split = sample.split(my_data$co2_emission, SplitRatio = 0.8)
tr_s = subset(my_data, split == TRUE)
t_s = subset(my_data, split == FALSE)

attach(data_complete)

plot(co2_emission, year)
plot(co2_emission, make)
plot(co2_emission, model)
plot(co2_emission, description)
plot(co2_emission, euro_standard)
plot(co2_emission, transmission_type)
plot(co2_emission, engine_capacity)
plot(co2_emission, fuel_type)
plot(co2_emission, urban_metric)
plot(co2_emission, extra_urban_metric)
plot(co2_emission, combined_metric)
plot(co2_emission, noise_level)
plot(co2_emission, fuel_cost_6000_miles)


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
resid <- model_reduced_collinearity$residuals
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
residui<-model_reduced_collinearity$residuals
yfit<-fitted(model_reduced_collinearity)
plot(yfit, abs(residui), ylab="Residui", xlab="Fitted", main="Residui in valore assoluto vs fitted")


# ==============================================================
# QQ-Plot: I quantili del modello seguono una distribuzione normale poichè non vi è molta deviazione dal normal QQ-plot. 
# ==============================================================


# 4) Outliers: 
# ==============================================================
# COLLINEARITA' e CORRELAZIONE per eliminare qualche regressore: 
# ==============================================================
# L'analisi si effettua mediante l'utilizzo dei boxplot, i punti al di fuori del box sono considerati outliers (sotto determinate condizioni -> CODICE LUCA)


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

car::vif(model)

# DOPO LE RIFLESSIONI: sono stati eliminati i regressori che presentano un VIF oltre i 10 e che sono in correlazione con altri regressori con VIF minore di 10. 
model_reduced_collinearity <- lm(co2_emission ~ year + euro_standard + transmission_type + engine_capacity +
             fuel_type + extra_urban_metric + noise_level, data = tr_s)
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
# k-FOLD CROSS VALIDATION 
# ==============================================================

train.control <- trainControl(method = "cv", number = 10)
model_validation <- train(co2_emission ~ year + euro_standard + transmission_type + engine_capacity +
                            fuel_type + extra_urban_metric + noise_level, data = tr_s, method = "lm",
               trControl = train.control)



