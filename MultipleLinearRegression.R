setwd("~/GitHub/SDA")
data_complete <- read.csv("dataset/data_complete.csv", header=TRUE)

head(data_complete)
names(data_complete)

# PREPREOCCESSING FOR ENCODING CATEGORICAL DATA
# data_complete$Colonna = factor(data_complete$Colonna, levels = c('', '', ''),
#                               labels = c(1,2,3))

install.packages('caTools')
library(caTools)
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
y_pred = predict(model, newdata = t_s)

# library(MASS)
# step.model <- stepAIC(model, direction = "both", trace = FALSE)
# summary(step.model)
# y_pred = predict(model, newdata = t_s)


# ==============================================================
# COLLINEARITA' e CORRELAZIONE per eliminare qualche regressore: 
# ==============================================================
library(corrplot)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

car::vif(model)

# DOPO LE RIFLESSIONI: 
model_reduced_collinearity <- lm(co2_emission ~ year + euro_standard + transmission_type + engine_capacity +
             fuel_type + extra_urban_metric + noise_level, data = tr_s)
summary(model_reduced_collinearity)

y_pred = predict(model_reduced_collinearity, newdata = t_s)

tr_s.cor = cor(tr_s, use="pairwise.complete.obs")
round(tr_s.cor, 2);

library(corrplot)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# ==============================================================
# STUDIO LINEARITA' attraverso i residui 
# ==============================================================
dev.new()
plot(model_reduced_collinearity)

# 1) I residui devono avere distribuzione gaussiana: 
resid <- model_reduced_collinearity$residuals
hist(resid)
# è più o meno a forma di campana. 
# Altrimenti si guarda il QQ-plot: se i residui seguono una linea retta allora essi sono normalmente distribuiti. 

# ==============================================================
# OMOSCHEDASTICITà: varianza costante nei dati
# ==============================================================

# si osserva attraverso il tracciamento di una linea orizzontale
plot(model_reduced_collinearity)
# Analisi: ipotesi di omoschedasticità è soddisfatta se dal grafico non si ha evidenza di diversi livelli di varianza e i punti sembrano essere
# distributi in modo causale, in contrasto con la visualizzazione di un certo pattern. 


# ==============================================================
# RESIDUAL VS FITTED
# ==============================================================

# Osservando il grafico dei residui e dei valori fittati, si osserva un certo pattern lineare. Quindi, si può concludere che il modello non è
# lineare e tale situazione è risolvibile o eliminando alcuni predittori o aggiungendo trasformazioni al modello. 

# ==============================================================
# QQ-Plot
# ==============================================================

# I quantili del modello seguono una distribuzione normale poichè non vi è molta deviazione dal normal QQ-plot. Sulla base del grafico Residual vs Leverage
# si può vedere che alcuni punti dati sono raggruppati insieme, mentre si preferisce una loro distribuzione uniforme. 

confint(model_reduced_collinearity, level=.95)

resid<- model$residuals
hist(resid)
qqnorm(resid)
qqline(resid)
plot(model)










