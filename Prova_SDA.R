data_complete <- read.csv("C:/Users/Utente/Desktop/data_complete.csv", header=TRUE)

head(data_complete)
names(data_complete)
# PREPREOCCESSING FOR ENCODING CATEGORICAL DATA
# data_complete$Colonna = factor(data_complete$Colonna, levels = c('', '', ''),
#                               labels = c(1,2,3))

install.packages('caTools')
library(caTools)
split = sample.split(data_complete$co2_emission, SplitRatio = 0.8)
tr_s = subset(data_complete, split == TRUE)
t_s = subset(data_complete, split == FALSE)

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

# Guardo la correlazione per eliminare i predittori altamente correlati
my_data <- data_complete[,c(1,2,3,7,8,9,10,11,12,13,14,15,16)]
my_data.cor = cor(my_data, use="pairwise.complete.obs")
round(my_data.cor, 2)


fit1 <- lm(co2_emission ~ euro_standard + fuel_cost_6000_miles + fuel_type + engine_capacity + transmission_type
           + noise_level, data = tr_s)

my_data <- data_complete[,c(7,16,10,9,8,14)]

summary(fit1)

confint(model, level=.95)

resid<- model$residuals
hist(resid)
qqnorm(resid)
qqline(resid)
 
plot(model)