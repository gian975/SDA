# Osservando la correlazione, si nota che il predittore engine_capacity seppur presentando una correlazione di 
# 0.80 sull'uscita co2_emission, ottiene un intervallo di confidenza stretto e prossimo allo zero. 
# Di conseguenza si trasforma la relazione sull'uscita in forma non lineare. 

set.seed(1)
plot(co2_emission, log(engine_capacity))
abline(lm(co2_emission~log(engine_capacity)), col="red") 
lines(lowess(co2_emission,log(engine_capacity)), col="blue") 

model_not_linear_log <- lm(co2_emission ~ euro_standard + transmission_type + log(engine_capacity) +
              fuel_type + combined_metric  + noise_level, data = tr_s)
summary(model_not_linear_log)
confint(model_not_linear_log, level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_not_linear)

plot(model_not_linear)


# ==============================================================
# STEP-WISE SELECTION 
# ==============================================================
set.seed(5)
step.model_not_linear <- stepAIC(model_not_linear, direction = "both", scope = formula(model_not_linear), trace = FALSE)
confint(step.model, level=.95)
summary(step.model_CM)

anova(model_not_linear, step.model_not_linear)


# ==============================================================
# BEST MODEL SELECTION CM: SERVE DAVVERO? Noise Level comunque presenta un intervallo di confidenza dell'ordine di 1/10
# ==============================================================
x <-regsubsets(co2_emission~euro_standard + transmission_type + engine_capacity +
                 fuel_type + combined_metric  + noise_level, data=tr_s_high_leverage, nvmax = 6, method = "seqrep")
summary(x)
summary(model_not_linear)
model_not_linear <- lm(co2_emission ~ euro_standard + transmission_type + log(engine_capacity) +
                         fuel_type + combined_metric, data = tr_s_high_leverage)
summary(model_not_linear)
confint(model_not_linear, level=.95)

# ==============================================================
# k-FOLD CROSS VALIDATION 
# ==============================================================

train.control <- trainControl(method = "cv", number = 10)
set.seed(6)
model_not_linear_validation <- train(co2_emission ~ euro_standard + transmission_type + log(engine_capacity) +
                            fuel_type + combined_metric + noise_level, data = tr_s, method = "lm",
                          trControl = train.control)
summary(model_not_linear_validation$finalModel)
confint(model_not_linear_validation$finalModel, level=.95)
mean((model_not_linear_validation$finalModel$residuals)^2)


# ==============================================================================================
set.seed(2)
model_not_linear_sqrt <- lm(co2_emission ~ euro_standard + transmission_type + sqrt(engine_capacity) +
                         fuel_type + combined_metric  + noise_level, data = tr_s)
summary(model_not_linear_sqrt)
confint(model_not_linear_sqrt, level=.95)

res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method ="number")
car::vif(model_not_linear_sqrt)

plot(model_not_linear_sqrt)


# ==============================================================
# STEP-WISE SELECTION 
# ==============================================================
set.seed(5)
step.model_not_linear_sqrt <- stepAIC(model_not_linear_sqrt, direction = "both", scope = formula(model_not_linear_sqrt), trace = FALSE)
confint(step.model_not_linear_sqrt, level=.95)
summary(step.model_not_linear_sqrt)

anova(model_not_linear_sqrt, step.model_not_linear_sqrt)



# ==============================================================================================

set.seed(1)
model_not_linear_poly_2 <- lm(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 2) +
                         fuel_type + combined_metric, data = tr_s)
summary(model_not_linear_poly_2)
confint(model_not_linear_poly_2, level=.95)

set.seed(1)
model_not_linear_poly_3 <- lm(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 3) +
                                fuel_type + combined_metric, data = tr_s)
summary(model_not_linear_poly_3)
confint(model_not_linear_poly_3, level=.95)

set.seed(1)
model_not_linear_poly_4 <- lm(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 4) +
                                fuel_type + combined_metric, data = tr_s)
summary(model_not_linear_poly_4)
confint(model_not_linear_poly_4, level=.95)


anova(model_not_linear_poly_2, model_not_linear_poly_3)
anova(model_not_linear_log, model_not_linear_sqrt)

