setwd("~/GitHub/SDA")
data_complete <- read.csv("dataset/data_complete.csv", header=TRUE)

head(data_complete)
names(data_complete)

my_data <- data_complete[,c(3,7,8,9,10,11,12,13,14,15,16)]
split = sample.split(my_data$co2_emission, SplitRatio = 0.8)
tr_s = subset(my_data, split == TRUE)
t_s = subset(my_data, split == FALSE)

model <- lm(co2_emission ~ year + euro_standard + transmission_type +
              fuel_type + urban_metric + extra_urban_metric + log(combined_metric) + (noise_level)^2 + fuel_cost_6000_miles, data = tr_s)

model <- lm(co2_emission ~ year + transmission_type +
              fuel_type + exp(urban_metric) + (log(extra_urban_metric))^2 + (log(combined_metric))^2 + exp(noise_level) + log(fuel_cost_6000_miles), data = tr_s)



res <- cor(my_data, use="pairwise.complete.obs")
round(res, 2)
dev.new()
plot.new()
dev.off()
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, method = "number")

combined_model_1 <- lm(co2_emission ~ year*euro_standard + transmission_type +
                       fuel_type + urban_metric*fuel_cost_6000_miles*extra_urban_metric*combined_metric + noise_level, data = tr_s)

combined_model_2<- lm(co2_emission ~ (year+euro_standard) + transmission_type +
                        fuel_type + (urban_metric+fuel_cost_6000_miles+extra_urban_metric+combined_metric) + noise_level, data = tr_s)

combined_model_3<- lm(co2_emission ~ year + transmission_type +
                        fuel_type + fuel_cost_6000_miles*combined_metric + noise_level, data = tr_s)


plot(combined_model_1)
plot(combined_model_2)
plot(combined_model_3)


car::vif(combined_model_1)
car::vif(combined_model_2)
car::vif(combined_model_3)


model<- lm(co2_emission ~ ., data = my_data)
y <-regsubsets(co2_emission~ ., data=my_data, nvmax = 10, method = "forward")

summary(y)$rsq
x <- lm(co2_emission ~ combined_metric + fuel_cost_6000_miles + fuel_type, data = my_data)
summary(x)
confint(x)
car::vif(x)

combined_model_1 <- lm(co2_emission ~ combined_metric+fuel_cost_6000_miles + fuel_type, data = my_data)
summary(combined_model_1)
confint(combined_model_1)
car::vif(combined_model_1)

combined_model_1 <- lm(co2_emission ~ combined_metric + fuel_type, data = my_data)
plot(combined_model_1)