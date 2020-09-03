attach(data_complete_1)

model_not_linear_sqrt <- (co2_emission ~ euro_standard + transmission_type + sqrt(engine_capacity) +
                              fuel_type + combined_metric  + noise_level)

model_not_linear_log <- (co2_emission ~ euro_standard + transmission_type + log(engine_capacity) +
                             fuel_type + combined_metric  + noise_level)

model_not_linear_poly_2 <-(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 2) +
                                fuel_type + combined_metric)

model_not_linear_poly_3 <-(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 3) +
                             fuel_type + combined_metric)

model_not_linear_poly_4 <-(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 4) +
                             fuel_type + combined_metric)

n = nrow(data_complete_1)

######### Validation Set Approch #########
train=sample(1:n,n/2)
test=(-train)
set.seed(1)
lm.fit=lm(model_not_linear_sqrt, data = data_complete_1 , subset = train)

# the estimated test MSE for the linear regression fit is:
y_true=data_complete_1$co2_emission
y_predict=predict(lm.fit,data_complete_1)
mean(((y_true-y_predict)[test])^2)

#Poly 2 trasformation
set.seed(1)
lm.fit=lm(model_not_linear_poly_2, data = data_complete_1 , subset = train)
# the estimated test MSE for the linear regression fit is:
y_true=data_complete_1$co2_emission
y_predict=predict(lm.fit,data_complete_1)
mean(((y_true-y_predict)[test])^2)

#Poly 3 trasformation
set.seed(1)
lm.fit=lm(model_not_linear_poly_3, data = data_complete_1 , subset = train)
# the estimated test MSE for the linear regression fit is:
y_true=data_complete_1$co2_emission
y_predict=predict(lm.fit,data_complete_1)
mean(((y_true-y_predict)[test])^2)

#Poly 4 trasformation
set.seed(1)
lm.fit=lm(model_not_linear_poly_4, data = data_complete_1 , subset = train)
# the estimated test MSE for the linear regression fit is 33.51284 (seed=1)
y_true=data_complete_1$co2_emission
y_predict=predict(lm.fit,data_complete_1)
mean(((y_true-y_predict)[test])^2)

#Log trasformation
set.seed(1)
lm.fit=lm(model_not_linear_log, data = data_complete_1 , subset = train)
# the estimated test MSE for the linear regression fit is:
y_true=data_complete_1$co2_emission
y_predict=predict(lm.fit,data_complete_1)
mean(((y_true-y_predict)[test])^2)


########## K-Fold Cross Validation ##########
library(boot)

glm.fit=glm(fit.linear ,data=data_complete)

cv.err=cv.glm(tr_s_high_leverage,glm.fit, K = 10)
cv.err$delta # The K-Fold Cross validation estimate for the test error is approximately  48.12944 (seed=1).

# K-Fold Cross validation for polynomial regressions with orders i=1,2,...,4.

cv.error=rep(0,4)
for (i in 1:4){
  glm.fit=glm(fit.poly2, data = data_complete)
  cv.error[i]=cv.glm(tr_s_high_leverage,glm.fit, K=10)$delta[1]
}
cv.error


########## Bootstrap ##########

# The boot.fn() function can also be used in order to create bootstrap estimates 
# for the intercept and slope terms by randomly sampling from among the observations with replacement
# We will compare the estimates obtained using the bootstrap to those obtained using the previous models
library(stringr)

# Polinomials-2 no-linear transformation
set.seed (2)

indice=nrow(data_complete_1)

model_not_linear_poly_2 <- lm(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 2) +
                                fuel_type + combined_metric, data = data_complete_1)

boot.fn=function(data,index){
  return(coef(lm(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 2) +
                   fuel_type + combined_metric ,data = data,subset=index)))
}
boot.fn(data_complete_1, 1:indice)

boot.fn(data_complete_1,sample(1:indice, 45441,replace=T))

# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.

b = boot(data_complete_1 ,boot.fn ,1000)

s = summary(lm(model_not_linear_poly_2,data = data_complete_1))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-2 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-2 transformation Std.errors:\n",c - se,"\n")

# Polinomials-3 no-linear transformation
set.seed (2)

indice=nrow(data_complete_1)

model_not_linear_poly_3 <- lm(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity,3) +
                                fuel_type + combined_metric, data = data_complete_1)

boot.fn=function(data,index){
  return(coef(lm(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 3) +
                   fuel_type + combined_metric ,data = data,subset=index)))
}
boot.fn(data_complete_1, 1:indice)

boot.fn(data_complete_1,sample(1:indice, 45441,replace=T))

# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.

b = boot(data_complete_1 ,boot.fn ,1000)

s = summary(lm(model_not_linear_poly_3,data = data_complete_1))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-3 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-3 transformation Std.errors:\n",c - se,"\n")

# Polinomials-4 no-linear transformation
set.seed (2)

indice=nrow(data_complete_1)

model_not_linear_poly_4 <- lm(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity,4) +
                                fuel_type + combined_metric, data = data_complete_1)

boot.fn=function(data,index){
  return(coef(lm(co2_emission ~ euro_standard + transmission_type + poly(engine_capacity, 4) +
                   fuel_type + combined_metric ,data = data,subset=index)))
}
boot.fn(data_complete_1, 1:indice)

boot.fn(data_complete_1,sample(1:indice, 45441,replace=T))

# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.

b = boot(data_complete_1 ,boot.fn ,1000)

s = summary(lm(model_not_linear_poly_4,data = data_complete_1))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-4 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-4 transformation Std.errors:\n",c - se,"\n")


# Log trasformation no-linear transformation
set.seed (2)

indice=nrow(data_complete_1)

model_not_linear_log <- lm(co2_emission ~ euro_standard + transmission_type + log(engine_capacity) +
                             fuel_type + combined_metric  + noise_level, data = data_complete_1)

boot.fn=function(data,index){
  return(coef(lm(co2_emission ~ euro_standard + transmission_type + log(engine_capacity) +
                   fuel_type + combined_metric  + noise_level,data = data,subset=index)))
}
boot.fn(data_complete_1, 1:indice)

boot.fn(data_complete_1,sample(1:indice, 45441,replace=T))

# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.

b = boot(data_complete_1 ,boot.fn ,1000)

s = summary(lm(model_not_linear_log,data = data_complete_1))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the log transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between log transformation Std.errors:\n",c - se,"\n")


# Sqrt trasformation no-linear transformation
set.seed (2)

indice=nrow(data_complete_1)

model_not_linear_sqrt <- lm(co2_emission ~ euro_standard + transmission_type + sqrt(engine_capacity) +
                              fuel_type + combined_metric  + noise_level, data = data_complete_1)

boot.fn=function(data,index){
  return(coef(lm(co2_emission ~ euro_standard + transmission_type + sqrt(engine_capacity) +
                   fuel_type + combined_metric  + noise_level,data = data,subset=index)))
}
boot.fn(data_complete_1, 1:indice)

boot.fn(data_complete_1,sample(1:indice, 45441,replace=T))

# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.

b = boot(data_complete_1 ,boot.fn ,1000)

s = summary(lm(model_not_linear_sqrt,data = data_complete_1))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the log transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between sqrt transformation Std.errors:\n",c - se,"\n")

######## Plot ########

## Plot linear model
dev.new()
plot(as.factor(seat_comfort),overall,main='Linear Model', xlab="seat comfort", ylab="overall")
xx=seq(min(seat_comfort),max(seat_comfort),along.with = seat_comfort)
ci_lin <- predict(lm(overall~seat_comfort,data=merComplete),newdata=data.frame(seat_comfort=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-2 transformation
dev.new()
plot(as.factor(seat_comfort),overall,main='Polinomial-2 Model', xlab="seat comfort", ylab="overall")
xx=seq(min(seat_comfort),max(seat_comfort),along.with = seat_comfort)
ci_lin <- predict(lm(overall~I(seat_comfort^2),data=merComplete),newdata=data.frame(seat_comfort=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-3 transformation
dev.new()
plot(as.factor(seat_comfort),overall,main='Polinomial-3 Model', xlab="seat comfort", ylab="overall")
xx=seq(min(seat_comfort),max(seat_comfort),along.with = seat_comfort)
ci_lin <- predict(lm(overall~I(seat_comfort^3),data=merComplete),newdata=data.frame(seat_comfort=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)

## Plot linear model with polinomials-4 transformation
dev.new()
plot(as.factor(seat_comfort),overall,main='Polinomial-4 Model', xlab="seat comfort", ylab="overall")
xx=seq(min(seat_comfort),max(seat_comfort),along.with = seat_comfort)
ci_lin <- predict(lm(overall~I(seat_comfort^4),data=merComplete),newdata=data.frame(seat_comfort=xx),se.fit = T,interval = "confidence")
matplot(xx,ci_lin$fit[,1],lty=1, ltw=2, col="red", type="l", add=T)



