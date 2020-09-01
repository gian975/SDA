
attach(t_s)
attach(tr_s_high_leverage)

fit.linear <- (co2_emission ~ euro_standard + transmission_type +
                 fuel_type + combined_metric)


n = nrow(tr_s_high_leverage)
c=nrow(tr_s_high_leverage)


######### Validation Set Approch #########
train=sample(1:n,n)
test=sample(1:c,c)
set.seed(1)
lm.fit=lm(fit.linear, data = tr_s_high_leverage , subset = train)

# the estimated test MSE for the linear regression fit is 33.51284 (seed=1)

y_true=data_complete$co2_emission
y_predict=predict(lm.fit,data_complete)
mean(((y_true-y_predict)[test])^2) 

y_true=tr_s_high_leverage$co2_emission
y_predict=predict(lm.fit,tr_s_high_leverage)
mean(((y_true-y_predict)[test])^2)


# the estimated test MSE for the linear regression reduced fit is 114.237
lm.fit2=lm(fit.reduced , data = data_complete, subset=train)

y_true=data_complete$co2_emission
y_predict=predict(lm.fit2,data_complete)
mean(((y_true-y_predict)[test])^2)

# use the poly() function to estimate the test error for the polynomials-2 transformation.
lm.fit3=lm(fit.poly2, data = data_complete, subset=train) 

# the estimated test MSE for the linear regression fit is 67.6406
y_true=data_complete$co2_emission
y_predict=predict(lm.fit3,data_complete)
mean(((y_true-y_predict)[test])^2)


# estimate the test error for the log transformation. 251.937
lm.fit4=lm(fit.log, data = data_complete, subset=train) 

y_true=data_complete$co2_emission
y_predict=predict(lm.fit4,data_complete)
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
## we obtain a test error higher -> 74.30688 73.93336 74.07724 74.46966


########## Bootstrap ##########

# The boot.fn() function can also be used in order to create bootstrap estimates 
# for the intercept and slope terms by randomly sampling from among the observations with replacement
# We will compare the estimates obtained using the bootstrap to those obtained using the previous models
library(stringr)
indice=nrow(tr_s_high_leverage)

# No-transformation
set.seed (2)
boot.fn=function(data,index){
  return(coef(lm(co2_emission ~  euro_standard + transmission_type +
                   fuel_type + combined_metric, data = tr_s_high_leverage,subset=index)))
}
boot.fn(tr_s_high_leverage, 1:indice)

# Boot estimate is not deterministic
boot.fn(tr_s_high_leverage,sample(1:n, 45441,replace=T))
boot.fn(tr_s_high_leverage,sample(1:n, 45441,replace=T))
# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.
b = boot(tr_s_high_leverage ,boot.fn ,1000)

s = summary(lm(fit.linear, data = tr_s_high_leverage))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the linear model
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between no-Transformation Std.errors:\n",c - se,"\n")


# Polinomials-2 no-linear transformation
set.seed (2)

boot.fn=function(data,index){
  return(coef(lm(co2_emission ~ poly(year,2) + poly(euro_standard,2) + transmission_type + poly(engine_capacity,2) +
                   fuel_type + poly(fuel_cost_6000_miles,2) + poly(noise_level,2),data = data,subset=index)))
}
boot.fn(data_complete, 1:indice)

boot.fn(data_complete,sample(1:indice, 45441,replace=T))

# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.

b = boot(data_complete ,boot.fn ,1000)

s = summary(lm(fit.poly2,data = data_complete))

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

boot.fn=function(data,index){
  temp <- index
  return(coef(lm(fit.poly3, data = data, subset=temp)))
}

boot.fn(merComplete, 1:n)

boot.fn(merComplete,sample(1:n, 79576,replace=T))

b = boot(merComplete ,boot.fn ,1000)

s = summary(lm(fit.poly3, data = merComplete))

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

boot.fn=function(data,index){
  return(coef(lm(fit.poly4, data = data,subset=index)))
}

boot.fn(merComplete, 1:n)


boot.fn(merComplete,sample(1:n, 79576,replace=T))

boot(merComplete ,boot.fn ,1000)

summary(lm(fit.poly4, data = merComplete))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the poly-4 transformation
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between poly-4 transformation Std.errors:\n",c - se,"\n")


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


