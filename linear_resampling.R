attach(tr_s_outliers)
attach(t_s_1)

model_reduced_collinearity_CM <-(co2_emission ~ euro_standard + transmission_type +
                                      fuel_type + combined_metric  + noise_level)


n = nrow(tr_s_outliers)
c = nrow(t_s_1)

######### Validation Set Approch #########
train=sample(1:n,n)
test=sample(1:c,c)
set.seed(1)
lm.fit=lm(model_reduced_collinearity_CM, data = tr_s_outliers , subset = train)

y_true=t_s_1$co2_emission
y_predict=predict(lm.fit,t_s_1)
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
indice=nrow(tr_s_outliers)

# No-transformation
set.seed (2)
boot.fn=function(data,index){
  return(coef(lm(co2_emission ~ euro_standard + transmission_type +
                   fuel_type + combined_metric  + noise_level, data = tr_s_outliers,subset=index)))
}
boot.fn(tr_s_outliers, 1:indice)

# Boot estimate is not deterministic
boot.fn(tr_s_outliers,sample(1:n, 34375,replace=T))
boot.fn(tr_s_outliers,sample(1:n, 34375,replace=T))
# We use the boot() function to compute the standard errors 
# of 1,000 bootstrap estimates for the intercept and slope terms.
b = boot(tr_s_outliers ,boot.fn ,1000)

s = summary(lm(model_reduced_collinearity_CM, data = tr_s_outliers))

# Take all std. errors of the bootstrap estimate 
x <- capture.output(b)
x <- str_extract(x, "^t[0-9.]+.*$")
x <- x[!is.na(x)]
se <- as.numeric(unlist(str_extract_all(x, '[0-9.]+$')))

# Take all std. errors of the linear model
c = s$coefficients[ ,2]
c = as.numeric(c)

cat("\nDifference between no-Transformation Std.errors:\n",c - se,"\n")

#Difference between sqrt transformation Std.errors:\n",c - se,"\n")

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



