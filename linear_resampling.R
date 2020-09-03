attach(tr_s_outliers)

model_reduced_collinearity_CM <-(co2_emission ~ euro_standard + transmission_type +
                                      fuel_type + combined_metric  + noise_level)


n = nrow(tr_s_outliers)


######### Validation Set Approch #########
train=sample(1:n,n/2)
test=(-train)
set.seed(1)
lm.fit=lm(model_reduced_collinearity_CM, data = tr_s_outliers , subset = train)

y_true=tr_s_outliers$co2_emission
y_predict=predict(lm.fit,tr_s_outliers)
mean(((y_true-y_predict)[test])^2)


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
boot.fn(tr_s_outliers,sample(1:n, 44918,replace=T))
boot.fn(tr_s_outliers,sample(1:n, 44918,replace=T))
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



