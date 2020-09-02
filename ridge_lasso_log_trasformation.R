library(glmnet)
attach(tr_s)



fit.linear <-(co2_emission ~ euro_standard + transmission_type + log(engine_capacity) +
                             fuel_type + combined_metric  + noise_level)
# The model.matrix() function is particularly useful for creating x; not only does it produce a matrix 
# corresponding to the 9 predictors but it also automatically transforms any qualitative
# variables into dummy variables (recommended).
# The latter property is important because glmnet() can only take numerical, quantitative inputs.

x = model.matrix(fit.linear, tr_s)[,-1] #[-1] means no intercept

y = tr_s$co2_emission


#=================================
#  RIDGE
#=================================

#VALORI ARBITRARI DI LAMBDA

grid=10^seq(10,-2,length=100) # Lambda values grid (from 10^10 to 10^-2)

ridge.mod=glmnet(x,y,alpha=0,lambda=grid)

#Associated with each value of lambda is a vector of ridge regression 
#coefficients, stored in a matrix that can be accessed by coef(), 
#in this case 12x100, 11+intercept for each lambda value:
dim(coef(ridge.mod))


#50-ESIMO VALOREDI LAMBDA

# We expect the coefficient estimates to be much smaller, in terms of l2 norm, 
#when a large value of lambda is used, as compared to when a small value is used. 
ridge.mod$lambda[50] # grid[50] = 11497.57

coef(ridge.mod)[,50] # corresponding coefficients

sqrt(sum(coef(ridge.mod)[-1,50]^2)) # l2 norm = 0.33 (DISTANZA DA 0)

#60 ESIMO VALORE DI LAMBDA
ridge.mod$lambda[60] # lambda = 705.48

coef(ridge.mod)[,60] # corresponding coefficients
sqrt(sum(coef(ridge.mod)[-1,60]^2)) # l2 norm = 3.82> l2 for lambda[50]

# obtain the ridge regression coefficients for a new lambda, say 50:
predict(ridge.mod,s=50,type="coefficients")[1:7,] 


# Validation approach to estimate test error
set.seed(1)

n = nrow(tr_s)

train=sample(1:n,n/2)
test=(-train)

y.test=y[test]

# fit a ridge regression model on the training set, and evaluate its MSE on the test set, using lambda = 4. 
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,]) # Note the use of the predict() function again. This time we get predictions for a test set, by replacing type="coefficients" with the newx argument.
mean((ridge.pred-y.test)^2) 
mean((mean(y[train ])-y.test)^2) # test MSE, if we had instead simply fit a model with just an intercept, we would have predicted each test observation using the mean of the training observations.

#ALL AUMENTARE DI LAMBDA I COEEFICIENTI DIMUISCONO E MSE AUMENTA , IN PARTICOLARE
# IL TEST MSE DELLA RIDGE CON LAMBDA = 10^10 CORRISPONDE AL TEST MSE ADDATTANDO
#IL MODELLO CON SOLO L'INTERCETTA 


# PREDICTION WITH LAMBDA= 10^10 , CORRESPONDING TO MODEL WITH ONLY INTERCEPT
ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2) 


# PREDICTION WITH LAMBDA= 0, CORRESPONDING TO LEAST SQUARE
ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train]) # corrected according to errata (glmnet pack updated)
mean((ridge.pred-y.test)^2) 

## CROSS VALIDATION TO CHOOSE THE BEST LAMBDA 

set.seed (1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
dev.new()
plot(cv.out)
bestlam=cv.out$lambda.min; 
bestlam; 
log(bestlam) 
cv.out$lambda.1se
ridge.pred=predict(ridge.mod,s=bestlam ,newx=x[test,])
mean((ridge.pred-y.test)^2)  #IMPROVEMENT OF MSE RESPECT LAMBDA=4


#Finally refit our ridge regression model on the full data set with the best lambda

out=glmnet(x,y,alpha=0)
predict(out,type="coefficients",s=bestlam)[1:7,]
# As expected, none of the coefficients are zero
# ridge regression does not perform variable selection!
dev.new()
plot.new()
plot(out,label = T, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:7, legend = colnames(x), cex = .5)



#=================================
#  LASSO
#=================================

# use the argument alpha = 1 to perform lasso

lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
dev.new()
plot(lasso.mod,label = T)
dev.new()
plot.new()
plot(lasso.mod,label = T, xvar = "lambda")
# perform cross-validation
set.seed (1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
dev.new()
plot(cv.out)
bestlam=cv.out$lambda.min
print(bestlam) 
print(log(bestlam)) 
print(cv.out$lambda.1se)
lasso.pred=predict(lasso.mod,s=bestlam ,newx=x[test,]) 
mean((lasso.pred-y.test)^2)  

lasso.pred=predict(lasso.mod,s=0,newx=x[test,],exact=T,x=x[train,],y=y[train])
mean((lasso.pred-y.test)^2)
# However, the lasso has a substantial advantage:

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:6,]
lasso.coef
lasso.coef[lasso.coef!=0]
cat("Number of coefficients equal to 0:",sum(lasso.coef==0),"\n")

# compare with OLS when only selected predictors are included. 

fit.lm=lm(fit.linear <- (co2_emission ~ euro_standard + fuel_cost_6000_miles + fuel_type + engine_capacity + year + transmission_type
                         + noise_level + combined_metric + urban_metric ), data=tr_s)
coef(fit.lm)
lasso.coef=predict(out,type="coefficients",s=0)[1:11,]
lasso.coef
# coef(lm(Salary~., data=Hitters)) # differences on the 3rd place


