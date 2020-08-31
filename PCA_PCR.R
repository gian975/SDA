library(pls)
attach(tr_s_high_leverage)

#################
###### PCR ######
set.seed (2)

# Use the pcr() function:
# La sintassi per la pcr()funzione è simile 
#a quella per lm(), con alcune opzioni aggiuntive. L'impostazione scale=TRUEha l'effetto di standardizzare ogni predittore prima di generare i componenti principali, in modo che la scala su cui viene misurata ogni variabile non abbia effetto. L'impostazione validation="CV"causa il pcr()calcolo dell'errore di convalida incrociata dieci volte per ogni possibile valore di M , il numero di componenti principali utilizzati. Come al solito, imposteremo 
#un seme casuale per la coerenza:

pcr.fit=pcr(fit.linear,data = tr_s_high_leverage,scale=TRUE, validation ="CV")

#Data: 	X dimension: [8] 79576  
#       Y dimension: [1]  79576
# By default, "pcr" gives us the RMSE (Root MSE) in terms of prediction with croos validation approach
# For each component, the result gives us the RMSE, as the number of components changes, and the variance explained.

# First line: variance explained as the number of regressors changes.
# Second line: variance explained as a function of co2_EMISSION variance.
# (Non tutta la varianza dei regressori mi serve per spiegare la varianza di y)

# Variance:
#X                100.0  varianza speigata quando ci sono tutti i regressori 
#co2_emission      98.8  

summary(pcr.fit)
# Note that pcr() reports the root mean squared error; 
# It also provides the percentage of variance explained in the predictors
#and in the response using different numbers of components. 

# Plot the cross-validation scores
# Using val.type="MSEP" will cause the cross-validation MSE to be plotted.
dev.new()
validationplot(pcr.fit,val.type="MSEP",legendpos = "topright")
which.min(MSEP(pcr.fit)$val[1,,][-1])
# We see that the smallest CV error occurs when M = 8
# This suggests that there is no benefit in terms of reduction of dimensionality (M = 8)

# Now perform PCR on the training data and evaluate its test set performance:
set.seed (1)
x = model.matrix(fit.linear,data = tr_s_high_leverage)[,-1]

y = tr_s_high_leverage$co2_emission
train=sample(1:nrow(x), nrow(x)/2) # another typical approach to sample
test=(-train)
y.test=y[test]

pcr.fit=pcr(fit.linear,data = tr_s_high_leverage ,subset=train,scale=TRUE,
            validation ="CV")
dev.new()
# Plot MSE and RMSE 
validationplot(pcr.fit,val.type="MSEP",legendpos = "topright") 
minPCR <- which.min(MSEP(pcr.fit)$val[1,,][-1]); minPCR # M=8 shows the lowest CV error
dev.new()
plot(RMSEP(pcr.fit),legendpos = "topright")

# We compute the test MSE as follows:
pcr.pred=predict(pcr.fit,x[test,], ncomp=minPCR)
mean((pcr.pred-y.test)^2) # --> 8.00
# This test set MSE is competitive with the results obtained using ridge and the lasso

# Finally, we fit PCR on the full data set, using M = 8
pcr.fit=pcr(y~x,scale=TRUE,ncomp=which.min(MSEP(pcr.fit)$val[1,,][-1]))
summary(pcr.fit)
dev.new()
validationplot(pcr.fit,val.type="MSEP",legendpos = "topright")
dev.new()
plot(pcr.fit, ncomp = 8, asp = 1, line = TRUE)
coef(pcr.fit) ## get the coefficients

######################
######### PLS ########
set.seed (1)

# Using the plsr() function:
# Pls is similar to pcr, but is used to manage the variance of y 
# With pls, we don't concentrate our attention only on the regressions, but also the y variable
# Setting scale=TRUE has the effect of standardizing each predictor (scale projection).
# Setting validation = 'CV' has the effect to use cross validation to rate M parameter
pls.fit=plsr(fit.linear,data = tr_s_high_leverage, scale=TRUE, 
             validation ="CV")
summary(pls.fit)
dev.new()
validationplot(pls.fit,val.type="MSEP")
which.min(MSEP(pls.fit)$val[1,,][-1]) # M = 8

# Now perform Pls on the training data and evaluate its test set performance:
set.seed (1)
pls.fit=plsr(fit.linear,data = tr_s_high_leverage, subset=train, 
             scale=TRUE, validation ="CV")

validationplot(pls.fit,val.type="MSEP"); 
which.min(MSEP(pls.fit)$val[1,,][-1]); # M = 6
pls.pred=predict(pls.fit,x[test,],ncomp=4)
mean((pls.pred-y.test)^2) # --> 8.00

# The test MSE is comparable to (slightly higher) the test MSE obtained using ridge regression, the lasso, and PCR.

# Finally, we perform PLS using the full data set, using M = 4, 
pls.fit=plsr(fit.linear,data = tr_s_high_leverage ,scale=TRUE,ncomp=4)
summary(pls.fit)
#Final result: with 4 components we can explain the same variance of y obtained with 8 components