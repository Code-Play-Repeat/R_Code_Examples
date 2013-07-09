# R project
load ("ozone.RData")
library(MASS)
library(leaps)
library(boot)

for (i in seq(from=2,to=73,by=1))
 x[,i] = as.numeric(levels(x[,i])[x[,i]])

# Replacing all bad values with the medians of their respective rows.
# This is because program crashes everytime it trys to compute a na
# value.
for (j in seq(from=1,to=(dim(x)[2]),by=1))
{ 
 badrows = which(is.na(x[,j]))
 if (length(badrows) > 0)
 {
   replacevalue = median(x[-badrows,j])
   x[badrows,j] = replacevalue
 }	
}
# Truncating the date column 
# because this is non-informative to predicting the Ozone day
x = x[,-1]

# Storing the column names in a vector for easy access. 
xn <- colnames(x)
nrows = dim(x)[1]

baseFormula = "Class ~ 1 "
# this is the starting formula for the stepwise regression model
base = glm(baseFormula,data=x, family = binomial(link ="logit"))


# Using format specified in formgen.R to simplify the formula manipulation.

vars2use = c(1:72)
formula1 = "Class ~ "
formula2 = paste(xn[vars2use],sep="",collapse=" + ")
fullFormula = paste(formula1,formula2,sep="")
# this is the max formula for the stepwise regression model
full = glm (fullFormula, data= x, family = binomial(link = "logit"))

# using the AIC criterion for stepwise regression
smAnalysis <- addterm (base,full, test ="Chisq", trace = FALSE)

print(smAnalysis)

# This finds all the predictors marked with *** and places their indices in z
# subtracting by -1 to get the proper index that corresponds to the predictor.
z = which(smAnalysis [5]<0.001)-1

# Now using the predictors that the stepwise regression finds, 
# a formula is created from those predictors.
# It is created by making a subset of the data from the predictors.
# This is so we can try reducing it by 
# using regsubsets to do best subsets regression.

sub = subset(x, select = c(xn[z]))
sub <- cbind (sub, x[73])


# Using the regsubsets function to reduce the model through
# best subsets regression. 
# It goes from 1-5 sized subsets and record the best subset of each size.
leaps <- regsubsets (Class~., data = sub, nbest = 1, nvmax = 5, really.big = TRUE)
print(summary(leaps))

# top choices WSR6, WSR10, WSR12, T4, T5, T6, T7, T15, T23, T_PK, T_AV
# index:	   7,    11,    13,   31, 32, 33, 34,  42,  50,  51, 	52

w = c(7, 11, 13, 31, 32, 33, 34, 42, 50 , 51, 52)
formula1 = "Class ~ "
formula2 = paste(xn[w],sep="",collapse=" + ")
reducedFormula = paste(formula1,formula2,sep="")
# this is the reduced formula created from the predictors selected by regsubsets.
reduced = glm (reducedFormula, data= x, family = binomial(link = "logit"))

# testing stepModel using leave-one-out cross-validation
confusion1 = matrix(0,nrow=2,ncol=2)
for (k in seq(from= 1, to = 2533, by = 1))
{

	xtest = x[k,]
	xtrain = x[-k,]
	
	# z contains the indices of the predictors favored by the Step Model anaylsis
	q = xtrain[,73]	
	m = xtrain [,z]
	
	stepModelTest <- glm(q~., data = m, family = binomial(link ="logit"))
	qt = xtest[,73]
	mt = xtest[,z]

	actual0 = which(qt==0)
	actual1 = which(qt==1)

	ypred = predict(stepModelTest, newdata=mt)

	qpred = round (1/(1+exp(-ypred)))

	# TRUE POSITIVES (TP)
	confusion1[1,1] = confusion1[1,1]+sum(qpred[actual1]==1)

	# FALSE POSITIVES (FP)
	confusion1[2,1] = confusion1[2,1]+sum(qpred[actual0]==1)

	# FALSE NEGATIVES (FN)
	confusion1[1,2] = confusion1[1,2]+sum(qpred[actual1]==0)

	# TRUE NEGATIVES (TN)
	confusion1[2,2] = confusion1[2,2]+sum(qpred[actual0]==0)
}


print(confusion1)

# classification percentage accuracy = 100*(TP + TN)/nrows
pctacc1 = 100*(confusion1[1,1]+confusion1[2,2])/nrows
print(pctacc1)

#### end of testing step model

## start of testing reduced model using leave-one-out cross-validation

confusion2 = matrix(0,nrow=2,ncol=2)
for (k in seq(from= 1, to = 2533, by = 1))
{

	xtest = x[k,]
	xtrain = x[-k,]
	
	# w contains the indices of the predictors favored by the leaps anaylsis
	q = xtrain[,73]	
	m = xtrain [,w]
	
	reducedModelTest <- glm(q~., data = m, family = binomial(link ="logit"))
	qt = xtest[,73]
	mt = xtest[,w]

	actual0 = which(qt==0)
	actual1 = which(qt==1)

	ypred = predict(reducedModelTest, newdata=mt)

	qpred = round (1/(1+exp(-ypred)))

	# TRUE POSITIVES (TP)
	confusion2[1,1] = confusion2[1,1]+sum(qpred[actual1]==1)

	# FALSE POSITIVES (FP)
	confusion2[2,1] = confusion2[2,1]+sum(qpred[actual0]==1)

	# FALSE NEGATIVES (FN)
	confusion2[1,2] = confusion2[1,2]+sum(qpred[actual1]==0)

	# TRUE NEGATIVES (TN)
	confusion2[2,2] = confusion2[2,2]+sum(qpred[actual0]==0)
}


print(confusion2)

# classification percentage accuracy = 100*(TP + TN)/nrows
pctacc2 = 100*(confusion2[1,1]+confusion2[2,2])/nrows
print(pctacc2)

#### end of testing reduced model







