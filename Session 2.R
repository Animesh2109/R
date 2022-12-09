warpbreaks
view(warpbreaks)
boxplot(warpbreaks)  

# creating the boxplot of the dataset, outliers are shown as 2 distnict points

boxplot(warpbreaks)$out   
# gives outliers
# create a boxplot that labels the outliers
# install package: ggstatsplot

ggbetweenstats(warpbreaks,wool,breaks,outliers.tagging= TRUE)
# breaks is having data, wool has 2 categories. in A, freq of L is 27 and freq of M in categories A and B
# outlier is existing in category in A

outliers=boxplot(warpbreaks$breaks,plot = FALSE)$out
outliers

x=warpbreaks
x=x[-which(x$breaks %in% outliers),]
x

boxplot(x)
boxplot(x)$out



#LOGISTIC REGRESSION
# package used:blorr

# the general equations for logistic regression
# y = 1/(1+e^(-(a+b1x1+b2x2+....+bnxn)))
# x = is your predictor
# a and b are the coefficients of IV which are always numeric constant
# Function for LR is glm()

view(mtcars)
head(mtcars)
lr = mtcars[,c("am","cyl","hp","wt")] 
# c() fetching of few columns
head(lr)

# am is the dependent variable which is categorical (dichotomous)
# H0: cyl, hp and wt has no impact on am
# H1: cyl, hp and wt has impact on am
am_lr=glm(formula = am ~cyl+hp+wt,data=lr,family = binomial)
am_lr=glm()
am_lr

summary(am_lr)
# in the summmary p-value of wt<0.05, hence H0 is rejected, therefore wt is impacting am

#PROBIT and LOGIT MODEL

# Logit model for logistics distribution of errors
# probit model for probabilistic model
# while importing data remove heading (e,g v1,v2..)

mydata <-read.csv("C:/Niraj/All Files/IBS/sem.4/Advance Business Analytics/Logistic Regression/probit_insurance.csv", header=TRUE,stringsAsFactors = TRUE)
mydata
head(mydata)

# Define the variables
mydata
attach(probit_insurance)
#define the variables
y=cbind(ins)
x=cbind(retire,age,hstatusg,hhincome,educyear,married,hisp)
summary
summary(x)

#conversion into table
table
table(y)/sum(table(y))

#regression coefficient 
olsreg<- lm(y~x)
summary(olsreg)

#logit model coefficient
logit<- glm(y~x,family=binomial(link='logit'))
summary(logit)
exp(logit$coefficients)

# Probit Model coefficients
probit<- glm(y~x,family=binomial(link='probit'))
summary(probit)

# Regression marginal effects
coef(olsreg)

# Logistic Model Marginl Effect
Logitscalar = mean(dlogis(predict(logit, type = "link")))
Logitscalar*coef(probit)

# Regression Predicted Probabilities
polsreg = predict(olsreg)
summary(polsreg)

# Logit model predicted probabilities
plogit = predict(logit,type = "response")
summary(plogit)

#Probit model predicted probabilities
pprobit = predict(probit,type="response")
summary(pprobit)

# percent of correctly predicted values
table(true=y, pred =round(fitted(probit)))
table(true=y, pred =round(fitted(logit)))

