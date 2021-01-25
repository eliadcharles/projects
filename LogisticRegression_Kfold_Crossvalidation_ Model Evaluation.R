#Code

library(stargazer)
data <- read.csv("./titanic(1).csv",header = TRUE)

data<-data[c(1,2,4,5,6,7,8,9)]#removing name row

set.seed(5)

data1 <- data
#converting variables 
data1$Pclass <- as.factor(data1$Pclass)
data1$Embarked <- as.factor(data1$Embarked)
data1$Sex <- as.factor(data1$Sex)
data1$Fare <- as.integer(data1$Fare)
data1$Survived <- as.factor(data1$Survived) 

#copying data for model generation


#standardising
data1$Fare <- scale(data$Fare)
data1$Age <-scale(data$Age)

#PART 1 ----
gender.model <- glm(formula = Survived ~ Sex, data = data1, family = binomial)
#stargazer(gender.model)
summary(gender.model)

gender.modelAIC <- AIC(gender.model)
gender.PsuedoRsqr <- with(gender.model,1-deviance/null.deviance)

#inverse of odds shows males are 12x more likely to have died. 
1/exp(coef(gender.model))

#probability of survival given female and male
library(ISLR)
probs = predict(gender.model, type = "response")
probs <- probs[1:2]

#PART 2 ----

#EDA
data2 <- data1
names(data2)[names(data2) == "Siblings.Spouses.Aboard"] <- "Less.Than.2"
names(data2)[names(data2) == "Embarked"] <- "Southampton"
names(data2)[names(data2) == "Parents.Children.Aboard"] <- "Parent.Children"
str(data2)

data2$Less.Than.2 <- ifelse(test = data2$Less.Than.2 <= 1, yes = "Yes", no = "No")
data2$Parent.Children <- ifelse(test = data2$Parent.Children == 0, yes = "No", no = "Yes")
data2$Southampton <- ifelse(test = data2$Southampton == "S", yes = "Yes", no = "No" )
data2$Survived <- ifelse(test = data2$Survived == "0", yes = "Died", no = "Survived" )

data2$Survived <- as.factor(data2$Survived)
data2$Less.Than.2 <- as.factor(data2$Less.Than.2)
data2$Parent.Children <- as.factor(data2$Parent.Children)
data2$Southampton <- as.factor(data2$Southampton)



#plotting %'s of number of passengers who embarked from southampton, traveled in groups less then 2
#and if you traveled with a parent or child
Southamptontbl <- proportions(table(data2$Southampton))*100
Southamptontbl
Grouptbl <- proportions(table(data2$Less.Than.2))*100
Grouptbl
Parent.Childrentbl <- proportions(table(data2$Parent.Children ))*100
Parent.Childrentbl

barplot(Grouptbl, ylim = c(0,100), ylab = "%", col =  c("lightslategrey", "lightsteelblue"))
barplot(Southamptontbl, main = 'Travelled from Southampton', ylim = c(0,100), ylab = "%")
barplot(Parent.Childrentbl, main = 'Parents and/or Children Aboard', ylim = c(0,100), ylab = "%")

#comparing Fare and Class for multicollinearity, box plot and Anova

data1$Pclass <- as.numeric(data1$Pclass)

boxplot(Fare ~ Pclass, data = data1,ylab = ("Standardised Fare Price"), xlab = ("Ticket Class"),col =  c("lightsteelblue1", "lightsteelblue2", "lightslategrey" ))
Fare.class <- cbind(data1$Pclass, data1$Fare)
cor(Fare.class, method = 'kendall', use = 'pairwise')
data1$Pclass <- as.factor(data1$Pclass) #turn back to factor for further analysis.

data3 <- data1
data3$Survived <- ifelse(test = data3$Survived == "0", yes = "Died", no = "Survived" )
boxplot(Fare ~ Survived, data = data3, ylab = ("Standardised Fare Price"), xlab = ("") )
SurvivedVsPclass <- table(data3$Survived, data3$Pclass)
Pclass.percentages <- proportions(SurvivedVsPclass, 2)
Pclass.percentages <- Pclass.percentages*100
Pclass.percentages
#comparison of proportion of deaths in each class 
barplot(Pclass.percentages, beside = T, ylab = "%", xlab= "Ticket Class", ylim = c(0,100) ,legend.text = TRUE,col =c("lightslategrey", "lightsteelblue") , args.legend = list(x="topleft"))


Fare.class.ANOVA <- aov(Fare ~ Pclass, data = data1)
summary(Fare.class.ANOVA)


#comparing SSA and PCA
cor(data1$Siblings.Spouses.Aboard,data1$Parents.Children.Aboard)


#model using all data
full.model <- glm(Survived ~ . , data = data1, family = binomial)
#stargazer(full.model)
summary(full.model)


#full.model fit assessment
full.PsuedoRsqr <- with(full.model,1-deviance/null.deviance)
full.modelAIC <-AIC(full.model)

#model1 -non-significant indicators removed.
mod1 <- glm(Survived ~ Pclass + Sex + Age + Siblings.Spouses.Aboard , family= binomial, data = data1)
summary(mod1)
#stargazer(mod1)
#model1 fit assessment
mod1.PsuedoRsqr <- with(mod1,1-deviance/null.deviance)
mod1AIC <-AIC(mod1)




#individual assessment of new dummy variables in comparison to old. 

#fare model 
Fare.model <- glm(Survived ~ Fare, data = data1, family = binomial)
summary(Fare.model)

FareVsSurvive <- table(data1$Fare,data1$Survived)
boxplot(Fare ~ Survived, data = data1, ylab = "Standardised Fare Price" )

FareVsSurvive.ANOVA <- aov(Fare ~ Survived, data = data1)
summary(FareVsSurvive.ANOVA)
#initial parent.child 
parent.model <- glm(formula = Survived ~ Parents.Children.Aboard , data = data1, family = binomial)
summary(parent.model)

#dummy parent child - Parent.child
dummyparent.mod <- glm(formula = Survived ~ Parent.Children, data = data2, family = binomial)
summary(dummyparent.mod)

#initial embarking model
embark.model <- glm(formula = Survived ~ Embarked , data = data1, family = binomial)
summary(embark.model)

#dummy embarking model - Southampton 
dummyembark.mod <- glm(formula = Survived ~ Southampton, data = data2, family = binomial)
summary(dummyembark.mod)

# initial sibling spouse model
SiblingSpouse.model <- glm(formula = Survived ~ Siblings.Spouses.Aboard, data = data1, family = binomial)
summary(SiblingSpouse.model)

#dummy sibling spouse model -Less.Than.2
Less.Than2.model <- glm(formula = Survived ~ Less.Than.2, data = data2, family = binomial)
summary(Less.Than2.model)






#modeling with new dummy variables- Southampton and Parent.Children are still not significant indicators
mod2a<- glm(Survived ~ Pclass + Sex + Age + Less.Than.2 + Southampton + 
              Parent.Children, family= binomial, data = data2)
summary(mod2a)

#model with only significant indicator
mod2b<- glm(Survived ~ Pclass + Sex + Age + Less.Than.2, family= binomial, data = data2)
summary(mod2b)
#stargazer(mod2b)
mod2b.PsuedoRsqr <- with(mod2b,1-deviance/null.deviance)
model2bAIC <-AIC(mod2b)
exp(coef(mod2b))



#COMPARISON OF MODELS ----
#residual plots and QQ plots 

#full model
library(statmod)

gender.resid = qres.binom(gender.model)
plot(gender.resid)
abline(0,0, col = 'red')
qqnorm(gender.resid, main = "Gender Model")
qqline(gender.resid)


full.resid = qres.binom(full.model)
plot(full.resid)
abline(0,0, col = 'red')
qqnorm(full.resid, main = "Full Model")
qqline(full.resid)

mod1.resid = qres.binom(mod1)
plot(mod1.resid)
abline(0,0, col = 'red')
qqnorm(mod1.resid, main = 'Model 1')
qqline(mod1.resid)

mod2b.resid = qres.binom(mod2b)
plot(mod2b.resid)
abline(0,0, col = 'red')
qqnorm(mod2b.resid, main = 'Model 2b')
qqline(mod2b.resid)



library(caret)
library(ROCR)
library(ggplot2)
library(lattice)
crossValSettings <- trainControl(method = "repeatedcv", number = 10, 
                                 savePredictions = TRUE)

#gender cross validation and ROC performance plot and area under curve
gender.crossVal <- train(Survived ~ Sex,
                         data = data1, family = "binomial", method = "glm", 
                         trControl = crossValSettings)
#confusion matrix 
gender.predictions <- predict(gender.crossVal, newdata = data1)
gender.confMat <- confusionMatrix(data = gender.predictions, data1$Survived)
gender.confMat

#ROC/AUC
gender.prob <- predict(gender.model, data, type = "response")
gender.prediction <- prediction(gender.prob, data2$Survived)
gender.perfomance <- performance(gender.prediction, measure = "tpr", x.measure = "fpr")
gender.AUC <- performance(gender.prediction, measure = "auc")
gender.AUC@y.values


#full.model cross validation and ROC performance plot and area under curve. 
fullmodel.crossVal <- train(Survived ~ . , data = data1, family = "binomial", 
                            method = "glm", trControl = crossValSettings)
#confusion matrix 
fullmodel.predictions <- predict(fullmodel.crossVal, newdata = data1)
fullmodel.confMat <- confusionMatrix(data = fullmodel.predictions, data1$Survived)
fullmodel.confMat

#ROC/AUC
fullmodel.prob <- predict(full.model, data1, type = "response")
fullmodel.prediction <- prediction(fullmodel.prob, data$Survived)
fullmodel.perfomance <- performance(fullmodel.prediction, measure = "tpr", x.measure = "fpr")
fullmodel.AUC <- performance(fullmodel.prediction, measure = "auc")
fullmodel.AUC@y.values



#model1 cross validation and ROC performance plot and area under curve
mod1.crossVal <- train(Survived ~ Pclass + Sex + Age + Siblings.Spouses.Aboard,
                       data = data1, family = "binomial", method = "glm", 
                       trControl = crossValSettings)
#confusion matrix 
mod1.predictions <- predict(mod1.crossVal, newdata = data1)
mod1.confMat <- confusionMatrix(data = mod1.predictions, data1$Survived)
mod1.confMat
#ROC/AUC
mod1.prob <- predict(mod1, data1, type = "response")
mod1.prediction <- prediction(mod1.prob, data1$Survived)
mod1.perfomance <- performance(mod1.prediction, measure = "tpr", x.measure = "fpr")
mod1.AUC <- performance(mod1.prediction, measure = "auc")
mod1.AUC@y.values

#model2b cross validation and ROC performance plot and area under curve
mod2b.crossVal <- train(Survived ~ Pclass + Sex + Age + Less.Than.2,
                       data = data2, family = "binomial", method = "glm", 
                       trControl = crossValSettings)
#confusion matrix 
mod2b.predictions <- predict(mod2b.crossVal, newdata = data2)
mod2b.confMat <- confusionMatrix(data = mod2b.predictions, data2$Survived)
mod2b.confMat

#ROC/AUC
mod2b.prob <- predict(mod2b, data2, type = "response")
mod2b.prediction <- prediction(mod2b.prob, data2$Survived)
mod2b.perfomance <- performance(mod2b.prediction, measure = "tpr", x.measure = "fpr")
mod2b.AUC <- performance(mod2b.prediction, measure = "auc")
mod2b.AUC@y.values

plot(mod2b)


#ROC plots

plot(fullmodel.perfomance, col = "red")
plot(mod2b.perfomance, col = "blue", add = TRUE)
plot(gender.perfomance, col = "green", add = TRUE)
legend("topleft", legend = c("Gender","mod2b", "Full"), col = c("green", "blue", "red"),lty = 1:2, cex = 0.8, title = "Models")

#CALCULATING IF JACK DAWSONS DEATH WAS REALISTIC ----

str(data)
#model2b logistic plot 
mod2b.preds <- data.frame(
  POS = mod2b$fitted.values,
  Survived = data$Survived)

mod2b.preds <- mod2b.preds[
  order(mod2b.preds$POS, decreasing = FALSE),]
mod2b.preds$rank <- 1:nrow(mod2b.preds)

ggplot(data = mod2b.preds, aes(x=rank, y=Survived)) + 
  geom_point(aes(color=Survived), alpha=1, shape=4, stroke=2) +
  geom_smooth(method = 'glm', se = FALSE, col = "black", 
              method.args = list(family = "binomial")) + 
  xlab("Index") +
  ylab("Predicted Probability of Survival") +
  ggtitle("Predictors mod2b")

ggplot(data = mod2b.preds, aes(x=rank, y=POS, color = Survived)) + 
  geom_point(alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted Probability of Survival") +
  ggtitle("Predictors mod2b")

#using model2b 
mod2b.OR.ratio <- cbind(OR = (1/exp(coef(mod2b))),exp(confint(mod2b)))
mod2b.OR.ratio
exp(coef(mod2b))

jackage.scaled <- (20 - mean(data$Age))/sd(data$Age)

x <- 1.4123299 - 2.5068016  - 2.6941332  -0.6206716*jackage.scaled + 1.4463070 
x
probability.jack.survived <- exp(x)/(1+exp(x))
probability.jack.survived # realistic

