install.packages("tidyr")
install.packages("rpart")
library(tidyr)
library(rpart)
library(tree)
library(glmnet)
library(splines)
library(gam)

set.seed(999)

#Q1

house_votes <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.data", 
           header=FALSE)
#house_votes_names <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/voting-records/house-votes-84.names",header=FALSE)

#(a)
class(house_votes) #check if the loaded data is a data frame
#since it has all data in one column, split it using separate()

house_votes_full <- separate(house_votes,1,c("class_name",
                                             "handicapped",
                                             "water",
                                             "adoption",
                                             "physician",
                                             "el",
                                             "religious",
                                             "anti",
                                             "aid",
                                             "mx",
                                             "immigration",
                                             "synfuels",
                                             "education",
                                             "superfund",
                                             "crime",
                                             "duty",
                                             "export"),sep=",")
head(house_votes_full)
summary(house_votes_full)
apply(house_votes_full,2,function(x) class(x))
house_votes_full <- as.data.frame(apply(house_votes_full,2,function(x)
  ifelse(x=="?",names(which(table(x) == max(table(x)[-1]))),x)))
house_votes_factors<-house_votes_full
for(i in 1:ncol(house_votes_full))
house_votes_factors[,i] <- as.factor(house_votes_full[,i])

train <- sample(1:nrow(house_votes_factors),0.9*nrow(house_votes_factors),FALSE)
house_votes_train <- house_votes_factors[train,]

#(b)
rpart_pars <- rpart.control(20,6,0,4,5,2,1000,0,ncol(house_votes_factors))
rpart_house_votes <- rpart(class_name~.,house_votes_train,method="class",control=rpart_pars)
summary(rpart_house_votes)
plot(rpart_house_votes)
text(rpart_house_votes,pretty=0)  
probs_house_votes <- predict(rpart_house_votes,house_votes_factors[-train,-1])

predicted_house_votes <- as.data.frame(house_votes_factors[-train,1])
names(predicted_house_votes) <- c("actual")

for(i in 1:nrow(probs_house_votes))
{
  predicted_house_votes[i,"predicted"] <- ifelse(probs_house_votes[i,1] > 0.5,"democrat","republican" )
}

rpart_predicted_table <- table(predicted_house_votes[,1],predicted_house_votes[,2])
(rpart_predicted_table[1,1]+rpart_predicted_table[2,2])/sum(rpart_predicted_table)

#(c)
prune_house_votes <- prune.rpart(rpart_house_votes,0.01)
plot(prune_house_votes)
text(prune_house_votes,pretty=0) 

#(d)
house_votes_factors_new <- data.frame("n","n","n","y","y","y","y","n","n","y",
                             "n","y","n","y","n","n")
names(house_votes_factors_new) <- names(house_votes_factors)[-1]

new_predict_1 <- predict(rpart_house_votes,house_votes_factors_new)
new_predicted_1 <- ifelse(new_predict_1[,1] > 0.5,"democrat","republican")
new_predicted_1
new_predicted_2 <- predict(prune_house_votes,house_votes_factors_new)
new_predicted_2 <- ifelse(new_predict_1[,1] > 0.5,"democrat","republican")
new_predicted_2

##########################################################################################


#Q2
abalone <- as.vector(read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",
                          header=FALSE))

#(a)
abalone_full <- separate(abalone,1,c("sex",
                                     "Length",
                                     "Diameter",
                                     "Height",
                                     "Whole_weight",
                                     "Shucked_weight",
                                     "Viscera_weight",
                                     "Shell_weight",
                                     "Rings"),sep=",")
class(abalone_full)
head(abalone_full)
summary(abalone_full)
abalone_full[,1] <- as.factor(abalone_full[,1])
abalone_full[,-1]<- as.data.frame(apply(abalone_full[,-1],2,function(x) as.double(x)))

split_sample <- sample(1:4177,3133,replace=FALSE)
abalone_train <- abalone_full[split_sample,]
abalone_test_x <- abalone_full[-split_sample,-9]


#(b)
hist(abalone_full$Rings)
data_correlation <- cor(abalone_full[,-1])
data_covariance <- cov(abalone_full[,-1])
plot(abalone_full$Rings,abalone_full$sex)
pairs(abalone_full[,c(5,8,9)])
pairs(abalone_full)

#(c)
abalone_lm_fit <-lm(Rings~sex+Length+Diameter+Height+Whole_weight, data=abalone_train) 
summary(abalone_lm_fit)

abalone_lm_predict <- predict(abalone_lm_fit,abalone_train[,-9])

#explain ability of our model
TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_lm_predict-abalone_train[,9])^2)
(TSS-RSS)/TSS

abalone_lm_fit <-lm(Rings~sex+Length+Diameter+Height, data=abalone_train) 
summary(abalone_lm_fit)

abalone_lm_predict <- predict(abalone_lm_fit,abalone_train[,-9])

#explain ability of our model
TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_lm_predict-abalone_train[,9])^2)
(TSS-RSS)/TSS

#(d)

abalone_lm_fit_all <- lm(Rings~.,data=abalone_train)
summary(abalone_lm_fit_all)

abalone_lm_predict_all <- predict(abalone_lm_fit_all,abalone_train[,-9])

TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_lm_predict_all-abalone_train[,9])^2)
(TSS-RSS)/TSS

#(e)
#5 predictors#########

# forward <- function(data)
# {
#   step = 1
#   model = "Rings ~ "
#   predictor = c()
#   variable = colnames(data)[-6]
#   while(TRUE)
#   {
#     p_value = c()
#     for(i in variable)
#     {
#       curmodel = paste0("Rings ~ ", paste(c(predictor,i),collapse = " + "))
#       fit = lm(curmodel, data = data)
#       p = summary(fit)$coefficients[,4][step+1]
#       p_value = c(p_value, p)
#     }
#     newPred = which.min(p_value)
#     if(p_value[newPred] <= 0.05)
#     {
#       predictor = c(predictor, variable[newPred])
#       model = paste0("Rings ~ ", paste(predictor,collapse = " + "))
#       variable = variable[-newPred]
#       # print step wise result
#       print(sprintf("=========Step %i: =========", step))
#       print("p_value of each element:")
#       print(p_value)
#       print(sprintf("Model in step %i: ", step))
#       print(model)
#       step = step + 1
#     }else break
#   }
#   # Show final results
#   print(sprintf("===== The final model is: %s =====", model))
#   final.fit = lm(model, data = data)
#   summary(final.fit)
# }
# 
# forward(abalone_train[,c(1,2,3,4,5,9)])

abalone_quad_5 <- lm(Rings~poly(Diameter,2)+poly(Length,2)+poly(Height,2)
                     +poly(Whole_weight,2)+I(sex=="I"), data=abalone_train)
summary(abalone_quad_5)
abalone_quad_5_predict <- predict(abalone_quad_5,abalone_train)

TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_quad_5_predict-abalone_train[,9])^2)
(TSS-RSS)/TSS


#8 predictors##############
abalone_quad_8 <- lm(Rings~poly(Length,2)+
                     poly(Diameter,2)+
                       poly(Height)+
                       poly(Whole_weight,2)+
                       poly(Shucked_weight,2)+
                       poly(Viscera_weight,2)+
                       poly(Shell_weight,2)+sex, data=abalone_train)
summary(abalone_quad_8)
abalone_quad_8_predict <- predict(abalone_quad_8,abalone_train)

TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_quad_8_predict-abalone_train[,9])^2)
(TSS-RSS)/TSS



#(f)
x=model.matrix(Rings~.,abalone_train)
test_x <- model.matrix(Rings~.,abalone_full[-split_sample,])
ridge_glmnet <- glmnet(x,abalone_train[,9],alpha=0)
cv_ridge <- cv.glmnet(x,abalone_train[,9],alpha=0,nfolds=4,standardize=TRUE)
plot(cv_ridge)
summary(cv_ridge)
cv_ridge$lambda.min

# random_test_sample <- sample(split_sample,784,FALSE)
# x=model.matrix(Rings~.,abalone_train[-random_test_sample,])
# ridge_glmnet <- glmnet(x[,-9],x[,9],alpha=0)
# ridge_glmnet
# summary(ridge_glmnet)
# coef(ridge_glmnet)[,1]
# for(i in 1:dim(coef(ridge_glmnet))[2])
# {
#   for(j in 1:dim(coef(ridge_glmnet))[1])
#   {
#     
#   }
# }

#(g)
forward <- function(data)
{
  step = 1
  model = "Rings ~ "
  predictor = c()
  variable = c(colnames(data)[-9],"poly(Length,2)","poly(Diameter,2)",
               "poly(Height,2)","poly(Whole_weight,2)",
               "poly(Shucked_weight,2)","poly(Viscera_weight,2)",
               "poly(Shell_weight,2)")
  while(TRUE)
  {
    p_value = c()
    for(i in variable)
    {
      curmodel = paste0("Rings ~ ", paste(c(predictor,i),collapse = " + "))
      fit = lm(curmodel, data = data)
      p = summary(fit)$coefficients[,4][step+1]
      p_value = c(p_value, p)
    }
    newPred = which.min(p_value)
    if(p_value[newPred] <= 0.05)
    {
      predictor = c(predictor, variable[newPred])
      model = paste0("Rings ~ ", paste(predictor,collapse = " + "))
      variable = variable[-newPred]
      # print step wise result
      print(sprintf("=========Step %i: =========", step))
      print("p_value of each element:")
      print(p_value)
      print(sprintf("Model in step %i: ", step))
      print(model)
      step = step + 1
    }else break
  }
  # Show final results
  print(sprintf("===== The final model is: %s =====", model))
  final.fit = lm(model, data = data)
  summary(final.fit)
}

forward(abalone_train)

abalone_forward <- lm(Rings~Shell_weight+Shucked_weight+Diameter+Whole_weight+sex, data=abalone_train)
summary(abalone_forward)
abalone_forward_predict <- predict(abalone_forward,abalone_train)

TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_forward_predict-abalone_train[,9])^2)
RSS
(TSS-RSS)/TSS

#(h)
abalone_spline <- gam(Rings~s(Shell_weight,6)+s(Shucked_weight,6)+s(Diameter,6)
                      +s(Whole_weight,6)+s(Height,6)+poly(Length,2)+
                      poly(Length,2)+poly(Diameter,2)+
                      poly(Whole_weight,2)+sex,
                      abalone_train,family="gaussian")

abalone_spline_predict <- predict(abalone_spline,abalone_train)

TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_spline_predict-abalone_train[,9])^2)
RSS
(TSS-RSS)/TSS

#(i)
abalone_lm_predict <- predict(abalone_lm_fit,abalone_test_x)
TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_lm_predict-abalone_full[-split_sample,9])^2)
(TSS-RSS)/TSS
mean((abalone_lm_predict-abalone_full[-split_sample,9])^2)

abalone_lm_predict_all <- predict(abalone_lm_fit_all,abalone_test_x)
TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_lm_predict_all-abalone_full[-split_sample,9])^2)
(TSS-RSS)/TSS
mean((abalone_lm_predict_all-abalone_full[-split_sample,9])^2)

abalone_quad_5_predict <- predict(abalone_quad_5,abalone_test_x)
TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_quad_5_predict-abalone_full[-split_sample,9])^2)
(TSS-RSS)/TSS
mean((abalone_quad_5_predict-abalone_full[-split_sample,9])^2)

abalone_quad_8_predict <- predict(abalone_quad_8,abalone_test_x)
TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_quad_8_predict-abalone_full[-split_sample,9])^2)
(TSS-RSS)/TSS
mean((abalone_quad_8_predict-abalone_full[-split_sample,9])^2)


cv_ridge_predict <- predict(ridge_glmnet,s=cv_ridge$lambda.min,test_x)
mean((cv_ridge_predict-abalone_full[-split_sample,9])^2)

abalone_forward_predict <- predict(abalone_forward,abalone_test_x)
TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_forward_predict-abalone_full[-split_sample,9])^2)
(TSS-RSS)/TSS
mean((abalone_forward_predict-abalone_full[-split_sample,9])^2)

abalone_spline_predict <- predict(abalone_spline,abalone_test_x)
TSS = sum((abalone_train[,9]-mean(abalone_train[,9]))^2)
RSS = sum((abalone_spline_predict-abalone_full[-split_sample,9])^2)
(TSS-RSS)/TSS
mean((abalone_spline_predict-abalone_full[-split_sample,9])^2)

#Q3
set.seed(999)
X1 <- rnorm(20,20,5)
plot(X1)
X2 <- rnorm(20,0.1)
plot(X2)
X3 <- rbeta(20,50,5)
plot(X3)
#Y <- (X2+X3)^2+X1+X3+10
Y <- X1*200 + X2/X3 + X3^2
#Y <- X1*200 + X2/X3 + X3^2
plot(X1,Y)
plot(X2,Y)
plot(X3,Y)
lm__fit <- lm(Y~X1+X2+X3)
summary(lm__fit)
lm_fit_1 <- lm(Y~X1)
summary(lm_fit_1)
lm_fit_2 <- lm(Y~X2)
summary(lm_fit_2)
lm_fit_3 <- lm(Y~X3)
summary(lm_fit_3)

X4 <- rnorm(20,20,5)
plot(X4)
X5 <- X4*rnorm(20,0,1)
plot(X5)
X6 <- 7*X4+100
plot(X6)
Y2 <- X4*X5*X6
plot(Y2,X4)
plot(Y2,X5)
plot(Y2,X6)
lm_Y2 <- lm(Y2~X4+X5+X6)
summary(lm_Y2)
lm_X4 <- lm(Y2~X4)
summary(lm_X4)
lm_X5 <- lm(Y2~X5)
summary(lm_X5)
lm_X6 <- lm(Y2~X6)
summary(lm_X6)


