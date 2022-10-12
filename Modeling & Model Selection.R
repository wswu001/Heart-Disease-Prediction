require(ggplot2)
require(class)
require(caret)
require(MASS)

heart=read.csv("heart.csv")

## Split data
set.seed(999)
idx <- sample(1:nrow(heart), nrow(heart)*.9, replace = FALSE)
heart_train <- heart[idx, ]
heart_test <- heart[-idx, ]

########## Model Selection ########## 

## Built initial full model and null model
full.model = glm_fit <- glm(HeartDisease~., data = heart_train, family="binomial")
null.model = glm_fit <- glm(HeartDisease~1, data = heart_train, family="binomial")

n = nrow(heart_train)

## Bidirectional Stepwise
# BIC as criteria
step.fit = stepAIC(null.model, scope = list(upper = full.model, lower = ~1), trace=F, direction="both", k=log(n))

step.fit.summary <- step.fit$anova
step.fit.summary

# AIC as criteria
step.fit2 = stepAIC(null.model, scope = list(upper = full.model, lower = ~1), trace=F, direction="both", k=2)

step.fit.summary2 <- step.fit2$anova
step.fit.summary2

## Best Possible Subset
require(bestglm)

qwer = heart_train
#can only take in numeric and factors

qwer$Age = as.numeric(qwer$Age)
qwer$Sex = as.factor(qwer$Sex)
qwer$ChestPainType = as.factor(qwer$ChestPainType)
qwer$RestingBP = as.numeric(qwer$RestingBP)
qwer$Cholesterol = as.numeric(qwer$Cholesterol)
qwer$FastingBS = as.numeric(qwer$FastingBS)
qwer$RestingECG = as.factor(qwer$RestingECG)
qwer$MaxHR = as.numeric(qwer$MaxHR)
qwer$ExerciseAngina = as.factor(qwer$ExerciseAngina)
qwer$Oldpeak = as.numeric(qwer$Oldpeak)
qwer$ST_Slope = as.factor(qwer$ST_Slope)
qwer$HeartDisease = as.numeric(qwer$HeartDisease)


test = bestglm(qwer, family = binomial, IC = "BIC")
test$BestModel
# This is the same model as stepBIC

## Record formula from above results
step.BIC.formula = HeartDisease ~ ST_Slope + ChestPainType + Sex + FastingBS + ExerciseAngina + 
  Cholesterol + Oldpeak

step.AIC.formula = HeartDisease ~ ST_Slope + ChestPainType + Sex + FastingBS + ExerciseAngina + 
  Cholesterol + Oldpeak + Age

#best.subset.formula = HeartDisease ~ ST_Slope + ChestPainType + Sex + FastingBS + ExerciseAngina +  Cholesterol + Oldpeak 
