library(dplyr) # data wrangling
library(lme4) # modeling
library(ggplot2) # visualization
library(arm) # standard errors
library(lmerTest)
read_csv = function(file){
  return(read.table(file,header=TRUE,sep=','))
}
library(lattice)

dataset=read_csv("#Link to the synthetic data#")
attach(dataset)
head(dataset)

dataset$Semester=dataset$Semester-1 # recentering time 
head(dataset)

modelA <- lmer(ECTS_cleared ~ 1 + (1|ID),  data = dataset, REML = FALSE)
summary(modelA)

modelB <- lmer(ECTS_cleared ~ Semester + (Semester|ID), data = dataset, REML = FALSE)
summary(modelB)


modelC <- lmer(ECTS_cleared ~ Gender*Semester + (Semester|ID), data = dataset, REML = FALSE)
summary(modelC)


modelD <- lmer(ECTS_cleared ~ Marital_status*Semester + ( Semester|ID), data = dataset, REML = FALSE)
summary(modelD)


modelE <- lmer(ECTS_cleared ~ Marital_status*Semester + Gender*Semester + (Semester|ID), data = dataset, REML = FALSE)
summary(modelE)

modelF <- lmer(ECTS_cleared ~ Marital_status*Semester + Gender + (Semester|ID), data = dataset, REML = FALSE)
summary(modelF)

modelG <- lmer(ECTS_cleared~ Gender*Semester + Marital_status + (Semester|ID), data = dataset, REML = FALSE)
summary(modelG)

modelH <- lmer(ECTS_cleared ~ Gender*Marital_status + Semester + (Semester|ID), data = dataset, REML = FALSE)
summary(modelH)

modelI <- lmer(ECTS_cleared ~ Marital_status*Semester + Marital_status*Gender + (Semester|ID), data = dataset, REML = FALSE)
summary(modelI)


# Testing the normality and homoscedasticity assumptions for Model I Starts


resid_I <- residuals (modelI)
qqnorm(resid_I, ylab = "Level 1 Residuals", xlab =  "Normal Score", abline(h=0), ylim=c(-15, 15),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
ran_I_level2_randomeffect <- ranef(modelI) #creating the randomeffects
qqnorm(ran_I_level2_randomeffect$ID[[1]],ylab = 'Level 2 Residuals For Intercept', xlab = 'Normal Score', ylim=c(-10,10), abline(h=0), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5 )
qqnorm(ran_I_level2_randomeffect$ID[[2]],ylab = 'Level 2 Residuals For Slope', xlab = 'Normal Score', ylim=c(-5,5), abline(h=0), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
plot(dataset$Semester,resid_I, ylim=c(-15, 15), ylab="Level 1 Residuals ",
     xlab="Level 1 Predictor: Semester", abline(h=0),cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

plot(dataset$Gender[dataset$Semester==0], ran_I_level2_randomeffect$ID[[1]], 
     ylab="Level 2 Residuals of Intercept", xlab=" Gender", ylim=c(-3, 3), abline(h=0), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

plot(dataset$Marital_status[dataset$Semester==0], ran_I_level2_randomeffect$ID[[1]], 
     ylab="Level 2 Residuals of Intercept", xlab=" Marital_Status", ylim=c(-3, 3), abline(h=0), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)


plot(dataset$Gender[dataset$Semester==0], ran_I_level2_randomeffect$ID[[2]],
     ylab="Level 2 Residuals of Slope", xlab=" Gender", ylim=c(-3, 3), abline(h=0), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

plot(dataset$Marital_status[dataset$Semester==0], ran_I_level2_randomeffect$ID[[2]],
     ylab="Level 2 Residuals of Slope", xlab= " Marital_Status",  ylim=c(-3, 3), abline(h=0), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

# Testing the normality and homoscedasticity assumptions for Model I Ends



# Pairwise model comparison using ANOVA

anova(modelA,modelI)
anova(modelB,modelI)
anova(modelC,modelI)
anova(modelD,modelI)
anova(modelE,modelI)
anova(modelF,modelI)
anova(modelG,modelI)
anova(modelH,modelI)



