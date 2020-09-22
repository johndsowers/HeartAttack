#####################################################
#           Load Packages (if Required)             #
#####################################################

if(!require(githubinstall)) install.packages("githubinstall", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(splines2)) install.packages("splines2", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(stringi)) install.packages("stringi", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(rmarkdown)) install.packages("rmarkdown", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(tinytex)) install.packages("latexpdf", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(tinytex)) install.packages("tinytex", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(packrat)) install.packages("packrat", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(rsconnect)) install.packages("rsconnect", repos = "http://cran.us.r-project.org", quiet=TRUE)
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org", quiet=TRUE)

#####################################################
#                 Load Libraries                    #
#####################################################

library(githubinstall)
library(RCurl)
library(httr)
library(tidyverse)
library(lubridate)
library(caret)
library(gam)
library(foreach)
library(splines2)
library(stringi)
library(data.table)
library(gridExtra)
library(ggplot2)
library(rmarkdown)
library(knitr)
library(devtools)
library(latexpdf)
library(tinytex)
library(packrat)
library(rsconnect)
library(readxl)

#####################################################
#            Reset Variables / Set Options          #
#####################################################

rm(list=ls())  # Clears all variables
options(digits=5) 

#####################################################
#       Kaggle Heart Attack Prediction Data         # 
#####################################################

##### GitHub .csv Data File - Unzipped frin original Kaggle URL

KaggleURL <- "https://www.kaggle.com/nareshbhat/health-care-data-set-on-heart-attack-possibility"
GitHubURL <- "https://github.com/johndsowers/HeartAttack/raw/master/heart.csv"
dl <- tempfile()
download.file(GitHubURL, dl)
heart <- as.data.frame(read_csv(dl))
heart <- heart %>% mutate(target=as.numeric(target))
heart

#####################################################
#     Creating 90% Train / 10% Test Data Sets       #
#####################################################

set.seed(1) # Set Seed to 1
test_index <- createDataPartition(y = heart$target, times = 1, p = 0.1, list = FALSE)
training <- heart[-test_index,]  # Create Training Data Set
testing <- heart[test_index,] # Create Test Data Set

#####################################################
#                 INTRODUCTION                      #
#        (Purpose / Initial Data Analysis)          #
#####################################################

##### Reference https://artificialintelligence-news.com/2019/05/14/ml-algorithm-predicts-heart-attacks/
##### Possibility of recreating this 90% prediction rate utiling the 13 predictors in the Kaggle website

#####################################################
#            Initial Data Analysis                  #
#####################################################

################# Data Set Overview #################

str(heart)

#####################################################
#              METHODOLOGY / ANALYSIS               #
#        (Purpose / Initial Data Analysis)          #
#####################################################

################## Analysis by Age ##################

##### Patient Age Proportions Figure

age_p_distribution.fig <- heart %>% 
   group_by(age) %>% 
   summarize(p = length(target)/nrow(heart), .groups="drop") %>%
   ggplot(aes(age, p)) +
      geom_bar(stat="identity") +
      geom_hline(yintercept=.0244, size=1.0, color="red") +
      annotate("text", x = 33.5, y = .0264, label = "Mean = .0244") +
      ggtitle("Patient Distribution by Age") +
      xlab("Age (Years)") +
      ylab("Proportion of Patients") 

#####  Confirmed Heart Disease Age Proportions Figure

age_p_disease.fig <- heart %>% 
   group_by(age) %>% 
   mutate(n_total = length(age)) %>%
   summarize(prop_disease = sum(target==1)/n_total, prop_health= sum(target==0)/n_total, .groups="drop") %>% 
   ggplot(aes(age, prop_disease)) +
      geom_point() +
      geom_smooth(method = "loess", formula='y ~ x') + 
      geom_hline(yintercept=0.545, size=1.0, color="red") +
      annotate("text", x = 34.5, y = .575, label = "Mean = 0.545") +
      ylim(0, 1) +
      ggtitle("Heart Disease Proportions by Age") +
      xlab("Age (Years)") +
      ylab("Proportion of Cases") 

grid.arrange(age_p_distribution.fig, age_p_disease.fig, ncol=2) # Plot proportion distributions

################## Analysis by Sex ##################

##### Patient Gender Proportions Figure

sex_p_distribution.fig <- heart %>% 
   group_by(sex) %>% 
   summarise(p = length(target)/nrow(heart), .groups="drop") %>%
   mutate(sex = ifelse(sex==0,"Female", "Male")) %>%
      ggplot(aes(sex, p)) +
      geom_bar(stat="identity") +
      ggtitle("Patient Distribution by Gender") +
      xlab("Gender") +
      ylab("Proportion of Patients") 

#####  Confirmed Heart Disease Gender Proportions Figure

sex_p_disease.fig <- heart %>%
   group_by(sex) %>% 
   summarise(p = sum(as.numeric(target)/length(target)), .groups="drop") %>% 
   mutate(sex = ifelse(sex==0,"Female", "Male")) %>%
   ggplot(aes(sex,p)) +
      geom_bar(stat="identity") +
      geom_hline(yintercept=.6, size=1.0, color="red") +
      annotate("text", x = 2.3, y = .62, label = "Mean = 0.6") +
      ggtitle("Heart Disease Proportions by Gender") +
      xlab("Gender") +
      ylab("Proportion of Cases")

grid.arrange(sex_p_distribution.fig, sex_p_disease.fig, ncol=2) # Plot proportion distributions

########### Analysis by Chest Pain (cp) #############
 
##### Patient chest pain Proportions Figure

cp_p_distribution.fig <- heart %>% 
   group_by(cp) %>%
   summarise(p=length(cp)/nrow(heart), .groups="drop") %>%
   ggplot(aes(cp, p)) +
      geom_bar(stat="identity") +
      ggtitle("Patient Distribution by CP") +
      xlab("Chest Pain (CP) Score") +
      ylab("Proportion of Patients") 

#####  Confirmed Heart Disease Chest Pain Proportions Figure

cp_p_disease.fig <- heart %>%
   group_by(cp) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(cp,p)) +
   geom_bar(stat="identity") +
   geom_hline(yintercept=.645, size=1.0, color="red") +
   annotate("text", x = 0, y = .625, label = "Mean = 0.645") +
   ggtitle("Heart Disease Proportions by CP") +
   xlab("Chest Pain (CP) Score") +
   ylab("Proportion of Cases")

grid.arrange(cp_p_distribution.fig, cp_p_disease.fig, ncol=2) # Plot proportion distributions

### Analysis by Resting Blood Pressure (trestbps) ###

##### Patient BP Proportions Figure

trestbps_p_distribution.fig <- heart %>% 
   group_by(trestbps) %>%
   summarise(p=length(cp)/nrow(heart), .groups="drop") %>%
   ggplot(aes(trestbps, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by BP") +
   xlab("Blood Perssure (BP) Level") +
   ylab("Proportion of Patients") 

#####  Confirmed Heart Disease BP Proportions Figure

trestbps_p_disease.fig <- heart %>%
   group_by(trestbps) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(trestbps,p)) +
   geom_smooth(method='loess', formula='y ~ x') +
   geom_point() +
   #geom_bar(stat="identity") +
   geom_hline(yintercept=.51, size=1.0, color="red") +
   annotate("text", x = 190, y = .56, label = "Mean = 0.510") +
   ggtitle("Heart Disease Proportions by BP") +
   xlab("Blood Pressure (BP) Level") +
   ylab("Proportion of Cases")

grid.arrange(trestbps_p_distribution.fig, trestbps_p_disease.fig, ncol=2) # Plot proportion distributions

########## Analysis by Cholesterol (chol) ###########
 
##### Patient Age Cholesterol Figure

chol_p_distribution.fig <- heart %>% 
   group_by(chol) %>%
   summarise(p=length(cp)/nrow(heart), .groups="drop") %>%
   ggplot(aes(chol, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by Chol") +
   xlab("Cholesteral (Chol) Level") +
   ylab("Proportion of Patients") 

#####  Confirmed Heart Disease Cholesterol Proportions Figure

chol_p_disease.fig <- heart %>%
   group_by(chol) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(chol,p)) +

   geom_smooth(method='loess', formula='y ~ x') +
   geom_point() +
   geom_hline(yintercept=.521, size=1.0, color="red") +
   annotate("text", x = 510, y = .557, label = "Mean = 0.521") +
   ggtitle("Heart Disease Proportions by Chol") +
   xlab("Cholesterol (Chol) Level") +
   ylab("Proportion of Cases")

grid.arrange(chol_p_distribution.fig, chol_p_disease.fig, ncol=2) # Plot proportion distributions

########## Analysis by Blood Sugar (fbs) ############

##### Patient Blood Sugar Proportions Figure

fbs_p_distribution.fig <- heart %>% 
   group_by(fbs) %>%
   mutate(fbs=ifelse(exang==0,"False","True")) %>%  
   summarise(p=length(cp)/nrow(heart), .groups="drop") %>%
   ggplot(aes(fbs, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by FBS") +
   xlab("Fasting Blood Sugar (fbs) Level (>120 mg/dl)") +
   ylab("Proportion of Patients") 

#####  Confirmed Heart Disease Blood Sugar Proportions Figure

fbs_p_disease.fig <- heart %>%
   mutate(fbs=ifelse(exang==0,"False","True")) %>%
   group_by(fbs) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(fbs,p)) +
   geom_bar(stat="identity") +
   geom_hline(yintercept=.531, size=1.0, color="red") +
   annotate("text", x = 2.3, y = .545, label = "Mean = 0.531") +
   ggtitle("Heart Disease Proportions by FBS") +
   xlab("Fasting Blood Sugar (fbs) Level (>120 mg/dl)") +
   ylab("Proportion of Cases")

grid.arrange(fbs_p_distribution.fig, fbs_p_disease.fig, ncol=2) # Plot proportion distributions

#### Analysis by Electrocardiographic (restecg) #####

##### Patient ECG Results Proportions Figure

ekg_p_distribution.fig <- heart %>% 
   group_by(restecg) %>%
   summarise(p=length(cp)/nrow(heart), .groups="drop") %>%
   ggplot(aes(restecg, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by EKG") +
   xlab("Electrocardiogram (EKG) Score") +
   ylab("Proportion of Patients") 

#####  Confirmed Heart Disease ECG Proportions Figure

ekg_p_disease.fig <- heart %>%
   group_by(restecg) %>% 
   summarise(p = sum(as.numeric(target)/length(target)), .groups="drop") %>% 
   ggplot(aes(restecg,p)) +
   geom_smooth(method='loess', formula='y ~ x') +
   geom_point() +
   geom_hline(yintercept=.333, size=1.0, color="red") +
   annotate("text", x = 0.25, y = .343, label = "Mean = 0.333") +
   ggtitle("Heart Disease Proportions by FBS") +
   xlab("Electrocardiogram (EKG) Score") +
   ylab("Proportion of Cases")

grid.arrange(ekg_p_distribution.fig, ekg_p_disease.fig, ncol=2) # Plot proportion distributions

####### Analysis by Max Heart Rate (thalach) ########

##### Patient Max Heart Rate Proportions Figure

hr_p_distribution.fig <- heart %>% 
   group_by(thalach) %>%
   summarise(p=length(cp)/nrow(heart), .groups="drop") %>%
   ggplot(aes(thalach, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by HR") +
   xlab("Maximum Heart Rate (HR)") +
   ylab("Proportion of Patients") 

#####  Confirmed Heart Disease Max Heart Rate Proportions Figure

hr_p_disease.fig <- heart %>%
   group_by(thalach) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(thalach,p)) +
   geom_smooth(method='loess', formula='y ~ x') +
   geom_point() +
   geom_hline(yintercept=.491, size=1.0, color="red") +
   annotate("text", x = 85, y = .527, label = "Mean = 0.491") +
   ggtitle("Heart Disease Proportions by HR") +
   xlab("Maximum Heart Rate (HR)") +
   ylab("Proportion of Cases")

grid.arrange(hr_p_distribution.fig, hr_p_disease.fig, ncol=2) # Plot proportion distributions

########### Analysis by Angina (exang) ##############

##### Patient Angina Proportions Figure

ang_p_distribution.fig <- heart %>% 
   mutate(exang=ifelse(exang==0,"Not Present","Present")) %>%
   group_by(exang) %>%
   summarise(p=length(cp)/nrow(heart), .groups="drop") %>%
   ggplot(aes(exang, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by Angina") +
   xlab("Exercise Induced Angina") +
   ylab("Proportion of Patients") 

#####  Confirmed Heart Disease Angina Proportions Figure

ang_p_disease.fig <- heart %>%
   mutate(exang=ifelse(exang==0,"Not Present","Present")) %>%
   group_by(exang) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(exang,p)) +
   geom_bar(stat="identity") +
   geom_hline(yintercept=.464, size=1.0, color="red") +
   annotate("text", x = 2.2, y = .484, label = "Mean = 0.464") +
   ggtitle("Heart Disease Proportions by Angina") +
   xlab("Exercise Induced Angina") +
   ylab("Proportion of Cases")

grid.arrange(ang_p_distribution.fig, ang_p_disease.fig, ncol=2) # Plot proportion distributions

################ Analysis by oldpeak #################

##### Patient Oldpeak Proportions Figure

oldpeak_p_distribution.fig <- heart %>% 
   group_by(oldpeak) %>%
   summarise(p=length(oldpeak)/nrow(heart), .groups="drop") %>%
   ggplot(aes(oldpeak, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by ST") +
   xlab("ST Depression Score") +
   ylab("Proportion of Patients")  

#####  Confirmed Heart Disease Oldpeak Proportions Figure

oldpeak_p_disease.fig <- heart %>%
   group_by(oldpeak) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(oldpeak,p)) +
   geom_smooth(method='loess', formula='y ~ x') +
   geom_point() +
   geom_hline(yintercept=.393, size=1.0, color="red") +
   annotate("text", x = 5.5, y = .427, label = "Mean = 0.393") +
   ggtitle("Heart Disease Proportions by ST") +
   xlab("ST Depression Score") +
   ylab("Proportion of Cases")

grid.arrange(oldpeak_p_distribution.fig, oldpeak_p_disease.fig, ncol=2) # Plot proportion distributions

################# Analysis by slope #################

##### Patient slope Proportions Figure

slope_p_distribution.fig <- heart %>% 
   group_by(slope) %>%
   summarise(p=length(slope)/nrow(heart), .groups="drop") %>%
   ggplot(aes(slope, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by Slope") +
   xlab("Slope") +
   ylab("Proportion of Patients")  

#####  Confirmed Heart Disease Slope Proportions Figure

slope_p_disease.fig <- heart %>%
   group_by(slope) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(slope,p)) +
   geom_smooth(method='loess', formula='y ~ x') +
   geom_point() +
   geom_hline(yintercept=.693, size=1.0, color="red") +
   annotate("text", x = 0.23, y = .703, label = "Mean = 0.693") +
   ggtitle("Heart Disease Proportions by Slope") +
   xlab("Slope") +
   ylab("Proportion of Cases")

grid.arrange(slope_p_distribution.fig, slope_p_disease.fig, ncol=2) # Plot proportion distributions

################### Analysis by ca ##################

##### Patient Blood Vessel Numbers Proportions Figure

ca_p_distribution.fig <- heart %>% 
   group_by(ca) %>%
   summarise(p=length(oldpeak)/nrow(heart), .groups="drop") %>%
   ggplot(aes(ca, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by Flourosopy") +
   xlab("Number Blood Vessels Colored by Flourosopy") +
   ylab("Proportion of Patients")  

#####  Confirmed Heart Disease Blood Vessels Proportions Figure

ca_p_disease.fig <- heart %>%
   group_by(ca) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(ca,p)) +
   geom_smooth(method='loess', formula='y ~ x') +
   geom_point() +
   geom_hline(yintercept=.44, size=1.0, color="red") +
   annotate("text", x = 0.35, y = .455, label = "Mean = 0.44") +
   ggtitle("Heart Disease Proportions by Flourosopy") +
   xlab("Number Blood Vessels Colored by Flourosopy") +
   ylab("Proportion of Cases")

grid.arrange(ca_p_distribution.fig, ca_p_disease.fig, ncol=2) # Plot proportion distributions

################## Analysis by thal #################

##### Patient Thalassemia Proportions Figure

thal_p_distribution.fig <- heart %>% 
   group_by(thal) %>%
   summarise(p=length(oldpeak)/nrow(heart), .groups="drop") %>%
   ggplot(aes(thal, p)) +
   geom_bar(stat="identity") +
   ggtitle("Patient Distribution by Thalassemia") +
   xlab("Thalassemia") +
   ylab("Proportion of Patients")  

#####  Confirmed Heart Disease Thalassemia Proportions Figure

thal_p_disease.fig <- heart %>%
   group_by(thal) %>% 
   summarise(p = sum(as.numeric(target))/length(target), .groups="drop") %>% 
   ggplot(aes(thal,p)) +
   geom_smooth(method='loess', formula='y ~ x') +
   geom_point() +
      geom_hline(yintercept=.464, size=1.0, color="red") +
   annotate("text", x = 0.33, y = .474, label = "Mean = 0.464") +
   xlab("Thalassemia") +
   ylab("Proportion of Cases")

grid.arrange(thal_p_distribution.fig, thal_p_disease.fig, ncol=2) # Plot proportion distributions

#####################################################
#                       RESULTS                     #
#                      (Summary)                    #
#####################################################

##### Initialize Variables

method <- c("lm", "glm", "loess", "knn", "rf", "ensemble")
accuracy <- c(NA, NA, NA, NA, NA, NA)
heart_models <- data.frame(method, accuracy)

predictor <- c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal")
importance <- data.frame(predictor=predictor)

##### Linear Regression (LM) Model

# Train
train_lm <- train(target~., method="lm", data=training)

# Predict
predict_lm <- round(predict(train_lm, newdata=testing))

# Confusion Matrix
cm_lm <- confusionMatrix(as.factor(predict_lm), as.factor(testing$target))
cm_lm$byClass
heart_models[1,2] <- cm_lm$overall[["Accuracy"]]
heart_models[1,]

# Variable Importance 
imp_lm <- data.frame(predictor=row.names(varImp(train_lm)$importance), 
          lm=varImp(train_lm)$importance$Overall) 
importance <- right_join(importance,imp_lm, by="predictor")
importance[,c(1,2)] %>% arrange(desc(lm))

##### Logistic Regression (GLM) Model

# Train
train_glm <- train(target~., method="glm", data=training)

# Predict
predict_glm <- round(predict(train_glm, newdata=testing))

# Confusion Matrix
cm_glm <- confusionMatrix(as.factor(predict_glm), as.factor(testing$target))
cm_glm$byClass
heart_models[2,2] <- cm_glm$overall[["Accuracy"]]
heart_models[2,]

# Variable Importance
imp_glm <- data.frame(predictor=row.names(varImp(train_glm)$importance), 
                     glm=varImp(train_glm)$importance$Overall) 
importance <- right_join(importance,imp_glm, by="predictor")
importance[,c(1,3)] %>% arrange(desc(glm))

#####(Loess)

# Train
train_loess <- train(target~., method="gamLoess", data=training)

# Predict
predict_loess <- round(predict(train_loess, newdata=testing))

# Confusion Matrix
cm_loess <- confusionMatrix(as.factor(predict_loess), as.factor(testing$target))
cm_loess$byClass
heart_models[3,2] <- cm_loess$overall[["Accuracy"]]
heart_models[3,]

# Variable Importance
imp_loess <- data.frame(predictor=row.names(varImp(train_loess)$importance), 
                      loess=varImp(train_loess)$importance$Overall) 
importance <- right_join(importance,imp_loess, by="predictor")
importance[,c(1,4)] %>% arrange(desc(loess))
 
##### K-Nearest Neighbors (KNN)

# Train
train_knn <- train(target~., method="knn", data=training, tuneGrid = data.frame(k=seq(0:5)))
train_knn$bestTune

# Predict
predict_knn <- round(predict(train_knn, newdata=testing))

# Confusion Matrix
cm_knn <- confusionMatrix(as.factor(predict_knn), as.factor(testing$target))
cm_knn$byClass
heart_models[4,2] <- cm_knn$overall[["Accuracy"]]
heart_models[4,]

# Variable Importance
imp_knn <- data.frame(predictor=row.names(varImp(train_knn)$importance), 
                        knn=varImp(train_knn)$importance$Overall) 
importance <- right_join(importance,imp_knn, by="predictor")
importance[,c(1,5)] %>% arrange(desc(knn))

##### RF (Random Forest)

# Train
train_rf <- train(target~., method="rf", data=training, tuneGrid = data.frame(mtry=c(1,2,4,8)))
train_rf$bestTune

# Predict
predict_rf <- round(predict(train_rf, newdata=testing))

# Confusion Matrix
cm_rf <- confusionMatrix(as.factor(predict_rf), as.factor(testing$target))
cm_rf$byClass
heart_models[5,2] <- cm_rf$overall[["Accuracy"]]
heart_models[5,]

#Variable Importance
imp_rf <- data.frame(predictor=row.names(varImp(train_rf)$importance), 
                      rf=varImp(train_rf)$importance$Overall) 
importance <- right_join(importance,imp_rf, by="predictor")
importance[,c(1,6)] %>% arrange(desc(rf))

################# Best Model - Ensemble ###############

# Train & Predict
predict_ensemble <- as.factor(round((as.numeric(predict_lm)+as.numeric(predict_glm)+ (predict_rf))/3))

# Confusion Matrix
cm_ensemble <- confusionMatrix(as.factor(predict_ensemble), as.factor(testing$target))
cm_ensemble$byClass
heart_models[6,2] <- cm_ensemble$overall[["Accuracy"]]
heart_models[6,]

#Variable Importance
importance <- importance %>% mutate(ensemble=(lm+glm+rf)/3)
importance[,c(1,7)] %>% arrange(desc(ensemble))

#####################################################
#                      CONCLUSION                   #
#             (Limitations / Further Studies)       #
#####################################################