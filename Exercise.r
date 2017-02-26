# Libraries
library(readxl)
library(dplyr)
library(anytime)
library(reshape2)
library(ggplot2)
library(randomForest)
library(e1071)
library(survival)
library(glmnet)

# Read File
questions<-read_excel("Exercise.xlsx", sheet = 1, col_names = FALSE)
var.def<-read_excel("Exercise.xlsx", sheet = 2, col_names = FALSE)
Study<-read_excel("Exercise.xlsx", sheet = 3)

## Q1: Recode age into four categories (<65, 66-75, 75-85, 85+) and report the distributions.
# Recode Age column into oridinal variable and store into new column
Study$Age2 <- as.factor( # Define new column as a factor/categorical variable
  case_when(Study$s1 <= 65 ~ 'Young-Old',
            between(Study$s1, 66, 75) ~ 'Med-Old',
            between(Study$s1, 76, 85) ~ 'Old',
            Study$s1 >= 86 ~ 'Old-Old'
  )
)

# Conduct histogram to illustrate patients' age distribution
hist(as.numeric(Study$Age2), main = "Age Distribution", xlab = "Age Range")

# The sample (n = 209) consists of four age groups: <65 (n=77), 66-75 (n=44), 76-85 (n=40), 
# and 85+ (n=48).

## Q2: Assuming the data was collected on May 2016, calculate the duration of diagnosis 
## (in months) by using the month (q2_1) and year of diagnosis (q2_2).
# Merge month and year columns of diagnosis into one column
x <- with(Study, sprintf("%s %04d", q2_1, q2_2))
# Convert merged column into DateTime variable for later use
Diagnosis_Date<-data.frame(
  DateTime = as.POSIXct(paste0('01-',x), format = "%d-%m %Y"))
# Set Collection_Date as DateTime variable for later use
Collection_Date <- anydate("20160501000000") 
# Obtain Duration of Diagnosis by getting the time difference between Date of Diagnosis
# and Date of Collection
Study$Diagnosis_Duration<-as.vector(# Set time difference as days in order to obtain months
  difftime(Collection_Date, Diagnosis_Date[[1]], unit = "days")/365.25*12)
# Report summary of Duration of Diagnosis
summary(Study$Diagnosis_Duration)
# Illustrate frequency with a histogram
hist(Study$Diagnosis_Duration, xlab = "Duration of Diagnosis (in Months)", 
     main = "Duration of Diagnosis of Patients")

## Q3: Report the frequencies of first-line and second-line treatments. How do these 
## treatments differ statistically between those who are symptomatic (q1 = 1) versus 
## those who are not symptomatic (q1 = 0)?

# Report frequency of first-line and second-line treatments
colSums(Study[,5:8])
colSums(Study[,17:20])

# Prepare data for chi-square test
Q3_Study<-data.frame(t(data.frame(as.vector(colSums(Study[,5:8])), 
                                 as.vector(colSums(Study[,17:20])))),
                    c("First_Treatment", "Second_Treatment"))
colnames(Q3_Study)<-c("Anti-androgen", "Docetaxel", "Abiraterone", "Other", "Treatment")

# melt the data frame for plotting
Q3_Study.m <- melt(Q3_Study, id.vars='Treatment')

# plot everything
ggplot(Q3_Study.m, aes(Treatment, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

# Chi-square Test
chisq.test(as.matrix(Q3_Study[,-5]))

# For the first treatment, 102 patients reported receiving Anti-Androgen, 158 reported 
# receiving Docetaxel, 146 reported receiving Abiraterone, and 11 reported receiving Other.
# For the second treatment, 103 patients reported receiving Anti-Androgen, 162 reported 
# receiving Docetaxel, 166 reported receiving Abiraterone, and 7 reported receiving Other.
# Chi-square test results indicated that treatment type frequencies were not signicantly
# different across treatment phase, χ²(3, N = 209) = 1.71, p = 0.63.


## Q4: Calculate the mean (and standard deviation) duration of each first-line treatment. 
## Assuming that the first-line treatment ends exactly at the same time as the 
## *first* second-line treatment is initiated. Test this using the most appropriate 
## statistical test.

# Prepare month and year columns for each first-line treatment and merge into one
# dataframe
df1<-as.data.frame(Study[,9:10])
df1<-as.data.frame(apply(df1, 2, as.numeric))
df1$Treatment<-"anti-androgen"
df2<-as.data.frame(Study[,11:12])
df2<-as.data.frame(apply(df2, 2, as.numeric))
df2$Treatment<-"docetaxel"
df3<-as.data.frame(Study[,13:14])
df3<-as.data.frame(apply(df3, 2, as.numeric))
df3$Treatment<-"abiraterone"
df4<-as.data.frame(Study[,15:16])
df4<-as.data.frame(apply(df3, 2, as.numeric))
df4$Treatment<-"other"
t1<-as.data.frame(cbind(df1,df2,df3,df4))

# Convert dataframe from wide to long format for later use
test1<-do.call('rbind', lapply(seq(3, 12, by = 3), function(x) { y <- t1[,(x-2):x ]; 
y <- do.call("cbind", list(mo = colnames(y)[1], yr = colnames(y)[2], y ));
colnames(y)[3:4] <- c('mo_val', 'yr_val');
y }))
# Remove first two columns
test1<-test1[,c(-1,-2)]
# Define column names
colnames(test1)<-c("Month","Year", "Treatment")
# Merge month and year columns and redefine new column as datetime variable
T_One <- with(test1, sprintf("%s %04g", Month, Year))
First_Treatment_Date<-data.frame(
  DateTime = as.POSIXct(paste0('01-',T_One), format = "%d-%m %Y"))
First_Treatment_Date$Treatment<-test1$Treatment # Insert treatment variable for later use

# Subset Second Treatment Dates and Select the earliest date
# Subset Second Treatment Dates
df1<-as.data.frame(Study[,21:24])
df1<-as.data.frame(apply(df1, 2, as.numeric))
df2<-as.data.frame(Study[,25:28])
df2<-as.data.frame(apply(df2, 2, as.numeric))
# Convert dataframes from wide to long format
test1<-reshape(df1, direction="long", varying=split(names(df1), rep(seq_len(ncol(df1)/2), 2)))
test2<-reshape(df2, direction="long", varying=split(names(df2), rep(seq_len(ncol(df2)/2), 2)))
# Remove unwanted columns and merge remaining columns into new data frame
test1<-test1[,c(-1,-4)]
test2<-test2[,c(-1,-4)]
test3<-as.data.frame(cbind(test1,test2))
# Convert datarame from wide to long format and remove unwanted columns
test<-reshape(test3, direction="long", varying=split(names(test3), rep(seq_len(ncol(test3)/2), 2)))
test<-as.data.frame(as.matrix(test[,c(-1,-4)]))
colnames(test)<-c("Month","Year") # Rename Columns
# Merge month and year columns together, redefine merged column as DateTime variable
Anchor <- with(test, sprintf("%s %04g", Month, Year))
Anchor_Date<-data.frame(
  DateTime = as.POSIXct(paste0('01-', Anchor), format = "%d-%m %Y"))
sort(Anchor_Date[[1]]) # Sort through column to obtain earliest date
Anch_Date<-anydate("20070101000000") # Define earliest date for later use

# Obtain First Treatment Duration by getting the time difference between First Treatment
# Date and Earlist Second Treatment Date
first_treat_dura <-as.vector( # Set difference as days and convert result into months
  difftime(First_Treatment_Date[[1]], Anch_Date, unit = "days")/365.25*12)

# Merge important columns into new dataframe
Q4_Study<-cbind(First_Treatment_Date, first_treat_dura)
# Rename columns
colnames(Q4_Study)<-c("First_Treatment_Date", "Treatment", "First_Treatment_Duration")
Q4_Study$Treatment<-as.factor(Q4_Study$Treatment) # Set column as factor

# Obtain the mean and standard deviation of First Treatment Duration by Treatment Group
aggregate(Q4_Study[, 3], list(Q4_Study$Treatment), 
          function(x) c(mean = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE)))

# Apply Analysis of Variance of First Treatment Duration By Groups
Q4_aov<-aov(First_Treatment_Duration~Treatment, Q4_Study)

summary(Q4_aov) # Summary of Model Fit
model.tables(Q4_aov, "means", se = TRUE) # Groups Means and Grand Mean
TukeyHSD(Q4_aov) # Post-Hoc Test

# A one-way between subjects ANOVA was conducted to compare the effect of treatment 
# on the duration of first-line treatment in Anti-androgen, Docetaxel, Abiraterone, 
# and Other treatment conditions. There was not a significant effect of treatment on 
# the duration of first-line treatment for the four conditions 
# [F(3, 499) = 0.476, p = 0.699]. Tukey's Post-Hoc Test further supported that the
# treatment groups' length of time were not statistically significant.

## Q5: Is the duration of docetaxel use in first-line significantly different between 
## patients 75 and older vs. patients under 75? Test this using the most appropriate 
## statistical test.
# Subset dataframe for Analysis
Q5_df<-as.data.frame(Study[,c(1,11,12)])
Q5_df<-as.data.frame(apply(Q5_df, 2, as.numeric)) # Set column as numeric
colnames(Q5_df)<-c("Age","Month","Year") # Rename variables

# Merge columns, and redefine merged column as DateTime variable
Q5_T_One <- with(Q5_df, sprintf("%s %04g", Month, Year))
Q5_1st_Treat_Date<-data.frame(
  DateTime = as.POSIXct(paste0('01-',Q5_T_One), format = "%d-%m %Y"))
Q5_1st_Treat_Date$Age<-Q5_df$Age # Append new column
# Define Anchor Date (taken from Q4)
Anch_Date<-anydate("20070101000000") 
# Obtain time difference 
Q5_1st_Treat_Date$first_treat_dura <-as.vector( # set difference in days and then convert into months
  difftime(Q5_1st_Treat_Date[[1]], Anch_Date, unit = "days")/365.25*12)
# Dummy code variable variable in <75 and >= 75 age groups
Q5_1st_Treat_Date$Age_Dummy[Q5_1st_Treat_Date$Age < 75] <- 0
Q5_1st_Treat_Date$Age_Dummy[Q5_1st_Treat_Date$Age >= 75] <- 1

# Conduct Two-Sample T-test
t.test(first_treat_dura~Age_Dummy,Q5_1st_Treat_Date, paired=FALSE)
aggregate(first_treat_dura ~ Age_Dummy, Q5_1st_Treat_Date, sd)

# A two-samples t-test was conducted to compare docetaxel use in first-line 
# treatment duration in under 75 and over/equal to 75 age groups.
# There was a not a significant difference in the scores for 
# under 75 (M=27.82, SD=21.33) and equal to and over 75 (M=29.29, SD=21.44) age groups; 
# t(129.04)=-0.41, p = 0.68.

## Q6: Please predict the number of hospitalizations from age, symptomatic status, 
## years diagnosed, and first-line treatment. Use whichever regression technique you 
## believe is most appropriate.

# Set Training Data
set.seed(1)
train = sample(1:nrow(Study), nrow(Study)/2)

# Build Linear Regression Model
Q6_m<-lm(q8~Age2+q1+Diagnosis_Duration+q3_1+q3_2+q3_3+q3_4, Study, subset = train, na.action=na.omit)
# Predict training data on rest of the data and obtain y-hats
yhat_lm=predict(Q6_m, newdata=Study[-train,])
# Store observed responses
Q6_test=Study[-train,"q8"]
# Plot predicted and observed responses
plot(yhat_lm,Q6_test[[1]])
abline(0,1) # AB Line
cor(yhat_lm,Q6_test[[1]]) # Produce correlaton
mean((yhat_lm-Q6_test[[1]]^2)) # Produce mean
boxplot((yhat_lm-Q6_test[[1]]^2)) # Produce boxplot
summary(Q6_m) # Summary of Model Fit
# Do Random Forest
set.seed(1)
Q6_rf<-randomForest(q8~Age2+q1+Diagnosis_Duration+q3_1+q3_2+q3_3+q3_4, 
                    Study, subset = train, mtry=7, importance=TRUE)
# Predict training data on rest of the data and obtain y-hats
yhat_rf=predict(Q6_rf, newdata=Study[-train,])
# Plot predicted and observed responses
plot(yhat_rf,Q6_test[[1]])
abline(0,1) # AB Line
cor(yhat_rf,Q6_test[[1]]) # Produce correlation
mean((yhat_rf-Q6_test[[1]]^2)) # Produce MSE
boxplot((yhat_rf-Q6_test[[1]]^2)) # Produce boxplot
importance(Q6_rf) # Show variable importance
varImpPlot(Q6_rf)

# Linear Regression and Random Forest Regression techniques were applied to predict
# the number of hospitalizations based on age, symptomatic status, years diagnosed,
# and first-line treatment. Overall, both models reported great overall fit, with 
# Linear Regression at 84% variance explained and was found to be statistically significant
# [F(57,151)=20.14, p<.001], and Random Forest at 82% variance explained.

## Q7: Please predict ECOG and number of metastasis from age, symptomatic status, 
## years diagnosed, and first-line treatment. Use whichever regression technique you 
## believe is most appropriate.

# Prepare response variables for modeling
Study[["y3"]] <- paste0(Study$q5, Study$q7)
Study$y3<-as.numeric(Study$y3)

# Set training data
set.seed(1)
train = sample(1:nrow(Study), nrow(Study)/2)

# Build Linear Regression Model
Q7_m<-lm(y3~Age2+q1+Diagnosis_Duration+q3_1+q3_2+q3_3+q3_4, Study, subset = train, na.action=na.omit)
# Predict training data on rest of the data and obtain y-hats
yhat_lm=predict(Q7_m, newdata=Study[-train,])
# Store observed responses from non-training data
Q7_test=Study[-train,"y3"]
# Plot predicted vs observed observations from different data sets
plot(yhat_lm,Q7_test[[1]])
cor(yhat_lm,Q7_test[[1]]) # Product correlation between predicted and observed
abline(0,1) # AB Line
mean((yhat_lm-Q7_test[[1]]^2)) # MSE
boxplot((yhat_lm-Q7_test[[1]]^2)) # Boxplot

summary(Q7_m) # Summary of Model Fit
# Do Random Forest
set.seed(1)
Q7_rf<-randomForest(y3~Age2+q1+Diagnosis_Duration+q3_1+q3_2+q3_3+q3_4, 
                    Study, subset = train, mtry=7, importance=TRUE)
# Take training dataset and predict non training dataset, and store y-hats
yhat_rf=predict(Q7_rf, newdata=Study[-train,])
# Plot predicted vs observed observations from different data sets
plot(yhat_rf,Q7_test[[1]])
abline(0,1) # AB Line
cor(yhat_rf,Q7_test[[1]]) # correlation between predicted and observed
mean((yhat_rf-Q7_test[[1]]^2))     # MSE
boxplot((yhat_rf-Q7_test[[1]]^2))  # Boxplot
importance(Q7_rf) # Show variable importance
varImpPlot(Q7_rf)

# Linear Regression and Random Forest Regression techniques were applied to predict
# the number of hospitalizations based on age, symptomatic status, years diagnosed,
# and first-line treatment. Overall, both models reported great overall fit, with 
# Linear Regression at 89% variance explained and was found to be statistically significant
# [F(57,151)=30.16, p<.001], and Random Forest at 81% variance explained.

## Q8: Please predict the presence or absence of a bone metastatis from age, 
## symptomatic status, years diagnosed, and first-line treatment. Use whichever 
## regression technique you believe is most appropriate.

Q8_m<-glm(as.factor(q6)~Age2+q1+Diagnosis_Duration+q3_1+q3_2+q3_3+q3_4, 
          Study, family = 'binomial')
probs_lm=predict(Q8_m, Study, type = "response")

Q8_lm.pred=rep("Down",209)
Q8_lm.pred[probs_lm>.5]="Up"

table(Q8_lm.pred, Study$q6)

(0+105)/209

# Do SVM
Q8_svm<-svm(as.factor(q6)~Age2+q1+Diagnosis_Duration+q3_1+q3_2+q3_3+q3_4, 
                    Study, kernel="linear", cost=10, scale=FALSE, type = "C-classification",
            probability=TRUE)
probs_svm=predict(Q8_svm, Study, type="response", probability=TRUE)

Q8_svm.pred=rep("Down",209)
Q8_svm.pred[as.numeric(probs_svm)>.5]="Up"

table(Q8_svm.pred, Study$q6)

(0+105)/209

## Q9: Please conduct a survival analysis (Kaplan Meier; Cox regression) predciting 
## overall survival (the time from diagnosis to death). For patients who are still alive, 
## assuming the censoring date is May 2016. If conducting a Cox regression, use variables 
## you believe appropriate to predict overall survival.

## Obtain Censor Date
dat<-as.data.frame(sapply(Study[,c(3,4,34,35)], as.numeric ))
dat[is.na(dat)] <- " "
meh<-data.frame(Diagnosis_Date = with(dat, sprintf("%s %04s", q2_1, q2_2)),
                Deceased_Date = with(dat, sprintf("%s %04s", q10_1, q10_2))
)
meh[] <- lapply(meh, as.character)
meh$Diagnosis_Date <- apply(meh, 1, function(x) sample(x[trimws(x) != ""], 1))
meh$Diagnosis_Date<-as.Date(as.POSIXct(paste0('01-', meh$Diagnosis_Date), format = "%d-%m %Y"))
meh$Deceased_Date <- apply(meh, 1, function(x) sample(x[trimws(x) != ""], 1))
meh$Deceased_Date<-as.Date(as.POSIXct(paste0('01-', meh$Deceased_Date), format = "%d-%m %Y"))
meh$Collection_Date = anydate("20160501000000")
# Take Collection Date if Deceased Date is unavailable and store into new column
meh$Latest_Date = apply(meh[,2:3],1,min, na.rm=TRUE)

Study$Latest_Date <-meh$Latest_Date
Study$Diagnosis_Duration<-as.vector(difftime(meh$Latest_Date, 
                                            meh$Diagnosis_Date, unit = "days"))

## Convert Diagnosis Date, First Treatment Date, Second Treatment Date, and Death
# "q3_1mo" "q3_1yr" "q3_2mo" "q3_2yr" "q3_3mo" "q3_3yr" "q3_4mo" "q3_4yr"
dat <- as.data.frame(sapply(Study[,9:16], as.numeric ))
dat[is.na(dat)] <- " "
meh<-data.frame(Treat_One = with(dat, sprintf("%s %04s", q3_1mo, q3_1yr)),
                Treat_Two = with(dat, sprintf("%s %04s", q3_2mo, q3_2yr)),
                Treat_Three = with(dat, sprintf("%s %04s", q3_3mo, q3_3yr)),
                Treat_Four = with(dat, sprintf("%s %04s", q3_4mo, q3_4yr))
)
meh[] <- lapply(meh, as.character)
meh$five <- apply(meh, 1, function(x) sample(x[trimws(x) != ""], 1))
meh$five<-data.frame(as.POSIXct(paste0('01-', meh$five), format = "%d-%m %Y"))
colnames(meh[,5])<- "five"
meh$five<-as.Date(meh$five)

# "q4_1mo" "q4_1yr" "q4_2mo" "q4_2yr" "q4_3mo" "q4_3yr" "q4_4mo" "q4_4yr"
dat <- as.data.frame(sapply(Study[,21:28], as.numeric ))
dat[is.na(dat)] <- " "
mei<-data.frame(Treat_One = with(dat, sprintf("%s %04s", q4_1mo, q4_1yr)),
                Treat_Two = with(dat, sprintf("%s %04s", q4_2mo, q4_2yr)),
                Treat_Three = with(dat, sprintf("%s %04s", q4_3mo, q4_3yr)),
                Treat_Four = with(dat, sprintf("%s %04s", q4_4mo, q4_4yr))
)
mei[] <- lapply(mei, as.character)
mei$five <- apply(mei, 1, function(x) sample(x[trimws(x) != ""], 1))
mei$five<-data.frame(as.POSIXct(paste0('01-', mei$five), format = "%d-%m %Y"))
colnames(mei[,5])<- "five"
mei$five<-as.Date(mei$five)

Q9_Study<-data.frame(Diagnosis_Date = as.Date(as.POSIXct(paste0('01-',with(Study, sprintf("%s %04d", q2_1, q2_2))), format = "%d-%m %Y")),
                    First_Treat_Date = meh$five,
                    Second_Treat_Date = mei$five
)
Q9_Study<-as.data.frame(cbind(Study,Q9_Study))
Q9_Study<-Q9_Study[,c(-1,-3:-28,-30,-34:-35,-38)]

# First Model
survObject<- Surv(Study$Diagnosis_Duration, Study$q9)

Q9_Surv_model <- coxph(survObject~
                         Age2+q1+q5+q7+q8, 
                       Study) # Removed q6 because NA result
                             # +q8 not significant
step(Q9_Surv_model)

plot(survfit(Q9_Surv_model), 
     xscale = 365.25,
     xlab = "Years after diagnosis",
     ylab = "Proportion survived",
     main = "Baseline Hazard Curve")

## Second Model
## Lasso for variable selection before Fitting Survival Model
ezlasso=function(df,yvar,folds=10,trace=F,alpha=1){
  x<-model.matrix(as.formula(paste(yvar,"~.")),data=df)
  x=x[,-1] ##remove intercept
  
  glmnet1<-glmnet::cv.glmnet(x=x,y=df[,yvar],type.measure='mse',nfolds=folds,alpha=alpha)
  
  co<-coef(glmnet1,s = "lambda.1se")
  inds<-which(co!=0)
  variables<-row.names(co)[inds]
  variables<-variables[!(variables %in% '(Intercept)')];
  return( c(yvar,variables));
}

ezlasso(Q9_Study, yvar = Study$Diagnosis_Duration, folds=10, trace=F, alpha=1)

# Set training data
set.seed(1)
train = sample(1:nrow(Study), nrow(Study)/2)
test = (-train)
x = model.matrix(Diagnosis_Duration+q9~.,Q9_Study)[,c(-5,-7)]
y = Q9_Study[,c(5,7)]

lasso.mod<-glmnet(x[train,],y[train,], alpha=1,lambda=grid)
plot(lasso.mod)

# Third Model. Use Random Forest for variable selection

