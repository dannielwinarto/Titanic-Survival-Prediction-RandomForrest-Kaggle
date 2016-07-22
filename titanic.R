# VARIABLE DESCRIPTIONS:
# 
# survival        Survival
# (0 = No; 1 = Yes)
# pclass          Passenger Class
# (1 = 1st; 2 = 2nd; 3 = 3rd)
# name            Name
# sex             Sex
# age             Age
# sibsp           Number of Siblings/Spouses Aboard
# parch           Number of Parents/Children Aboard
# ticket          Ticket Number
# fare            Passenger Fare
# cabin           Cabin
# embarked        Port of Embarkation
# (C = Cherbourg; Q = Queenstown; S = Southampton)
# 
# SPECIAL NOTES:
#   Pclass is a proxy for socio-economic status (SES)
# 1st ~ Upper; 2nd ~ Middle; 3rd ~ Lower
# 
# Age is in Years; Fractional if Age less than One (1)
# If the Age is Estimated, it is in the form xx.5
# 
# With respect to the family relation variables (i.e. sibsp and parch)
# some relations were ignored.  The following are the definitions used
# for sibsp and parch.
# 
# Sibling:  Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
# Spouse:   Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)
# Parent:   Mother or Father of Passenger Aboard Titanic
# Child:    Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic
# 
# Other family relatives excluded from this study include cousins,
# nephews/nieces, aunts/uncles, and in-laws.  Some children travelled
# only with a nanny, therefore parch=0 for them.  As well, some
# travelled with very close friends or neighbors in a village, however,
# the definitions do not support such relations.

rm(list = ls())
setwd("C:/Users/dan_9/Desktop/COURSERA + SELF STUDY/Kaggle/Titanic")

library(dplyr)
library(ggplot2)
library(gridExtra)
library(rpart) # grow classification tree
library(randomForest)
library(stringr) 
library(missForest) # imputation
library(mice) # imputation
library(VIM) # visualization missing value

genderclassmodel = read.csv("genderclassmodel.csv")
gendermodel = read.csv("gendermodel.csv")
training = read.csv("train.csv")
testing = read.csv("test.csv")

glimpse(training)
head(training)
tail(training)
glimpse(testing)
head(testing)
tail(testing)

# adding column "Survived" on the testing dataset
testing$Survived = NA
# move this column to the 2nd for binding purpose
testing = testing[,c(1, grep("Survived", colnames(testing)), 2 :  (grep("Survived", colnames(testing))-1))]

full = rbind(training, testing)
glimpse(full)


######################################################################
# PART 1 - DATA CLEANING, MANIPULATION, AND EXPLORATORY ANALYSIS
######################################################################

# column name should be character instead of factor 
full$Name = as.character(full$Name)

# checking how many NA for each column for both training and testing data
trainingNAs = aggr(training, numbers=T, sortVars=F)
trainingNAs
testingNAs = aggr(testing, numbers=T, sortVars=F)
testingNAs
fullNAs = aggr(full, numbers=T, sortVars=F)
fullNAs

# We need to see Fare column and fill it with the value that make sense,
# in this case, we will fill the cell with based on the passanger's other attributes, compared with other passanger,
# which is Pclass, Age >=50, Embarked from Southampton

which(is.na(full$Fare))
# [1] 1044

full[1044,]
# PassengerId Survived Pclass               Name  Sex  Age SibSp Parch Ticket Fare Cabin    Embarked honorific GenderAgeClass FareClass Surname
# 1044        1044       NA      3 Storey, Mr. Thomas male 60.5     0     0   3701   NA       S         Mr.     Adult Male      <NA>  Storey

# to estimate the fare of passenger ID   1044, we need to find the mean value of people similar to his pattern:
full %>% filter(!is.na(Fare), Pclass == 3, Age >= 50, Embarked == "S" ) %>% summarise(mean(Fare)) 
# 8.43042 <- we need to fill the NA of passanger #1044's fare with this value

full$Fare[1044] = 8.43042

# Since column Age contains the most of NAs, 177 NAs, we better to do exploratory analysis with these raw data, before we perform
# imputation of missing values. Because our imputation may alter our view towards raw dataset if we perform the exploratory later

# In these exploratory analysis, we will exclude the observations with NAs in Age

plotAgeRaw1 = ggplot(full[!is.na(full$Age),], aes(x = Age )) +
  geom_histogram() +
  facet_grid(~Sex) +
  coord_cartesian(ylim =  seq(0,100,5))+
  scale_y_continuous(breaks = seq(0,100,5)) +
  scale_x_continuous(breaks = seq(0,90,10)) +
  ggtitle("Age Dsitribution of Passenger by Gender\n RAWDATA")

plotAgeRaw2 = ggplot(full[!is.na(full$Age),], aes(x = Age, fill = as.factor(Sex))) +
  geom_histogram(position = "identity", alpha = .4) +
  scale_fill_manual(values=c("green", "purple")) +
  coord_cartesian(ylim =  seq(0,100,5))+
  scale_y_continuous(breaks = seq(0,100,5)) +
  scale_x_continuous(breaks = seq(0,90,10)) +
  ggtitle("Age Dsitribution of Passenger by Gender\n RAWDATA") +
  labs(fill="")

medians = aggregate(Age ~  Sex, full, median)

plotAgeRaw3 = ggplot(full[!is.na(full$Age),], aes(x = Sex, y = Age)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0,80,5)) +
  geom_text( data = medians, aes( label = Age, y = Age), vjust = -.5) +
  ggtitle("Age Dsitribution of Passenger by Gender\n RAW")

plotAgeRaw1
plotAgeRaw2
plotAgeRaw3

# we would like to analyze based on honorifics system on their name, 
# and ultimately group it into adult male, female, young male

full$honorific = str_extract(full$Name, pattern = "\\,[ ][A-z]*[ ]*[A-z]*\\.")
full$honorific = gsub(", ","",full$honorific)

full$honorific = as.factor(full$honorific)
table(full$honorific)

# then we need to find the age range of each honorific

SurvivalRateByHonorific = full %>%
  filter(Survived != "NA") %>%
  group_by(honorific) %>%
  summarise(minAge = min(Age, na.rm = T),maxAge = max(Age, na.rm = T), PassengerCount = n(),
            PassengerSurvived =  sum(Survived == 1), survivalRate = round(sum(Survived == 1)/n(),3)) %>%
  arrange(desc(PassengerSurvived))
SurvivalRateByHonorific

# We can clearly see there are 18 honorific name being used in this dataset, 
# However we can classified all 18 into three different group:
# Female, young male, adult male
# The reason is because Miss and Mrs does not really tell the age of the a female.
# Furthermore, it is very clear that female has higher survival rate

full$GenderAgeClass[full$honorific ==  "Miss." | full$honorific ==  "Mrs."| full$honorific ==  "Mlle."| 
                      full$honorific ==  "Lady."|full$honorific ==  "Mme."|full$honorific ==  "Ms."| 
                      full$honorific ==  "the Countess."|full$honorific ==  "Dona."] = "Female"

full$GenderAgeClass[full$honorific ==  "Mr." |  full$honorific ==  "Dr."| full$honorific ==  "Col."|
                      full$honorific ==  "Major."| full$honorific ==  "Sir."|full$honorific ==  "Capt."|
                      full$honorific ==  "Don."| full$honorific ==  "Jonkheer."|full$honorific ==  "Rev."] = "Adult Male"

full$GenderAgeClass[full$honorific ==  "Master."] = "Young Male"

full$GenderAgeClass = as.factor(full$GenderAgeClass)

SurvivalRateByGenderAge = full %>%
  filter(Survived != "NA") %>%
  group_by(GenderAgeClass) %>%
  summarise(minAge = min(Age, na.rm = T),maxAge = max(Age, na.rm = T), PassengerCount = n(),
            PassengerSurvived =  sum(Survived == 1), survivalRate = round(sum(Survived == 1)/n(),3)) %>%
  arrange(desc(PassengerSurvived))
SurvivalRateByGenderAge

ggplot(SurvivalRateByGenderAge, aes(x = reorder(GenderAgeClass, -survivalRate), y = survivalRate, group = 1)) +
  geom_line()+
  geom_text(aes(label = survivalRate), vjust = -.3, hjust = -.3 , size = 5, color = "red") +
  ggtitle("Survival Rate by Gender-Age Class")+
  ylim(0,1)+
  xlab("Gender-Age Class") +
  ylab("Survival Rate")


ggplot(full[!is.na(full$Survived),], aes(x = Survived, y = GenderAgeClass)) +
  ggtitle("Gender-Age Class survival cases")+
  scale_x_continuous( breaks = seq(0,1,1))+
  xlab("0 = not-Survived \t 1 = Survived ") +
  ylab("Gender Age Class")+
  geom_jitter(width = .65, color = "blue")

# Intuitively, it seems like there are strong correlation between variable "pclass" and "fare"
# since pclass  = 1 means the highest and tend to have higher fare, then we expect strong negative correlation between there two variables
cor(full$Pclass[!is.na(full$Survived)], full$Fare[!is.na(full$Survived)])
# turned out its not as high as we expected, so that means it is necessary to keep both variables

boxplot(full$Fare, na.rm = T)
# for quantile, we used only the training data
Q = quantile(full$Fare[!is.na(full$Survived)], probs = seq(0,1,.2) )  

FareClass = matrix(ncol = 1, nrow = nrow(full)) 

for( i in 1 : length(Q)){
  FareClass[full$Fare >= Q[i] & full$Fare <= Q[i+1]] = i
}

table(FareClass)
FareClass = as.factor(FareClass)
levels(FareClass) = c("cheapest","cheap","medium","high","highest")
FareClass =  factor(FareClass, levels = rev(levels(FareClass)))
full$FareClass = FareClass

levels(full$Embarked) = c("Unknown", "Cherbourg", "Queenstown", "Southampton")
table(full$Embarked)

ggplot(data = full, aes(x = Embarked, fill = FareClass)) +
  geom_bar()

SurvivalRateByFareClass = full %>%
  filter(Survived != "NA") %>%
  group_by(FareClass) %>%
  summarise(PassengerCount = n(),PassengerSurvived =  sum(Survived == 1),
            survivalRate = round(sum(Survived == 1)/n(),3)) %>%
  arrange(desc(FareClass))
SurvivalRateByFareClass

ggplot(SurvivalRateByFareClass, aes(x = FareClass, y = survivalRate, group = 1)) +
  geom_line(size = 1.2)+
  geom_text(aes(label = survivalRate), vjust = -.3, hjust = -.3 , size = 5, color = "red") +
  ggtitle("Survival Rate by Fare Class")+
  ylim(0,1)+
  xlab("Fare Class") +
  ylab("Survival Rate")

SurvivalRateByPclass = full %>%
  filter(Survived != "NA") %>%
  group_by(Pclass) %>%
  summarise(PassengerCount = n(),PassengerSurvived =  sum(Survived == 1),
            survivalRate = round(sum(Survived == 1)/n(),3)) %>%
  arrange(desc(Pclass))
SurvivalRateByPclass

ggplot(SurvivalRateByPclass, aes(x = factor(Pclass), y = survivalRate, group = 1)) +
  geom_line(size = 1.2)+
  geom_text(aes(label = survivalRate), vjust = -.3, hjust = -.3 , size = 5, color = "red") +
  ggtitle("Survival Rate by Passenger Class")+
  ylim(0,1)+
  xlab("Passenger Class") +
  ylab("Survival Rate")

SurvivalRateByGenderSib = full %>%
  filter(Survived != "NA") %>%
  group_by(Sex, SibSp) %>%
  summarise(PassengerCount = n(),PassengerSurvived =  sum(Survived == 1),
            survivalRate = round(sum(Survived == 1)/n(),3))
SurvivalRateByGenderSib

ggplot( SurvivalRateByGenderSib, aes(x = factor(SibSp), y = survivalRate, group = interaction(Sex), color = Sex)) +
  geom_line(size = 1.2) +
  geom_text(aes(label = survivalRate), vjust = -.3, hjust = -.3 , size = 5, color = "red") +
  ggtitle("Survival Rate by Sex - Number of Siblings aboard")+
  ylim(0,1)+
  xlab("# of sibs") +
  ylab("Survival Rate")

SurvivalRateByGenderParch = full %>%
  filter(Survived != "NA") %>%
  group_by(Sex, Parch) %>%
  summarise(PassengerCount = n(),PassengerSurvived =  sum(Survived == 1),
            survivalRate = round(sum(Survived == 1)/n(),3))
SurvivalRateByGenderParch

ggplot( SurvivalRateByGenderParch, aes(x = factor(Parch), y = survivalRate, group = interaction(Sex), color = Sex)) +
  geom_line(size = 1.2) +
  geom_text(aes(label = survivalRate), vjust = -.3, hjust = -.3 , size = 5, color = "red") +
  ggtitle("Survival Rate by Sex - Number of Parent-children aboard")+
  ylim(0,1)+
  xlab("# of Parent-Children") +
  ylab("Survival Rate")

# Among the people who travels with siblings/parents, are they survived together or not, this method is not perfect due to limitation of
# knowing whos travel with who, the method that we used here is just based on the surname, a spouse may have different surname which may cause
# error in this analysis

full$Surname = str_extract(full$Name, pattern = "[A-z]*" )

SurvivalRateSibSp = full %>%   filter(Survived != "NA" & SibSp != 0) %>%
  group_by(SibSp, Surname) %>%
  summarise(PassengerCount = n(),PassengerSurvived =sum(Survived == 1),
            survivalRate = round(sum(Survived == 1)/n(),3)) %>%
  arrange(desc(survivalRate))

ggplot(SurvivalRateSibSp, aes(x = SibSp, y = survivalRate)) +
  scale_x_continuous( breaks = seq(0,8,1))+
  scale_y_continuous( breaks = seq(0,1,.5))+
  xlab("# of siblings ") +
  ylab("Survival Rate") +
  ggtitle("Survival Rate per case by number of siblings") +
  geom_jitter(width =.5, color = "blue")

# observation : Most people with siblings/ traveling together chose to either survive together or not survive together

######################################################################
# PART 2 - IMPUTATION OF MISSING VALUES USING MULTIPLE ALGORITHM (MEDIAN, MISSFORREST, AND MICE)
# We ended up using missforrest algorithm, which yield us the best score among three
######################################################################


# Next, we need to work on NAs in Age column, we will use library(missRandom) and library(mice) for the imputation of missing values 
# in Age column

# To compare before and after missing value imputation, we will do exploratory analysis before and after missing value imputation

# 1st approach we predict the missing Age using median by gender/sex 
# this approach yield to the best score we can achieve so far 

# medians = aggregate(Age ~  Sex, full, median)
# medians

# full$Age[is.na(full$Age) & full$Sex == "female"] = 27
# full$Age[is.na(full$Age) & full$Sex == "male"] = 28

# 2nd approach we predict the missing Age using missForrest Package, which predict missing values using random forrest 
# eliminating columns: PassengerId,Survived,Name,Parch, Ticket,Cabin, because Age should not be correlated to these columns

temp = full[-c(1,2,4,8,9,11,16)]
head(temp)
set.seed(100)
Age.rf = missForest(temp)
full$Age = Age.rf$ximp$Age
summary(temp)

plotAge.rf1 = ggplot(full[!is.na(full$Age),], aes(x = Age )) +
  geom_histogram() +
  facet_grid(~Sex) +
  coord_cartesian(ylim =  seq(0,150,5))+
  scale_y_continuous(breaks = seq(0,150,5)) +
  scale_x_continuous(breaks = seq(0,90,10)) +
  ggtitle("Age Dsitribution of Passenger by Gender\n MISSFORREST")

plotAge.rf2 = ggplot(full[!is.na(full$Age),], aes(x = Age, fill = as.factor(Sex))) +
  geom_histogram(position = "identity", alpha = .4) +
  scale_fill_manual(values=c("green", "purple")) +
  scale_y_continuous(breaks = seq(0,150,5))+
  scale_x_continuous(breaks = seq(0,90,10)) +
  ggtitle("Age Dsitribution of Passenger by Gender\n MISSFORREST") +
  labs(fill="")

grid.arrange(plotAgeRaw2, plotAge.rf2 , ncol=2)
grid.arrange(plotAgeRaw1, plotAge.rf1 , ncol=2)

# 3rd approach we predict the missing Age using MICE Package, this time we will use "pmm" method, (predictive mean matching)
# eliminating columns: PassengerId,Survived,Name,Parch, Ticket,Cabin, because Age should not be correlated to these columns
# temp2 = full[-c(1,2,4,9,11,16)]
# head(temp2)
# set.seed(100)
# micePmm = mice(temp2 , method = "norm")
# # micePmm$imp$Age$`3`
# micePmm$method
# temp2  = complete(micePmm,3)
# full$Age = temp2$Age
# 
# plotAge.mice1 = ggplot(full[!is.na(full$Age),], aes(x = Age )) +
#   geom_histogram() +
#   facet_grid(~Sex) +
#   coord_cartesian(ylim =  seq(0,100,5))+
#   scale_y_continuous(breaks = seq(0,100,5)) +
#   scale_x_continuous(breaks = seq(0,90,10)) +
#   ggtitle("Age Dsitribution of Passenger by Gender\n MICE")
# 
# plotAge.mice2 = ggplot(full[!is.na(full$Age),], aes(x = Age, fill = as.factor(Sex))) +
#   geom_histogram(position = "identity", alpha = .4) +
#   scale_fill_manual(values=c("green", "purple")) +
#   scale_y_continuous(breaks = seq(0,100,5))+
#   scale_x_continuous(breaks = seq(0,90,10)) +
#   ggtitle("Age Dsitribution of Passenger by Gender\n MICE") +
#   labs(fill="")
# 
# medians = aggregate(Age ~  Sex, full, median)
# 
# plotAge.mice3 = ggplot(full[!is.na(full$Age),], aes(x = Sex, y = Age)) +
#   geom_boxplot() +
#   scale_y_continuous(breaks = seq(0,80,5)) +
#   geom_text( data = medians, aes( label = Age, y = Age), vjust = -.5) +
#   ggtitle("Age Dsitribution of Passenger by Gender\n MICE")
# 
# 
# grid.arrange(plotAgeRaw1, plotAge.mice1 , ncol=2)
# grid.arrange(plotAgeRaw2, plotAge.mice2 , ncol=2)
# grid.arrange(plotAgeRaw3, plotAge.mice3 , ncol=2)


train = full[!is.na(full$Survived),]
test = full[is.na(full$Survived),]

summary(train)
summary(test)

# we need to tune our random forrest, we need to find the best number of variables "mtry"
set.seed(100)
tuneRF(x = train[,c(3,5,7,8,10,12,13,14,15,6)], 
       y = train[,2],
       stepFactor=1.5, 
       improve=1e-5)

set.seed(100)
titanic.rf = randomForest(factor(Survived) ~ Pclass + Sex + SibSp  + Parch + Fare + Embarked + honorific + GenderAgeClass + FareClass + Age ,
                          data = train,
                          mtry = 2)

plot(titanic.rf, ylim=c(0,0.36))
legend('topright', colnames(titanic.rf$err.rate), col=1:3, fill=1:3)

prediction = predict(titanic.rf, test)
submission = data.frame(PassengerId = test$PassengerId, Survived = prediction)
write.csv(submission, "submission.csv", row.names = FALSE)

importance(titanic.rf)



######################################################################
# END
######################################################################









######################################################################
# Experiment using Trees - rpart package
######################################################################

# library(rpart)
# set.seed(100)
# titanic.rp = rpart(factor(Survived) ~ Pclass + Sex + SibSp  + Parch + Fare + Embarked + honorific + GenderAgeClass + FareClass + Age ,
#                           data = train)
# 
# plot(titanic.rp, uniform =TRUE)
# text(titanic.rp, use.n = TRUE, all = TRUE) 
# prediction = predict(titanic.rp, test, type = "class")
# submission = data.frame(PassengerId = test$PassengerId, Survived = prediction)
# write.csv(submission, "submission.csv", row.names = FALSE)
