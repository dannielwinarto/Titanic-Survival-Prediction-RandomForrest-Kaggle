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
library(randomForest)
library(stringr)

genderclassmodel = read.csv("genderclassmodel.csv")
gendermodel = read.csv("gendermodel.csv")
training = read.csv("train.csv")
testing = read.csv("test.csv")
head(testing)

glimpse(training)
head(training)
tail(training)
head(testing)
tail(testing)

# adding column "Survived" on the testing dataset
testing$Survived = NA
# move this column to the 2nd for binding purpose
testing = testing[,c(1, grep("Survived", colnames(testing)), 2 :  (grep("Survived", colnames(testing))-1))]

full = rbind(training, testing)
glimpse(full)

# column name should be character instead of factor 
full$Name = as.character(full$Name)

# deleting the columns: ticket, cabin
# training$Ticket = NULL
# training$Cabin = NULL

# checking how many NA for each column
colnum = NULL
for(i in 1 : ncol(full)){
  colnum[i] = sum(is.na(full[,i]))
}


colNull  = data.frame(NULL)
for (i in 1:length(names(full))) {
  colNull[i,1] = names(full)[i]
  colNull[i,2] = sum(is.na(full[,i]))
}

# V1  V2
# 1  PassengerId   0
# 2     Survived 418
# 3       Pclass   0
# 4         Name   0
# 5          Sex   0
# 6          Age 263
# 7        SibSp   0
# 8        Parch   0
# 9       Ticket   0
# 10        Fare   1
# 11       Cabin   0
# 12    Embarked   0
# 13   honorific   0

which(is.na(full$Fare))
# [1] 1044

full[1044,]
# PassengerId Survived Pclass               Name  Sex  Age SibSp Parch Ticket Fare Cabin    Embarked honorific GenderAgeClass FareClass Surname
# 1044        1044       NA      3 Storey, Mr. Thomas male 60.5     0     0   3701   NA       S         Mr.     Adult Male      <NA>  Storey

# to estimate the fare of passenger ID   1044, we need to find the mean value of people similar to his pattern:
full %>% filter(!is.na(Fare), Pclass == 3, Age >= 50, Embarked == "S" ) %>% summarise(mean(Fare)) 
# 8.43042 <- we need to fill the NA of passanger #1044's fare with this value

full$Fare[1044] = 8.43042


# column age contains a lot of missing values (263 records), so we would like to analyze based on honorifics system on their name, 
# and ultimately group it into adult male, adult female, young male, young female

full$honorific = str_extract(full$Name, pattern = "\\,[ ][A-z]*[ ]*[A-z]*\\.")
full$honorific = gsub(", ","",full$honorific)

table(full$honorific)

# then we need to find the age range of each honorific

SurvivalRateByHonorific = full %>%
  filter(Survived != "NA") %>%
  group_by(honorific) %>% 
  summarise(minAge = min(Age, na.rm = T),maxAge = max(Age, na.rm = T), PassengerCount = n(), 
            PassengerSurvived =  sum(Survived == 1), survivalRate = round(sum(Survived == 1)/n(),3)) %>%
  arrange(desc(PassengerSurvived))

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


SurvivalRateByGenderAge = full %>%
  filter(Survived != "NA") %>%
  group_by(GenderAgeClass) %>% 
  summarise(minAge = min(Age, na.rm = T),maxAge = max(Age, na.rm = T), PassengerCount = n(), 
            PassengerSurvived =  sum(Survived == 1), survivalRate = round(sum(Survived == 1)/n(),3)) %>%
  arrange(desc(PassengerSurvived))
  
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
  xlab("Survived = 1, not-Survived = 0") + 
  ylab("Gender Age Class")+
  geom_jitter(width = .65, color = "blue")    

# exploratory analysis

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

library(MASS)
?randomForest
glimpse(full)
set.seed(100)

full$honorific = as.factor(full$honorific)
full$GenderAgeClass = as.factor(full$GenderAgeClass)


train = full[!is.na(full$Survived),]
test = full[is.na(full$Survived),]

titanic.rf = randomForest(factor(Survived) ~ Pclass + Sex + SibSp  + Parch + Fare + Embarked + honorific + GenderAgeClass + FareClass,
                          data = train)

plot(titanic.rf, ylim=c(0,0.36))
legend('topright', colnames(titanic.rf$err.rate), col=1:3, fill=1:3)

prediction <- predict(titanic.rf, test)
table(prediction,genderclassmodel[,2])

mean(prediction != genderclassmodel$Survived)

importance(titanic.rf)


