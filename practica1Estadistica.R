#generation of a normal distribution. 
v1=rnorm(200, mean=0, sd=1)
v2=rexp(200)
v3=rnorm(200, mean=10,sd=1)
summary(v1)
#Import excel
library(readxl)
ExcelPract1Estadistica <- read_excel("~/Documents/Master Data science/SMDE/Prácticas/Práctica1/ExcelPract1Estadistica.xlsx")
View(ExcelPract1Estadistica)
#Work with the data as a dataframe. 
taula_v1=data.frame(x1=v1)
#Definition of the intervals, categories to be used. 
taula_v1_cat=transform(taula_v1, cat = 
                         ifelse(x1 < -1,"-1",
                         ifelse(x1 < -0.5,"-0.5", 
                          ifelse(x1 < 0,"0", 
                          ifelse(x1 < 0.5,"0.5",
                          ifelse(x1 <1,"1","Inf"))))))
ExcelPract1Estadistica=transform(ExcelPract1Estadistica, Categoria = 
                         ifelse(SPECIAL < -1,"-1",
                                ifelse(SPECIAL < -0.5,"-0.5", 
                                       ifelse(SPECIAL < 0,"0", 
                                              ifelse(SPECIAL < 0.5,"0.5",
                                                     ifelse(SPECIAL <1,"1","Inf"))))))
#Counting the amount of elements in each category “table” function. 
taula_freq_v1=as.data.frame(with(taula_v1_cat, table(cat)))
taula_freq_excel=as.data.frame(with(ExcelPract1Estadistica, table(Categoria)))

#Creation of the table containing the frequencies of both ditributions
taula_freq=data.frame(x1=taula_freq_v1[2],x2=taula_freq_excel[2])

#Chisquare test
Test=chisq.test(taula_freq, correct=FALSE)
Test
hist(taula_freq[2:6,1], 
     main="Histogram of R frequency", 
     xlab="Frequency")
hist(taula_freq[2:6,2], 
     main="Histogram of Excel frequency", 
     xlab="Frequency")
#Exercice 1 for exponential distr
v2=rexp(200)
summary(v2)
#Import excel
library(readxl)
ExcelPract1EstadisticaExp <- read_excel("~/Documents/Master Data science/SMDE/Prácticas/Práctica1/ExcelPract1EstadisticaExp.xlsx")
#Work with the data as a dataframe. 
taula_v2=data.frame(x2=v2)
#Definition of the intervals, categories to be used. 
taula_v2_cat=transform(taula_v2, cat = 
                         ifelse(x2 < -1,"-1",
                                ifelse(x2 < -0.5,"-0.5", 
                                       ifelse(x2 < 0,"0", 
                                              ifelse(x2 < 0.5,"0.5",
                                                     ifelse(x2 <1,"1","Inf"))))))
ExcelPract1Estadistica=transform(ExcelPract1EstadisticaExp, Categoria = 
                                   ifelse(SPECIAL < -1,"-1",
                                          ifelse(SPECIAL < -0.5,"-0.5", 
                                                 ifelse(SPECIAL < 0,"0", 
                                                        ifelse(SPECIAL < 0.5,"0.5",
                                                               ifelse(SPECIAL <1,"1","Inf"))))))
#Counting the amount of elements in each category “table” function. 
taula_freq_v2=as.data.frame(with(taula_v2_cat, table(cat)))
taula_freq_excel=as.data.frame(with(ExcelPract1Estadistica, table(Categoria)))
#Creation of the table containing the frequencies of both ditributions
taula_freq2=data.frame(x1=taula_freq_v2[2],x2=taula_freq_excel[2])
Test=chisq.test(taula_freq2, correct=FALSE)
Test
hist(taula_freq2[2:6,1], 
     main="Histogram of R frequency", 
     xlab="Frequency")
hist(taula_freq2[2:6,2], 
     main="Histogram of Excel frequency", 
     xlab="Frequency")
#Normal with mean 10

#Work with the data as a dataframe. 
taula_v3=data.frame(x3=v3)
#Definition of the intervals, categories to be used. 
taula_v3_cat=transform(taula_v3, cat = 
                         ifelse(x3 < -1,"-1",
                                ifelse(x3 < -0.5,"-0.5", 
                                       ifelse(x3 < 0,"0", 
                                              ifelse(x3 < 0.5,"0.5",
                                                     ifelse(x3 <1,"1","Inf"))))))
ExcelPract1Estadistica=transform(ExcelPract1Estadistica, Categoria = 
                                   ifelse(SPECIAL < -1,"-1",
                                          ifelse(SPECIAL < -0.5,"-0.5", 
                                                 ifelse(SPECIAL < 0,"0", 
                                                        ifelse(SPECIAL < 0.5,"0.5",
                                                               ifelse(SPECIAL <1,"1","Inf"))))))
#Counting the amount of elements in each category “table” function. 
taula_freq_v3=as.data.frame(with(taula_v3_cat, table(cat)))
taula_freq_excel3=as.data.frame(with(ExcelPract1Estadistica, table(Categoria)))

#Creation of the table containing the frequencies of both ditributions
taula_freq3=data.frame(x1=taula_freq_v3[2],x2=taula_freq_excel3[2])

#Chisquare test
Test=chisq.test(taula_freq, correct=FALSE)
Test
hist(taula_freq3[2:6,1], 
     main="Histogram of R frequency", 
     xlab="Frequency")
hist(taula_freq3[2:6,2], 
     main="Histogram of Excel frequency", 
     xlab="Frequency")



#Chisquare test
Test=chisq.test(taula_freq2, correct=FALSE)
Test
hist(taula_freq2[2:6,1], 
     main="Histogram of R frequency", 
     xlab="Frequency")
hist(taula_freq[2:6,2], 
     main="Histogram of Excel frequency", 
     xlab="Frequency")
#Exercice 2 


#Provided code
library(RcmdrMisc)
Norm_v1=rnorm(200, mean=0, sd=1) 
Norm_v2=rnorm(200, mean=10, sd=1) 
Norm_v3=rnorm(200, mean=0, sd=1)
#Create and exponential to do the tests.
exp = rexp(200, rate=1)

Norm_v1n=data.frame(x1=Norm_v1, x2="v1") 
Norm_v2n=data.frame(x1=Norm_v2, x2="v2") 
Norm_v3n=data.frame(x1=Norm_v3, x2="v3") 
data=mergeRows(Norm_v1n, Norm_v2n, common.only=FALSE)
data=mergeRows(as.data.frame(data), Norm_v3n, common.only=FALSE)

AnovaModel1 <- aov(x1 ~ x2, data=data) 
summary(AnovaModel1)
Boxplot(x1~x2, data=data, id.method="y")
#Independent test
dwtest(AnovaModel1, alternative = "two.sided")
#Population are normal? (normality test)
shapiro.test(Norm_v1)
shapiro.test(Norm_v2)
shapiro.test(Norm_v3)
shapiro.test(exp) #As we can see, the P.values is ultralow compared to the other ones that rounded 1.
#Homogenety tests
lmtest::bptest(AnovaModel1)
#All the test are pased.
#ANOVA
library("lmtest", lib.loc="~/Library/R/3.3/library")
library(FactoMineR)
data(geomorphology, package="FactoMineR")
#Now we'll implement the tests using the library lmtest in order eto test the ANOVA
#Normality test o the varibles of geomorphology 
#Get the unique from the drift
uniqueNames = data.frame(unique(geomorphology$Drift))
#Test for each drift population
for(i in 1:nrow(uniqueNames)){
  shapiroData <- subset(geomorphology, Drift==as.character(uniqueNames$unique.geomorphology.Drift.[i]),select = Block.size.median)
  print("Shapiro test on: ")
  print(as.character(uniqueNames$unique.geomorphology.Drift.[i]))
  if(nrow(shapiroData)>3) {
    print(shapiro.test(shapiroData$Block.size.median))
    }
  else{print("Must have 3 or more rows in order to perform the shapiro test.")}
  }
  #blocksize --> drift
Anova2 <- aov(Block.size.median ~ Drift ,data= geomorphology)
#Independent test
lmtest::dwtest(formula = Anova2,alternative = "two.sided")
#Homogenety tests
lmtest::bptest(Anova2)
Boxplot(Block.size.median ~ Drift, data=geomorphology, id.method="y")

# wind --> drift
#Normality test
for(i in 1:nrow(uniqueNames)){
  shapiroData <- subset(geomorphology, Drift==as.character(uniqueNames$unique.geomorphology.Drift.[i]),select = Wind.effect)
  print("Shapiro test on: ")
  print(as.character(uniqueNames$unique.geomorphology.Drift.[i]))
  if(nrow(shapiroData)>3) {
    print(shapiro.test(shapiroData$Wind.effect))
  }
  else{print("Must have 3 or more rows in order to perform the shapiro test.")}
}
Anova3 <- aov(Wind.effect ~ Drift ,data= geomorphology)
#Independent test
lmtest::dwtest(formula = Anova3,alternative = "two.sided")
#Homogenety tests
lmtest::bptest(Anova3)
Boxplot(Wind.effect ~ Drift, data=geomorphology, id.method="y")


#LINIAR MODEL

data(decathlon, package="FactoMineR")
#rename

#Model1
colnames(decathlon)[10] <- "D1500M"
colnames(decathlon)[5] <- "D400M"
colnames(decathlon)[1] <- "D100M"
RegModel1 <- lm(D1500M~., data=decathlon)
RegModel1
summary(RegModel1)
plot(RegModel1)
#Independicy test 
lmtest::dwtest(RegModel1, alternative = "two.sided")
#homogenity test
lmtest::bptest(RegModel1)
#Normality test
shapiro.test(residuals(RegModel1))
#all - rang

RegModel2 <- lm(D1500M~. -Rank, data=decathlon)
RegModel2
summary(RegModel2)
plot(RegModel2)

#Independicy test 
lmtest::dwtest(RegModel2, alternative = "two.sided")
#homogenity test
lmtest::bptest(RegModel2)
#Normality test
shapiro.test(residuals(RegModel2))
#all - rank and - competition BEST ONE 0.9977

RegModel3 <- lm(D1500M~. -Rank -Competition, data=decathlon)
RegModel3
summary(RegModel3)
window()
par(mfrow=c(1,1))
plot(RegModel3)
#Independicy test 
lmtest::dwtest(RegModel3, alternative = "two.sided")
#homogenity test
lmtest::bptest(RegModel3)
#Normality test
shapiro.test(residuals(RegModel3))
# FOURTH QUESTION: USE THE MODEL TO PREDICT THE BEHAVIOR OF AN ATHLETE.

#Prediction for all variables.
predict(RegModel3, newdata=decathlon, interval="prediction")
predict(RegModel3, newdata=decathlon, interval="confidence")

#Prediction for 400 and 1500M
RegModel4 <- lm(D1500M~D400M, data=decathlon)
#Independicy test 
lmtest::dwtest(RegModel4, alternative = "two.sided")
#homogenity test
lmtest::bptest(RegModel4)
#Normality test
shapiro.test(residuals(RegModel4))
new <- data.frame(decathlon$D400M)
colnames(new)[1] <- "D400M"
predict(RegModel4, newdata=new, interval="prediction")
predict(RegModel4, newdata=new, interval="confidence")

#prediction for rank and 400M,1500M,100M, points

RegModel5 <- lm(Rank~ D100M + D400M + D1500M + Points, data=decathlon)
#Independicy test 
lmtest::dwtest(RegModel5, alternative = "two.sided")
#homogenity test
lmtest::bptest(RegModel5)
#Normality test
shapiro.test(residuals(RegModel5))
new <- data.frame(decathlon$D100M,decathlon$D400M,decathlon$D1500M,decathlon$Points)
colnames(new)[1] <- "D100M"
colnames(new)[2] <- "D400M"
colnames(new)[3] <- "D1500M"
colnames(new)[4] <- "Points"
predict(RegModel5, newdata=new, interval="prediction")
predict(RegModel5, newdata=new, interval="confidence")
plot (RegModel5)

#PCA 

#Delete competition because is not quantitative variable, is qualitative
library(FactoMineR)
pcaData <- subset( decathlon, select = -c(Competition,Rank,Points))
PCA(pcaData)

