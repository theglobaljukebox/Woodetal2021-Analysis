#This code performs power analysis for inter-rater reliability analyses based on previous data published in:
#Savage, P. E., Merritt, E., Rzeszutek, T., & Brown, S. (2012). CantoCore: A new cross-cultural song classification scheme. Analytical Approaches to World Music, 2(1), 87–137. https://doi.org/10.31234/osf.io/s9ryg

#This code requires installing and loading the following packages:
#Install:
install.packages("irr")
install.packages("psych")
install.packages("googlesheets4")
install.packages("lsr")
install.packages("pwr")


#Load:
library(irr)
library(psych)
library(googlesheets4)
library(lsr)
library(pwr)

options(stringsAsFactors = FALSE)

###Power analysis in preparation for doing inter-rater reliability analysis of Global Jukebox data

#Read previous data from Savage et al. (2012) AAWM appendix
(d <- as.data.frame(read.csv("SavageEtAl(2012)AAWMAppendixReliability(CantometricsOnly).csv")))

#Hypothesis 1 power analysis/test
cohensD(d$Kappa,
        mu = 0) # d = 1.53
pwr.t.test(n = , d = 1.53 , sig.level = 0.017, power = 0.8, type = c("one.sample"), alternative=c("greater")) # n = 6.236148 (i.e., at least 7 variables)
t.test(d$Kappa, alternative="greater") #t = 9.1686, df = 35, p-value = 3.906e-11

#Hypothesis 2 power analysis/test
> cohensD(d[c(2:3,7:9,13:14,27),3],d[c(1,4:6,10:12,15:26,28:37),3]) # 1.196364
pwr.t.test(n = , d = 1.196 , sig.level = 0.017, power = 0.8, type = c("two.sample"), alternative=c("greater")) # n = 13.46669 (i.e., at least 28 variables [14 x2 groups])
t.test(d[c(1,4:6,10:12,15:26,28:37),3], d[c(2:3,7:9,13:14,27),3], alternative="greater") # t = 2.6524, df = 8.4855, p-value = 0.01385

#Hypothesis 3 power analysis/test
cohensD(d[c(1, 3, 10, 11, 19, 25, 30, 33),3],d[c(2,4:9,12:18,20:24,26:29,31:32,34:37),3]) # 1.082842
pwr.t.test(n = , d = 1.082842 , sig.level = 0.017, power = 0.8, type = c("two.sample"), alternative=c("greater")) # n = 16.15038 (i.e., at least 34 variables [17 x2 groups])
t.test(d[c(1, 3, 10, 11, 19, 25, 30, 33),3],d[c(2,4:9,12:18,20:24,26:29,31:32,34:37),3], alternative="greater") # t = 2.1679, df = 8.8922, p-value = 0.02934

