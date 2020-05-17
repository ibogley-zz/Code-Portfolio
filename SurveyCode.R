#################### Intro Code ######################
## Packages ##
library(pacman)
p_load(readxl,cellranger,base)

## Data ##
data <-  read_xlsx("C:/Users/IB/Desktop/Sheltercare/2020 Employee Satisfaction Survey (Responses).xlsx",range = cell_limits(c(2,2), c(64,24)))
Meaning1 <- data$`Meaningful Work [I feel appreciated for the work that I do at ShelterCare.]`
Meaning2 <- data$`Meaningful Work [I can envision working for ShelterCare for an extended period.]`
Meaning3 <- data$`Meaningful Work [My work at ShelterCare helps meet an important need in the community.]`
Meaning4 <- data$`Meaningful Work [My voice is heard in my department/program]`
Meaning5 <- data$`Meaningful Work [The internal culture at ShelterCare is in line with the organizational mission and values.]`
Emotion  <- data$`Emotional Safety in the Workplace (1 completely unsafe, 10 completely safe)`
Physical <- data$`Physical Safety in the Workplace (1 completely unsafe, 10 completely safe)`
Super1   <- data$`Supervision [Acknowledges the accomplishments and hard work of the team.]`
Super2   <- data$`Supervision [Communicates the needs of my program/department to the rest of ShelterCare.]`
Super3   <- data$`Supervision [Effective at problem solving when unexpected challenges arise.]`
Super4   <- data$`Supervision [Effectively communicated with all team members.]`
Super5   <- data$`Supervision [Encourages career development.]`
Super6   <- data$`Supervision [Holds effective meetings as needed for the team.]`
Super7   <- data$`Supervision [Provides appropriate & applicable training opportunities.]`
Super8   <- data$`Supervision [Sufficiently meets requirements for audits, contracts, and/or billings with outside agencies.]`
Exec1    <- data$`Management/Executive Leadership [Effectively communicated with all team members.]`
Exec2    <- data$`Management/Executive Leadership [Acknowledges the accomplishments and hard work of the team.]`
Exec3    <- data$`Management/Executive Leadership [Holds effective meetings as needed for the team.]`
Exec4    <- data$`Management/Executive Leadership [Effective at problem solving when unexpected challenges arise.]`
Exec5    <- data$`Management/Executive Leadership [Sufficiently meets requirements for audits, contracts, and/or billings with outside agencies.]`
Exec6    <- data$`Management/Executive Leadership [Communicates the needs of my program/department to the rest of ShelterCare.]`
Exec7    <- data$`Management/Executive Leadership [Provides appropriate & applicable training opportunities.]`
Exec8    <- data$`Management/Executive Leadership [Encourages career development.]`

#Other#
clr      <- c("#00FF00","#FFFF00","#FFA500","#ff0000")
clr2     <- c("#FFFFFF","#DCDCDC","#D3D3D3","#C0C0C0","#A9A9A9")
rord1    <- c("Strongly Agree", "Somewhat Agree", "Somewhat Disagree", "Strongly Disagree")
rord2    <- c("Excellent", "Sufficient", "Minimal", "Poor")
######################################################

###1. Basic Analysis:
## Infographic of each question
#Meaning1
CountM1 <- c(24,22,10,4)
barplot(CountM1,names.arg = rord1,ylab = "Frequency", main = "I feel appreciated for the work that I do at ShelterCare.", col = clr)

#Meaning2
CountM2 <- c(28,18,9,5)
barplot(CountM2,names.arg = rord1,ylab = "Frequency", main = "I can envision working for ShelterCare for an extended period.", col = clr)

#Meaning3
CountM3 <- c(49,9,1,1)
barplot(CountM3,names.arg = rord1,ylab = "Frequency", main = "My work at ShelterCare helps meet an important need in the community.", col = clr)

#Meaning4
CountM4 <- c(32,16,8,4)
barplot(CountM4,names.arg = rord1,ylab = "Frequency", main = "My voice is heard in my department/program", col = clr)

#Meaning5
CountM5 <- c(28,21,10,1)
barplot(CountM5,names.arg = rord1,ylab = "Frequency", main = "The internal culture at ShelterCare is in line with the organizational mission and values.", col = clr)

#Emotion
hist(Emotion, col = clr2, breaks = 4, main = "Emotional Safety")

#Physical
hist(Physical, col = clr2, breaks = 4, main = "Physical Safety")

#Super1
CountS1 <- c(28,21,9,2)
barplot(CountS1,names.arg = rord2,ylab = "Frequency", main = "Acknowledges the accomplishments and hard work of the team.", col = clr)

#Super2
CountS2 <- c(29,22,7,2)
barplot(CountS2,names.arg = rord2,ylab = "Frequency", main = "Communicates the needs of my program/department to the rest of ShelterCare.", col = clr)

#Super3
CountS3 <- c(24,23,7,6)
barplot(CountS3,names.arg = rord2,ylab = "Frequency", main = "Effective at problem solving when unexpected challenges arise.", col = clr)

#Super4
CountS4 <- c(33,17,7,3)
barplot(CountS4,names.arg = rord2,ylab = "Frequency", main = "Effectively communicated with all team members.", col = clr)

#Super5
CountS5 <- c(34,22,2,1)
barplot(CountS5,names.arg = rord2,ylab = "Frequency", main = "Encourages career development.", col = clr)

#Super6
CountS6 <- c(26,24,6,4)
barplot(CountS6,names.arg = rord2,ylab = "Frequency", main = "Holds effective meetings as needed for the team.", col = clr)

#Super7
CountS7 <- c(24,23,9,4)
barplot(CountS7,names.arg = rord2,ylab = "Frequency", main = "Provides appropriate & applicable training opportunities.", col = clr)

#Super8
CountS8 <- c(22,22,10,6)
barplot(CountS8,names.arg = rord2,ylab = "Frequency", main = "Sufficiently meets requirements for audits, contracts, and/or billings with outside agencies.", col = clr)

#Exec1
CountE1 <- c(12,22,19,6)
barplot(CountE1,names.arg = rord2,ylab = "Frequency", main = "Effectively communicated with all team members.", col = clr)

#Exec2
CountE2 <- c(16,21,16,6)
barplot(CountE2,names.arg = rord2,ylab = "Frequency", main = "Acknowledges the accomplishments and hard work of the team.", col = clr)

#Exec3
CountE3 <- c(12,23,16,7)
barplot(CountE3,names.arg = rord2,ylab = "Frequency", main = "Holds effective meetings as needed for the team.", col = clr)

#Exec4
CountE4 <- c(12,26,17,5)
barplot(CountE4,names.arg = rord2,ylab = "Frequency", main = "Effective at problem solving when unexpected challenges arise.", col = clr)

#Exec5
CountE5 <- c(22,28,8,1)
barplot(CountE5,names.arg = rord2,ylab = "Frequency", main = "Sufficiently meets requirements for audits, contracts, and/or billings with outside agencies.", col = clr)

#Exec6
CountE6 <- c(12,24,15,8)
barplot(CountE6,names.arg = rord2,ylab = "Frequency", main = "Communicates the needs of my program/department to the rest of ShelterCare.", col = clr)

#Exec7
CountE7 <- c(13,22,19,6)
barplot(CountE7,names.arg = rord2,ylab = "Frequency", main = "Provides appropriate & applicable training opportunities.", col = clr)

#Exec8
CountE8 <- c(12,20,20,8)
barplot(CountE8,names.arg = rord2,ylab = "Frequency", main = "Encourages career development.", col = clr)
