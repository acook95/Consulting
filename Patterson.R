library(readxl)
library(magrittr)
library(ggplot2)
library(nnet)

survey_data <- read_excel("Patterson_Capstone_Data_Cat_Num_031021.xls")


## Bucketing independent variables

survey_data$Frequency <- ifelse(survey_data$`Q6 Birth Frequency`=="More common than 1 in 2,000 births", "rare",
                                ifelse(survey_data$`Q6 Birth Frequency`=="1 in 2,000-50,000 births", "rare",
                                       ifelse(survey_data$`Q6 Birth Frequency`=="1 in 50,000-200,000 births", "rare",
                                              ifelse(survey_data$`Q6 Birth Frequency`=="1 in 200,000-500,000 births", "ultra-rare",
                                                     ifelse(survey_data$`Q6 Birth Frequency`=="1 in 500,000-1,000,000 births", "ultra-rare",
                                                            ifelse(survey_data$`Q6 Birth Frequency`=="Fewer than 1 in 1,000,000 births", "ultra-rare",
                                                                   survey_data$`Q6 Birth Frequency`))))))


survey_data$Size <- ifelse(survey_data$`Q10 Size (#Members)`=="Fewer than 60", "small",
                           ifelse(survey_data$`Q10 Size (#Members)`=="60-300", "small",
                                  ifelse(survey_data$`Q10 Size (#Members)`=="300-1,000", "medium",
                                         ifelse(survey_data$`Q10 Size (#Members)`=="1,000-10,000", "large",
                                                ifelse(survey_data$`Q10 Size (#Members)`=="More than 10,000", "large",
                                                       survey_data$`Q10 Size (#Members)`)))))

survey_data$Age <- ifelse(survey_data$`Q9 Age`=="Less than 5 years ago", "younger",
                          ifelse(survey_data$`Q9 Age`=="5-10 years ago", "younger",
                                 ifelse(survey_data$`Q9 Age`=="10-15 years ago", "older",
                                        ifelse(survey_data$`Q9 Age`=="15+ years ago", "older",
                                               survey_data$`Q9 Age`))))



# create a variable which indicates the organization's #1 priority
# code below isn't working
survey_data$Priority <- ifelse(survey_data$`Q21.11 Rank Awareness`==1, "Awareness", "none")
survey_data$Priority <- ifelse(survey_data$`Q21.12 Rank Fam Edu`==1, "Fam Edu", survey_data$Priority)
survey_data$Priority <- ifelse(survey_data$`Q21.13 Rank Provider Edu`==1, "Provider Edu", survey_data$Priority)
survey_data$Priority <- ifelse(survey_data$`Q21.14 Rank Support Fam`==1, "Support Fam", survey_data$Priority)
survey_data$Priority <- ifelse(survey_data$`Q21.15 Rank Resource`==1, "Resource", survey_data$Priority)
survey_data$Priority <- ifelse(survey_data$`Q21.16 Rank Research`==1, "Research", survey_data$Priority)
survey_data$Priority <- ifelse(survey_data$`Q21.17 Rank Research Policy`==1, "Research Policy", survey_data$Priority)
survey_data$Priority <- ifelse(survey_data$`Q21.18 Rank Advocacy`==1, "Advocacy", survey_data$Priority)




# code below isn't working
# survey_data$Priority <- ifelse(survey_data$`Q21.11 Rank Awareness`== 1, "Awareness",
#                                ifelse(survey_data$`Q21.12 Rank Fam Edu` == 1, "Fam Edu",
#                                       ifelse(survey_data$`Q21.13 Rank Provider Edu` == 1, "Provider Edu",
#                                              ifelse(survey_data$`Q21.14 Rank Support Fam` == 1, "Support Fam",
#                                                     ifelse(survey_data$`Q21.15 Rank Resource` == 1, "Resource",
#                                                            ifelse(survey_data$`Q21.16 Rank Research` == 1, "Research",
#                                                                   ifelse(survey_data$`Q21.17 Rank Research Policy` == 1, "Research Policy",
#                                                                          ifelse(survey_data$`Q21.18 Rank Advocacy` == 1, "Advocacy", "None"))))))))
#






# histograms of independent variables

ggplot(data = survey_data) + geom_bar(aes(Age, fill = Size)) +
  ggtitle("Histogram of Organization Age by Size")
ggplot(data = survey_data) + geom_bar(aes(Size))
ggplot(data = survey_data) + geom_bar(aes(Frequency))

ggplot(data = survey_data) + geom_bar(aes(Frequency, fill = Age)) +
  ggtitle("Histogram of Disease Frequency by Age")
ggplot(data = survey_data) + geom_bar(aes(Frequency, fill = Size)) +
  ggtitle("Histogram of Disease Frequency by Size")
ggplot(data = survey_data) + geom_bar(aes(Frequency, fill = Age)) +
  facet_wrap(vars(Size))


# determining unique values -- issues with Q26 and Q23.4
unique(survey_data$`Q26 Challenges`)
unique(survey_data$`Q23.4 No Research`)
unique(survey_data$`Q23.1 Research Int/Ext`)
unique(survey_data$`Q21.1 Rate Awareness`)
unique(survey_data$`Q21.11 Rank Awareness`)
unique(survey_data$`Q21.17 Rank Research Policy`)
unique(survey_data$`Q21.18 Rank Advocacy`)
unique(survey_data$Priority)



# histograms of Research Int/Ext

ggplot(data = subset(survey_data, !is.na(`Q23.1 Research Int/Ext`) & !is.na(Frequency))) +
  geom_bar(aes(`Q23.1 Research Int/Ext`, fill = Frequency)) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1))
ggplot(data = subset(survey_data, !is.na(`Q23.1 Research Int/Ext`) & !is.na(Size))) +
  geom_bar(aes(`Q23.1 Research Int/Ext`, fill = Size))
ggplot(data = subset(survey_data, !is.na(`Q23.1 Research Int/Ext`) & !is.na(Age))) +
  geom_bar(aes(`Q23.1 Research Int/Ext`, fill = Age))


# experimenting with multinomial modeling

model1 <- multinom(`Q26 Challenges` ~ Frequency + Age + Size, data = survey_data)
model1




model2 <- multinom(Rank ~ Frequency + Age + Size, data = priority)
model2

plot(fitted(model2), resid(model2))

model3 <- multinom(`Q23.1 Research Int/Ext` ~ Frequency + Age + Size, data = subset(survey_data, !is.na(`Q23.1 Research Int/Ext`)))
model3
summary(model3)


