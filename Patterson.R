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


# histograms of challenges

ggplot(data = survey_data) + geom_bar(aes(`Q26 Challenges`, fill = Frequency))
ggplot(data = survey_data) + geom_bar(aes(`Q26 Challenges`, fill = Size))
ggplot(data = survey_data) + geom_bar(aes(`Q26 Challenges`, fill = Age))



# multinomial modeling

model1 <- multinom(`Q26 Challenges` ~ Frequency + Age + Size, data = survey_data)
model1


unique(survey_data$`Q26 Challenges`)
unique(survey_data$`Q23.4 No Research`)
unique(survey_data$`Q23.1 Research Int/Ext`)
unique(survey_data$`Q21.1 Rate Awareness`)
