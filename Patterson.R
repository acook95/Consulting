library(readxl)
library(magrittr)
library(ggplot2)
library(nnet)
library(arm)
library(dplyr)
library(tidyverse)
library(xlsx)
library(tidyr)
library(readxl)
library(kableExtra)
library(broom)
library(caret)
library(car)

survey_data <- read_excel("Patterson_Capstone_Data_Cat_Num_031021.xls")


### INITIAL DATA PROCESSING ###

# Bucketing independent variables
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

survey_data$Research <- ifelse(survey_data$`Q23.1 Research Int/Ext`=="Supported research conducted by external entities", "External",
                               ifelse(survey_data$`Q23.1 Research Int/Ext`=="Conducted own research (acted as the primary sponsor and owned the results)", "Internal",
                                      survey_data$`Q23.1 Research Int/Ext`))

survey_data$Budget <- ifelse(survey_data$`Q12 Budget`=="No budget", "Lowest",
                             ifelse(survey_data$`Q12 Budget`=="$0 to $10,000", "Lowest",
                                    ifelse(survey_data$`Q12 Budget`=="$10,000 to $50,000",  "Medium-Low",
                                           ifelse(survey_data$`Q12 Budget`=="$50,000 to $100,000", "Medium-Low",
                                                  ifelse(survey_data$`Q12 Budget`=="$100,000 to $200,000", "Medium-High",
                                                         ifelse(survey_data$`Q12 Budget`=="$200,000 to $500,000", "Medium-High",
                                                                ifelse(survey_data$`Q12 Budget`=="$500,000 to $1,000,000", "Highest",
                                                                       ifelse(survey_data$`Q12 Budget`=="Over $1,000,000", "Highest",
                                                                              survey_data$`Q12 Budget`))))))))

survey_data$FDA <- ifelse(survey_data$`Q25.3 No FDA` == "There are trials in progress now", "In progress",
                          ifelse(survey_data$`Q25.3 No FDA` == "If not in progress, please describe the barriers facing the FDA-approval process (i.e., lack of research)", "Not in progress", NA))


# determining unique values -- issues with Q26 and Q23.4
unique(survey_data$`Q26 Challenges`)
unique(survey_data$`Q23.4 No Research`)
unique(survey_data$`Q23.1 Research Int/Ext`)
unique(survey_data$`Q21.1 Rate Awareness`)
unique(survey_data$`Q21.11 Rank Awareness`)
unique(survey_data$`Q21.17 Rank Research Policy`)
unique(survey_data$`Q21.18 Rank Advocacy`)
unique(survey_data$`Q25 FDA Thx`)

# remove unknowns
survey_data%<>%subset(survey_data$Frequency!="Unknown")
survey_data%<>%subset(survey_data$Age!="Unknown")
survey_data%<>%subset(survey_data$Size!="Unknown")
survey_data%<>%subset(survey_data$Budget!="Unknown")


# create new data frame for orgs top priority/data cleaning for priority model
survey_data%>%dplyr::select("Q4 Organization Name", "Frequency", "Size","Age","Budget",c(31:46))->prior_bi
prior_bi$`Q4 Organization Name`[is.na(prior_bi$`Q4 Organization Name`)]<-"Anonymous"
prior_bi[,14:21][is.na(prior_bi[,14:21])]<-0
prior_bi[,6:13][is.na(prior_bi[,6:13])]<-"NA"

haverank<-prior_bi[which(apply(prior_bi,1,function(x) paste0(ifelse(x[14:21]==1,1,0),collapse=""))!="00000000"),]

rank_final<-haverank
rank_final["bicode"]<-ifelse(apply(rank_final,1,function(x) paste0(ifelse(x[14:21]==1,1,0),collapse=""))=="00000000",apply(rank_final,1,function(x) paste0(ifelse(x[6:13]=="Extremely important",1,0),collapse="")),apply(rank_final,1,function(x) paste0(ifelse(x[14:21]==1,1,0),collapse="")))
rank_final["priority"]<-apply(rank_final,1,function(x) switch(x[22],"10000000"="Awareness","01000000"="Fam Edu","00100000"="Provide Edu","00010000"="Support Fam","00001000"="Fam Resource","00000100"="Research","00000010"="Research Policy","00000001"="Advocacy"))
rank_final$Age%<>%factor()
rank_final$Frequency%<>%factor()
rank_final$Size%<>%factor()
rank_final$Budget%<>%factor(ordered =FALSE, levels = c("Highest", "Medium-High", "Medium-Low", "Lowest"))


# create new data frame/data cleaning for Research model
data23.1 <- survey_data[, c(72,73,74,75,76)]
names(data23.1) <- c("Frequency","Size", "Age", "Research","Budget")

for(i in 1:dim(data23.1)[2]){
  data23.1[,i][data23.1[,i] == ""] <- NA
  data23.1[,i][data23.1[,i] == "Unknown"] <- NA
}

data23.1 <- na.omit(data23.1)


# creating new data frame / data cleaning for FDA variable
data25.3 <- survey_data[, c(72,73,74,76,77)]
names(data25.3) <- c("Frequency","Size", "Age","Budget","FDA")

for(i in 1:dim(data25.3)[2]){
  data25.3[,i][data25.3[,i] == ""] <- NA
  data25.3[,i][data25.3[,i] == "Unknown"] <- NA
}

data25.3 <- na.omit(data25.3)



### EDA ###

# histograms of independent variables
ggplot(data = subset(survey_data, !is.na(Size))) + geom_bar(aes(Age, fill = Size)) +
  ggtitle("Histogram of Organization Age by Size")

ggplot(data = subset(survey_data, !is.na(Frequency))) + geom_bar(aes(Frequency, fill = Age)) +
  ggtitle("Histogram of Disease Frequency by Age")

ggplot(data = subset(survey_data, !is.na(Size) & !is.na(Frequency))) + geom_bar(aes(Size, fill = Frequency)) +
  ggtitle("Histogram of Org. Size by Frequency")

ggplot(data = survey_data) + geom_bar(aes(Frequency, fill = Age)) +
  facet_wrap(vars(Size))


# scatterplots of independent variables
ggplot(data = survey_data) + geom_jitter(aes(Age, Frequency), alpha = 0.5)
ggplot(data = survey_data) + geom_jitter(aes(Age, Size), alpha = 0.5)
ggplot(data = survey_data) + geom_jitter(aes(Frequency, Size), alpha = 0.5)

# chi-square tests for independence
survey_data$Age%<>%factor()
survey_data$Frequency%<>%factor()
survey_data$Size%<>%factor()
survey_data$Budget%<>%factor(ordered =FALSE, levels = c("Highest", "Medium-High", "Medium-Low", "Lowest"))
chisq.test(survey_data$Frequency,survey_data$Size)
chisq.test(survey_data$Frequency,survey_data$Age)
chisq.test(survey_data$Age,survey_data$Size)
chisq.test(survey_data$Budget,survey_data$Size)
chisq.test(survey_data$Budget,survey_data$Age)
chisq.test(survey_data$Budget,survey_data$Frequency)

# histograms of Research Int/Ext
ggplot(data = subset(survey_data, !is.na(Research) & !is.na(Frequency))) +
  geom_bar(aes(Research, fill = Frequency)) +
  ggtitle("Histogram of Type of Research") +
  facet_wrap(vars(Age))

ggplot(data = subset(survey_data, !is.na(Research) & !is.na(Size))) +
  geom_bar(aes(Research, fill = Size))

ggplot(data = subset(survey_data, !is.na(Research) & !is.na(Age))) +
  geom_bar(aes(Research, fill = Age))

ggplot(data23.1, aes(x=Research, fill=Frequency))+geom_bar()+xlab("Research type")+ylab("Organizition Count")+ggtitle("Histogram of Frequency by Organizition Count VS Research")

ggplot(data23.1, aes(x=Research, fill=Budget))+geom_bar()+xlab("Research type")+ylab("Organizition Count")+ggtitle("Histogram of Budget by Organizition Count VS Research")


# histograms of Priorities
ggplot(data = subset(rank_final,!is.na(Frequency))) + geom_bar(aes(priority)) +
  ggtitle("Histogram of Organizations' Top Priority") +
  facet_wrap(vars(Frequency)) + theme(axis.text.x = element_text(angle = 90))

ggplot(data = subset(rank_final,!is.na(Frequency))) +
  geom_bar(aes(priority, fill = Frequency), position = "dodge") +
  ggtitle("Histogram of Organizations' Top Priority") +
  theme(axis.text.x = element_text(angle = 0)) + facet_wrap(nrow = 2, vars(Age))

# histograms of FDA Therapy
ggplot(data = subset(survey_data, !is.na(`Q25 FDA Thx`) & !is.na(Frequency))) +
  geom_bar(aes(`Q25 FDA Thx`, fill = Frequency)) +
  ggtitle("Histogram of FDA Therapy") +
  xlab("FDA Therapy in Progress?") + facet_wrap(vars(Age))

ggplot(data = subset(survey_data, !is.na(`Q25 FDA Thx`) & !is.na(Age))) +
  geom_bar(aes(`Q25 FDA Thx`, fill = Age)) +
  ggtitle("Histogram of FDA Therapy") +
  xlab("FDA Therapy in Progress?")


### MODELING ###

# response variable: Priority
m2<-multinom(priority~Frequency+Budget,data=rank_final,family=binomial(link="logit"))

broom::tidy(m2, exponentiate = FALSE, conf.int = TRUE)%>% kable(digits = 2, format = "markdown")

pred2<-fitted(m2)
resid2<-residuals(m2)
par(mfrow=c(2,4))
for(i in 1:8){
  binnedplot(pred2[,i],resid2[,i],main="")
  title(colnames(pred2)[i])
}

a<-c("rare","ultra-rare")
b<-c("Highest","Medium-High","Medium-Low","Lowest")
nd<- expand.grid(a,b)
colnames(nd)<-c("Frequency","Budget")
nd["class"]<-predict(m2,newdata=nd)
nd%<>%arrange(desc(Frequency,Budget))
nd

# response variable: research

mod23.1_1<-multinom(Research~Frequency+Budget,data=data23.1,family=binomial(link="logit"))
summary(mod23.1_1)

broom::tidy(mod23.1_1, exponentiate = FALSE, conf.int = TRUE) %>% kable(digits = 2, format = "markdown")

a<-c("rare","ultra-rare")
b<-c("Highest","Medium-High","Medium-Low","Lowest")
nd<- expand.grid(a,b)
colnames(nd)<-c("Frequency","Budget")
nd["class"]<-predict(mod23.1_1,newdata=nd)
nd%<>%arrange(desc(Frequency,Budget))
nd

pred23.1_1<-fitted(mod23.1_1)
resid23.1_1<-residuals(mod23.1_1)
binnedplot(pred23.1_1,resid23.1_1)

pred23.1_1<-fitted(mod23.1_1)
resid23.1_1<-residuals(mod23.1_1)
par(mfrow=c(1,3))
for(i in 1:3){
  binnedplot(pred23.1_1[,i],resid23.1_1[,i],main="")
  title(colnames(pred23.1_1)[i])
}

# response variable: FDA therapy

mod25.1_6<-multinom(FDA~Frequency+Budget,data=data25.3,family=binomial(link="logit"))
summary(mod25.1_6)

broom::tidy(mod25.1_6, exponentiate = FALSE, conf.int = TRUE)%>% kable(digits = 2, format = "markdown")


mod25.1_2<-multinom(FDA~Frequency,data=data25.3,family=binomial(link="logit"))
summary(mod25.1_2)

mod25.1_4<-multinom(FDA~Budget,data25.3,family=binomial(link="logit"))
summary(mod25.1_4)

pred25.1_6<-fitted(mod25.1_6)
resid25.1_6<-residuals(mod25.1_6)
binnedplot(pred25.1_6,resid25.1_6)
par(mfrow=c(1,2))

for(i in 1:2){
  binnedplot(pred25.1_6[,i],resid25.1_6[,i],main="")
  title(colnames(pred25.1_6)[i])
}



# citation
citation(package = "readxl")
citation(package = "magrittr")
citation(package = "ggplot2")
citation(package = "nnet")
citation(package = "arm")







### APPENDIX ###

# more plots
size_pri<-rank_final%>%group_by(Size,priority)%>%summarise(num=n())%>%arrange(desc(num),.by_group=TRUE)
ggplot(size_pri,aes(x=Size,y=num,fill=priority,group=num))+geom_bar(stat="identity",position="dodge")+ggtitle("#1 Priority of different sizes")+geom_text(aes(label=num),position=position_dodge(.9))

age_pri<-rank_final%>%group_by(Age,priority)%>%summarise(num=n())%>%arrange(desc(num),.by_group=TRUE)
ggplot(age_pri,aes(x=Age,y=num,fill=priority,group=num))+geom_bar(stat="identity",position="dodge")+ggtitle("#1 Priority of different ages")+geom_text(aes(label=num),position=position_dodge(.9))

freq_pri<-rank_final%>%group_by(Frequency,priority)%>%summarise(num=n())%>%arrange(desc(num),.by_group=TRUE)
ggplot(freq_pri,aes(x=Frequency,y=num,fill=priority,group=num))+geom_bar(stat="identity",position="dodge")+ggtitle("#1 Priority of different frequencies")+geom_text(aes(label=num),position=position_dodge(.9))

freq_pri<-rank_final%>%group_by(Budget,priority)%>%summarise(num=n())%>%arrange(desc(num),.by_group=TRUE)
ggplot(freq_pri,aes(x=Budget,y=num,fill=priority,group=num))+geom_bar(stat="identity",position="dodge")+ggtitle("#1 Priority of different budgets")+geom_text(aes(label=num),position=position_dodge(.9))

agebysize<-survey_data%>%group_by(Age,Size)%>%summarise(num=n())%>%arrange(desc(num),.by_group=TRUE)
ggplot(data = agebysize,aes(Age, y=num,fill = Size,group=num)) + geom_bar(position="dodge",stat="identity") +ggtitle("Histogram of Organization Age by Size")+geom_text(aes(label=num),position=position_dodge(.9))



