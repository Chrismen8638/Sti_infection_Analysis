##Setting a working directory
setwd("C:/Users/User/Desktop/R Training")
getwd()
##Importing stata data
library(haven)
stidata_unclean<-read_dta("C:/Users/User/Desktop/R Training/stidata_unclean.dta")
View(stidata_unclean)
#Binary logistic regression
#creation of extra variables
Data1=stidata_unclean
attach(Data1)
Data1$use_condoms ="NA"
Data1$use_condoms[Data1$n12usecondom=="1 yes"]="Yes"
Data1$use_condoms[Data1$n12usecondom =="2 no" |Data1$n12usecondom=="2 No"]="No"
table(Data1$use_condoms)
#table(Data1$n12usecondom)



Data1$sex_partner_for_last_1_year="NA"
Data1$sex_partner_for_last_1_year[Data1$sexpartner1year==1]="Yes"
Data1$sex_partner_for_last_1_year[Data1$sexpartner1year ==0]="No"
#table(Data1$sexpartner1year)
table(Data1$sex_partner_for_last_1_year)




Data1$Education_level="NA"
Data1$Education_level[Data1$a4levelofeducation=="1 none"|Data1$a4levelofeducation=="2 primary"]="Primary and below"
Data1$Education_level[Data1$a4levelofeducation=="3 secondary"|Data1$a4levelofeducation=="4 tertiary"]="secondary and above"
table(Data1$Education_level)

Data1$Marital_status="NA"
Data1$Marital_status[Data1$a5maritalstatus=="1 single"|Data1$a5maritalstatus=="4 divorcee"|Data1$a5maritalstatus=="5 widowed"]="Not Married"
Data1$Marital_status[Data1$a5maritalstatus=="3 co-habiting"|Data1$a5maritalstatus=="2 married"]="Married"
table(Data1$Marital_status)

Data1$Occupation="NA"
Data1$Occupation[Data1$a2occupation=="1 unemployed"|Data1$a2occupation=="4 student"]="unemployed"
Data1$Occupation[Data1$a2occupation=="2 informal"|Data1$a2occupation=="3 formal"]="employed"
table(Data1$Occupation)

table(Data1$casestatus)
Data1$sti_infection=NA
Data1$sti_infection[Data1$casestatus==2|Data1$casestatus==3]=1
Data1$sti_infection[Data1$casestatus==1]=0
summary(Data1$sti_infection)
table(Data1$sti_infection)

model1=glm(Data1$sti_infection~Data1$use_condoms+sex_partner_for_last_1_year+Education_level+Marital_status+Occupation,family=binomial(link=logit),data=Data1)
model1
summary(model1)
coef(model1)
exp(coef(model1))
