adult <- read.csv('adult_sal.csv')
head(adult)

library(dplyr)

adult <- select(adult, -X)

str(adult)

summary(adult)

table(adult$type_employer)

group_emp <- function(job){
    if (job=='Never-worked' | job=='Without-pay'){
        return('Unemployed')
    }else if(job=='Local-gov' | job=='State-gov'){
        return('SL-gov')
    }else if(job=='Self-emp-inc' | job=='Self-emp-not-inc'){
        return('self-emp')
    }else{
        return(job)
    }
}

adult$type_employer <- sapply(adult$type_employer, group_emp)
    
table(adult$type_employer)

table(adult$marital)

group_mar <- function(mar){
    if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
        return('Not-Married')
    }else if(mar=='Never-married'){
        return(mar)
    }else{
        return('Married')
    }
}

adult$marital <- sapply(adult$marital, group_mar)

table(adult$marital)

table(adult$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan','Laos','Philippines','Vietnam','Taiwan','Thailand')
North.America <- c('Canada','United-States','Puerto-Rico')
Europe <- c('England','France','Germany','Greece','Holand-Netherlands','Hungary','Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
Latin.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador','El-Salvador','Guatemala','Haiti','Honduras','Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru','Jamaica','Trinadad&Tobago')
Other <- c('South')

group_cty <- function(cty){
    if (cty %in% Asia){
        return('Asia')
    }else if(cty %in% Europe){
        return('Europe')
    }else if(cty %in% North.America){
        return('North.America')
    }else if(cty %in% Latin.South.America){
        return('Latin.South.America')
    }else{
        return('Other')
    }
}
        
adult$country <- sapply(adult$country, group_cty)
    
table(adult$country)

str(adult)

table(adult$education)

group_edu <- function(edu){
    if (edu=='12th' | edu=='11th' | edu=='10th' | edu=='9th' | edu=='7th-8th' | edu=='5th-6th' | edu=='1st-4th' | edu=='Preschool'){
        return('Not-HS-Grad')
    }else if(edu=='Assoc-acdm' | edu=='Assoc-voc' | edu=='Some-college' | edu=='Prof-school'){
        return('Post-HS-Grad')
    }else{
        return(edu)
    }
}

adult$education <- sapply(adult$education, group_edu)

table(adult$education)

table(adult$occupation)

group_occ <- function(occ){
    if(occ=='Handlers-cleaners' | occ=='Machine-op-inspct' | occ=='Craft-repair'){
        return('maintain-serv')
    }else if(occ=='Protective-serv' | occ=='Armed-Forces'){
        return('protect-serv')
    }else{
        return(occ)
    }
}

adult$occupation <- sapply(adult$occupation, group_occ)

table(adult$occupation)

library(Amelia)

adult[adult=='?'] <- NA

adult$type_employer <- as.factor(adult$type_employer)
adult$education <- as.factor(adult$education)
adult$marital <- as.factor(adult$marital)
adult$relationship <- as.factor(adult$relationship)
adult$race <- as.factor(adult$race)
adult$country <- as.factor(adult$country)
adult$income <- as.factor(adult$income)
adult$occupation <- as.factor(adult$occupation)
adult$sex <- as.factor(adult$sex)

str(adult)

table(adult$type_employer)

missmap(adult)

missmap(adult, y.at=c(1),y.labels=c(''),col=c('yellow','black'))

adult <- na.omit(adult)

missmap(adult, y.at=c(1),y.labels=c(''),col=c('yellow','black'))

str(adult)

library(ggplot2)
library(dplyr)

ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth=1) + theme_bw()

ggplot(adult, aes(hr_per_week)) + geom_histogram() + theme_bw()

head(adult)

adult <- rename(adult, region = country)
head(adult)

pl1 <- ggplot(adult, aes(region)) + geom_bar(aes(fill=income), color='black') + theme_bw()
pl2 <- pl1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
pl2

str(adult)

library(caTools)

set.seed(101)

sample <- sample.split(adult$income,SplitRatio=0.7)

train <- subset(adult, sample==T)
test <- subset(adult, sample==F)

model <- glm(income ~., family=binomial(link ='logit'), data=train)

summary(model)

new.step.model <- step(model)

summary(new.step.model)

test$predicted.income <- predict(model, newdata = test, type='response')

table(test$income, test$predicted.income > 0.5)

acc <- (6371+1425)/(6371+1425+870+549)
acc

6371/(6371+549)

6371/(6371+870)

test$predicted.income2 <- predict(new.step.model, newdata = test, type='response')

table(test$income, test$predicted.income2 > 0.5)


