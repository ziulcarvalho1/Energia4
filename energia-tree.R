
##  library(xlsx)
library(stringr)
##library(inum)library(TH.data)library(multcomp)library(matrixStats)library(survival)library(coin)library(sandwich)library(libcoin)library(strucchange)
##library(Formula)
library(FSelect)
library(rpart.plot)
library(caret)
library(rpart)
library(rpart.plot)
library(data.tree)
library(party)
library(partykit)
library(caTools)
library(ElemStatLearn)
library(dplyr)
setwd("/AI/Energia4/")

data2 <- read.csv("dia-re.csv") ## Items
str(data2)

##data2$typephone <- as.factor(data1$typephone, levels = c(0, 1), labels = c('Mobile', 'landline'))
##prop.table(table(data2$range))
str(data2)


## select the meaninful colunms for the analysis
data0 <- select(data2,regiao,precipitac,ttmax,ttmin,sol,ttmed,ttumid, ena, pld, range) ##, ,
##data0 <- mutate(data2, spoke = factor(spoke)) ##, rescen = factor(rescen), tel_o = factor(tel_o)
data_sample = sample.split(data0$range ,SplitRatio=0.80) ##here we separate the file to be the nodes
data1 = subset(data0,data_sample==TRUE)  ##trainning data
data5 = subset(data0,data_sample==FALSE) ##test data
head(data1)
str(data1)
library(rpart)
library(rpart.plot)
dim(data1)
fit1 <- rpart(range~regiao+precipitac+ttmax+ttmin+sol+ttmed+ttumid+ena, data = data1)
fit<- ctree(range~regiao+precipitac+ttmax+ttmin+sol+ttmed+ttumid+ena, data = data1)
predicao <- predict(fit, data5)
print(fit)
rpart.plot(fit1, box.palette = "RdBu", shadow.col="gray", nn=TRUE)
confusionMatrix(predicao, data5$range)
prp(fit1)
summary(fit)
##plot(fit, type = "simple")

png("fit.png", res=80, height=800, width=3000) 
print(data1)
##plot(fit, main = "Classificacao de Range",     type = "simple",     gp = gpar(fontsize = 6),     margins = NULL)
##dev.off()