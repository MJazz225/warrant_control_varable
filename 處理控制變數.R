
##處理變數
##LnReturn, Momentum, Turnover are month data
##LnME, Illiquidty are day data
library(dplyr)
library(data.table)

rm(list=ls())
setwd("C:\\Users\\User\\Documents\\研究所\\論文")
rawdata <- read.csv("1203.csv", sep = ",", stringsAsFactors = F)
rawdata_month <- read.csv("1203month.csv", sep = ",", stringsAsFactors = F)

Name <- c('COID',	'MDATE',	'CLOSE',	'MV','TURNOVER',	'VOLUME',	'AMOUNT',	'OUTSTANDING', 'LNRETURN')
data_all <- cbind(rawdata$證券代碼, rawdata[3:ncol(rawdata)])
data_all <- data_all[2:nrow(data_all),]
colnames(data_all) <- Name
Names <- c('COID',	'Name',	'MDATE',	'CLOSE',	'TURNOVER',	'LNRETURN_MONTH')
data_month <- rawdata_month[2:nrow(rawdata_month),]
colnames(data_month) <- Names


data_all <- data_all[order(data_all[,1], decreasing = F),]
data_month <- data_month[order(data_month[,1], decreasing = F),]

data_all[,3] <- as.numeric(data_all[,3])
data_all[,4] <- as.numeric(data_all[,4])
data_all[,5] <- as.numeric(data_all[,5])
data_all[,6] <- as.numeric(data_all[,6])
data_all[,7] <- as.numeric(data_all[,7])
data_all[,8] <- as.numeric(data_all[,8])
data_all[,9] <- as.numeric(data_all[,9])

## log for market capital
ME <- log(as.numeric(data_all$MV))
data_all1 <- cbind(data_all, ME)
data_all1 <- data_all1[order(data_all1[,1], decreasing = F),]

##momentum
data_month <- data_month[order(data_month[,1], decreasing = F),]
MOMENTUM <- NULL
monthreturn <- as.data.frame(as.numeric(data_month[,5]))
for (i in 1:nrow(monthreturn)) {
  
  cat(i, "/", nrow(monthreturn), "\n")
  
  a=i
  
  b=i+11
  
  tem <- sum(monthreturn[c(a:b),])
  
  MOMENTUM <- rbind(MOMENTUM, tem)
}
MOMENTUM <- MOMENTUM[1:140614,]
data_month <- cbind(data_month, MOMENTUM)

##illiquidity --> aabsolute value of return divided by dollar trading volume, averaged oer trading days t-22 to t-1
rm(list=ls())
library(data.table)
setwd("C:\\Users\\User\\Documents\\研究所\\論文")
data_all1 <- read.csv("data_all1208.csv", sep = ",", stringsAsFactors = F) #只有illiquidity
data_month <- read.csv("datamonth1208.csv", sep = ",", stringsAsFactors = F)
data_all1$AMOUNT[ data_all1$AMOUNT == 0] <- 1
data_all1$LNRETURN[ is.na(data_all1$LNRETURN)] <- 0
illiquidity_amount <- abs(data_all1$LNRETURN)/data_all1$AMOUNT
illiquidity_amount <- as.data.frame(illiquidity_amount)
ILLIQUIDITY <- NULL
for (i in 1:nrow(illiquidity_amount)) {
  
  cat(i, "/", nrow(illiquidity_amount), "\n")
  
  a=i
  b=i+19
  
  tem <- illiquidity_amount[a:b,]
  tem <- mean(tem)
  
  ILLIQUIDITY <- c(ILLIQUIDITY, tem)

}  "睡覺醒來illiquidity amount跑完把ILLIQUIDITY加進data_all1, 然後刪掉原本的ILLIQUIDITY"

ILLI <- NULL
for (i in 1:nrow(illiquidity_amount)) {
  
  cat(i, "/", nrow(illiquidity_amount), "\n")
  
  a=i
  b=i+19
  
  tem <- illiquidity_amount[a:b,]
  tem <- mean(tem)
  
  ILLI <- c(ILLI, tem)
  
}
ILLI1 <- as.data.frame(ILLI)
ILLI2 <- ILLI1[1:1860461,]
ILLI3 <- ILLI1[1860462:1860480,]
ILLI1 <- c(ILLI3, ILLI2)
ILLI1 <- as.data.frame(ILLI1)
colnames(ILLI1) <- "ILLIQUIDITY"

TURNOVER <- data_1$TURNOVER.1
TURNOVER <- as.data.frame(TURNOVER)
colnames(TURNOVER) <- "MONTHTURNOVER"
MONTHRET <- data_1$MONTHRET
MONTHRET <- as.data.frame(MONTHRET)
data_1 <- data_1[,1:10]
data_1 <- cbind(data_1, ILLIQUIDITY1, TURNOVER, MONTHRET)
data_month <- cbind(data_month$COID, data_month[,c(3:7)])

write.csv(data_1, "data_all1215.csv", sep = ",", row.names = F, col.names = T)
write.csv(data_month, "datamonth1208.csv", sep = ",", row.names = F, col.names = T)

##turnover --> stock turnover of the prior month
rm(list=ls())
library(data.table)
setwd("C:\\Users\\User\\Documents\\研究所\\論文")
data_all1 <- read.csv("data_all1208.csv", sep = ",", stringsAsFactors = F) #只有illiquidity
data_month <- read.csv("datamonth1208.csv", sep = ",", stringsAsFactors = F)

CUMTURNOVER <- NULL
MONTHRET <- NULL
for (i in 1:nrow(data_all1)) {
  
  cat(i, "/", nrow(data_all1), "\n")
  
  a=i
  b=i+19
  
  tem <- data_all1[a:b,5]
  tem <- sum(tem)
  temmonthret <- data_all1[a:b,9]
  temmonthret <- sum(temmonthret)
  
  CUMTURNOVER <- rbind(CUMTURNOVER, tem)
  MONTHRET <- rbind(MONTHRET, temmonthret)
  
}
repeat1 <- rep(NA,20)
CUMTURNOVER <- as.data.frame(CUMTURNOVER)
MONTHRET <- as.data.frame(MONTHRET)

CUMTURNOVER2 <- CUMTURNOVER[1:1860461,]
CUMTURNOVER3 <- CUMTURNOVER[1860462:1860480,]
CUMTURNOVER1 <- c(CUMTURNOVER3, CUMTURNOVER2)
CUMTURNOVER1 <- as.data.frame(CUMTURNOVER1)
colnames(CUMTURNOVER1) <- "TURNOVER"

MONTHRET2 <- MONTHRET[1:1860461,]
MONTHRET3 <- MONTHRET[1860462:1860480,]
MONTHRET1 <- c(MONTHRET3, MONTHRET2)
MONTHRET1 <- as.data.frame(MONTHRET1)
colnames(MONTHRET1) <- "MONTHRET"


ILLIQUIDITY <- data_all1$ILLIQUIDITY
ILLIQUIDITY <- as.data.frame(ILLIQUIDITY)
ILLIQUIDITY2 <- ILLIQUIDITY[1:1860461,]
ILLIQUIDITY3 <- ILLIQUIDITY[1860462:1860480,]
ILLIQUIDITY1 <- c(ILLIQUIDITY3, ILLIQUIDITY2)
ILLIQUIDITY1 <- as.data.frame(ILLIQUIDITY1)
colnames(ILLIQUIDITY1) <- "ILLIQUIDITY"

data_all1 <- data_all1[,1:10]
data_all1 <- cbind(data_all1, ILLIQUIDITY1, CUMTURNOVER1, MONTHRET1)
write.csv(data_all1, "data_all1212.csv", sep = ",", row.names = F, col.names = T) ##有illiquidity, turnover和month return


####合并資料 全部都是list的資料
rm(list=ls())
library(data.table)
setwd("C:\\Users\\User\\Documents\\研究所\\論文")
data_1 <- read.csv("data_all1212.csv", sep = ",", stringsAsFactors = F) ##有illiquidity, turnover和month return
data_month <- read.csv("datamonth1208.csv", sep = ",", stringsAsFactors = F)


list_data <- split(data_1, f = data_1$MDATE)
list_data <- list_data[23:length(list_data)] ##去掉2015,2016年的資料

list_month <- split(data_month, f = data_month$MDATE)
list_month <- list_month[25:length(list_month)] ##去掉2015,2016年的資料

data2 <- rbindlist(list_data, fill = T)
monthdata1 <- rbindlist(list_month, fill = T)

list_data <- split(data2, f = data2$COID)
list_month <- split(monthdata1, f = monthdata1$data_month.COID)

list_data_date <- split(data2, f = data2$MDATE)
list_data_month <- split(monthdata1, f = monthdata1$MDATE)



for (i in 1:length(list_data)) {
  
  cat(i, "/", length(list_data), "\n")
  
  temdata <- list_data[[i]]
  
  temlist <- split(temdata, f = temdata$MDATE)
}




