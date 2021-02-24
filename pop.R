#***********************************#
# Edited by Qiao Jiaying
# Last updated: 2021/2/18
# 历年人口数
# 更新至2020年
#***********************************#

# 前期准备 ------------------------------------------------------------
options(scipen=200)

library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)

setwd('E:\\code\\Accident\\data')


# 3. 历年人口数-分年份,性别,年龄组,城郊-------

# total
pop <- read_excel('pop19512020n.xls',sheet = '全市total',skip=1,col_names=T)
pop1 <- pop[1:48,1:21]
pop1$label <- 'total'
# Male
pop <- read_excel('pop19512020n.xls',sheet = '全市male',skip=1,col_names=T)
pop2 <- pop[1:48,1:21]
pop2$label <- 'male'
# Female
pop <- read_excel('pop19512020n.xls',sheet = '全市female',skip=1,col_names=T)
pop3 <- pop[1:48,1:21]
pop3$label <- 'Female'
# 中心城区total
pop4 <- read_excel('pop19512020n.xls',sheet = '中心城区total',skip=1,col_names=T)
pop4$label <- 'CTotal'
# 中心城区male
pop5 <- read_excel('pop19512020n.xls',sheet = '中心城区male',skip=1,col_names=T)
pop5$label <- 'Cmale'
# 中心城区female
pop6 <- read_excel('pop19512020n.xls',sheet = '中心城区female',skip=1,col_names=T)
pop6$label <- 'Cfemale'
# 郊区县total
pop7 <- read_excel('pop19512020n.xls',sheet = '郊区县total',skip=1,col_names=T)
pop7$label <- 'JTotal'
# 郊区县male
pop8 <- read_excel('pop19512020n.xls',sheet = '郊区县male',skip=1,col_names=T)
pop8$label <- 'Jmale'
# 郊区县female
pop9 <- read_excel('pop19512020n.xls',sheet = '郊区县female',skip=1,col_names=T)
pop9$label <- 'Jfemale'
# 合并
popall <- rbind(pop1,pop2,pop3,pop4,pop5,pop6,pop7,pop8,pop9)
names(popall)[1:2] <- c('Year','N')
popall$Year <- as.numeric(popall$Year)
popall <- popall %>% filter(Year>=2000)
write.csv(popall,'popall.csv',row.names=F) 

# 重构数据集popsaa
popsaa <- gather(popall,AgeGroup,count,-label,-Year) %>% 
  select(Year,AgeGroup,label,count) %>% 
  filter(label %in% c('Cfemale','Cmale','Jfemale','Jmale'))%>%  
  arrange(Year,AgeGroup)
popsaa$AgeGroup <- ordered(popsaa$AgeGroup,levels=c('0-','1-','5-','10-','15-','20-','25-',
                                                    '30-','35-','40-','45-','50-','55-','60-',
                                                    '65-','70-','75-','80-','85-'))
popsaa$Sex <- car::recode(popsaa$label,"'Cmale'=1;'Cfemale'=2;'Jmale'=1;'Jfemale'=2")
popsaa$Area <- car::recode(popsaa$label,"'Cmale'=1;'Cfemale'=1;'Jmale'=2;'Jfemale'=2")
popsaa$Area <- as.factor(popsaa$Area)
popsaa$Sex <- as.factor(popsaa$Sex)
popsaa$count <- as.numeric(popsaa$count)
popsaa <- popsaa %>% select(-label) 
popsaa <- popsaa[!is.na(popsaa$AgeGroup),]
write.csv(popsaa,'popsaa.csv',row.names=F) 
