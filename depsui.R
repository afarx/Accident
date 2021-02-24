#***********************************#
# Edited by Qiao Jiaying
# Last updated: 2021/01/19
# n = 166 + 55
# (2020.1-2020.11 Dep/Sui)
#***********************************#

# 前期准备 ------------------------------------------------------------
options(scipen=200)

library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)
library(stringr)

setwd('E:\\code\\Accident\\data')

# 读入文件 ------------------------------------------------------------
code <- read.csv('E:\\基础资料\\行政区划.csv')
popage <- read.csv('E:\\code\\traffic\\data\\popage.csv')
edc20 <- read_excel('2020年1-12月伤害死亡.xlsx')
edc0219 <- read_excel('2002_19自杀死亡.xlsx')
edc0219f <- read_excel('2002_2019自杀f32.xlsx')

edc0219 <- bind_rows(edc0219,edc0219f)
edc0219dup <- edc0219[duplicated(edc0219$Name)=='TRUE'| duplicated(edc0219$SSID)=='TRUE',]
edc0219 <- edc0219[duplicated(edc0219$Name)=='FALSE'| duplicated(edc0219$SSID)=='FALSE',]

# 02-19
names(edc0219)
edc16 <- edc0219[,c(1,3,5,10,11,12,17:24,13)]
edc16$age <- interval(edc16$BirthDate,edc16$DeathDate) %>% as.period() %>% year()
edc16$Deathyear <- year(ymd(as.Date(edc16$DeathDate)))

edc16$hu<-  str_sub(edc16$Addr,1,2)

edc16$huji <- ifelse(is.na(edc16$上海行政区划代码),ifelse(edc16$hu=='上海',1,2),1)

edc16hu <- edc16 %>% filter(huji==1)

edc16hu$ICD.3 <-  str_sub(edc16hu$ICD,1,3)
e0219sui <- edc16hu %>% filter(ICD.3 %in% paste0('X',60:84)|ICD.3 %in% paste0('F',32))
e0219suif <- e0219sui %>% select(3,4,5,8,11,12,13,14)
names(e0219suif) <- c('性别编码','出生日期','死亡日期','直接死亡原因编码b',
                      '其他疾病诊断代码1','其他疾病诊断代码2','其他疾病诊断代码3',
                      '根本死因代码')

e0219suif$出生日期 <- ymd(e0219suif$出生日期)
e0219suif$死亡日期 <- ymd(str_sub(e0219suif$死亡日期,1,10))

# 重编码----------------------------------------------------------------------
edc208 <- edc20[,c('性别编码','出生日期','死亡日期','直接死亡原因编码b',
                   '其他疾病诊断代码1','其他疾病诊断代码2','其他疾病诊断代码3',
                   '根本死因代码','户籍区分')]

edc208hu <- edc208 %>% filter(户籍区分=='沪籍人口')
edc208hu$ICD.3 <-  str_sub(edc208hu$根本死因代码,1,3)
edc208hu <- edc208hu %>% filter(ICD.3 %in% paste0('X',60:84)|ICD.3 %in% paste0('F',32)) %>% select(1:8)

edc208hu$出生日期 <- ymd(edc208hu$出生日期)
edc208hu$死亡日期 <- ymd(edc208hu$死亡日期)

# 02-20数据集合成--------------------------------------------------------------
edc208hu <- bind_rows(e0219suif,edc208hu)

edc208hu$age <- interval(edc208hu$出生日期,edc208hu$死亡日期) %>% as.period() %>% year()

edc208hu$deathyear <- year(ymd(edc208hu$死亡日期))

edc208hu$ICD.3 <-  str_sub(edc208hu$`根本死因代码`,1,3)
edc208hu$zjICD.3 <-  str_sub(edc208hu$`直接死亡原因编码b`,1,3)
edc208hu$qt1ICD.3 <-  str_sub(edc208hu$`其他疾病诊断代码1`,1,3)
edc208hu$qt2ICD.3 <-  str_sub(edc208hu$`其他疾病诊断代码2`,1,3)
edc208hu$qt3ICD.3 <-  str_sub(edc208hu$`其他疾病诊断代码3`,1,3)

table(edc208hu$ICD.3)

# 抑郁症（F32.9）自杀筛查------------------------------------------------
dep1 <-  edc208hu %>% filter(ICD.3 %in% 'F32') # 根本死因2367例
table(dep1$deathyear,dep1$ICD.3)

dep1zj <-  dep1 %>% filter(zjICD.3 %in% paste0('X',60:84)) #直接死因自害 1492例
table(dep1zj$deathyear,dep1zj$ICD.3)

# 自杀抑郁症筛查-------------------------------------------------------
sui1 <-  edc208hu %>% filter(ICD.3 %in% paste0('X',60:84)) #自杀 11651例
#间接死因抑郁 # 441例
sui1dp <-  sui1 %>% filter(qt1ICD.3 %in% paste0('F',32)|qt2ICD.3 %in% paste0('F',32)|qt3ICD.3 %in% paste0('F',32)) 

# 再结合
dsuid <- union(dep1zj,sui1dp)
table(dsuid$deathyear,dsuid$ICD.3)

dsuid$ICD <- ifelse(dsuid$ICD.3=='F32',dsuid$zjICD.3,dsuid$ICD.3)
table(dsuid$deathyear,dsuid$ICD)


## 检查死因描述中有“自杀”、“跳”、“自缢”的说法(仅限于有病史&尸检记录)---------------------------------
dep2 <- setdiff(dep1,dep1zj)
d1ga <- dep2[str_detect(dep2$`死者生前病史及症状体征`,'自杀'),]
d2ga <- dep2[str_detect(dep2$`死者生前病史及症状体征`,'跳'),]
d3ga <- dep2[str_detect(dep2$`死者生前病史及症状体征`,'自缢'),]

d1gan <- dep2[str_detect(dep2$`尸表（尸解）检验记录`,'自杀'),]
d2gan <- dep2[str_detect(dep2$`尸表（尸解）检验记录`,'跳'),]
d3gan <- dep2[str_detect(dep2$`尸表（尸解）检验记录`,'自缢'),]

dga <- bind_rows(d1ga,d2ga,d3ga,d1gan,d2gan,d3gan)
ddga <- dga[!is.na(dga$证件号码),] #关键字筛查出 23例

dep3 <- setdiff(dep2,ddga) #dep3 非自杀死亡的抑郁症死亡（人工需再确认）

dsui <- union(dep1zj,ddga) #180例/224例

## 检查死因描述中有“抑郁”的说法
sui2 <- setdiff(sui1,sui1dp)
s1ga <- sui2[str_detect(sui2$`死者生前病史及症状体征`,'抑郁'),]
s1gan <- sui2[str_detect(sui2$`尸表（尸解）检验记录`,'抑郁'),]

s1g <- bind_rows(s1ga,s1gan)
s1g <- s1g[!is.na(s1g$证件号码),] #有抑郁症的自杀死亡（有些提及了但并不是，人工需再确认）

suid <- union(sui1dp,s1g) #66例/639例

## 后续考虑文本分析

