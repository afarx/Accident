#***********************************#
# Edited by Qiao Jiaying
# Last updated: 2020/11/23
# n = xx
# (2015.1-2020.6 External Death Cause)
#***********************************#

# 前期准备 ------------------------------------------------------------
options(scipen=200)

library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)

setwd('E:\\code\\Accident\\data')

# 读入文件 ------------------------------------------------------------
edc0219 <- read_excel('2002_19自杀死亡.xlsx')
edc20 <- read_excel('2020年1-11月伤害死亡.xlsx')
code <- read.csv('E:\\基础资料\\行政区划.csv')
popage <- read.csv('E:\\code\\traffic\\data\\popage.csv')

edc2001 <- read_excel('2001.xlsx')
edcICD9TO10 <- read_excel('ICD9to10 FINAL.xlsx')

# 2001 ------------------------------------------------------------
e2001 <- edc2001[,c('NAME','AGE','SEX','BIRTH','DEATH','ICD','ICDE')]
e2001$BIRTH <- ymd(e2001$BIRTH)
e2001$DEATH <- ymd(e2001$DEATH)
#年龄计算
e2001$age <- interval(e2001$BIRTH,e2001$DEATH) %>% as.period() %>% year()
ee <- e2001 %>% filter(!is.na(e2001$ICDE))

#ICD转换
ee$icde <- paste0('E','',ee$ICDE)
ee$id <- c(1:nrow(ee))
ee2001 <- left_join(ee,edcICD9TO10,by=c('icde'='9code'))

ee2001$deathyear <- year(ee2001$DEATH)
ee2001$icd <-  str_sub(ee2001$`10code`,1,3)

ee2001a <- ee2001 %>%  filter(icd %in% paste0('X',60:84)) %>% select(deathyear,SEX,age,icd,NAME,id)
ee2001b <- ee2001a[!duplicated(ee2001a$id),] %>% select(deathyear,SEX,age,icd)
names(ee2001b) <- c('deathyear','sex','age','icd')


# 2002-2020 ------------------------------------------------------------
# 子集
# 02-19
names(edc0219)
edc16 <- edc0219[,c(1,5,10,11,12,24,13)]
edc16$age <- interval(edc16$BirthDate,edc16$DeathDate) %>% as.period() %>% year()
edc16$Deathyear <- year(ymd(as.Date(edc16$DeathDate)))

edc16$hu<-  str_sub(edc16$Addr,1,2)

edc16$huji <- ifelse(is.na(edc16$上海行政区划代码),ifelse(edc16$hu=='上海',1,2),1)

edc16hu <- edc16 %>% filter(huji==1)

edc16hu$ICD.3 <-  str_sub(edc16hu$ICD,1,3)
e0219sui <- edc16hu %>% select(9,2,8,12) %>% filter(ICD.3 %in% paste0('X',60:84))
names(e0219sui) <- c('deathyear','sex','age','icd')

# 20.11
edc208 <- edc20[,c('性别编码','出生日期','死亡日期','根本死因代码','户籍区分')]
edc208hu <- edc208 %>% filter(户籍区分=='沪籍人口')
edc208hu$age <- interval(edc208hu$出生日期,edc208hu$死亡日期) %>% as.period() %>% year()
edc208hu$deathyear <- year(ymd(edc208hu$死亡日期))
edc208hu$ICD.3 <-  str_sub(edc208hu$根本死因代码,1,3)
e20sui <- edc208hu %>% select(7,1,6,8) %>% filter(ICD.3 %in% paste0('X',60:84))
names(e20sui) <- c('deathyear','sex','age','icd')
  
# 构建2001-2020.11 沪籍自杀全数据集
esui <- bind_rows(ee2001b,e0219sui,e20sui)

teen <-  esui %>% filter(age<=22 & age>=7 & deathyear>=2001)

teen$yearg <- car::recode(teen$deathyear,"2001:2010=1;2011:2020=2")
teen$ageg <- car::recode(teen$age,"7:11=1;12:15=2;16:18=3;19:22=4")

#X80高坠 /X71溺水 /X70自缢 /X68 农药 /X67 煤气/X61 其他/
teen$methodg <- car::recode(teen$icd,"'X80'=1;'X71'=2;'X70'=3;'X68'=4;'X67'=5;else=6")

table(teen$yearg)
table(teen$yearg,teen$ageg)
table(teen$yearg,teen$sex,teen$ageg)

table(teen$yearg,teen$ageg)
table(teen$yearg,teen$methodg,teen$ageg)
table(teen$methodg)

table(teen$deathyear,teen$methodg)

#其他分析---------------------------------------------------------------
# 行政区划
edc208d <- left_join(edc208,code,by=c('审核区县'='区'))
edc208dd <- edc208d %>% select(25,2:24)
# 统一变量名
names_edc <- names(edc208dd)
names(edc208dd) <- paste0('x', 1:ncol(edc208dd))
names(edc16) <- paste0('x', 1:ncol(edc16))
names(names_edc) <- names(edc16)
names_edc
# 日期转字符
edc208dd$x11 <- as.character(edc208dd$x11)
edc16$x2 <- as.character(edc16$x2)
# 批量修改变量格式
edc16 %<>% mutate_if(is.numeric,as.character)
edc208dd %<>% mutate_if(is.numeric,as.character)
# 合并数据库
edc <- bind_rows(edc16,edc208dd)
# 统一变量格式
edc$x2 <- ymd(as.Date(edc$x2))
edc$x10 <- ymd(as.Date(edc$x10))
edc$x11 <- ymd(as.Date(edc$x11))

# 生成新变量 ------------------------------------------------------------
# 根本死因代码x24 
edc$ICD.1 <- str_sub(edc$x24,1,1)
table(edc$ICD.1)
edc$ICD.3 <- str_sub(edc$x24,1,3)
table(edc$ICD.3)


#*****************ICD分类****************#

# W00-W19 跌倒
# W20-W49 无生命机械
# W50-W64 有生命机械
# W65-W74 淹溺
# W75-W84 呼吸威胁
# w85-w99 电流辐射高压

# X00-X99 火
# X10-X19 烫
# X20-X29 有毒动植物
# X30-X39 自然灾害（火山地震洪水）
# X40-X49 意外中毒
# X50-X57 操劳过度、旅行和贫困
# X58-X59 未特指

# X60-X84 自害***
# X85-Y09 加害**

# Y10-Y34 意图不确定
# Y35-Y36 依法处置
# Y40-Y84 手术并发症
# Y85-Y89 后遗症
# Y90-Y98 其他

# F00-F09 器质性精神障碍
## 阿尔茨海默/痴呆
# F10-F19 使用精神药物
# F20-F29 精神分裂症
# F30-F39 心境障碍
# F40-F48 神经症性、应激相关、躯体形式
# F50-F59 生理紊乱
# F60-F69 行为障碍
# F70-F79 精神发育迟滞
# F80-F89 心理发育障碍
# F90-F98 起源于童年的行为和情绪障碍
# F99 精神病NOS

#************************************************#

# 频数分布
icd3 <- edc %>% count(ICD.3) %>% arrange(desc(n))

# 死因类别-----------------------------------------------------------
# X60-X84 自害
suicide <- edc %>% filter(ICD.3 %in% paste0('X',60:84))
table(suicide$ICD.3)

# X85-Y09 加害
assault <- edc %>% filter(ICD.3 %in% c(paste0('X',85:99),paste0('Y',01:09)))
table(assault$ICD.3)

# Suicide数据集---------------------------------------------------------
# suicide 分年份
# 年份提取
suicide$year <- substr(suicide$x11,1,4)
suicide$month <- substr(suicide$x11,6,7)

table(suicide$year,suicide$month) #未见明显差异

# 年龄
suicide$age <- ifelse(is.na(suicide$x12),(year(suicide$x11)- year(suicide$x10)),suicide$x12)
suicide[is.na(suicide$age),]

summary(suicide$age) #年龄

# 子集
teen <- suicide %>% filter(age<=22 & age>=7 & year>=2002)

teen$yearg <- car::recode(teen$year,"2002:2010=1;2011:2020=2")
teen$ageg <- car::recode(teen$age,"7:11=1;12:15=2;16:18=3;19:22=4")
teen$methodg <- car::recode(teen$ICD.3,"'X80'=1;'X71'=2;'X70'=3;'X68'=4;'X67'=5;else=6")

teenage <- teen %>% count(yearg,ageg,x5)
