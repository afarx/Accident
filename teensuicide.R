#***********************************#
# Edited by Qiao Jiaying
# Last updated: 2021/1/5
# n = xx
# (2002.1-2019.12 Suicide)
#***********************************#

# 前期准备 ------------------------------------------------------------
options(scipen=200)

library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)

setwd('E:\\code\\Accident\\data')

# 读入文件 ------------------------------------------------------------
edc0218 <- read_excel('2002_19自杀死亡.xlsx')

code <- read.csv('E:\\科室资料\\行政区划.csv')
popage <- read.csv('E:\\code\\traffic\\data\\popage.csv')

# 子集
edc0219 <- edc0218[,1:25]

# 统一变量名
names_edc <- names(edc0219)
names(edc0219) <- paste0('x', 1:ncol(edc0219))

edc0219$ICD.3 <- str_sub(edc0219$x24,1,3)
table(edc0219$ICD.3)

# 自杀-----------------------------------------------------------

suicide <- edc0219 %>% filter(ICD.3 %in% paste0('X',60:84))

# X60-X84 自害
table(suicide$ICD.3)

# 年份提取
suicide$year <- substr(suicide$x11,1,4)

suicide <- suicide %>% filter(year %in% 2002:2019)

suicide$month <- substr(suicide$x11,6,7)
table(suicide$year) #未见明显差异

table(suicide$year,suicide$month) #未见明显差异

table(suicide$year,suicide$x5) %>% prop.table(1) #性别

suicide$age <- year(suicide$x11)- year(suicide$x10)
suicide[is.na(suicide$age),]

summary(suicide$age) #年龄

teen <- suicide %>% filter(suicide$age<=18)

# 亚组分析
# X70上吊 /X80跳楼 /X71 跳河 /X68 农药 /X67 煤气/X78 割腕/X61 安眠药/
table(teen$year)
table(teen$year,teen$month)
table(teen$year,teen$ICD.3)

teen <- suicide %>% filter(suicide$age<=18 & year>=2010)

# 亚组分析
# X70上吊 /X80跳楼 /X71 跳河 /X68 农药 /X67 煤气/X78 割腕/X61 安眠药/
table(teen$year)
table(teen$year,teen$month)
# X70上吊 /X80跳楼 /X71 跳河 /X68 农药 /X67 煤气/X78 割腕/X61 安眠药/
table(teen$year,teen$ICD.3)

table(suicide$year,suicide$x5) %>% prop.table(1) #性别
table(teen$year,teen$x5) %>% prop.table(1) %>% round(2) #性别
