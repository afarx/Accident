# 前期准备 ------------------------------------------------------------
options(scipen=200)

library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)
library(ggsci)
library(ggthemes)

setwd('E:\\code\\Accident\\data')
#时间序列
esui <- read.csv('esui.csv')
popage <- read.csv('popsaa.csv')

popyear <- popage %>% group_by(Year,Sex) %>% summarise(count=sum(count,na.rm = T))

#分月份
esuiym <- esui %>% count(deathyear,deathmonth,sex)

esuiympop <- left_join(esuiym,popyear,by=c('deathyear'='Year','sex'='Sex'))
esuiympop$rate <- esuiympop$n/esuiympop$count*100000

esui2020 <- esuiympop %>% filter(deathyear>=2016)

# 折线图(率) year*sex
ggplot(esui2020,aes(x=deathmonth,y=rate,group=sex,color=sex)) + geom_line(size=0.3)+
  scale_x_continuous(breaks = seq(1,12,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.75))+
  # title="Number of suicide death:2000-2020",
  labs(x="Year", y="Death Number")+ #标题横纵标目
  guides(color=guide_legend(title="Gender"))+
  scale_color_lancet()+theme_hc()+facet_grid(~deathyear)

# 分年份、性别
esuiy <- esui %>% count(deathyear,sex)
popyear <- popage %>% group_by(Year,Sex) %>% summarise(count=sum(count,na.rm = T))
esuiypop <- left_join(esuiy,popyear,by=c('deathyear'='Year','sex'='Sex'))
esuiypop$rate <- esuiypop$n/esuiypop$count*100000

# 折线图(率) year,sex
ggplot(esuiypop,aes(x=deathyear,y=rate,color=sex,group=sex)) + geom_line(size=0.3)+
  scale_x_continuous(breaks = seq(2001,2020,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,7))+
  # title="Number of suicide death:2000-2020",
  labs(x="Year", y="Rate of death") #标题横纵标目
 
# 分年份、年龄组
esuiy <- esui %>% count(deathyear,AgeGroup)
popyear <- popage %>% group_by(Year,AgeGroupN) %>% summarise(count=sum(count,na.rm = T))
esuiypop <- left_join(esuiy,popyear,by=c('deathyear'='Year','AgeGroup'='AgeGroupN'))
esuiypop$rate <- esuiypop$n/esuiypop$count*100000

# 折线图(率) year,sex
ggplot(esuiypop,aes(x=deathyear,y=rate,color=AgeGroup,group=AgeGroup)) + geom_line(size=0.3)+
  scale_x_continuous(breaks = seq(2001,2020,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,25))+
  # title="Number of suicide death:2000-2020",
  labs(x="Year", y="Rate of death") #标题横纵标目
