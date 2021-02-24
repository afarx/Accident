# 前期准备 ------------------------------------------------------------
options(scipen=200)

library(tidyverse)
library(lubridate)
library(ggsci)
library(ggthemes)

setwd('E:\\code\\Accident\\data')
esui <- read.csv('esui.csv')
popage <- read.csv('popsaa.csv')

# 重分组
esui$AgeGroup <- as.factor(esui$AgeGroup)
esui$deathyearN <- car::recode(esui$deathyear,"2001:2005='1';2006:2010='2';2011:2015='3';2016:2020='4'")
esui$sex <- as.factor(esui$sex)

popage <- popage %>% filter(Year>=2001)
popage$AgeGroupN <- car::recode(popage$AgeGroup,"c('0-','1-','5-','10-','15-','20-','25-')='1';c('30-','35-','40-','45-')='2';
 c('50-','55-','60-','65-')='3';else='4'")
popage$AgeGroupN <- as.factor(popage$AgeGroupN)
popage$YearN <- car::recode(popage$Year,"2001:2005='1';2006:2010='2';2011:2015='3';2016:2020='4'")
popage$Sex <- as.factor(popage$Sex)

# 折线图(率) year*sex(同时两个分组)
popyear <- popage %>% group_by(YearN,Sex) %>% summarise(count=sum(count,na.rm = T))
esuiym <- esui %>% count(deathyearN,deathmonth,sex)

esuiympop <- left_join(esuiym,popyear,by=c('deathyearN'='YearN','sex'='Sex'))
esuiympop$rate <- esuiympop$n/esuiympop$count*100000

esuiympop$deathyearN <- as.factor(esuiympop$deathyearN)
ggplot(esuiympop,aes(x=deathmonth,y=rate,group=interaction(deathyearN,sex),color=deathyearN,linetype=sex)) + geom_line(size=2)+
  scale_x_continuous(breaks = seq(2001,2020,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,0.75))+
  # title="Number of suicide death:2000-2020",
  labs(x="Year", y="Death Number")+ #标题横纵标目
  guides(color=guide_legend(title="deathyearN"))+
  scale_color_lancet()+theme_hc()

# 分年份
esuiy <- esui %>% count(deathyear)
popyear <- popage %>% group_by(Year) %>% summarise(count=sum(count,na.rm = T))
esuiypop <- left_join(esuiy,popyear,by=c('deathyear'='Year'))
esuiypop$rate <- esuiypop$n/esuiypop$count*100000

# 折线图(率) year,sex
ggplot(esuiypop,aes(x=deathyear,y=rate)) + geom_line(size=0.3)+
  scale_x_continuous(breaks = seq(2001,2020,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,7))+
  # title="Number of suicide death:2000-2020",
  labs(x="Year", y="Rate of death") #标题横纵标目

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
esuiy <- esui %>% count(deathyear,AgeGroup,sex)
popyear <- popage %>% group_by(Year,AgeGroupN,Sex) %>% summarise(count=sum(count,na.rm = T))
esuiypop <- left_join(esuiy,popyear,by=c('deathyear'='Year','AgeGroup'='AgeGroupN','sex'='Sex'))
esuiypop$rate <- esuiypop$n/esuiypop$count*100000

# 折线图(率) year,sex,agegroup
ggplot(esuiypop,aes(x=deathyear,y=rate,color=sex,group=sex)) + geom_line(size=0.3)+
  scale_x_continuous(breaks = seq(2001,2020,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,25))+
  # title="Number of suicide death:2000-2020",
  labs(x="Year", y="Rate of death")+
  facet_grid(~AgeGroup)

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
