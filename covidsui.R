#***********************************#
# Edited by Qiao Jiaying
# Last updated: 2021/2/4
# n = xx
# (2001.1-2020.12 Suicide)
# Pandemic vs previous years
#***********************************#

# 前期准备 ------------------------------------------------------------
options(scipen=200)

library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)
library(ggsci)
library(ggthemes)

setwd('E:\\code\\Accident\\data')

# 读入文件 ------------------------------------------------------------
edc0219 <- read_excel('2002_19自杀死亡.xlsx')
edc20 <- read_excel('2020年1-12月伤害死亡.xlsx')
code <- read.csv('E:\\基础资料\\行政区划.csv')
popage <- read.csv('popsaa.csv')

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

ee2001a <- ee2001 %>%  filter(icd %in% paste0('X',60:84)) 
ee2001b <- ee2001a[!duplicated(ee2001a$id),] %>% select(NAME,deathyear,SEX,BIRTH,DEATH,age,icd)
names(ee2001b) <- c('name','deathyear','sex','birth','death','age','icd')


# 2002-2020 ------------------------------------------------------------
# 子集
# 02-19
names(edc0219)
edc16 <- edc0219[,c(1,3,5,10,11,12,24,13)]
edc16$age <- interval(edc16$BirthDate,edc16$DeathDate) %>% as.period() %>% year()
edc16$Deathyear <- year(ymd(as.Date(edc16$DeathDate)))

edc16$hu<-  str_sub(edc16$Addr,1,2)

edc16$huji <- ifelse(is.na(edc16$上海行政区划代码),ifelse(edc16$hu=='上海',1,2),1)

edc16hu <- edc16 %>% filter(huji==1)

edc16hu$ICD.3 <-  str_sub(edc16hu$ICD,1,3)
e0219sui <- edc16hu %>% select(2,10,3,4,5,9,13) %>% filter(ICD.3 %in% paste0('X',60:84))
names(e0219sui) <- c('name','deathyear','sex','birth','death','age','icd')

# 20.12
edc208 <- edc20[,c('姓名','性别编码','出生日期','死亡日期','根本死因代码','户籍区分')]
edc208hu <- edc208 %>% filter(户籍区分=='沪籍人口')
edc208hu$age <- interval(edc208hu$出生日期,edc208hu$死亡日期) %>% as.period() %>% year()
edc208hu$deathyear <- year(ymd(edc208hu$死亡日期))
edc208hu$ICD.3 <-  str_sub(edc208hu$根本死因代码,1,3)
e20sui <- edc208hu %>% select(1,8,2,3,4,7,9) %>% filter(ICD.3 %in% paste0('X',60:84))
names(e20sui) <- c('name','deathyear','sex','birth','death','age','icd')

# 构建2001-2020.12 沪籍自杀全数据集--------------------------------------------
ee2001b$birth <- ymd(str_sub(ee2001b$birth,1,10))
ee2001b$death <- ymd(str_sub(ee2001b$death,1,10))
e0219sui$birth <- ymd(str_sub(e0219sui$birth,1,10))
e0219sui$death <- ymd(str_sub(e0219sui$death,1,10))
e20sui$birth <- ymd(str_sub(e20sui$birth,1,10))
e20sui$death <- ymd(str_sub(e20sui$death,1,10))

esui <- bind_rows(ee2001b,e0219sui,e20sui)%>% filter(deathyear>=2001)
table(esui$deathyear)
esui$birthyear <- lubridate::year(esui$birth)
esui$birthmonth <- lubridate::month(esui$birth)
esui$birthday <- lubridate::day(esui$birth)

esui$sx <- esui$birthyear %% 12 
esui$sxx <- car::recode(esui$sx,"0='Monkey';1='Rooster';2='Dog';3='Pig';
                        4='Rat';5='OX';6='Tiger';7='Rabbit';8='Dragon';
                        9='Snake';10='Horse';11='Sheep'")
b <- table(esui$sxx) %>% as.data.frame() %>% arrange(Freq)


esui$deathmonth <- lubridate::month(esui$death)

table(esui$deathmonth,useNA = 'ifany') #检查空值
esui[is.na(esui$deathmonth),]

esui$deathday <- lubridate::day(esui$death)

esui$deathweekdays <- lubridate::wday(esui$death, label = TRUE)

esui$deathweek <- lubridate::week(esui$death)

esui$AgeGroup <- car::recode(esui$age,"lo:30=1;31:49=2;50:69=3;70:hi=4")

#write.csv(esui,'esui.csv',row.names = F)

table(esui$deathweekdays)
a <- table(esui$deathday) %>% as.data.frame() %>% arrange(Freq)


# 热力图
#all
x <- as.matrix(table(esui$deathyear,esui$deathmonth))
heatmap(x,Rowv = NA, Colv = NA)

#male
esui2 <- esui %>% dplyr::filter(sex=='1')
x <- as.matrix(table(esui2$deathyear,esui2$deathmonth))
heatmap(x,Rowv = NA, Colv = NA)

#female
esui3 <- esui %>% dplyr::filter(sex=='2')
x <- as.matrix(table(esui3$deathyear,esui3$deathmonth))
heatmap(x,Rowv = NA, Colv = NA)

# 折线图(数量)
ggplot(esuiy,aes(x=deathyear,y=n,group=sex,color=sex)) + geom_line(size=2)+
  #geom_text(nudge_y = 1,nudge_x = 0.5,color='black')+
  scale_x_continuous(breaks = seq(2000,2019,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,200))+
  # title="Number of suicide death:2000-2020",
  labs(x="Year", y="Death Number")+ #标题横纵标目
  guides(color=guide_legend(title="Gender"))+
  scale_color_lancet()+theme_hc()+
  theme(
    plot.title= element_text(size=18,hjust = 0.5,vjust =1, face="bold"),
    axis.title.x =  element_text(size=12, face = "bold"),
    axis.title.y =  element_text(size=12, face = "bold"),
    axis.text.x = element_text(angle=90) #修改坐标轴样式
  ) +facet_wrap(~AgeGroup)



#X80高坠 /X71溺水 /X70自缢 /X68 农药 /X67 煤气/X61 其他/
esui$methodg <- car::recode(esui$icd,"'X70'=1;'X80'=2;'X68'=3;'X71'=4;'X67'=5;else=6")
table(esui$deathyear,esui$methodg)

# 星座---------------------------------
zodiac <- function(mon,day){
  zodiac <- 
  ifelse(mon==12,ifelse(day<22,'Sagittarius','Capricorn'),
  ifelse(mon==1,ifelse(day<20,'Capricorn','Aquarius'),
  ifelse(mon==2,ifelse(day<19,'Aquarius','Pisces'),
  ifelse(mon==3,ifelse(day<21,'Pisces','Aries'),
  ifelse(mon==4,ifelse(day<20,'Aries','Taurus'),
  ifelse(mon==5,ifelse(day<21,'Taurus','Gemini'),
  ifelse(mon==6,ifelse(day<21,'Gemini','Cancer'),
  ifelse(mon==7,ifelse(day<23,'Cancer','Leo'),
  ifelse(mon==8,ifelse(day<23,'Leo','Virgo'),
  ifelse(mon==9,ifelse(day<23,'Virgo','Libra'),
  ifelse(mon==10,ifelse(day<23,'Libra','Scorpio'),
  ifelse(day<22,'Scorpio','Sagittarius'))))))))))))
}

esui$zodiac <- zodiac(esui$birthmonth,esui$birthday)

esui$zodiac4 <- car::recode(esui$zodiac,"c('Aries','Leo','Sagittarius')='Fire';
                                         c('Gemini','Libra','Aquarius')='Wind';
                                         c('Pisces','Cancer','Scorpio')='Water';
                                         c('Taurus','Virgo','Capricorn')='Soil'")

a <- table(esui$zodiac4) %>% as.data.frame() %>% arrange(Freq)
b1 <- table(esui$zodiac) %>% as.data.frame() %>% arrange(Freq)
c <- table(esui$birthmonth) %>% as.data.frame() %>% arrange(Freq)

deathd <- read.csv('E:\\code\\CCD104\\data\\deathd.csv')
deathd$birthyear <- lubridate::year(deathd$x25)
deathd$birthmonth <- lubridate::month(deathd$x25)
deathd$birthday <- lubridate::day(deathd$x25)
deathd$zodiac <- zodiac(deathd$birthmonth,deathd$birthday)
b <- table(deathd$zodiac) %>% as.data.frame() %>% arrange(Freq)
bb <- left_join(b,b1,by='Var1')
bb$rate <- bb$Freq.y/bb$Freq.x*100 
bb <- bb %>% arrange(rate)

# 2020产院出生-----------------------
setwd('E:\\code\\Accident\\data')
birth2020 <- read_excel('birth2020hu.xlsx')
names(birth2020) <- c('name','sex','birth')

birth2020$birthyear <- lubridate::year(birth2020$birth)
birth2020$birthmonth <- lubridate::month(birth2020$birth)
birth2020$birthday <- lubridate::day(birth2020$birth)
birth2020$zodiac <- zodiac(birth2020$birthmonth,birth2020$birthday)

u <- table(birth2020$zodiac) %>% as.data.frame() %>% arrange(Freq)
bb <- left_join(u,b1,by='Var1')

bb$rate <- bb$Freq.y/bb$Freq.x*100 
bb <- bb %>% arrange(rate)

uu <- table(birth2020$birthmonth) %>% as.data.frame() %>% arrange(Freq)
