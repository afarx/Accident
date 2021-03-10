#***********************************#
# Edited by Qiao Jiaying
# Last updated: 2021/2/4
# n = xx
# (2002.1-2020.12 Suicide)
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
edc20 <- read_excel('2002_2020年抑郁+自杀死亡.xlsx')
popage <- read.csv('popsaa.csv')

# 2002-2020 ------------------------------------------------------------

edc20$ICD.3 <-  str_sub(edc20$ICD,1,3)
edc20$ICD<-toupper(edc20$ICD)
edc20$DCa<-toupper(edc20$DCa)
edc20$DCb<-toupper(edc20$DCb)
edc20$DCc<-toupper(edc20$DCc)
edc20$DCd<-toupper(edc20$DCd)
edc20$OC1<-toupper(edc20$OC1)
edc20$OC2<-toupper(edc20$OC2)
edc20$OC3<-toupper(edc20$OC3)

# 子集(根本死因为自杀) edc20ns
edc20n <- edc20 %>% filter(ishu=='1') 
edc20n$age <- interval(edc20n$BirthDate,edc20n$DeathDate) %>% as.period() %>% year()

edc20ns <- edc20n %>% filter(ICD.3 %in% paste0('X',60:84))
edc20ns$deathyear <- year(edc20ns$DeathDate)
edc20ns <- edc20ns %>% filter(deathyear>=2002)

edc20ns$BirthDate <- ymd(edc20ns$BirthDate)
edc20ns$DeathDate <- ymd(edc20ns$DeathDate)

table(edc20ns$deathyear)

# # 子集二（死因链有自杀）edc20ad
edc20a <- edc20 %>% filter(ishu=='1') 
edc20a$age <- interval(edc20a$BirthDate,edc20a$DeathDate) %>% as.period() %>% year()

edc20ad <- edc20a <-subset(edc20a,grepl("X6|X7|X80|X81|X82|X83|X84|Y87.0",edc20a$ICD)| 
                                          grepl("X6|X7|X80|X81|X82|X83|X84|Y87.0",edc20a$DCa)| 
                                          grepl("X6|X7|X80|X81|X82|X83|X84|Y87.0",edc20a$DCb)| 
                                          grepl("X6|X7|X80|X81|X82|X83|X84|Y87.0",edc20a$DCc)|
                                          grepl("X6|X7|X80|X81|X82|X83|X84|Y87.0",edc20a$DCd))
edc20ad$deathyear <- year(edc20ad$DeathDate)

edc20ad <- edc20ad %>% filter(deathyear>=2002)

edc20ad$BirthDate <- ymd(edc20ad$BirthDate)
edc20ad$DeathDate <- ymd(edc20ad$DeathDate)

a1 <- edc20ad %>% count(deathyear)
a2 <- edc20ns %>% count(deathyear)

a <- left_join(a1,a2,by='deathyear')
aa <- a %>% gather(sui,n,-deathyear)

# 折线图(数量)
ggplot(aa,aes(x=deathyear,y=n,group=sui,color=sui)) + geom_line(size=2)

edc_dif <- setdiff(edc20ad,edc20ns)
table(edc_dif$ICD.3)

#write.csv(edc20ad,'edc20ad.csv')
# 重构数据（将自杀作为根本死因）（将抑郁作为促进死因）

enew <- edc20ad

enew$realicd <- ifelse(str_sub(enew$DCd,1,3) %in% paste0('X',60:84),enew$DCd,
                       ifelse(str_sub(enew$DCc,1,3) %in% paste0('X',60:84),enew$DCc,
                              ifelse(str_sub(enew$DCb,1,3) %in% paste0('X',60:84),enew$DCb,
                                     ifelse(str_sub(enew$DCa,1,3) %in% paste0('X',60:84),enew$DCa,NA))))
enew$ifdep <- ifelse(str_sub(enew$DCd,1,3) %in% paste0('F32'),enew$DCd,
                       ifelse(str_sub(enew$DCc,1,3) %in% paste0('F32'),enew$DCc,
                              ifelse(str_sub(enew$DCb,1,3) %in% paste0('F32'),enew$DCb,
                                     ifelse(str_sub(enew$DCa,1,3) %in% paste0('F32'),enew$DCa,
                                            ifelse(str_sub(enew$OC1,1,3) %in% paste0('F32'),enew$OC1,
                                                   ifelse(str_sub(enew$OC2,1,3) %in% paste0('F32'),enew$OC2,
                                                          ifelse(str_sub(enew$OC3,1,3) %in% paste0('F32'),enew$OC3,NA)))))))
table(enew$ifdep,useNA = 'ifany')
enew$ifdepyn <- ifelse(is.na(enew$ifdep),2,1)
table(enew$deathyear,enew$ifdepyn) %>% prop.table(1)

enew$ri3 <- str_sub(enew$realicd,1,3)
table(enew$ri3)

enew_dep <- enew %>% filter(ifdepyn==1)
enew_nodep <- enew %>% filter(ifdepyn==2)

table(enew_dep$Sex,enew_dep$ri3)%>% prop.table(2) #农药
table(enew_nodep$Sex)

table(enew$deathyear,enew$Sex) %>% prop.table(1) #女性比例在下降

table(enew$deathyear)

# 数量趋势-----
# 分性别趋势
aa <- enew %>%  count(deathyear,Sex)
ggplot(aa,aes(x=deathyear,y=n,group=Sex,color=Sex)) + geom_line(size=2)

# 分年龄段趋势
enew$AgeGroup <- car::recode(enew$age,"lo:29=1;30:49=2;50:69=3;70:hi=4")
enew$AgeGroup <- as.factor(enew$AgeGroup)
aa <- enew %>%  count(deathyear,AgeGroup)
ggplot(aa,aes(x=deathyear,y=n,group=AgeGroup,color=AgeGroup)) + geom_line(size=2)

table(enew$ifdepyn,enew$AgeGroup) %>% prop.table(1)

# 分城郊趋势
enew$Area_CJ <- car::recode(enew$qdis,"c('101','104','105','106','107','109','110')='1';else='2'")
table(enew$Area_CJ)
enew$Area_CJ <- as.factor(enew$Area_CJ)
aa <- enew %>%  count(deathyear,Area_CJ)
ggplot(aa,aes(x=deathyear,y=n,group=Area_CJ,color=Area_CJ)) + geom_line(size=2)

table(enew$ifdepyn,enew$Area_CJ) %>% prop.table(1)

aa <- enew %>%  count(deathyear,AgeGroup,Area_CJ)
ggplot(aa,aes(x=deathyear,y=n,group=interaction(AgeGroup,Area_CJ),color=AgeGroup,linetype=Area_CJ)) + geom_line(size=2)

aa <- enew %>%  count(deathyear,Sex,Area_CJ)
ggplot(aa,aes(x=deathyear,y=n,group=interaction(Sex,Area_CJ),color=Sex,linetype=Area_CJ)) + geom_line(size=2)

aa <- enew %>%  count(deathyear,ifdepyn,Area_CJ)
ggplot(aa,aes(x=deathyear,y=n,group=interaction(ifdepyn,Area_CJ),color=as.factor(ifdepyn),linetype=Area_CJ)) + geom_line(size=2)

# 率趋势-----
# 先降后升
# APC
# 年度死亡率
popi <- popage %>% group_by(Year) %>% summarise(count=sum(count,na.rm = T)) 
ni <- enew %>% count(deathyear)
popn <- left_join(ni,popi,by=c('deathyear'='Year')) 
popn$rate <- popn$n/popn$count*100000 

#popout0 <- popn %>% gather(type,number,-deathyear) %>% spread(deathyear,number)  #横向扩展
write.csv(popn,'popout0.csv')

# 年度分性别死亡率
popage$Sex <- as.character(popage$Sex)
popi <- popage %>% group_by(Year,Sex) %>% summarise(count=sum(count,na.rm = T)) 
enew$Sex <- as.character(enew$Sex)
ni <- enew %>% count(deathyear,Sex)
popnsex <- left_join(ni,popi,by=c('deathyear'='Year','Sex'='Sex')) 
popnsex$rate <- popnsex$n/popnsex$count*100000

write.csv(popnsex,'popout1.csv')

# 年度分年龄段死亡率
popage$AgeGroupN <- car::recode(popage$AgeGroup,"c('0-','1-','5-','10-','15-','20-','25-')='<30';
                                c('30-','35-','40-','45-')='30-49';
                                c('50-','55-','60-','65-')='50-69';
                                else='70+'")
popi <- popage %>% group_by(Year,AgeGroupN) %>% summarise(count=sum(count,na.rm = T)) 

enew$AgeGroup <- car::recode(enew$age,"lo:29='<30';30:49='30-49';50:69='50-69';70:hi='70+'")
ni <- enew %>% count(deathyear,AgeGroup)
popnage <- left_join(ni,popi,by=c('deathyear'='Year','AgeGroup'='AgeGroupN')) 
popnage$rate <- popnage$n/popnage$count*100000

write.csv(popnage,'popout2.csv')

# 年度分城郊死亡率
popi <- popage %>% group_by(Year,Area) %>% summarise(count=sum(count,na.rm = T)) 
popi$Area <- as.factor(popi$Area)
ni <- enew %>% count(deathyear,Area_CJ)
popnarea <- left_join(ni,popi,by=c('deathyear'='Year','Area_CJ'='Area')) 
popnarea$rate <- popnarea$n/popnarea$count*100000

write.csv(popnarea,'popout3.csv')


# 百分比趋势----
# 性别
# 男性比例不断上升
esex <- enew %>%
  group_by(deathyear,Sex) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

write.csv(esex,'esex.csv')

# 年龄
# 不规律变化
eage <- enew %>%
  group_by(deathyear,AgeGroup) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

write.csv(eage,'eage.csv')

# 城郊
# 城市所占百分比逐年下降
# 郊区所占百分比逐年上升
earea <- enew %>%
  group_by(deathyear,Area_CJ) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

write.csv(earea,'earea.csv')

# 抑郁
# 逐年上升
edep <- enew %>%
  group_by(deathyear,ifdepyn) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

write.csv(edep,'edep.csv')

# 自杀方式
# 农药比例逐年减少（自杀方式第二25.24% 下降至自杀方式第四 3.93%）
# 跳楼比例逐年上升（自杀方式第三15.22% 上升至自杀方式第一 39.54%）
# 上吊比例稳定（自杀方式第一33.37% 下降至自杀方式第二 35.84%）

# X60 退热药 X61 安眠药 X62 致幻药 X63 胆碱药 X64 未特指药 X65 酒精 X66 有机溶剂 
# X67 一氧化碳 X68 杀虫剂 X69 其他农药 X70 悬吊窒息 X71 淹溺 X72 手枪 X73 步枪 X74 火器 X75 爆炸
# X76 烧死 X77 蒸气自害 X78 尖锐物体 X79 钝器 X80 高处跳下 X81 移动物体 X82 撞机动车 X83 其他自害 X84 未特指
table(enew$suimode)
enew$suimode <- car::recode(enew$ri3,"c('X60','X61','X62','X63','X64','X65','X66')='Drug';
                            c('X67')='CO';c('X68','X69')='Pesticide';c('X70')='Hang';
                            c('X71')='Drown';c('X78','X79')='Object';c('X80')='Jump';
                            c('X72','X74','X76','X77','X81','X82','X83','X84')='Others'")
table(enew$realicd,useNA = 'ifany')
enew1 <- enew %>% filter(!is.na(enew$suimode))
emode <- enew1 %>%
  group_by(deathyear,suimode) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

write.csv(emode,'emode.csv')

# Draw some pictures--------------------------------------------------------------------------
# 折线图
ggplot(popn,aes(x=deathyear,y=rate)) + geom_line(size=2)+
  scale_x_continuous(breaks = seq(2002,2020,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,7.5))+
  labs(title="Suicide Rates in Shanghai,China in 2002-2020",
       x="Year", y="Suicide rate per 100,000 population")+ #标题横纵标目
  guides(color=guide_legend(title="Gender"))+
  scale_color_jama()+theme_hc()+
  theme(
    plot.title= element_text(size=14,hjust = 0.5,vjust =1, face="bold"),
    axis.title.x =  element_text(size=12, face = "bold"),
    axis.title.y =  element_text(size=12, face = "bold"),
    axis.text.x = element_text(angle=90) #修改坐标轴样式
  )

popnsex$Sex <- factor(popnsex$Sex,levels = c(1,2),labels = c('Men','Women'))
ggplot(popnsex,aes(x=deathyear,y=rate,group=Sex,color=Sex)) + geom_line(size=2)+
  scale_x_continuous(breaks = seq(2002,2020,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,7.5))+
  labs(title="Suicide Rates in Shanghai,China in 2002-2020 by gender",
       x="Year", y="Suicide rate per 100,000 population")+ #标题横纵标目
  guides(color=guide_legend(title="Gender"))+
  scale_color_jama()+theme_hc()+
  theme(
    plot.title= element_text(size=14,hjust = 0.5,vjust =1, face="bold"),
    axis.title.x =  element_text(size=12, face = "bold"),
    axis.title.y =  element_text(size=12, face = "bold"),
    axis.text.x = element_text(angle=90) #修改坐标轴样式
  )

ggplot(popnage,aes(x=deathyear,y=rate,group=AgeGroup,color=AgeGroup)) + geom_line(size=2)+
  scale_x_continuous(breaks = seq(2002,2020,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,25))+
  labs(title="Suicide Rates in Shanghai,China in 2002-2020 by AgeGroup",
       x="Year", y="Suicide rate per 100,000 population")+ #标题横纵标目
  guides(color=guide_legend(title="AgeGroup"))+
  scale_color_jama()+theme_hc()+
  theme(
    plot.title= element_text(size=14,hjust = 0.5,vjust =1, face="bold"),
    axis.title.x =  element_text(size=12, face = "bold"),
    axis.title.y =  element_text(size=12, face = "bold"),
    axis.text.x = element_text(angle=90) #修改坐标轴样式
  )

popnarea$Area_CJ <- factor(popnarea$Area_CJ,levels = c(1,2),labels = c('Urban','Rural'))
ggplot(popnarea,aes(x=deathyear,y=rate,group=Area_CJ,color=Area_CJ)) + geom_line(size=2)+
  scale_x_continuous(breaks = seq(2002,2020,1))+
  scale_y_continuous(expand = c(0,0),limits = c(0,7.5))+
  labs(title="Suicide Rates in Shanghai,China in 2002-2020 by Area",
       x="Year", y="Suicide rate per 100,000 population")+ #标题横纵标目
  guides(color=guide_legend(title="Area"))+
  scale_color_jama()+theme_hc()+
  theme(
    plot.title= element_text(size=14,hjust = 0.5,vjust =1, face="bold"),
    axis.title.x =  element_text(size=12, face = "bold"),
    axis.title.y =  element_text(size=12, face = "bold"),
    axis.text.x = element_text(angle=90) #修改坐标轴样式
  )

# 堆积条形图
esex$Sex <- as.factor(esex$Sex )
ggplot(edep,aes(x=deathyear,y=prop,fill=ifdepyn))+
  geom_bar(stat='identity',width=0.6,position = 'stack')+
  labs(x="Year",
       y="Percentage of depression")+ 
  scale_fill_jama()+
  theme(
    plot.title= element_text(size=18,hjust = 0.5,vjust =1, face="bold"),
    axis.title.x =  element_text(size=12, face = "bold"),
    axis.title.y =  element_text(size=12, face = "bold"),
    axis.text.x = element_text(angle=90) #修改坐标轴样式
  )

edep$ifdepyn <- as.factor(edep$ifdepyn)
ggplot(edep,aes(x=deathyear,y=prop,fill=ifdepyn))+
  geom_bar(stat='identity',width=0.6,position = 'stack')+
  labs(x="Year",
       y="Percentage of depression")+ 
  scale_fill_jama()+
  theme(
    plot.title= element_text(size=18,hjust = 0.5,vjust =1, face="bold"),
    axis.title.x =  element_text(size=12, face = "bold"),
    axis.title.y =  element_text(size=12, face = "bold"),
    axis.text.x = element_text(angle=90) #修改坐标轴样式
  )

# 堆积条形图
emode$suimode <- as.factor(emode$suimode)
ggplot(emode,aes(x=deathyear,y=prop,fill=suimode))+
  geom_bar(stat='identity',width=0.6,position = 'stack')+
  labs(x="Year",
       y="Percentage of depression")+ 
  scale_fill_lancet()+
  theme(
    plot.title= element_text(size=18,hjust = 0.5,vjust =1, face="bold"),
    axis.title.x =  element_text(size=12, face = "bold"),
    axis.title.y =  element_text(size=12, face = "bold"),
    axis.text.x = element_text(angle=90) #修改坐标轴样式
  )

emode1 <- enew1 %>% filter(Sex=='1') %>% 
  group_by(deathyear,suimode) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

ggplot(emode1,aes(x=deathyear,y=prop,fill=suimode))+
  geom_bar(stat='identity',width=0.6,position = 'stack')+
  labs(x="Year",
       y="Percentage of depression")+ 
  scale_fill_lancet()+
  theme(
    plot.title= element_text(size=18,hjust = 0.5,vjust =1, face="bold"),
    axis.title.x =  element_text(size=12, face = "bold"),
    axis.title.y =  element_text(size=12, face = "bold"),
    axis.text.x = element_text(angle=90) #修改坐标轴样式
  )

# 亚组分析 deathyear Sex AgeGroup Area_CJ suimode ifdepyn-----------------------
# Sex
enew %>%
  group_by(Sex) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# Sex*AgeGroup
# 50-69年龄段占比最多，相对来说，男性在70岁以下年龄段自杀占比较多（男73.5%，女67.2%），
enew %>%
  group_by(Sex,AgeGroup) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# Sex*Area_CJ
# 毫无差别
enew %>%
  group_by(Sex,Area_CJ) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# Sex*suimode
# 女性农药更多，其他差异不明显
enew %>%
  group_by(Sex,suimode) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# Sex*ifdepyn
# 女性自杀者中抑郁比例更高
enew %>%
  group_by(Sex,ifdepyn) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )
#-----------------------------------------------------------#
# AgeGroup*Sex
# 各年龄段男性占比均高于女性，相对来说，老年女性占比上升
enew %>%
  group_by(AgeGroup,Sex) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# AgeGroup*Area_CJ
# 郊区（55%）高于城区（45%），各年龄段无明显差异
enew %>%
  group_by(AgeGroup,Area_CJ) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# AgeGroup*Suimode
# 跳楼是 <30 30-49主要自杀方式 上吊是50-69 70+主要自杀方式 
# 随着年龄段变化，上吊占比越来越高
enew %>%
  group_by(AgeGroup,suimode) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# AgeGroup*ifdepyn
# 50-69年龄组抑郁占比最高
enew %>%
  group_by(AgeGroup,ifdepyn) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )
#-----------------------------------------------------------#
# Area_CJ*Sex
# 毫无差别
enew %>%
  group_by(Area_CJ,Sex) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# Area_CJ*AgeGroup
# 毫无差别
enew %>%
  group_by(Area_CJ,AgeGroup) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# Area_CJ*suimode
# 城区 跳楼（37.2%）上吊（34.9%） 溺水（7.52%） CO（6.48%）
# 郊区 上吊（39.7%）跳楼（22.7%） 农药（17.2%） 溺水（10.6%）
enew %>%
  group_by(Area_CJ,suimode) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# Area_CJ*ifdepyn
# 城区抑郁报告率更高（17.2% vs 12.5%）
enew %>%
  group_by(Area_CJ,ifdepyn) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

#-----------------------------------------------------------#
# suimode*Sex
# 溺水、农药 女性更多
enew %>%
  group_by(suimode,Sex) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# suimode*AgeGroup
# CO 30-49; Drug 70+;Hang 50-69(40%) 70+(35%)
enew %>%
  group_by(suimode,AgeGroup) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# suimode*Area_CJ
# 毫无差别
enew %>%
  group_by(suimode,Area_CJ) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# suimode*suimode
# 80.1%农药发生在郊区
enew %>%
  group_by(suimode,suimode) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# suimode*ifdepyn
# 女性自杀者中抑郁比例更高
enew %>%
  group_by(suimode,ifdepyn) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )
#-----------------------------------------------------------#
# ifdepyn*Sex
# 抑郁自杀 女性更多
enew %>%
  group_by(ifdepyn,Sex) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# ifdepyn*AgeGroup
# 抑郁自杀中 50-69最多（47.2%）
enew %>%
  group_by(ifdepyn,AgeGroup) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# ifdepyn*Area_CJ
# 抑郁自杀 城市比例高
enew %>%
  group_by(ifdepyn,Area_CJ) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

# ifdepyn*suimode
# 抑郁自杀中 跳楼高于上吊
enew %>%
  group_by(ifdepyn,suimode) %>%
  summarise(count = n() ) %>%
  mutate( prop = count / sum(count)*100 )

