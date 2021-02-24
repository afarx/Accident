# 载入包
library(tidyverse)
library(surveillance)
library(lubridate)

# 导入数据
setwd('E:/code/Accident/data')
esui <- read.csv('esui.csv')


# 构建sts数据（freq=365/52/12）
esuiym <- esui %>% count(deathyear,deathweek)
dates <- as.Date("2001-01-01") + 7 * 0:(nrow(esuiym) - 1) 
momo <- new("sts", epoch = as.numeric(dates), start = c(2001, 1),  freq = 52, 
            observed = esuiym$n, epochAsDate = TRUE)

# 绘图
plot(momo[year(momo) >= 2001, ], type = observed ~ time | unit) 
plot(momo, ylab = "No. of deaths", type = observed ~ time)

# 通过control选择algorithm
control2<- list(noPeriods=10,populationOffset=FALSE,
                fitFun="algo.farrington.fitGLM.flexible",
                b=5,w=1,weightsThreshold=2.58,
                pastWeeksNotIncluded=26,
                pThresholdTrend=1,trend=TRUE,
                thresholdMethod="nbPlugin",alpha=0.1)
a1 <- farringtonFlexible(momo,control=control2)

plot(a1)

# 构建sts数据（freq=365/52/12）
# 指定日期变量
# 按月
esuiym <- esui %>% count(deathyear,deathmonth)
esuiym$ym <- paste0(esuiym$deathyear,'-',esuiym$deathmonth,'-','1') %>% as.Date()

momo <- new("sts", epoch = as.numeric(esuiym$ym), start = c(2001, 1),  freq = 12, 
            observed = esuiym$n, epochAsDate = TRUE)

# 绘图
plot(momo[year(momo) >= 2001, ], type = observed ~ time | unit) 
plot(momo, ylab = "No. of deaths", type = observed ~ time)

# 通过control选择algorithm
control2<- list(noPeriods=10,populationOffset=FALSE,
                fitFun="algo.farrington.fitGLM.flexible",
                b=5,w=1,weightsThreshold=2.58,
                pastWeeksNotIncluded=26,
                pThresholdTrend=1,trend=TRUE,
                thresholdMethod="nbPlugin",alpha=0.1)
a1 <- farringtonFlexible(momo,control=control2)

plot(a1)
