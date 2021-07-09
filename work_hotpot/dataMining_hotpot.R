###################################### TASK 0 ######################################

## 清除工作环境
cat('\014')    ## 清空命令行窗口

rm(list = ls())    ## 清空工作变量

## 加载所需的包
library(dplyr)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(MASS)
library(plyr)
library(pROC)
library(RColorBrewer)
library(ROCR)
library(Rwordseg)
library(scales)
library(stringr)
library(tm)
library(wordcloud2)

## 设置系统工作路径
setwd('F:\\复旦大学\\课程课件\\新大二下学期\\1 数据挖掘\\Homework\\第5次作业')

## 设置字体样式
windowsFonts(chineseFont = windowsFont('等线'))
windowsFonts(mathFont = windowsFont('Calibri'))

###################################### TASK 1 ######################################

## 读入数据集hot.pot
hot.pot <- read.csv('HotPotForMac.csv', header = T, fileEncoding = 'UTF-8')

## 设置“字段1”和“字段2”的数据格式为字符
hot.pot$'字段1' <- as.character(hot.pot$'字段1')
hot.pot$'字段2' <- as.character(hot.pot$'字段2')

## 修改字段名称为商家店名和附加信息
names(hot.pot)[2:3] <- c('merchant.name', 'additional.information')

## 查看数据记录情况
summary(hot.pot)

###################################### TASK 2 ######################################

## 将季均销量转换为对数季均销量log.quarterly.sales.volume
hot.pot$log.quarterly.sales.volume <- log(hot.pot$quarterly.sales.volume + 1)

## 绘制对数季均销量的分布直方图
ggplot(hot.pot, aes(log.quarterly.sales.volume)) +
    geom_histogram(bins = 12, binwidth = 1, fill = '#FFBFBF', color = '#FF7F7F') +
    labs(x = '对数(季均销量 + 1)', y = '频数', title ='西安火锅团购季均销量分布') +
    theme(title = element_text(family = 'chineseFont', face = 'bold', size = 12)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(family = 'mathFont', face = 'bold', size = 10)) + 
    theme(axis.text.y = element_text(family = 'mathFont', face = 'bold', size = 10)) +
    scale_x_continuous(breaks = seq(0, 10, 2))

## 查看季均销量的基本统计值
summary(hot.pot$quarterly.sales.volume)

## 查看季均销量为0的数量
sum(hot.pot$quarterly.sales.volume == 0)

###################################### TASK 3 ######################################

## 绘制上线天数的分布直方图
ggplot(hot.pot, aes(online.intervel)) +
    geom_histogram(binwidth = 30, fill = '#FFBFBF', color = '#FF7F7F') +
    labs(x = '团购上线天数(天)', y = '频数', title ='西安火锅团购上线天数分布') +
    theme(title = element_text(family = 'chineseFont', face = 'bold', size = 12)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(family = 'mathFont', face = 'bold', size = 10)) + 
    theme(axis.text.y = element_text(family = 'mathFont', face = 'bold', size = 10)) +
    scale_x_continuous(breaks = seq(0, 1200, 300))

## 查看上线天数的基本统计值
summary(hot.pot$online.intervel)

## 绘制折扣力度的分布直方图
ggplot(hot.pot, aes(100 * discount)) +
    geom_histogram(binwidth = 5, fill = '#FFBFBF', color = '#FF7F7F') +
    labs(x = '团购折扣力度(%)', y = '频数', title ='西安火锅团购折扣力度分布') +
    theme(title = element_text(family = 'chineseFont', face = 'bold', size = 12)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(family = 'mathFont', face = 'bold', size = 10)) + 
    theme(axis.text.y = element_text(family = 'mathFont', face = 'bold', size = 10)) +
    scale_x_continuous(breaks = seq(0, 100, 25))

## 查看上线天数的基本统计值
summary(hot.pot$discount)

###################################### TASK 4 ######################################

## 合并商家店名文本并删除非中文字符
words.line <- gsub('[0-9a-zA-Z]', '', Reduce('paste', hot.pot$merchant.name))

## 统计商家店名的高频词
words.frequency <- data.frame(table(segmentCN(words.line)))

## 修改高频词表的列名
names(words.frequency) <- c('word', 'frequency')

## 删除高频词表中的无意义词组
words.frequency <- words.frequency[-which(words.frequency[, 1] == '路' |
                                              words.frequency[, 1] == '区' |
                                              words.frequency[, 1] == '小区' |
                                              words.frequency[, 1] == '总店' | 
                                              words.frequency[, 1] == '门店' |
                                              words.frequency[, 1] == '分店' | 
                                              words.frequency[, 1] == '店' |
                                              words.frequency[, 1] == '火锅'), ]

## 按词频排序高频词表
words.frequency <- words.frequency[order(words.frequency[, 2], decreasing = T), ]

## 绘制商家店名的词云
wordcloud2(words.frequency[1:500, ], size = 1.5, minSize = 0.6, gridSize = 2,
           fontFamily = '微软雅黑', fontWeight = 'bold', shuffle = F,
           color = ifelse(words.frequency[, 2] > 200, '#F02222', '#C09292'), 
           backgroundColor = 'black', minRotation = -pi/2, maxRotation = pi/2,
           rotateRatio = 0.2, shape = 'circle', ellipticity = 0.7)

###################################### TASK 5 ######################################

## 离散化团购价为价格区间
hot.pot$price.interval[hot.pot$price < 50] <- '0-50'
hot.pot$price.interval[hot.pot$price >= 50 & hot.pot$price < 100] <- '50-100'
hot.pot$price.interval[hot.pot$price >= 100 & hot.pot$price < 150] <- '100-150'
hot.pot$price.interval[hot.pot$price >= 150 & hot.pot$price < 200] <- '150-200'
hot.pot$price.interval[hot.pot$price >= 200 & hot.pot$price < 250] <- '200-250'
hot.pot$price.interval[hot.pot$price >= 250 & hot.pot$price < 300] <- '250-300'
hot.pot$price.interval[hot.pot$price >= 300] <- '>300'

## 修改价格区间的顺序
hot.pot$price.interval <- factor(hot.pot$price.interval, 
                                 c('0-50', '50-100', '100-150', '150-200', '200-250', '250-300', '>300'))

## 绘制团购价对销量的箱线图
ggplot(hot.pot, aes(price.interval, log.quarterly.sales.volume)) +
    geom_boxplot(outlier.colour = '#7F7FFF', fill = '#BFBFFF', color = '#7F7FFF', varwidth = T) +
    labs(x = '团购价(元)', y = '对数(季均销量 + 1)', title ='火锅团购价对销量的影响') +
    theme(title = element_text(family = 'chineseFont', face = 'bold', size = 12)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(family = 'mathFont', face = 'bold', size = 10)) + 
    theme(axis.text.y = element_text(family = 'mathFont', face = 'bold', size = 10))

## 修改节假日通用的顺序
hot.pot$if.nonworkdays.available <- factor(hot.pot$if.nonworkdays.available, 
                                           c('周末法定节假日通用', '非周末法定节假日通用'))

## 绘制节假日通用对销量的箱线图
ggplot(hot.pot, aes(hot.pot$if.nonworkdays.available, log.quarterly.sales.volume)) +
    geom_boxplot(outlier.colour = '#7F7FFF', fill = '#BFBFFF', color = '#7F7FFF', varwidth = T) +
    labs(x = '是否周末法定节假日通用', y = '对数(季均销量 + 1)', title ='是否周末法定节假日通用对销量的影响') +
    theme(title = element_text(family = 'chineseFont', face = 'bold', size = 12)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(family = 'chineseFont', face = 'bold', size = 10)) + 
    theme(axis.text.y = element_text(family = 'mathFont', face = 'bold', size = 10))

###################################### TASK 6 ######################################

## 绘制团购评分对销量的散点图
ggplot(hot.pot, aes(hot.pot$'评分', log.quarterly.sales.volume)) +
    geom_point(color = '#6FFF6F') +
    labs(x = '评分', y = '对数(季均销量 + 1)', title ='火锅评分对销量的影响') +
    theme(title = element_text(family = 'chineseFont', face = 'bold', size = 12)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(family = 'mathFont', face = 'bold', size = 10)) + 
    theme(axis.text.y = element_text(family = 'mathFont', face = 'bold', size = 10))

###################################### TASK 7 ######################################

## 设置人名识别分词
segment.options(isNameRecognition = T)

## 进行人名识别分词
words <- segmentCN(hot.pot$merchant.name, nature = T)

## 标记包含人名词汇的商家
flag = ''
for(i in 1:nrow(hot.pot))
{
    for(j in length(words[[i]]))
    {
        if(names(words[[i]][j]) == 'nr')
        {
            hot.pot$name[i] <- '是'
            flag = 'r'
        }
        if(flag != 'r')
        {
            hot.pot$name[i] <- '否'
        }
    }
}

## 修改人名分类变量的顺序
hot.pot$name <- factor(hot.pot$name, c('否', '是'))

## 绘制人名分类变量对销量的箱线图
ggplot(hot.pot, aes(name, log.quarterly.sales.volume)) +
    geom_boxplot(outlier.colour = '#7F7FFF', fill = '#BFBFFF', color = '#7F7FFF', varwidth = T) +
    labs(x = '是否包含人名词汇', y = '对数(季均销量 + 1)', title ='是否包含人名词汇对销量的影响') +
    theme(title = element_text(family = 'chineseFont', face = 'bold', size = 12)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(family = 'mathFont', face = 'bold', size = 10)) + 
    theme(axis.text.y = element_text(family = 'mathFont', face = 'bold', size = 10))

################################### TASK 8 #########################################

## 转换团购销量为分类因变量
hot.pot$high.volume.interval[hot.pot$quarterly.sales.volume > 9] <- 1    ## 高销量水平
hot.pot$high.volume.interval[hot.pot$quarterly.sales.volume <= 9] <- 0    ## 低销量水平

## 提取逻辑回归所需的子数据集
hot.pot.glm <- hot.pot[, c('high.volume.interval', 'discount', 'price', 'privilege.for.new', 
                           'privilege.on.sale', 'district', 'if.nonworkdays.available', 
                           'time.duration', 'if.appointment.needed', 'WiFi.available', 
                           'parking', 'rooms.available', '评分', 'online.intervel',
                           'level.of.participants.limited', 'level.of.tickets.limited',
                           'place', 'food', 'taste', 'cook', 'people', 'style', 'HighFreq')]

################################### TASK 9 #########################################

## 逻辑回归建模
hot.pot.model <- glm(high.volume.interval ~ log(price) + . - price, 
                     family = binomial(), data = hot.pot.glm)

## 输出回归结果
summary(hot.pot.model)

################################### TASK 10 #########################################

## 生成预测结果数据集hot.pot.pred
hot.pot.pred <- predict(hot.pot.model, data = hot.pot)

## 计算预测概率prob
hot.pot.glm$prob <- exp(hot.pot.pred) / (1 + exp(hot.pot.pred))

## 生成prediction对象predObj
predObj <- prediction(hot.pot.glm$prob, hot.pot.glm$high.volume.interval)

## 计算ROC值hot.pot.roc
hot.pot.roc <- performance(predObj, 'tpr', 'fpr')

## 设置绘图格式
par(font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)

## 绘制ROC曲线
plot(hot.pot.roc, main = 'ROC曲线')

## 计算AUC值
performance(predObj, 'auc') @ y.values
