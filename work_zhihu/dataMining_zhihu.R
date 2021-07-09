###################################### TASK 0 ######################################

cat('\014')    ## 清空命令行窗口

rm(list = ls())    ## 清空工作变量

###################################### TASK 1 ######################################

setwd("F:\\复旦大学\\课程课件\\新大二下学期\\1 数据挖掘\\Homework\\第3次作业")    ## 设置系统工作路径

zhihu <- read.csv("zhihu.csv", stringsAsFactors = F)    ## 读入数据（需要修改zhihu.csv文件中第一行第一列为“编号”）
summary(zhihu)    ## 查看数据大致情况

opar <- par(no.readonly = T)    ## 保存绘图原格式

###################################### TASK 2 ######################################

zhihu$对数被关注数 <- log(zhihu$被关注数)    ## 计算对数被关注数

par(mfrow = c(1, 2), font.main = 2, font.axis = 2, font.lab = 2, lwd = 2)    ## 设置绘图格式

hist(zhihu$被关注数, freq = T, main = "被关注数直方图", col = "#7F7FFF", 
     xlab = "被关注数", ylab = "频数", xlim = c(0, 900000), ylim = c(0, 1800), 
     xaxt = "n", yaxt = "n")    ## 被关注数直方图
axis(1, at = seq(0, 900000, 300000), label = c("0", "30万", "60万", "90万"))    ## 设置x轴格式
axis(2, at = seq(0, 1800, 600))    ## 设置y轴格式

hist(zhihu$对数被关注数, freq = T, main = "对数被关注数直方图", col = "#7F7FFF", 
     xlab = "对数被关注数", ylab = "频数", xlim = c(0, 15), ylim = c(0, 400), 
     xaxt = "n", yaxt = "n")    ## 对数被关注数直方图
axis(1, at = seq(0, 15, 5))    ## 设置x轴格式
axis(2, at = seq(0, 360, 120))    ## 设置y轴格式

par(opar)    ## 格式复位

###################################### TASK 3 ######################################

########## 所在公司

head(summary(factor(zhihu$公司)), n = 20)    ## 列出出现次数最多的前20个公司

zhihu$company[zhihu$公司 == "阿里巴巴集团" | zhihu$公司 == "腾讯" | zhihu$公司 == "百度"] <- "BAT"    ## 标记为BAT
zhihu$company[is.na(zhihu$company)] <- "非BAT"    ## 未标记为BAT的数据标记为非BAT
zhihu$company <- factor(zhihu$company, c("BAT", "非BAT"))    ## 调整因子排序

par(font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)    ## 设置绘图格式

boxplot(zhihu$对数被关注数 ~ zhihu$company, main = "对数被关注数关于公司的箱线图", 
        xlab = "所在公司", ylab = "对数被关注数", col = c("#FF9933", "#7F7FFF"), 
        varwidth = T, names = c("BAT", "非BAT"))    ## 所在公司的箱线图

par(opar)    ## 格式复位

########## 职业

head(summary(factor(zhihu$职业)), n = 20)    ## 列出出现次数最多的前20个职业

zhihu$occupation[zhihu$职业 == "创始人" | zhihu$职业 == "CEO" | zhihu$职业 == "联合创始人" | zhihu$职业 == "合伙人"] <- "创始人"    ## 标记为创始人
zhihu$occupation[is.na(zhihu$occupation)] <- "非创始人"    ## 未标记为创始人的数据标记为非创始人
zhihu$occupation <- factor(zhihu$occupation, c("创始人", "非创始人"))    ## 调整因子排序

par(font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)    ## 设置绘图格式

boxplot(zhihu$对数被关注数 ~ zhihu$occupation, main = "对数被关注数关于职业的箱线图", 
        xlab = "职业", ylab = "对数被关注数", col = c("#FF9933", "#7F7FFF"), 
        varwidth = T, names = c("创始人或CEO", "其他职业"))    ## 职业的箱线图

par(opar)    ## 格式复位

########## 毕业院校

head(summary(factor(zhihu$毕业院校)), n = 20)    ## 列出出现次数最多的前20个毕业院校

zhihu$graduate[zhihu$毕业院校 == "北京大学" | zhihu$毕业院校 == "清华大学" | zhihu$毕业院校 == "复旦大学" | zhihu$毕业院校 == "上海交通大学" | zhihu$毕业院校 == "浙江大学"] <- "清北复交浙"    ## 标记为清北复交浙
zhihu$graduate[is.na(zhihu$graduate)] <- "非清北复交浙"    ## 未标记为清北复交浙的数据标记为非清北复交浙
zhihu$graduate <- factor(zhihu$graduate, c("清北复交浙", "非清北复交浙"))    ## 调整因子排序

par(font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)    ## 设置绘图格式

boxplot(zhihu$对数被关注数 ~ zhihu$graduate, main = "对数被关注数关于毕业院校的箱线图", 
        xlab = "毕业院校", ylab = "对数被关注数", col = c("#FF9933", "#7F7FFF"), 
        varwidth = T, names = c("清北复交浙", "其他院校"))    ## 毕业院校的箱线图

par(opar)    ## 格式复位

###################################### TASK 4 ######################################

summary(zhihu$提问数)    ## 查看提问数的分布情况

zhihu$question[zhihu$提问数 >= 7.00] <- "高问"    ## 标记为高问
zhihu$question[is.na(zhihu$question)] <- "低问"    ## 未标记为高问的数据标记为低问
zhihu$question <- factor(zhihu$question, c("高问", "低问"))    ## 调整因子排序

par(font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)    ## 设置绘图格式

boxplot(zhihu$对数被关注数 ~ zhihu$question, main = "对数被关注数关于提问数的箱线图", 
        xlab = "提问数", ylab = "对数被关注数", col = c("#FF9933", "#7F7FFF"), 
        varwidth = T, names = c("高问", "低问"))    ## 提问数的箱线图

par(opar)    ## 格式复位

###################################### TASK 5 ######################################

##### 点赞回答比

zhihu$点赞比[zhihu$点赞数 / zhihu$回答数 > 100] <- "高"    ## 标记为高点赞比
zhihu$点赞比[zhihu$点赞数 / zhihu$回答数 <= 100] <- "低"    ## 标记为低点赞比
zhihu$点赞比 <- factor(zhihu$点赞比, c("高", "低"))    ## 调整因子排序

summary(factor(zhihu$点赞比))    ## 统计点赞比情况

par(font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)    ## 设置绘图格式

boxplot(zhihu$对数被关注数 ~ zhihu$点赞比, main = "对数被关注数关于点赞比的箱线图", 
        xlab = "点赞回答比", ylab = "对数被关注数", col = c("#FF9933", "#7F7FFF"), 
        varwidth = T, names = c("高点赞比", "低点赞比"))    ## 点赞回答比的箱线图

par(opar)    ## 格式复位

##### 感谢回答比

zhihu$感谢比[zhihu$感谢数 / zhihu$回答数 > 10] <- "高"    ## 标记为高感谢比
zhihu$感谢比[zhihu$感谢数 / zhihu$回答数 <= 10] <- "低"    ## 标记为低感谢比
zhihu$感谢比 <- factor(zhihu$感谢比, c("高", "低"))    ## 调整因子排序

summary(factor(zhihu$感谢比))    ## 统计感谢比情况

par(font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)    ## 设置绘图格式

boxplot(zhihu$对数被关注数 ~ zhihu$感谢比, main = "对数被关注数关于感谢比的箱线图", 
        xlab = "感谢回答比", ylab = "对数被关注数", col = c("#FF9933", "#7F7FFF"), 
        varwidth = T, names = c("高感谢比", "低感谢比"))    ## 感谢回答比的箱线图

par(opar)    ## 格式复位

##### 分享回答比

zhihu$分享比[zhihu$分享数 / zhihu$回答数 > 0] <- "高"    ## 标记为高分享比
zhihu$分享比[zhihu$分享数 / zhihu$回答数 <= 0] <- "低"    ## 标记为低分享比
zhihu$分享比 <- factor(zhihu$分享比, c("高", "低"))    ## 调整因子排序

summary(factor(zhihu$分享比))    ## 统计分享比情况

par(font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)    ## 设置绘图格式

boxplot(zhihu$对数被关注数 ~ zhihu$点赞比, main = "对数被关注数关于分享比的箱线图", 
        xlab = "分享回答比", ylab = "对数被关注数", col = c("#FF9933", "#7F7FFF"), 
        varwidth = T, names = c("高分享比", "低分享比"))    ## 分享回答比的箱线图

par(opar)    ## 格式复位

###################################### TASK 6 ######################################

lm.zhihu <- zhihu[c("编号", "姓名", "性别", "被关注数", 
                    "company", "occupation", "graduate", 
                    "回答数", "提问数", "专栏数", 
                    "点赞比", "感谢比", "分享比", 
                    "对数被关注数")]    ## 提取数据进行线性回归

names(lm.zhihu)[5:7] <- c("公司", "职业", "毕业院校")    ## 修改变量名称

lm.zhihu$点赞回答比 <- zhihu$点赞数 / zhihu$回答数
lm.zhihu$感谢回答比 <- zhihu$感谢数 / zhihu$回答数
lm.zhihu$分享回答比 <- zhihu$分享数 / zhihu$回答数

library(psych)    ## 载入特征可视化的psych包
pairs.panels(lm.zhihu[c(3, 8, 9, 10, 15, 16, 17, 14)])    ## 查看散点图矩阵

model.zhihu <- lm(log(被关注数) ~ 性别 + 公司 + 职业 + 毕业院校 + 回答数 
                  + 提问数 + 专栏数 + 点赞比 + 感谢比 + 分享比, lm.zhihu)    ## 进行线性回归

summary(model.zhihu)    ## 查看线性回归分析

anova(model.zhihu)    ## 查看方差分析

corMatrix <- cor(lm.zhihu[c("性别", "回答数", "提问数", "专栏数", 
                            "点赞回答比", "感谢回答比", "分享回答比", 
                            "对数被关注数")])    ## 计算协方差矩阵

library(corrplot)    ## 载入方差可视化的corrplot包
corrplot(corMatrix, order = "FPC", method = "color", 
         type = "lower", tl.cex = 0.7, tl.col = rgb(0,0,0))    ## 识别并检验多重共线性

library(car)    ## 载入回归诊断的car包
vif(model.zhihu)    ## 查看方差膨胀因子以检验多重共线性

par(mfrow = c(2, 2), font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)    ## 设置绘图格式

plot(model.zhihu)    ## 查看模型诊断图

par(opar)    ## 格式复位