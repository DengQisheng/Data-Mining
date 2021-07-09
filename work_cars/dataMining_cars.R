###################################### TASK 1 ######################################

setwd("F:\\复旦大学\\课程课件\\新大二下学期\\1 数据挖掘\\Homework")    ## 设置系统工作路径

dat0 <- read.csv("第1次作业\\cars.csv", header = T, fileEncoding = "GB2312")    ## 读入数据
dim(dat0)    ## 12951行 19列

dat1 <- dat0[which(dat0$pailiang != -1 & dat0$paifang != -1), ]    ## 删除没有采集到关键变量（排放、排量）的数据
dim(dat1)    ## 12820行 19列

opar <- par(no.readonly = TRUE)    ## 保存绘图原格式

###################################### TASK 2 ######################################

dat2 <- dat1[which(duplicated.data.frame(dat1) == F), ]    ## 删除重复冗余的数据
dat3 <- dat2[which(dat2$yuanjia != 0), ]    ## 删除原价为0的数据

dat3$rvi <- dat3$baojia / dat3$yuanjia    ## 保值率 = 报价 / 原价 (RVI: Residual Value Indicator)
dat3$log_rvi <- log(dat3$baojia / (dat3$yuanjia - dat3$baojia))    ## 对数保值比率 = log(报价 / (原价 - 报价))

par(mfrow = c(1, 2), font.main = 2, font.axis = 2, font.lab = 2, lwd = 2)    ## 设置绘图格式

hist(dat3$rvi, freq = T, main = "汽车保值率直方图", col = "#7F7FFF", 
     xlab = "保值率", ylab = "频数", xlim = c(0, 1), ylim = c(0, 1500), 
     breaks = seq(0, 1, 0.05))    ## 保值率直方图

hist(dat3$log_rvi, freq = T, main = "汽车对数保值比率直方图", col = "#7F7FFF", 
     xlab = "对数保值比率", ylab = "频数", xlim = c(-3, 3), ylim = c(0, 2000), 
     breaks = seq(-5, 5, 0.25))    ## 对数保值比率直方图

par(opar)    ## 格式复位

summary(dat3)    ## 查看数据的统计情况

head(dat3[order(dat3$log_rvi), ], n = 10)    ## 查看保值率最低的10个数据
head(dat3[order(dat3$log_rvi, decreasing = T), ], n = 10)    ## 查看保值率最高的10个数据

###################################### TASK 3 ######################################

summary(dat3$paifang)    ## 查看排放标准情况
summary(dat3$pailiang)    ## 查看排量情况

for(i in 1:nrow(dat3))
{
    if(dat3[i, "paifang"] == "国五")    ## 合并排放标准
    {
        dat3[i, "nationalState"] <- "国五"
    }
    else if(dat3[i, "paifang"] == "国四")
    {
        dat3[i, "nationalState"] <- "国四"
    }
    else
    {
        dat3[i, "nationalState"] <- "国三及以下"
    }
    
    if(dat3[i, "pailiang"] < 1)    ## 合并排量
    {
        dat3[i, "class"] <- "微型轿车"
    }
    else if(dat3[i, "pailiang"] >= 1 & dat3[i, "pailiang"] < 1.6)
    {
        dat3[i, "class"] <- "普通级轿车"
    }
    else if(dat3[i, "pailiang"] >= 1.6 & dat3[i, "pailiang"] < 2.5)
    {
        dat3[i, "class"] <- "中级轿车"
    }
    else if(dat3[i, "pailiang"] >= 2.5 & dat3[i, "pailiang"] < 4)
    {
        dat3[i, "class"] <- "中高级轿车"
    }
    else
    {
        dat3[i, "class"] <- "高级轿车"
    }
}

dat3$nationalState <- factor(dat3$nationalState, levels = c("国五", "国四", "国三及以下"))    ## 调整排放标准顺序
dat3$class <- factor(dat3$class, levels = c("微型轿车", "普通级轿车", "中级轿车", "中高级轿车", "高级轿车"))    ## 调整排量顺序

par(font.main = 2, font.axis = 2, font.lab = 2, lwd = 2, pch = 1)    ## 设置绘图格式

boxplot(dat3$log_rvi ~ dat3$nationalState, main = "对数保值比率关于排放标准的箱线图", 
        xlab = "排放标准", ylab = "对数保值比率", col = "#FF9933")    ## 排放标准箱线图

boxplot(dat3$log_rvi ~ dat3$class, main = "对数保值比率关于排量的箱线图", 
        xlab = "排量", ylab = "对数保值比率", col = "#FF9933")    ## 排量箱线图

par(opar)    ## 格式复位

###################################### TASK 4 ######################################

dat <- read.csv("第1次作业\\cars-final.csv", header = T, fileEncoding = "GB2312")    ## 读入数据
datFinal <- dat[which(duplicated.data.frame(dat) == F), ]    ## 删除重复冗余的数据

colnames(datFinal)[c(30, 35)] <- c("其他厂商", "一汽大众奥迪")    ## 修改变量名称

summary(datFinal)    ## 查看数据的统计情况

for(i in 1:nrow(datFinal))    ## 处理哑变量
{
    if(datFinal[i, "SUV"] == 1)
    {
        datFinal[i, "车型"] <- "SUV"
    }
    else if(datFinal[i, "紧凑型车"] == 1)
    {
        datFinal[i, "车型"] <- "紧凑型车"
    }
    else if(datFinal[i, "中型车"] == 1)
    {
        datFinal[i, "车型"] <- "中型车"
    }
    else
    {
        datFinal[i, "车型"] <- "其他车型"
    }
}

datFinal$"车型" <- factor(datFinal$"车型", levels = c("其他车型", "SUV", "中型车", "紧凑型车"))    ## 调整车型顺序

par(mfrow = c(2, 4), font.main = 2, font.axis = 2, font.lab = 2, lwd = 1.8, pch = 1, las = 2)    ## 设置绘图格式

## 各厂商的轿车保值比率对车型的箱线图
boxplot(subset(datFinal, datFinal$"华晨宝马" == 1)$ratio ~ 
            subset(datFinal, datFinal$"华晨宝马" == 1)$"车型", 
        main = "华晨宝马", ylab = "对数保值比率", col = "#FF9933")

boxplot(subset(datFinal, datFinal$"上汽大众" == 1)$ratio ~ 
            subset(datFinal, datFinal$"上汽大众" == 1)$"车型", 
        main = "上汽大众", ylab = "对数保值比率", col = "#FF9933")

boxplot(subset(datFinal, datFinal$"上汽集团" == 1)$ratio ~ 
            subset(datFinal, datFinal$"上汽集团" == 1)$"车型", 
        main = "上汽集团", ylab = "对数保值比率", col = "#FF9933")

boxplot(subset(datFinal, datFinal$"上汽通用别克" == 1)$ratio ~ 
            subset(datFinal, datFinal$"上汽通用别克" == 1)$"车型", 
        main = "上汽通用别克", ylab = "对数保值比率", col = "#FF9933")

boxplot(subset(datFinal, datFinal$"上汽通用雪佛兰" == 1)$ratio ~ 
            subset(datFinal, datFinal$"上汽通用雪佛兰" == 1)$"车型", 
        main = "上汽通用雪佛兰", ylab = "对数保值比率", col = "#FF9933")

boxplot(subset(datFinal, datFinal$"一汽大众奥迪" == 1)$ratio ~ 
            subset(datFinal, datFinal$"一汽大众奥迪" == 1)$"车型", 
        main = "一汽大众奥迪", ylab = "对数保值比率", col = "#FF9933")

boxplot(subset(datFinal, datFinal$"长安福特" == 1)$ratio ~ 
            subset(datFinal, datFinal$"长安福特" == 1)$"车型", 
        main = "长安福特", ylab = "对数保值比率", col = "#FF9933")

boxplot(subset(datFinal, datFinal$"其他厂商" == 1)$ratio ~ 
            subset(datFinal, datFinal$"其他厂商" == 1)$"车型", 
        main = "其他厂商", ylab = "对数保值比率", col = "#FF9933")

par(opar)    ## 格式复位

###################################### TASK 5 ######################################

plot(datFinal$licheng, datFinal$ratio, main = "汽车对数保值比率对里程的散点图", 
     xlab = "里程", ylab = "对数保值比率", font.main = 2, font.lab = 2, font.axis = 2, 
     lwd = 2, pch = 16, cex = 0.6)    ## 保值比率对里程的散点图

par(opar)    ## 格式复位

####################################### END ########################################