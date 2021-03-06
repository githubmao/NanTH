#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20180930, by MaoYan
#
# Description:
# NanTH客车通过性分析数据导入。
#------------------------------------------------------------------------------#


library(data.table)
library(ggplot2)
library(gridExtra)
options(scipen = 200)


# 导入drivingTraj数据----
setwd(dir = "E:/R/NanTH/Data/DrivingTraj")  # 设置工作目录
kFileList <- list.files(pattern = "*.txt")  # 分析数据文件名
kDataName <- gsub(".txt", "", kFileList)  # 导入数据集命名
df.drivingtraj <- data.frame()  # 创建用于合并数据的数据集

for (kFileIdx in 1:length(kFileList)) {  # 导入数据
  
  tmp.df <- read.table(file = kFileList[kFileIdx],
                       header = TRUE,
                       sep = "",
                       stringsAsFactors = FALSE,
                       skip = 1,
                       col.names = c("posX", "posY"))
  
  kXDesignRowNo <- which(tmp.df$posX == "X_Design")
  kXTargetRowNo <- which(tmp.df$posX == "X_Target")
  
  tmp.df1 <- tmp.df[1:(kXDesignRowNo - 1),]
  tmp.df1$TrajTyp <- "DrivingTraj"
  
  tmp.df2 <- tmp.df[(kXTargetRowNo + 1):length(tmp.df$posX),]
  tmp.df2$TrajTyp <- "TargetTraj"
  
  tmp.df <- rbind(tmp.df1, tmp.df2)
  
  # 添加半径值
  tmp.df$curveRad <- strsplit(x = kDataName[kFileIdx], split = "_")[[1]][2]
  # 添加行驶速度
  tmp.df$drivingSpeed <- strsplit(x = kDataName[kFileIdx], split = "_")[[1]][3]
  
  df.drivingtraj <- rbind(df.drivingtraj, tmp.df)
}


# 导入latForCoeff数据----
setwd(dir = "E:/R/NanTH/Data/latForCoeff")
kFileList <- list.files(pattern = "*.txt")
kDataName <- gsub(".txt", "", kFileList)
df.latforcoeff <- data.frame()

for (kFileIdx in 1:length(kFileList)) {
  
  tmp.df <- read.table(file = kFileList[kFileIdx],
                       header = TRUE,
                       sep = "",
                       stringsAsFactors = FALSE,
                       skip = 1,
                       col.names = c("Station", "LatFor"))
  
  kStaRowNo <- which(tmp.df$Station == "Station")
  
  tmp.df1 <- tmp.df[1:(kStaRowNo[1]-1),]
  tmp.df1$LatForTyp <- "LatForA1"
  
  tmp.df2 <- tmp.df[(kStaRowNo[1]+1):(kStaRowNo[2]-1),]
  tmp.df2$LatForTyp <- "LatForA2"
  
  tmp.df3 <- tmp.df[(kStaRowNo[2]+1):length(tmp.df$Station),]
  tmp.df3$LatForTyp <- "LatForAll"
  
  tmp.df <- rbind(tmp.df1, tmp.df2, tmp.df3)
  
  # 添加半径值
  tmp.df$curveRad <- strsplit(x = kDataName[kFileIdx], split = "_")[[1]][2]
  # 添加行驶速度
  tmp.df$drivingSpeed <- strsplit(x = kDataName[kFileIdx], split = "_")[[1]][3]
  
  df.latforcoeff <- rbind(df.latforcoeff, tmp.df)
}

which(tmp.df$Station == "Station")[2]


# 导入longSpeed数据----
setwd(dir = "E:/R/NanTH/Data/longSpeed")
kFileList <- list.files(pattern = "*.txt")
kDataName <- gsub(".txt", "", kFileList)
df.longspeed <- data.frame()

for (kFileIdx in 1:length(kFileList)) {  # 导入数据
  
  kColName <- c("Station", "Speed")
  
  tmp.df <- fread(kFileList[kFileIdx],
                  header = T,
                  sep = "auto",
                  stringsAsFactors = FALSE,
                  data.table = FALSE,
                  skip = "Station",
                  col.names = kColName)
  
  # 添加半径值
  tmp.df$curveRad <- strsplit(x = kDataName[kFileIdx], split = "_")[[1]][2]
  # 添加行驶速度
  tmp.df$drivingSpeed <- strsplit(x = kDataName[kFileIdx], split = "_")[[1]][3]
  
  df.longspeed <- rbind(df.longspeed, tmp.df)
}


# 导入trackOff数据----
setwd(dir = "E:/R/NanTH/Data/trackOff")
kFileList <- list.files(pattern = "*.txt")
kDataName <- gsub(".txt", "", kFileList)
df.trackoff <- data.frame()

for (kFileIdx in 1:length(kFileList)) {  # 导入数据
  
  kColName <- c("Station", "TrackOff")
  
  tmp.df <- fread(kFileList[kFileIdx],
                  header = T,
                  sep = "auto",
                  stringsAsFactors = FALSE,
                  data.table = FALSE,
                  skip = "Station",
                  col.names = kColName)
  
  # 添加半径值
  tmp.df$curveRad <- strsplit(x = kDataName[kFileIdx], split = "_")[[1]][2]
  # 添加行驶速度
  tmp.df$drivingSpeed <- strsplit(x = kDataName[kFileIdx], split = "_")[[1]][3]
  
  df.trackoff <- rbind(df.trackoff, tmp.df)
}

