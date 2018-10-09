#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20180930, by MaoYan
#
# Description:
# NanTH客车通过性分析数据分析。
#------------------------------------------------------------------------------#

# 调用DataInput.R----
source(file = "E:/R/NanTH/DataInput.R", encoding = "utf-8")

df.drivingtraj$posX <- as.numeric(df.drivingtraj$posX)
df.drivingtraj$posY <- as.numeric(df.drivingtraj$posY)

df.longspeed$Station <- as.numeric(df.longspeed$Station)
df.longspeed$Speed <- as.numeric(df.longspeed$Speed)

df.latforcoeff$Station <- as.numeric(df.latforcoeff$Station)
df.latforcoeff$LatFor <- as.numeric(df.latforcoeff$LatFor)

df.longspeed$Station <- as.numeric(df.longspeed$Station)
df.longspeed$Speed <- as.numeric(df.longspeed$Speed)


# 1 RAD, 27m
# 1.1 RAD, 27m, V20----
# drivingTraj & longSpeed----
df.drivingtrajR27V20 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R27" &
                                 df.drivingtraj$drivingSpeed == "V20")

plot.drivingtrajR27V20 <- ggplot(data = df.drivingtrajR27V20,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(-30, 30)) +
  theme(legend.position = c(0.2, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR27V20


df.longspeedR27V20 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R27" &
                               df.longspeed$drivingSpeed == "V20")

plot.longspeedR27V20 <- ggplot(data = df.longspeedR27V20,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR27V20

grid.arrange(plot.drivingtrajR27V20, plot.longspeedR27V20, ncol=2, nrow=1)


# trackoff----
df.trackoffR27V20 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R27" &
                              df.trackoff$drivingSpeed == "V20")

plot.trackoffR27V20 <- ggplot(data = df.trackoffR27V20,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR27V20

max(df.trackoffR27V20$TrackOff)


# latforcoeff----
df.latforcoeffR27V20 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R27" &
                                 df.latforcoeff$drivingSpeed == "V20")

plot.latforcoeffR27V20 <- ggplot(data = df.latforcoeffR27V20,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR27V20


# 1.2 RAD, 27m, V30----
# drivingTraj & longSpeed----
df.drivingtrajR27V30 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R27" &
                                 df.drivingtraj$drivingSpeed == "V30")

plot.drivingtrajR27V30 <- ggplot(data = df.drivingtrajR27V30,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(-30, 30)) +
  theme(legend.position = c(0.2, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR27V30


df.longspeedR27V30 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R27" &
                               df.longspeed$drivingSpeed == "V30")

plot.longspeedR27V30 <- ggplot(data = df.longspeedR27V30,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR27V30

grid.arrange(plot.drivingtrajR27V30, plot.longspeedR27V30, ncol=2, nrow=1)


# trackoff----
df.trackoffR27V30 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R27" &
                              df.trackoff$drivingSpeed == "V30")

plot.trackoffR27V30 <- ggplot(data = df.trackoffR27V30,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR27V30

max(df.trackoffR27V30$TrackOff)


# latforcoeff----
df.latforcoeffR27V30 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R27" &
                                 df.latforcoeff$drivingSpeed == "V30")

plot.latforcoeffR27V30 <- ggplot(data = df.latforcoeffR27V30,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR27V30


# 1.3 RAD, 27m, V40----
# drivingTraj & longSpeed----
df.drivingtrajR27V40 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R27" &
                                 df.drivingtraj$drivingSpeed == "V40")

plot.drivingtrajR27V40 <- ggplot(data = df.drivingtrajR27V40,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(-30, 30)) +
  theme(legend.position = c(0.2, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR27V40


df.longspeedR27V40 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R27" &
                               df.longspeed$drivingSpeed == "V40")

plot.longspeedR27V40 <- ggplot(data = df.longspeedR27V40,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR27V40

grid.arrange(plot.drivingtrajR27V40, plot.longspeedR27V40, ncol=2, nrow=1)


# trackoff----
df.trackoffR27V40 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R27" &
                              df.trackoff$drivingSpeed == "V40")

plot.trackoffR27V40 <- ggplot(data = df.trackoffR27V40,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR27V40

min(df.trackoffR27V40$TrackOff)


# latforcoeff----
df.latforcoeffR27V40 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R27" &
                                 df.latforcoeff$drivingSpeed == "V40")

plot.latforcoeffR27V40 <- ggplot(data = df.latforcoeffR27V40,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR27V40


# 1.4 RAD, 27m, 小结----
# drivingTraj & longSpeed----
df.drivingtrajR27 <- subset(x = df.drivingtraj,
                            df.drivingtraj$curveRad == "R27" &
                              df.drivingtraj$TrajTyp == "DrivingTraj")

plot.drivingtrajR27 <- ggplot(data = df.drivingtrajR27,
                              aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(0, 30)) +
  scale_y_continuous(limits = c(-30, 30)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR27


# trackoff----
df.trackoffR27 <- subset(x = df.trackoff,
                         df.trackoff$curveRad == "R27")

plot.trackoffR27 <- ggplot(data = df.trackoffR27,
                           aes(x = Station, y = TrackOff)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR27


# latforcoeff----
df.latforcoeffR27 <- subset(x = df.latforcoeff,
                            df.latforcoeff$curveRad == "R27" &
                              df.latforcoeff$LatForTyp == "LatForAll")

plot.latforcoeffR27 <- ggplot(data = df.latforcoeffR27,
                              aes(x = Station, y = LatFor)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1,
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR27


# 2 RAD, 13m
# 2.1 RAD, 13m, V15----
# drivingTraj & longSpeed----
df.drivingtrajR13V15 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R13" &
                                 df.drivingtraj$drivingSpeed == "V15")

plot.drivingtrajR13V15 <- ggplot(data = df.drivingtrajR13V15,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(legend.position = c(0.2, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR13V15


df.longspeedR13V15 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R13" &
                               df.longspeed$drivingSpeed == "V15")

plot.longspeedR13V15 <- ggplot(data = df.longspeedR13V15,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR13V15

grid.arrange(plot.drivingtrajR13V15, plot.longspeedR13V15, ncol=2, nrow=1)


# trackoff----
df.trackoffR13V15 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R13" &
                              df.trackoff$drivingSpeed == "V15")

plot.trackoffR13V15 <- ggplot(data = df.trackoffR13V15,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR13V15

max(df.trackoffR13V15$TrackOff)


# latforcoeff----
df.latforcoeffR13V15 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R13" &
                                 df.latforcoeff$drivingSpeed == "V15")

plot.latforcoeffR13V15 <- ggplot(data = df.latforcoeffR13V15,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR13V15


# 2.2 RAD, 13m, V20----
# drivingTraj & longSpeed----
df.drivingtrajR13V20 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R13" &
                                 df.drivingtraj$drivingSpeed == "V20")

plot.drivingtrajR13V20 <- ggplot(data = df.drivingtrajR13V20,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(legend.position = c(0.2, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR13V20


df.longspeedR13V20 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R13" &
                               df.longspeed$drivingSpeed == "V20")

plot.longspeedR13V20 <- ggplot(data = df.longspeedR13V20,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 30, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR13V20

grid.arrange(plot.drivingtrajR13V20, plot.longspeedR13V20, ncol=2, nrow=1)


# trackoff----
df.trackoffR13V20 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R13" &
                              df.trackoff$drivingSpeed == "V20")

plot.trackoffR13V20 <- ggplot(data = df.trackoffR13V20,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR13V20

max(df.trackoffR13V20$TrackOff)


# latforcoeff----
df.latforcoeffR13V20 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R13" &
                                 df.latforcoeff$drivingSpeed == "V20")

plot.latforcoeffR13V20 <- ggplot(data = df.latforcoeffR13V20,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR13V20


# 2.3 RAD, 13m, V30----
# drivingTraj & longSpeed----
df.drivingtrajR13V30 <- subset(x = df.drivingtraj,
                               df.drivingtraj$curveRad == "R13" &
                                 df.drivingtraj$drivingSpeed == "V30")

plot.drivingtrajR13V30 <- ggplot(data = df.drivingtrajR13V30,
                                 aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(TrajTyp)), size = 1) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(legend.position = c(0.2, 0.3),
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR13V30


df.longspeedR13V30 <- subset(x =  df.longspeed,
                             df.longspeed$curveRad == "R13" &
                               df.longspeed$drivingSpeed == "V30")

plot.longspeedR13V30 <- ggplot(data = df.longspeedR13V30,
                               aes(x = Station, y = Speed)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.longspeedR13V30

grid.arrange(plot.drivingtrajR13V30, plot.longspeedR13V30, ncol=2, nrow=1)


# trackoff----
df.trackoffR13V30 <- subset(x = df.trackoff,
                            df.trackoff$curveRad == "R13" &
                              df.trackoff$drivingSpeed == "V30")

plot.trackoffR13V30 <- ggplot(data = df.trackoffR13V30,
                              aes(x = Station, y = TrackOff)) +
  geom_line(size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  theme(axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR13V30

min(df.trackoffR13V30$TrackOff)


# latforcoeff----
df.latforcoeffR13V30 <- subset(x = df.latforcoeff,
                               df.latforcoeff$curveRad == "R13" &
                                 df.latforcoeff$drivingSpeed == "V30")

plot.latforcoeffR13V30 <- ggplot(data = df.latforcoeffR13V30,
                                 aes(x = Station, y = LatFor)) +
  geom_line(aes(linetype = factor(LatForTyp)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR13V30


# 2.4 RAD, 13m, 小结----
# drivingTraj & longSpeed----
df.drivingtrajR13 <- subset(x = df.drivingtraj,
                            df.drivingtraj$curveRad == "R13" &
                              df.drivingtraj$TrajTyp == "DrivingTraj")

plot.drivingtrajR13 <- ggplot(data = df.drivingtrajR13,
                              aes(x = posX, y = posY)) +
  geom_path(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_y_continuous(limits = c(-15, 15)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.drivingtrajR13


# trackoff----
df.trackoffR13 <- subset(x = df.trackoff,
                         df.trackoff$curveRad == "R13")

plot.trackoffR13 <- ggplot(data = df.trackoffR13,
                           aes(x = Station, y = TrackOff)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.trackoffR13


# latforcoeff----
df.latforcoeffR13 <- subset(x = df.latforcoeff,
                            df.latforcoeff$curveRad == "R13" &
                              df.latforcoeff$LatForTyp == "LatForAll")

plot.latforcoeffR13 <- ggplot(data = df.latforcoeffR13,
                              aes(x = Station, y = LatFor)) +
  geom_line(aes(colour = factor(drivingSpeed)), size = 1) +
  geom_hline(yintercept = c(-0.2, -0.15, 0.15, 0.2),
             colour = c("red", "orange", "orange", "red"),
             size = 1,
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 45), breaks = seq(0, 45, 10)) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 10))

plot.latforcoeffR13
