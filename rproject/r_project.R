install.packages(c("tidyverse", "data.table", "devtools", "scatterplot3d", "plotly","PerformanceAnalytics"))
install.packages("ggmap")
install.packages("ggplot2")
install.packages("raster")
install.packages("rgeos")
install.packages("maptools")
install.packages("rgdal")

library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)

P <- fread("mappingas.csv", header = TRUE) #시각화할 데이터셋
data <-  fread("서울인구.csv", sep = ",") %>% as_tibble()
data %>% str()
data %>% summary()

map <- shapefile("SIG_202005/SIG.shp") #지리 정보 데이터셋
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
View(new_map)
new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
P_merge <- merge(seoul_map, P, by='id')




P_2<-fread("mappingmedic.csv",header=TRUE)

ggplot() + geom_polygon(data = P_merge,aes(x = long, y = lat, group = group, fill = 노인주거복지시설정원),color = "white")  +
  scale_fill_gradient(low = "gray", high = "purple",space = "Lab",guide = "colourbar") +labs(fill = "노인주거복지시설 정원수") +
  theme_void() +theme(legend.position = c(.10, .65),plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +  geom_text(data=P_2,aes(x = long, y = lat, label = paste(자치구, 노인주거복지시설정원, sep = "\n")))+
  labs(title = "서울시 노인주거복지시설정원")

ggplot() + geom_polygon(data = P_merge,aes(x = long, y = lat, group = group, fill = 의료복지정원),color = "white")  +
  scale_fill_gradient(low = "gray", high = "purple",space = "Lab",guide = "colourbar") +labs(fill = "의료복지정원 정원수") +
  theme_void() +theme(legend.position = c(.10, .65),plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +  geom_text(data=P_2,aes(x = long, y = lat, label = paste(자치구, 의료복지정원, sep = "\n")))+
  labs(title = "서울시 의료복지시설 정원수")

ggplot() + geom_polygon(data = P_merge,aes(x = long, y = lat, group = group, fill = 노인여가복지시설정원),color = "white")  +
  scale_fill_gradient(low = "gray", high = "purple",space = "Lab",guide = "colourbar") +labs(fill = "노인여가복지시설 정원수") +
  theme_void() +theme(legend.position = c(.10, .65),plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +  geom_text(data=P_2,aes(x = long, y = lat, label = paste(자치구, 노인여가복지시설정원, sep = "\n")))+
  labs(title = "서울시 노인여가복지시설정원 정원수")

ggplot() + geom_polygon(data = P_merge,aes(x = long, y = lat, group = group, fill = 재가노인복지시설정원),color = "white")  +
  scale_fill_gradient(low = "gray", high = "purple",space = "Lab",guide = "colourbar") +labs(fill = "재가노인복지시설 정원수") +
  theme_void() +theme(legend.position = c(.10, .65),plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +  geom_text(data=P_2,aes(x = long, y = lat, label = paste(자치구, 재가노인복지시설정원, sep = "\n")))+
  labs(title = "서울시 재가노인복지시설정원 정원수")


ggplot() + geom_polygon(data = P_merge,aes(x = long, y = lat, group = group, fill = 거주노인),color = "white")  +
  scale_fill_gradient(low = "gray", high = "red",space = "Lab",guide = "colourbar") +labs(fill = "65세 이상 거주노인 수") +
  theme_void() +theme(legend.position = c(.10, .65),plot.title = element_text(face = "bold", size = 18, hjust = 0.5)) +
  geom_text(data=P_2,aes(x = long, y = lat, label = paste(자치구, comma(거주노인), sep = "\n")))+
  labs(title = "서울시 65세 이상 거주노인수")


dataframe <- P_2 %>% group_by(자치구) %>% summarise(노인주거복지시설정원, 의료복지정원,노인여가복지시설정원,재가노인복지시설정원,거주노인)
dataframe



data2 <- data %>% group_by(자치구) %>% summarise(전체인구=sum(전체인구))
data2


data2 %>% ggplot(aes(자치구, 전체인구)) +labs(x="구별 인구 현황", y="단위(명)")+ ggtitle("서울시 인구")+
  geom_bar(stat="identity",fill="purple",alpha=0.7)+geom_text(aes(label=comma(전체인구), vjust=1))+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 30, color = "black"),axis.text.x=element_text(angle=45, size=15),
        axis.title=element_text(size=15,face='bold'))+scale_y_continuous(limits = c(0, 1400000),breaks = seq(0,1400000, by=200000),labels = comma)

DF <- data
DF <- data %>% group_by(자치구) %>% summarise(인구비율=c(전체인구="전체인구",'65세이상 인구'="65세이상 인구"),
                                               value=c(전체인구=sum(전체인구),"65세이상인구"=sum(`65세이상 인구`)))
#인구분포그래프
DF
DF %>% ggplot(aes(자치구, value, fill=인구비율)) +labs(x="구별 인구 현황", y="단위(명)")+ ggtitle("서울시 인구")+
  geom_bar(stat="identity",alpha=0.7, position = 'dodge')+geom_text(data=DF, aes(label=comma(value), vjust=1, angle=5), position = position_dodge(width = 0.9),size=3)+
  theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 30, color = "black"),axis.text.x=element_text(angle=45, size=15),
        axis.title=element_text(size=15,face='bold'))+scale_y_continuous(limits = c(0, 1600000),breaks = seq(0,1600000, by=200000),labels = comma)








