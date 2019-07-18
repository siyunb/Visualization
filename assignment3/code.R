library(maps)  
library(ggplot2) 
setwd("D:/bigdatahw/可视化/第三次作业")
world_map<-map_data("world")
ggplot(world_map,aes(x=long,y=lat,group=group))+geom_polygon(fill="white",colour="black")
ggplot(world_map,aes(x=long,y=lat,group=group))+geom_path() 
map("world",fill=TRUE,col=rainbow(200),ylim=c(-60,90),mar=c(0,0,0,0)) 
title("世界地图")
library(rgl)
require(rgl)
lat <- matrix(seq(90,-90, len=50)*pi/180, 50, 50, byrow=TRUE)
long <- matrix(seq(-180, 180, len=50)*pi/180, 50, 50)
r <- 6378.1  # 地球半径，以 千米 为单位
x <- r*cos(lat)*cos(long)
y <- r*cos(lat)*sin(long)
z <- r*sin(lat)
open3d()
persp3d(x, y, z, col="white",
        texture=system.file("textures/world.png ",package="rgl"),
        specular="black", axes=FALSE, box=FALSE,
        xlab="", ylab="", zlab="",
        normal_x=x, normal_y=y, normal_z=z)
####绘制中国地图
install.packages('mapdata')
library(maps)
library(mapdata)
map("china")
####画中国地图
library(maptools)
x=maptools::readShapePoly('D:/大数据作业/可视化/第三次作业/bou2_4p.shp')
plot(x)
plot(x,col=gray(924:0/924))
x[[2]]

getColor=function(mapdata,provname,provcol,othercol)
{
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  colIndex=sapply(mapdata@data$NAME,f,provname); 
  col=c(othercol,provcol)[colIndex+1];
  return(col);
}
provname=c("北京市","天津市","上海市","重庆市")
provcol=c("red","green","yellow","purple")
plot(x,col=getColor(x,provname,provcol,"white"))

data1<-read.csv("C:/Users/Administrator/Desktop/province.csv",header=TRUE,sep=",")
attach(data1)
##构建图例位置
data1<-read.csv("C:/Users/Administrator/Desktop/province.csv",header=TRUE,sep=",")
attach(data1)
##构建图例位置
nf <- layout(matrix(c(1,1,1,1,1,2,1,1,1),3,3,byrow=TRUE), c(3,1), c(3,1), TRUE)  
layout.show(nf)  
provcol=rgb(red=1-pop/max(pop)/2,green=1-pop/max(pop)/2,blue=0);
plot(x,col=getColor(x,data1$provname,provcol,"white"),xlab="",ylab="");

# 整理数据  
pop <- pop - min(pop)  
pop=pop-min(pop)  
# 添加图例  
par(mar=c(0,0,0,0))  
par(mar=c(1,1,2,0),cex=0.5)  
barplot(as.matrix(rep(1,31)),col=sort(provcol,dec=T),horiz=T,axes=F,border = NA )  
axis(1,seq(1,32,by=3),sort(pop[seq(1,32,by=3)])) 
library(REmap)
remapH(data,    
       maptype = 'china',
       theme = theme1,
       blurSize = 30,
       color = "blue",
       minAlpha = 10,
       opacity = 1,)


