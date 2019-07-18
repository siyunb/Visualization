###读入数据
setwd("D:/bigdatahw/大数据统计基础考试/2017《大数据统计基础》考试题/2017《大数据统计基础》考试题/") 
mydata<-read.csv("D:/bigdatabw/大数据统计基础考试/2017《大数据统计基础》考试题/2017《大数据统计基础》考试题/LoanStats3c.csv",header=T,skip=1)
###删除缺失值
mydata0<-na.omit(mydata)
###加载包
library(ggplot2)
library(gplots)
library(plyr)
library(dplyr)
library(showtext)  #使作图的字体更加丰富
library(RColorBrewer)  #增加调色板
library(maps)
library(mapdata)
library(maptools)
library(graphics)
library(REmap)
showtext_auto(enable = T)
showtext_begin()

###question1:分面的风玫瑰图
label<-c("< 1 year","1 year","2 years","3 years","4 years","5 years","6 years","7 years","8 years","9 years","10+ years")
mydata0$emp_length<- ordered(mydata0$emp_length, levels = label)
ggplot(mydata0,aes(x=grade,fill=emp_length))+
  geom_bar()+coord_polar(theta = 'x')+
  scale_fill_brewer(palette='Spectral')+facet_wrap(~home_ownership)+theme_bw()+ 
  labs(x="信用等级",y="频数",fill="工作时间",title='分面风玫瑰图')+scale_x_discrete()+coord_polar(theta="x")+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="gold3"),panel.background=element_rect(fill='aliceblue')) 



###question2:直方图
binsize=diff(range(mydata0$funded_amnt))/17
mydata2<-cut(mydata0$funded_amnt,breaks=c(0,2000*(1:15),max(mydata0$funded_amnt)))
p<-ggplot(mydata0,aes(x=funded_amnt))+geom_histogram(
  aes(y=..density..),binwidth=binsize,fill='slateblue4',color='white')+
  #stat_function(fun=dnorm,..)#统计函数，dnorm正态分布曲线
  stat_function(fun=dnorm,args=list(mean(mydata0$funded_amnt),sd(mydata0$funded_amnt)),size=1,color='orange')+
  #geom_density时核密度估计曲线
  geom_line(stat='density',color='lightgreen',size=1)+expand_limits(y=0)+
  theme(plot.background=element_rect(fill="floralwhite",colour="black",size=2),panel.background=element_rect(fill=NA),plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="tomato1"))+
  labs(x='承诺贷款金额',y='频数',title='承诺贷款金额直方图')
   data3<-as.data.frame(ggplot_build(p)$data[1])
  p+geom_text(data = data3,aes(x=x, y= density,label = count),color='blue',vjust=-0.1)+
  annotate(geom="text",x=1e4,y=8e-5,label="密度最大点",size=5,colour="black")+
  annotate(geom="text",x=2e3,y=2e-5,label='568',size=3,colour="red")+
  annotate(geom="text",x=4e3,y=5.4e-5,label='1499',size=3,colour="red")+
  annotate(geom="text",x=6e3,y=6e-5,label='1651',size=3,colour="red")+
  annotate(geom="text",x=8e3,y=5.9e-5,label='1616',size=3,colour="red")+
  annotate(geom="text",x=1e4,y=7.5e-5,label='2033',size=3,colour="red")+
  annotate(geom="text",x=1.2e4,y=5e-5,label='1390',size=3,colour="red")+
  annotate(geom="text",x=1.4e4,y=4.7e-5,label='1311',size=3,colour="red")+
  annotate(geom="text",x=1.6e4,y=2e-5,label='572',size=3,colour="red")+
  annotate(geom="text",x=1.8e4,y=1.6e-5,label='496',size=3,colour="red")+
  annotate(geom="text",x=2e4,y=2.5e-5,label='717',size=3,colour="red")+
  annotate(geom="text",x=2.2e4,y=5e-6,label='178',size=3,colour="red")+
  annotate(geom="text",x=2.4e4,y=1.3e-5,label='408',size=3,colour="red")+
  annotate(geom="text",x=2.6e4,y=1e-6,label='86',size=3,colour="red")+
  annotate(geom="text",x=2.8e4,y=4e-6,label='168',size=3,colour="red")+
  annotate(geom="text",x=3e4,y=4e-6,label='160',size=3,colour="red")+
  annotate(geom="text",x=3.2e4,y=1e-6,label='60',size=3,colour="red")+
  annotate(geom="text",x=3.4e4,y=1e-5,label='314',size=3,colour="red")

###question3:密度图
#生成几何对象
p=ggplot(mydata0,aes(x=loan_amnt,y=log(annual_inc)))
#默认等高线图进行分箱化处理
p+geom_point(alpha=0.2)+stat_bin2d()+scale_fill_gradient(low="lightblue",high="red")+stat_density2d()+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="slateblue2"),panel.background=element_rect(fill='papayawhip'))+
  labs(x='贷款金额',y='年收入',title='等高线分箱密度曲线图')

#将height映射给线条颜色
p+stat_density2d(aes(colour=..level..))+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="slateblue2"),panel.background=element_rect(fill='papayawhip'))+
  annotate("rect",xmin=2500,xmax=12500,ymin=2.6e4,ymax=7e4,alpha=0.3,fill="red")+labs(x='贷款金额',y='年收入',title='贷款总额与年收入的相关密度图',color='水平')

###question4:热图
mydata3<-
  mydata0 %>%
  subset(select=c(loan_amnt,installment,annual_inc,dti,total_acc)) 
#标准化处理
mydata3<-scale(mydata3)
#删除有缺失值的行
mydata3<-na.exclude(mydata3)
obsnum<-rownames(mydata3)
n<-sample(obsnum,50)
mydata3=mydata3[n,]
dis1<-dist(mydata3,method="euclidean") 
dis1<-as.matrix(dis1)
heatmap(dis1,main="样本间距离热力图")
#美化热力图
heatmap.2(dis1,main="美化热力图",scale="column")

#画地图
data1<-read.csv("D:/bigdatahw/大数据统计基础考试/2017《大数据统计基础》考试题/2017《大数据统计基础》考试题/province.csv",header=T)
data1<-data1[-c(1,2),]
colnames(data1)[1] <-'省市' 
attach(data1)
map("china")
x=maptools::readShapePoly('D:/大数据作业/大数据统计基础考试/2017《大数据统计基础》考试题/2017《大数据统计基础》考试题/bou2_4p.shp')
plot(x)
provname<-c("北京市", "天津市", "河北省", "山西省", "内蒙古自治区",
            "辽宁省", "吉林省", "黑龙江省", "上海市", "江苏省",
            "浙江省", "安徽省", "福建省", "江西省", "山东省",
            "河南省", "湖北省", "湖南省", "广东省",
            "广西壮族自治区", "海南省", "重庆市", "四川省", "贵州省",
            "云南省", "西藏自治区", "陕西省", "甘肃省", "青海省",
            "宁夏回族自治区", "新疆维吾尔自治区")

pop<-data1$年末人口数
#整理数据
pop<-as.numeric(as.character(pop))
pop <-pop-min(pop)  
getColor=function(mapdata,provname,provcol,othercol)
{
  f=function(x,y) ifelse(x %in% y,which(y==x),0);
  colIndex=sapply(mapdata@data$NAME,f,provname); 
  col=c(othercol,provcol)[colIndex+1];
  return(col);
}

##构建图例位置
nf <- layout(matrix(c(1,1,1,1,1,2,1,1,1),3,3,byrow=TRUE), c(3,1), c(3,1), TRUE)  
layout.show(nf)  
provcol=rgb(red=1-pop/max(pop)/2,green=1-pop/max(pop)/2,blue=0)
plot(x,col=getColor(x,provname,provcol,"white"),xlab="",ylab="",main='2016年中国各省市人口分布状况')

# 添加图例  
par(mar=c(0,0,0,0))  
par(mar=c(1,1,2,0),cex=0.5)  
barplot(as.matrix(rep(1,31)),col=sort(provcol,dec=T),horiz=T,axes=F,border = NA )  
axis(1,seq(1,32,by=3),sort(pop[seq(1,32,by=3)])) 

###再画地图
data1$省市<-provname
province <- data.frame(get_geo_position (provname))
str(province)
names(province)[3] <- c("NAME")
colnames(data1)[1] <-'NAME' 
china_data_REmap <- join(data1,province,type="full",by="NAME")  

china_map <- readShapePoly('D:/bigdatahw/大数据统计基础考试/2017《大数据统计基础》考试题/2017《大数据统计基础》考试题/bou2_4p.shp')    
china_map1 <- china_map@data
china_map1<-data.frame(china_map1,id=seq(0:924)-1)      
china_map2 <- fortify(china_map)
china_map3 <- join(china_map2, china_map1, type="full",by="id")  
china_map4 <- join(china_map3, data1, type="full",by="NAME")
china_data_REmap$地区生产总值<-as.numeric(as.character(china_data_REmap$地区生产总值))
china_data_REmap$年末人口数<-as.numeric(as.character(china_data_REmap$年末人口数))
china_map4$年末人口数<-as.numeric(as.character(china_map4$年末人口数))


theme_opts <- list(theme(panel.grid.minor = element_blank(),#设置网格线为空
                         panel.grid.major = element_blank(),#你可以去掉
                         panel.background = element_rect(fill="slategray1"),#设置图版背景色
                         plot.background = element_rect(fill="papayawhip"),#设置绘图区背景色
                         panel.border = element_blank(),
                         legend.background = element_rect(fill=rgb(red = 242, green = 242, blue = 242, max = 255)),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),#以上全是设置xy轴
                         plot.title = element_text(size=10)))
ggplot ()+
  geom_polygon(data=china_map4,aes(x=long,y=lat,group=group,fill=年末人口数),colour="gray40")+
  scale_fill_gradient(name="各省市人口分布",low="white",high="red")+
  geom_errorbar(data=china_data_REmap,aes(x=lon, ymin=lat, ymax=lat + 地区生产总值/7000 ),
                colour="gold",size=5, width=0,alpha=0.9)+
  geom_text(data =china_data_REmap,aes(x=lon,y=lat,label=NAME),colour="black",size=5,
            vjust=0,nudge_y=0.5)+
  labs(title ="各地区生产总值")+
  ylim (18, 54)+theme_opts

world1<-get_geo_position(coutry)

