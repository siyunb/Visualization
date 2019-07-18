library(ggplot2)
library(ggalt)
library(gplots)
library(animation)
library(devtools)
library(dplyr)
library(showtext)  #使作图的字体更加丰富
library(RColorBrewer)  #增加调色板
library(maps)
library(mapdata)
library(maptools)
library(graphics)
library(REmap)
library(mice)
library(ggthemes)
library(plyr)
library(reshape2)
library(baidumap)
library(ggmap)
library(plotly)
showtext_auto(enable = T)
showtext_begin()
######CO2排放量######
setwd("D:/bigdatahw/可视化/团队作业-王思雨/数据/") 
CO2index<-read.csv("D:/bigdatahw/可视化/团队作业-王思雨/数据/co2信息检索.csv",header=T,sep=',')
CO2data<-read.csv("D:/bigdatahw/可视化/团队作业-王思雨/数据/co2排放量.csv",header=T,sep=',',skip=4)
colnames(CO2index)[1] <- 'country' 
any(is.na(CO2index)) 
md.pattern(CO2index)#生成缺失报告
CO2index<-na.omit(CO2index)
summary(CO2index)
table(CO2index$Region)
CO2index$Region<-as.factor(CO2index$Region)
a<-CO2index$Region[6]
b<-CO2index$IncomeGroup[6]
CO2index$Region[CO2index$Region==a]=NA
CO2index$IncomeGroup[CO2index$IncomeGroup==b]=NA
CO2data<-CO2data[,c(-3,-4)]
CO2index<-CO2index[,c(-4,-5)]
colnames(CO2data)[2] <- 'country'
CO2<-merge(x = CO2index, y = CO2data, by = "country", all = TRUE)
CO2<-CO2[,c(-5:-15,-60:-63)]
#######进行数据筛选#######
for(i in 1:length(CO2$Region)){
  if (is.na(CO2$Region[i])){
    CO2$Region[i]=CO2$Region[i-1]
  }
}
for(i in 1:length(CO2$IncomeGroup)){
  if (is.na(CO2$IncomeGroup[i])){
    CO2$IncomeGroup[i]=CO2$IncomeGroup[i-1]
  }
}
CO2<-na.omit(CO2)
#进行tidy data的清洗与整理
colnames(CO2)  #查看变量名
#由于X为年度的意思，转换为tidy数据时没有必要带入X，因此首先对变量名进行操作删除X
names(CO2) <- gsub("X", "", names(CO2))  
CO2_tidy <- 
  CO2 %>%                                  #利用管道操作符
  melt(id = c("country", "Region","IncomeGroup","Country.Name"),     #控制两列变量在新表中不动
       variable.name = "year", 
       value.name = "cases",
       na.rm = TRUE)
#查看整理过的tidy数据的前15行
head(CO2_tidy,15)
CO2_line<-
        CO2_tidy[,c(3,5,6)] %>%
        dplyr::group_by(IncomeGroup,year) %>%
        dplyr::summarise(cases = mean(cases))
ggplot(CO2_line, aes(x=year, y=cases, colour=IncomeGroup,group=IncomeGroup,shape=IncomeGroup)) + 
  geom_line(size=1)+
  geom_point(size=2)+
  xlab("年份") + ylab("平均CO2强度")+
  ggtitle('四种不同收入国家的CO2强度图')+
  theme(plot.title = element_text(hjust =0.5,family="myFont",size=20,color="red"), panel.background=element_rect(fill='aliceblue',color='black'))



#######二氧化碳CO2排放######
CO2_em_index<-read.csv("D:/bigdatahw/可视化/团队作业-王思雨/数据/CO2排放索引.csv",header=T,sep=',')
CO2_em_data<-read.csv("D:/bigdatahw/可视化/团队作业-王思雨/数据/CO2排量.csv",header=T,sep=',',skip=4)
colnames(CO2_em_index)[1] <- 'country'
any(is.na(CO2_em_index)) 
md.pattern(CO2_em_index)#生成缺失报告
summary(CO2_em_index)
a<-CO2_em_index$Region[6]
b<-CO2_em_index$IncomeGroup[6]
CO2_em_index$Region[CO2_em_index$Region==a]=NA
CO2_em_index$IncomeGroup[CO2_em_index$IncomeGroup==b]=NA
CO2_em_data<-CO2_em_data[,c(-3,-4)]
CO2_em_index<-CO2_em_index[,c(-4,-5)]
colnames(CO2_em_data)[2] <- 'country'
CO2_em<-merge(x = CO2_em_index, y = CO2_em_data, by = "country", all = TRUE)
CO2_em<-CO2_em[,c(-60:-63)]
CO2_em<-na.omit(CO2_em)
#由于X为年度的意思，转换为tidy数据时没有必要带入X，因此首先对变量名进行操作删除X
names(CO2_em) <- gsub("X", "", names(CO2_em))  
CO2_em_tidy <- 
  CO2_em %>%                                  #利用管道操作符
  melt(id = c("country", "Region","IncomeGroup","Country.Name"),     #控制两列变量在新表中不动
       variable.name = "year", 
       value.name = "cases",
       na.rm = TRUE)
CO2_em_area<-
  CO2_em_tidy[,c(3,5,6)] %>%
  group_by(IncomeGroup,year) %>%
  summarise(cases = sum(cases)/1000000) #计算CO2排放总和，并重新设置量纲
CO2_em_area$IncomeGroup = factor(CO2_em_area$IncomeGroup, levels=c('Low income','Lower middle income','High income','Upper middle income'))   ## 设置柱条的顺序
ggplot(CO2_em_area,aes(x=year,y=cases,group=IncomeGroup,fill=IncomeGroup))+
  geom_area(alpha=0.65)+
  xlab("年份") + ylab("平均CO2强度")+
  ggtitle('四种不同收入类型的CO2排放图')+
  theme(plot.title = element_text(hjust =0.5,family="myFont",size=20,color="red"), panel.background=element_rect(fill='aliceblue',color='black'))


  

####画地图
world <- map_data("world")
world <- world[world$region != "Antarctica",] # 剔除南极洲
names(CO2_em_data) <- gsub("X", "", names(CO2_em_data)) 
CO2_map<-CO2_em_data[,c(1,57)]
colnames(CO2_map) <-c('country','emission') 
colnames(world)[5] <- 'country'
world<-world[,-6]
CO2_map<-join(world,CO2_map,type="full",by="country")
CO2_map$emission[CO2_map$country=='Russia']=1705345.684         #填补名字无法识别的
CO2_map$emission[CO2_map$country=='UK']=419820.162
CO2_map$emission[CO2_map$country=='USA']=5254279.285  
CO2_map$emission[CO2_map$country=='Yemen']=22698.730
CO2_map$emission[CO2_map$country=='Democratic Republic of the Congo']=4671.758
CO2_map$emission[CO2_map$country=='Egypt']=201894.019
CO2_map$emission[CO2_map$country=='South Korea']=587156.373
CO2_map$emission[CO2_map$country=='North Korea']=40527.684
CO2_map$emission[CO2_map$country=='Taiwan']=546273.873
CO2_map$emission[CO2_map$country=='Vietnam']=46273.873
CO2_map$emission[CO2_map$country=='Venezuela']=185220.170
CO2_map$emission[CO2_map$country=='Virgin Islands']=179.683
CO2_map$emission[CO2_map$country=='Republic of Congo']=3094.948
CO2_map$emission[CO2_map$country=='Ivory Coast']=11045.004
CO2_map<-na.omit(CO2_map)
##??CO2_map$emission<-log(CO2_map$emission)
CO2_map$emission<-as.numeric(as.character(CO2_map$emission))
serious<-get_geo_position(c('中国','美国'))
serious[2,2]=40.49937
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
                         plot.title = element_text(size=20,hjust = 0.5,family="myFont",color="navyblue")))
ggplot ()+
  geom_polygon(data=CO2_map,aes(x=long,y=lat,group=group,fill=emission),colour="gray40")+
  scale_fill_gradient(name="世界CO2排放总量",low="white",high="red")+
  geom_text(data =serious,aes(x=lon,y=lat,label=city),colour="black",size=5,vjust=0,nudge_y=0.5)+
  labs(title ="2014年度世界CO2排放总量")+
  theme_opts



####甲烷排放动图作为排放的主要污染物
Meth_em_index<-read.csv("D:/bigdatahw/可视化/团队作业-王思雨/数据/甲烷检索.csv",header=T,sep=',')
Meth_em_data<-read.csv("D:/bigdatahw/可视化/团队作业-王思雨/数据/甲烷排放.csv",header=T,sep=',',skip=4)
colnames(Meth_em_index)[1] <- 'country'
any(is.na(Meth_em_index)) 
md.pattern(Meth_em_index)         #生成缺失报告
summary(Meth_em_index)
a<-Meth_em_index$Region[6]        #清楚空值
b<-Meth_em_index$IncomeGroup[6]
Meth_em_index$Region[Meth_em_index$Region==a]=NA
Meth_em_index$IncomeGroup[Meth_em_index$IncomeGroup==b]=NA
Meth_em_data<-Meth_em_data[,c(-3,-4)]
Meth_em_index<-Meth_em_index[,c(-4,-5)]
colnames(Meth_em_data)[2] <- 'country'
Meth_em<-merge(x = Meth_em_index, y = Meth_em_data, by = "country", all = TRUE)
Meth_em<-Meth_em[,c(-5:-14,-58:-63)]
Meth_em<-na.omit(Meth_em)
#由于X为年度的意思，转换为tidy数据时没有必要带入X，因此首先对变量名进行操作删除X
names(Meth_em) <- gsub("X", "", names(Meth_em))  
Meth_em_tidy <- 
  Meth_em %>%                                  #利用管道操作符
  melt(id = c("country", "Region","IncomeGroup","Country.Name"),     #控制两列变量在新表中不动
       variable.name = "year", 
       value.name = "cases",
       na.rm = TRUE)
Meth_em_tidy <- tbl_df(Meth_em_tidy)
Meth_em_gif<-
  Meth_em_tidy[,c(2,5,6)] %>%
  dplyr::group_by(Region,year) %>%
  dplyr::summarise(cases = sum(cases)) #计算甲烷排放总和，并重新设置量纲
saveGIF({
  ani.options(interval=.1,convert = shQuote('D:/bigdatahw/可视化/ImageMagick-7.0.7-Q16/hp2xx.exe'))
  for(i in 1970:2012){
   year_data<-dplyr::filter(Meth_em_gif,year==i)
   year_data$Region = factor(year_data$Region, levels=c('East Asia & Pacific','Europe & Central Asia','North America','South Asia','Latin America & Caribbean','Sub-Saharan Africa','Middle East & North Africa'))
   p<-ggplot(year_data,aes(x=Region,y=cases,fill=Region, group = factor(1)))+geom_bar(stat = "identity", width = 0.5)  
   title<-as.character(i)
   p + theme_economist(base_size = 14) +theme(axis.ticks.length=unit(0.4,'cm'))+
     guides(fill=guide_legend(title=NULL))+
     theme(axis.title = element_blank())+
     xlab("大洲名称") + ylab("甲烷排放总量")+
     ggtitle(paste0('各大洲甲烷排放量,',title))+
     theme(plot.title = element_text(hjust = 0.5,family="myFont",size=26,color="blue")) 
  print(p)
  }
},movie.name = '甲烷排放时序变化柱状图.gif',outdir="D:/bigdatahw/可视化/",ani.width=700,ani.height=600)


###甲烷排放量小提琴图
theme_opts <- list(theme(panel.grid.minor = element_blank(),#设置网格线为空
                         panel.grid.major = element_blank(),#你可以去掉
                         panel.background = element_rect(fill="aliceblue",color='black'),#设置图版背景色
                         plot.background = element_rect(fill="ivory1"),#设置绘图区背景色
                         panel.border = element_blank(),
                         legend.background = element_rect(fill=rgb(red = 242, green = 242, blue = 242, max = 255)),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),#以上全是设置xy轴
                         plot.title = element_text(size=20,hjust = 0.5,family="myFont",color="navyblue")))
Meth2012<-dplyr::filter(Meth_em_tidy,year==2012)
ggplot(Meth2012,aes(x=Region,y=log(cases),fill=Region))+geom_violin(trim= FALSE,alpha=.35)+
   geom_boxplot(width=.1,fill="black",outlier.colour=NA)+geom_jitter()+theme_bw()+ 
   labs(x="大洲名称",y="log(排放量)",fill="大洲名称图例",title='各大洲甲烷排放量小提琴图')+
   theme_opts

###plotly交互图
NO2012<-dplyr::filter(Meth_em_tidy,year==2012)
p<-ggplot(data=NO2012,aes(x=log(cases),fill=Region))+
  geom_density(alpha=.75,colour="white")+
  facet_grid(Region~.) +theme_opts+labs(x="大洲名称",y="log(NO排放量)",fill="大洲名称图例",title='各大洲NO排放量交互式分布图')
ggplotly(p)

###交互式地图
world <- map_data("world")
world <- world[world$region != "Antarctica",] # 剔除南极洲
forestdata<-read.csv("D:/bigdatahw/可视化/团队作业-王思雨/数据/海洋森林.csv",header=T,sep=',')
forestdata<-forestdata[c(-1:-3,-218:-230),c(1,3,6)]
names(forestdata) <- c('country','forest','threatened') 
colnames(world)[5] <- 'country'
world<-world[,-6]
forest_map<-join(world,forestdata,type="full",by="country")
#a<-forestdata[45,2]
#forest_map$forest[forest_map$forest==a]=NA
forest_map$forest[forest_map$country=='Russia']=8149         #填补名字无法识别的
forest_map$forest[forest_map$country=='UK']=31
forest_map$forest[forest_map$country=='USA']=3101  
forest_map$forest[forest_map$country=='Yemen']=5
forest_map$forest[forest_map$country=='Democratic Republic of the Congo']=1526
forest_map$forest[forest_map$country=='Egypt']=1
forest_map$forest[forest_map$country=='South Korea']=50
forest_map$forest[forest_map$country=='North Korea']=62
forest_map$forest[forest_map$country=='Taiwan']=50
forest_map$forest[forest_map$country=='Vietnam']=50
forest_map$forest[forest_map$country=='Venezuela']=467
forest_map$forest[forest_map$country=='Virgin Islands']=179.683
forest_map$forest[forest_map$country=='Republic of Congo']=233
forest_map$forest[forest_map$country=='Ivory Coast']=107
###
forest_map$threatened[forest_map$country=='Russia']=35         #填补名字无法识别的
forest_map$threatened[forest_map$country=='UK']=5
forest_map$threatened[forest_map$country=='USA']=35 
forest_map$threatened[forest_map$country=='Yemen']=9
forest_map$threatened[forest_map$country=='Democratic Republic of the Congo']=15
forest_map$threatened[forest_map$country=='Egypt']=19
forest_map$threatened[forest_map$country=='South Korea']=11
forest_map$threatened[forest_map$country=='North Korea']=10
forest_map$threatened[forest_map$country=='Taiwan']=10
forest_map$threatened[forest_map$country=='Vietnam']=5
forest_map$threatened[forest_map$country=='Venezuela']=36
forest_map$threatened[forest_map$country=='Virgin Islands']=11
forest_map$threatened[forest_map$country=='Republic of Congo']=23
forest_map$threatened[forest_map$country=='Ivory Coast']=39
forest_map<-na.exclude(forest_map)
forest_map$forest<-as.numeric(as.character(forest_map$forest))
forest_map$threatened<-as.numeric(as.character(forest_map$threatened))

p<-ggplot ()+
  geom_polygon(data=forest_map,aes(x=long,y=lat,group=group,fill=threatened),colour="gray40")+
  scale_fill_gradient(name="森林面积",low="white",high="green")+
  labs(title ="2015年度各国森林面积")+
  theme_opts
ggplotly(p)

###温室气体排放热力图
gas_em<-read.csv("D:/bigdatahw/可视化/团队作业-王思雨/数据/温室气体排放趋势.csv",header=T,sep=',')
names(gas_em) <- gsub("X", "", names(gas_em)) 
gas_em<-gas_em[-216:-227,c(2,4,8,12)]
names(gas_em)<-c('CO2','Methane','Nitrous','other')
a<-gas_em$CO2[5]
gas_em$CO2[gas_em$CO2==a]=NA
gas_em<-na.exclude(gas_em)#删除有缺失值的行
gas_em<-data.frame(gas_em)
gas_em$CO2<-as.numeric(gas_em$CO2)
gas_em$Methane<-as.numeric(gas_em$Methane)
gas_em$Nitrous<-as.numeric(gas_em$Nitrous)
gas_em$other<-as.numeric(gas_em$other)
#标准化处理
gas_em<-scale(gas_em)
obsnum<-rownames(gas_em)
n<-sample(obsnum,50)
gas_em=gas_em[n,]
dis1<-dist(gas_em,method="euclidean") 
dis1<-as.matrix(dis1)
heatmap(dis1,main="样本间距离热力图")
heatmap.2(dis1,main="国家间的污染状况聚类",scale="column")   #美化热力图

