setwd("D:/大数据作业/可视化/第二次作业/") 
mydata=read.table("mydata.csv",head=TRUE,stringsAsFactors=FALSE, sep=",")
data=read.table("dailyprice.txt",head=TRUE,sep="\t")
mydata1=data[1:1132,]  ##截取2003/01/02一天的1132只股票成交记录
write.csv(mydata1,"D:/大数据作业/可视化/第二次作业/mydata1.csv",row.names = TRUE) #输出csv文件，为2003/01/02日一天所有股票交易情况
mydata=read.table("mydata.csv",head=TRUE,stringsAsFactors=FALSE, sep=",")
mydata=mydata[,-1]
##去除空值
mydata1 <- na.omit(mydata1)
##构造对总市值构造分类变量，利用summary确定分位数，来合理的分为四个档次
shizhi<-vector(mode="numeric",length=0)
summary(mydata1)
for(i in 1:dim(mydata1)[1])
{ 
  if(mydata1$mkt_cap[i]<=1.685e+09){
   shizhi[i]=4
  }
  if(mydata1$mkt_cap[i]<=2.297e+09 &&mydata1$mkt_cap[i]>1.685e+09){
    shizhi[i]=3
  }
  if(mydata1$mkt_cap[i]<=3.732e+09 &&mydata1$mkt_cap[i]>2.297e+09){
    shizhi[i]=2
  }
  if(mydata1$mkt_cap[i]>3.732e+09 ){
    shizhi[i]=1
  }
}
##将shizhi变为factor变量
shizhi=as.character(shizhi)
mydata1=data.frame(mydata1,shizhi)  #将新列shizhi分类变量合并到mydata矩阵中
library(psych)
library(car)
library(corrplot)
library(REmap)
library(ggplot2)
library(foreign)
library(RColorBrewer)
library(plotrix)
qplot(chg,volume,data=mydata)
p<-ggplot(mydata,aes(x=chg,y=log(volume)))+geom_point() 
p<-ggplot(mydata,aes(x=free_turn,y=log(amt)))+geom_point()+xlim(0,3)#截取大多数点所在位置，忽略少量离群点
p+stat_smooth(method=loess)      #添加局部加权多项式回归线 

##散点图绘制边际地毯
p<-ggplot(mydata,aes(x=log(mkt_freeshares),y=log(amt)))+geom_point()+geom_rug() #绘制总股本和自由流通市值的散点图
p+annotate("text",x=log(mydata[7,18]),y=log(mydata[7,9]),label="市值较高") 


##计算相关系数矩阵
mydata1=mydata[,-c(1,2,20,21)]
mycor = cor(mydata1, use = "everything")
corrplot(mycor, order = "hclust", addrect = 3, rect.col = "black", tl.cex = 0.7, tl.col = "black", type = "lower") 
##调节参数并根据order=hclust采用聚类算法聚成3类

##绘制核密度估计图
#生成几何对象 
p<-ggplot(mydata,aes(x=log(mkt_freeshares),y=log(amt)))
#默认等高线图 
p+geom_point()+stat_density2d(aes(colour=..level..))
##绘制瓦片图
p+stat_density2d(aes(fill=..density..),geom = "tile",contour= FALSE) #有填充颜色 
p+geom_point()+stat_density2d(aes(alpha=..density..),geom="tile",contour= FALSE)


##绘制气泡图
mydata2<-mydata[1:100,]                       ##截取一个矩阵
ggplot(mydata2,aes(x=log(mkt_freeshares),y=log(amt),size=mkt_cap))+
  geom_point(shape=21,colour="black",fill="lightblue")
##散点图
ggplot(mydata2,aes(x=log(mkt_freeshares),y=log(amt),shape=shizhi))+geom_point(size=1.5) #不同点状的散点图
ggplot(mydata2,aes(x=log(mkt_freeshares),y=log(amt),color=shizhi))+geom_point(size=1.5) #不同颜色的散点图

##分面散点图
p<-ggplot(mydata,aes(x=log(mkt_freeshares),y=log(amt)))+geom_point()
p+facet_grid(trade_status~shizhi)

##核密度曲线
ggplot(mydata,aes(x=log(mkt_cap)))+geom_density() 
ggplot(mydata,aes(x=mkt_cap))+geom_density(stat="density")+expand_limits(y=0)

##不同带宽的核密度曲线
ggplot(mydata,aes(x=log(mkt_cap)))+geom_line(stat="density",adjust=.25,colour="red")+geom_line(stat="density")+geom_line(stat="density",adjust=2,colour="blue")

##分组密度曲线
ggplot(mydata,aes(x=close,color=shizhi))+geom_density()
ggplot(mydata,aes(x=close,fill=shizhi))+geom_density(alpha=.35,colour="white")

##分面密度曲线
ggplot(mydata,aes(x=close))+geom_density()+facet_grid(shizhi~.)

##频数多边形
ggplot(mydata,aes(x=close))+geom_freqpoly(binwidth=5)

##频数直方图
ggplot(mydata,aes(x=close))+geom_histogram() 

##箱线图
boxplot(mydata$close) 
boxplot(close~shizhi,data=mydata,names=c("市值很高","市值较高","市值中等","市值较低")) ##传统箱线图
ggplot(mydata,aes(x=shizhi,y=close))+geom_boxplot()  ##ggplot箱线图
ggplot(mydata,aes(x=shizhi,y=close))+geom_boxplot(outlier.size=1.5, outlier.shape=21)  #通过设置outlier.size 和outlier.shape修改异常值参数
ggplot(mydata,aes(x=shizhi,y=close))+geom_boxplot(outlier.size=1.5, outlier.shape=21,notch=TRUE)#在箱线图中加槽口
ggplot(mydata,aes(x=shizhi,y=close))+geom_boxplot(outlier.size=1.5, outlier.shape=21,notch=TRUE)+
  stat_summary (fun.y="mean",geom="point",shape=23,size=3,fill="white")         #在箱线图中加如均值

##绘制小提琴图
ggplot(mydata,aes(x=shizhi,y=close))+geom_violin()
ggplot(mydata,aes(x=shizhi,y=close))+
  geom_violin()+
  geom_boxplot(width=.1,fill="black",outlier.colour=NA)+
  stat_summary(fun.y=median,geom="point",shape=23,size=3, fill="white")   #在小提琴图中加入箱线图，统计值，并省略异常值点
ggplot(mydata,aes(x=shizhi,y=close))+geom_violin(trim= FALSE)     #截断小提琴尾部
ggplot(mydata,aes(x=shizhi,y=close))+geom_violin(scale= "count")#小提琴的面积的大小与观测数成正比
ggplot(mydata,aes(x=shizhi,y=close))+geom_violin(adjust =2)     #调整小提琴平滑程度
ggplot(mydata,aes(x=shizhi,y=close))+geom_violin(adjust =0.5)

##查看25个点的形状
df2 <- data.frame(x = 1:5 , y = 1:25, z = 1:25) 
s <- ggplot(df2, aes(x = x, y = y)) 
s + geom_point(aes(shape = z), size = 4) + scale_shape_identity()

##绘制概率密度函数
set.seed(1) #用于设定随机数种子，一个特定的种子可以产生一个特定的伪随机序列，这个函数的主要目的是让模拟能够可重复出现 
x<-seq(-5,5,length.out=100) #（-5,5）中的100个数 
y <- dnorm(x,0,1)           #dnorm为正态分布的密度函数，在x序列上生成均值为0，方差为1的正态分布密度
plot(x,y,col="red",xlim=c(-5,5),ylim=c(0,1),type='l',  xaxs="i", yaxs="i",ylab='density',xlab='', main="The Normal Density Distribution") 
lines(x,dnorm(x,0,0.5),col="green") 
lines(x,dnorm(x,0,2),col="blue") 
lines(x,dnorm(x,-2,1),col="orange")   #绘制不同均值方差的正态密度曲线
legend("topright",legend=paste("m=",c(0,0,0,-2)," sd=", c(1,0.5,2,1)), lwd=1, col=c("red", "green","blue","orange")) #设置图例

##绘制累积分布函数
set.seed(1)   #设置随机数种子
x <- seq(-5,5,length.out=100)  #生成-5到5的均匀数列
y <- pnorm(x,0,1)              #生成累积概率密度
plot(x,y,col="red",xlim=c(-5,5),ylim=c(0,1),type='l',xaxs="i", yaxs="i",ylab='density',xlab='', main="The Normal Cumulative Distribution") 
lines(x,pnorm(x,0,0.5),col="green") 
lines(x,pnorm(x,0,2),col="blue") 
lines(x,pnorm(x,-2,1),col="orange")   #绘制不同均值和方差的正太密度曲线
legend("bottomright",legend=paste("m=",c(0,0,0,-2)," sd=", c(1,0.5,2,1)), lwd=1,col=c("red", "green","blue","orange"))

##QQ图检验正态分布
qqnorm(mydata$close) 
qqline(mydata$close)

##高位数据的展示
###散点图矩阵
#pairs函数 
pairs(mydata1[,1:6],main = "ScatterplotMatrix")     #未设置各个变量的分布图与轴须图
#scatterplotmatrix函数 
library(car) 
scatterplotMatrix(mydata1[,1:6],main = "Scatter plot Matrix")   #设置各个变量的分布图与轴须图

##计算相关系数统计图
mydata1=mydata[,-c(1,2,20,21)]
mycor = cor(mydata1, use = "everything")
corrplot(mycor, order = "hclust", addrect = 3, rect.col = "black", tl.cex = 0.7, tl.col = "black", type = "lower") 
##调节参数并根据order=hclust采用聚类算法聚成3类
corrplot(mycor,method = "shade")

##热图的数据的提取
a=0
yizhi<-vector(mode="numeric",length=0)
while (i <= 5322012){
  i=a*1132+1
  yizhi<-rbind (yizhi,data[i,])
  a=a+1
}
library("lubridate")
month<-month(yizhi$datetime)   #提取出月份
month<-month[-4703]
jidu<-vector(mode="numeric",length=0)
for(i in 1:length(month))
{ 
  if( month[i]>=1 &&month[i]<=3 ){
    jidu[i]="1"
  }
  if( month[i]>=4 &&month[i]<=6 ){
    jidu[i]="2"
  }
  if( month[i]>=7 &&month[i]<=9 ){
    jidu[i]="3"
  }
  if( month[i]>=10 &&month[i]<=12 ){
    jidu[i]="4"
  }
}                                #构造季度向量

yizhi<-yizhi[-4703,]
yizhi=data.frame(yizhi,jidu)
year<-year(yizhi$datetime)
year<-year[-4703]
yizhi=data.frame(yizhi,year)
which(is.na(yizhi$close))
##绘制热图
b=1
label<-vector(mode="numeric",length=0)
for(i in 1:dim(yizhi)[1])
{ 
  if( yizhi$close[i]>80){
    label[b]<-i
      b=b+1
  }
}    
yizhi<-yizhi[-label,]
write.csv(yizhi,"D:/大数据作业/可视化/第二次作业/yizhi.csv",row.names = TRUE) #输出一只股票的成交记录的时间序列数据
#提出异常值
p<-ggplot(yizhi,aes(x=year,y=jidu,fill = close)) 
p+ geom_tile()+ scale_fill_gradient(low = "yellow",high = "red")   #绘制热力图

##绘制热力图
mydata1<-mydata1[-c(1)]
rili<-subset(mydata1,select = c(open,high,volume,chg,mkt_cap))  #提取五个变量作为绘制热力图数据
rili[,1:5]<-scale(rili[,1:5])    #标准化处理
rili<-na.exclude(rili)   #删除有缺失值的行 
distance_rili<-dist(rili,method="euclidean")  #计算欧式距离
distance_rili<-as.matrix(distance_rili)        #数据类型转化为矩阵
distance_rili<- distance_rili[1:100,]          #截取前100只股票作为热力图数据
heatmap(distance_rili,main="Heatmap")
##热图中区块的颜色深浅表示两个观测点的距离远近，邻近的点对应的方格的颜色更深，而远处的点对应的方格颜色浅。
 
##平行坐标图
library("lattice")   #载入lattice包
pingxing<-subset(mydata1,select = c(open,high,volume,chg,mkt_cap,shizhi))   #构造平行图数据矩阵
pingxing[,1:5]<-scale(pingxing[,1:5])    #标准化处理
parallelplot(~pingxing[,1:5],pingxing,group=shizhi,horizontal.axis = FALSE)   #以市值为分组绘制平行图

parallelplot(~pingxing[,1:5]|shizhi, pingxing,horizontal.axis = FALSE, scales = list(x = list(rot = 90)))#分面化处理，生成基于不同组别内的平行坐标图，反应某组内观测点的差异
         
##雷达图
stars(pingxing[c(1:10), -6],scale = TRUE,main="星图") #绘制前10个标准化后观测的星图、
leida<-subset(mydata1,select = c(trade_code,open,high,volume,chg,mkt_cap,shizhi))#构造雷达图数据
stars(leida[1:4,2:6],locations=c(0,0),col.lines = 2:7,radius=FALSE,scale = TRUE, key.loc=c(0,0),lwd=1.5)  #绘制前4个数据的标准化雷达图
legend(0.5,0.8,cex=0.5,legend=leida$trade_code[1:4],col=c(1:4),lty=1)  #添加图例
library("knitr")
library("ggradar")        #载入程序包                    
leida1<-leida[1:4,-7]   
for(i in 2:dim(leida1)[2])
{ 
  leida1[,i]<-(leida1[,i]-min(leida1[,i]))/(max(leida1[,i])-min(leida1[,i]))
}                 
rownames(leida1) <- LETTERS[1:4]     #整合整合雷达矩阵,归一化
ggradar(leida1)    #另一种雷达图利用ggplot插件

##交互可视化
library("plotly")
plot_ly(mydata1,x=close,color=shizhi,type="box")  #交互箱线图
piedata=data.frame(value=c(138,137,138,136),group=c("1","2","3","4")) 
plot_ly(piedata,values=~value,labels=~group,type="pie") #交互式饼图
#我们需要另外采用layout 函数来单独定义，并且用xaxis、yaxis、title等参数完成

#交互式密度图
p<-ggplot(mydata1,aes(x=close,fill=shizhi))+geom_density(alpha=.35)+facet_grid(shizhi~.)
library("devtools")
devtools::install_github('hadley/ggplot2')
library("ggplot2")
ggplotly(p) 


# 绘制3D表面图
data("volcano") 
p3=plot_ly(z=~volcano) 
p3=add_surface(p3)
p3
