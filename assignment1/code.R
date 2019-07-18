library(psych)
library(car)
library(corrplot)
library(REmap)
library(ggplot2)
library(foreign)
library(RColorBrewer)
library(plotrix)
setwd("D:/大数据实验/可视化/北大国家发展中心数据/demographic_background/") 
mydata=read.dta("demographic_background.dta")
##绘制饼图
data<-mydata$bb006
summary(data)
which(is.na(data))
data<-na.omit(data)
summary(data)
par(mfrow = c(1, 2))
slices <- c(15759,1912)
lbls <- c("城镇", "乡村")
pie(slices, labels = lbls,col=brewer.pal(5,"Set2"),border="white",font=2,labelcex=1,explode=0.1,radius=0.95,main = "16岁之前受访者居住地")
##百分比饼图
pct <- round(slices/sum(slices) * 100)
lbls2 <- paste(lbls, " ", pct, "%", sep = "")
pie(slices, labels = lbls2, col=brewer.pal(5,"Set2"),border="white",font=2,labelcex=1,explode=0.1,radius=0.95, main = "16岁之前受访者居住地")


data1<-mydata$bd001
summary(data1)
which(is.na(data1))
data1<-na.omit(data1)
summary(data1)



mydata<-mydata[-which(is.na(mydata$bd001)),]
data1<-mydata$bd001
data1<-table(data1)
label<-c("文盲","能够读、写","私塾","小学毕业","初中毕业","高中毕业","中专","大专","本科","硕士","博士")
# 简单条形图
barplot(data1, main = "受访者学历柱状图", xlab = "学历", ylab = "人数",width=0.7,col="lightblue",names=label)

##直方图
data2<-mydata$bd006
summary(data2)
which(is.na(data2))
data2<-na.omit(data2)
summary(data2)
hist(data2,xlab="年龄",ylab="频数",main="读完书年龄",col="lightblue")

##ggplot2直方图
mydata<-mydata[-which(is.na(mydata$bd006)),]
ggplot(data=mydata,aes(x=bd006))+geom_histogram( fill = 'orchid1', colour = 'black')+xlab("年龄") + ylab("频数")+
               ggtitle('读完书年龄直方图')+
               theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="red"), panel.background=element_rect(fill='skyblue',color='black'))
               
##ggplot2饼图
mydata<-mydata[-which(is.na(mydata$bb006)),]
ggplot(mydata,aes(x=factor(1),fill=bb006)) +geom_bar()+coord_polar(theta="y")+ggtitle('受访者16岁前居住状况')+
                theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="red"), panel.background=element_rect(fill='aliceblue',color='black'))
##ggplot2柱状图
p<-ggplot(mydata,aes(x=bd001))+geom_bar(stat="count",fill="#FF6666",width = 0.77)           #绘制人口学分布特征
p + theme_wsj() +theme(axis.ticks.length=unit(0.4,'cm'))+
  guides(fill=guide_legend(title=NULL))+
  theme(axis.title = element_blank())+
  scale_x_discrete(name='类别',labels=label)+
  ggtitle('受访者学历柱状图')+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=26,color="blue")) 

##ggplot2堆积直方图
mydata=read.dta("demographic_background.dta")
mydata<-mydata[-which(is.na(mydata$bb006)),]
p<-ggplot(mydata,aes(x=bd006,fill=bb006))
p+geom_histogram(position="identity",alpha=0.6)+ggtitle('读书年龄与城乡居民堆积直方图')+
  theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="red"),panel.background=element_rect(fill='aliceblue')) + xlab("读完书年龄") + 
  ylab("频数") 



##ggplot2堆积直方图
mydata=read.dta("demographic_background.dta")
mydata<-mydata[-which(is.na(mydata$bb006)),]
p<-ggplot(mydata,aes(x=bd006,fill=bb006))
p+geom_histogram(position="identity",alpha=0.6)+facet_grid(bd001~bb006,scales="free")

##ggplot2分类箱线图
mydata=read.dta("demographic_background.dta")
mydata<-mydata[-which(is.na(mydata$bb006)),]
ggplot(mydata, aes(x = bb006, y =bd006 , fill = I("#FF6666"))) + xlab("居住位置") + 
       ylab("毕业年龄") + geom_boxplot(outlier.shape = NA) +
        theme_economist() + scale_x_discrete(labels = c("城镇", "乡村"))+ggtitle('受访者居住位置与学习年龄')+ylim(5,25)+
           theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="black"))

##ggplot2风玫瑰图
mydata=read.dta("demographic_background.dta")
mydata<-mydata[-which(is.na(mydata$bb006)),]
mydata$bb006 <- factor(mydata$bb006, levels = c("2 Village","1 City / Town"))
ggplot(mydata,aes(x=bd001,fill=bb006))+geom_bar()+scale_x_discrete(labels=label)+coord_polar(theta="x")+ggtitle('受访者学历人数居住分布风玫瑰图')+
          theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="red")) 


##ggplot2堆积柱状图
ggplot(mydata,aes(x=bd001,fill=bb006))+geom_bar()+scale_x_discrete(labels=label)+ggtitle('受访者学历人数居住分布堆积柱状图')+
                    theme(plot.title = element_text(hjust = 0.5,family="myFont",size=18,color="red")) +theme_wsj()
             
##绘制分面统计图





