Play_data<-read.csv("mmm.csv")
library(tidyverse)
colnames(Play_data)<-c("Year","EPS","Dividend","Payout","Sales")

library(reshape2)
d <- melt(Play_data, id.vars="Year")

d %>% filter(variable=="Sales") %>%ggplot(aes(x =as.factor(Year) , y = value,,label=value))+
  geom_bar(stat = "identity",position="dodge",color="dodgerblue1",fill="dodgerblue1")+theme_classic()+
  labs( x = "Year", y = "Sales (billions)")+geom_text(vjust=1,color="white")


d %>% filter(variable=="EPS" | variable=="Dividend") %>%ggplot(aes(x =as.factor(Year) , y = value , fill = as.factor(variable)))+
  geom_bar(stat = "identity",position="dodge")+theme_classic()+labs( x = "Year", y = "Dollars")+guides(fill=guide_legend(title=""))+
  theme(legend.position = c(0.2, 0.9))

percent <- function(x, digits = 0, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

d %>% filter(variable=="Payout") %>% mutate(label = percent(value)) %>%ggplot(aes(x =as.factor(Year) , y = value,group = 1,label=label))+
  geom_point(color="darkturquoise",size=1)+geom_line(color="darkturquoise")+theme_classic()+labs( x = "Year", y = "Payout Ratio")+
  scale_y_continuous(labels = scales::percent)+geom_text(vjust=0,color="darkturquoise")

percent <- function(x, digits = 1, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

library(ggrepel)

dd<-d %>% filter(variable=="Dividend") %>%  mutate(p_increased = (value - lag(value))/lag(value)) %>% mutate(label = percent((value - lag(value))/lag(value))) 

dd2<-d %>% filter(variable=="EPS") %>%  mutate(p_increased = (value - lag(value))/lag(value)) %>% mutate(label = percent((value - lag(value))/lag(value))) 

rbind(dd,dd2)%>%ggplot(aes(x =as.factor(Year) , y = p_increased,group=variable,label=label))+
  geom_line(aes(color=variable))+
  geom_point(aes(color=variable))+theme_classic()+labs( x = "Year", y = "YOY Growth")+
  scale_y_continuous(labels = scales::percent)+geom_text_repel(aes(color=variable),size = 3, force = 3)+
  guides(fill=guide_legend(title=""))+
  theme(legend.position = c(0.2, 0.9))



Project_div<-as.data.frame(seq(2019,2029,by=1))
colnames(Project_div)<-"Year"

geomSeq <- function(start,ratio,begin,end){
  begin=begin-1
  end=end-1
  start*ratio**(begin:end)
}

Dividends_p<-as.data.frame(geomSeq(5.76,1.02,1,11))
colnames(Dividends_p)<-"Dividends"
Project_new<-cbind(Project_div,Dividends_p)

EPS_p<-as.data.frame(geomSeq(7.81,1.02,1,11))
colnames(EPS_p)<-"EPS"
Project_new<-cbind(Project_new,EPS_p)

Project_new_new<- melt(Project_new, id.vars="Year")
Project_new_new %>% ggplot(aes(x =as.factor(Year) , y = value , fill = as.factor(variable)))+
  geom_bar(stat = "identity",position="dodge")+theme_classic()+labs( x = "Year", y = "Dollars")+guides(fill=guide_legend(title=""))+
  theme(legend.position = c(0.2, 1))


Dividends_yield<-as.data.frame(geomSeq(0.0406,1.02,1,11))
colnames(Dividends_yield)<-"Dividends_yield"
