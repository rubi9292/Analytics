setwd("/Users/rubisaini/Downloads")
data=read.csv("Suicides in India 2001-2012.csv")
str(data)

nrow(data)

names(data)

#1.	Which state has highest suicide rate?

data %>% group_by(State) %>% 
  summarize(count=sum(Total)) %>% 
  ggplot(aes(x=reorder(State,count),y=count))+
  geom_bar(stat='identity') + coord_flip() 

              
########## Impact of Gender on Suicide

sc_data %>% group_by(Year,Gender)%>%
  summarise(total=sum(Total))%>% 
  ggplot(aes(x=factor(Year),y=total,group=Gender,fill=Gender))+
  geom_area()+labs(x="Year",y="Count",fill="Gender")


###########################

#Which Age group makes more suicide attempts?  

data %>%  
  filter(!Age_group=="0-100+")%>%
  group_by(Age_group)%>% 
  summarise(total =sum(Total))%>% 
  ggplot(aes(x=Age_group,y=total,fill=Age_group))+geom_bar(stat="identity")

#Education level and suicide  

data %>% filter(Type_code =="Education_Status")%>% 
  group_by(Gender,Type)%>% 
  summarise(total=sum(Total))%>%
  ggplot(aes(x=Type,y=total,fill=Type))+
  geom_bar(stat="identity")+scale_fill_manual(values=colr)+coord_flip()

names(data)

#####Mmeans adopted
set.seed(1234)

total<- as.data.frame( data %>% filter(Type_code =="Means_adopted")%>%
  group_by(Type)%>% 
  summarise(tot=sum(Total)))

  wordcloud(words=total$Type, freq=total$tot, min.freq=25,max.freq=200)  

##### Major Cause
  
  
  data %>% filter(Type_code=="Causes" & !(Type=="Causes Not known") & !(Type=='Other Causes (Please Specity)'))%>% select(Year,Total,Type)%>% 
                       group_by(Year,Type)%>%summarise(total=sum(Total))%>%arrange(desc(total))%>%head(100)%>%
                       ggplot(aes(x=factor(Year),y=total,color=Type,group=Type))+
                       geom_line(size=1)+scale_color_manual(values=colr)+
    theme(legend.position = "bottom",axis.text.x = element_text(angle=65,vjust=0.5))+
      labs(x="Year",y="Count")+geom_point(size=2)
  