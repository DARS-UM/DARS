library(dplyr)

nd        <- read.csv("/Users/sofia/Documents/DARS/Jenny/Tables/Complete Summary-NEW DATA.csv")

nd$Origin <- paste(nd$Origin, "to", nd$Destination, sep = " ")

nd <- select(nd,-Destination) %>%
nd <- group_by(nd,Origin) %>%
  summarise(count = sum(count)) 


ggplot(nd, aes(x=Origin, y= count, fill=Year))+
  geom_bar(stat = "identity")+
  theme_bw()+
  labs(x="Direction of Student Traffic", y="Number of Students", title="Flow of Students within MSLAS",caption="New Data")
ggsave("Flow of Students within MSLAS-NEW DATA.png", device= "png", width = 25, height= 25, units="cm", path="/Users/sofia/Documents/DARS/Jenny/Simple Graphs")



#----------------------------------------
o_mslas <- read.csv("/Users/sofia/Documents/DARS/Jenny/Tables/MSLAS Summary-OLD DATA.csv")

o_mslas$Origin <- paste(o_mslas$Origin, "to", o_mslas$Destination, sep = " ")

o_mslas <- select(o_mslas,-Destination)
o_mslas<- group_by(o_mslas,Origin) %>%
  summarise(count = sum(count)) 

ggplot(o_mslas, aes(x=Origin, y= count,fill=Year))+
  geom_bar(stat = "identity")+
  theme_bw()+
  labs(x="Direction of Student Traffic", y="Number of Students", title="Flow of Students within MSLAS",caption="Old Data")
ggsave("Flow of Students within MSLAS-OLD DATA.png", device= "png", width = 25, height= 25, units="cm", path="/Users/sofia/Documents/DARS/Jenny/Simple Graphs")

#----------------------------
od      <- read.csv("/Users/sofia/Documents/DARS/Jenny/Tables/Complete Summary-OLD DATA.csv")
od$Origin <- paste(od$Origin, "to", od$Destination, sep = " ")

od <- select(od,-Destination)
od<- group_by(od,Origin) %>%
  summarise(count = sum(count)) 

ggplot(od, aes(x=Origin, y= count, fill=Year))+
  geom_bar(stat = "identity")+
  theme_bw()+
  labs(x="Direction of Student Traffic", y="Number of Students", title="Flow of Students accross all Faculties",caption="Old Data")
ggsave("Flow of Students across all Faculties-OLD DATA.png", device= "png", width = 25, height= 25, units="cm", path="/Users/sofia/Documents/DARS/Jenny/Simple Graphs")
