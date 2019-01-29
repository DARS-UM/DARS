library(ggalluvial)
library(tidyverse)

path= "/Users/sofia/Documents/DARS/Jenny/Graphs"

year1 <- "2015-2016"
year2 <- "2016-2017"
year3 <- "2017-2018"


#--------------------------------------NEW DATA
#DataFrame:
d1 <- data %>%
  group_by(Origin, Destination, Year) %>%
  summarise(count = n())             # %>%
  #filter(Year==year3)

#Summary table:
ttle <-"Complete"
write.table(d1, paste(ttle," Summary-NEW DATA.csv",sep=""), sep=",", row.names = F)

#Summary bar plot:
ggplot() +
  geom_bar(data, mapping = aes(x = Destination)) +
  facet_grid(Year~Origin)


#--------------------------------------OLD DATA
#DataFrame:
d2<- old_overview %>%
  group_by(Origin, Destination, Year) %>%
  summarise(count=n()) %>%
  #filter(Destination=="UCM" | Destination=="MSP"| Destination=="UCV") #%>% 
  #filter(Year==year3)
  filter(Origin=="UCM")

#Summary table:
ttle2 <- "Complete"
write.table(d2, paste(ttle2," Summary-OLD DATA.csv", sep=""), sep=",", row.names = F)

#Summary bar plot:
ggplot()+
  geom_bar(d2, mapping= aes(x=Destination))+
  facet_grid(Year~Origin)+
  theme_bw()

#--------------------------------------Alluvium Graph
year <-"2015-2018"
d <-d2 #can be d1 or d2
a<-"nan"

if (d==d1){a <- "NEW DATA" }
if(d==d2){a <- "OLD DATA"}
if(d!=d1 | d!=d2){a <- "nan"}

ggplot(as.data.frame(d),
       aes(y = count, axis1 = Origin, axis2 = Destination)) +
  geom_alluvium(aes(fill = Destination), width = 1/12) +
  geom_stratum(width = 1/30, fill = "black", color = "grey") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Origin", "Destination"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle(paste("Flow of Students from UCM for ", year, sep="")) +
  theme_bw()

ggsave(paste( a,"- Flow of UCM students for ", year,".png", sep=""), device= "png", width = 15, height= 15, units="cm", path=path)


