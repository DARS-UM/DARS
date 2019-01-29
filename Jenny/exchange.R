library(dplyr)

#IMPORT DATA
new_msp      <- read.csv("MSPtoUCM_new.csv",      na.strings = "")
new_msp_2017 <- read.csv("MSPtoUCM_2017_new.csv", na.strings = "")
new_ucv      <- read.csv("UCVtoUCM_new.csv",      na.strings = "")
new_ucv_2017 <- read.csv("UCVtoUCM_2017_new.csv", na.strings = "")

old_overview <-read.csv("OldOverview.csv", na.strings="")

#-----------------------------------------NEW DATA----------------------------
#*****************************************************************************

#----------------------FORMAT UCV---------------------------------- 

#---------2016/2017

# d <- new_msp
# 
# rbind(select(new_msp, "X...MSP.students.who.took.courses.at.UCM", "Courses") %>%
#   mutate(Origin = "MSP",
#          Destination = "UCM",
#          Year = "2016/2017") %>%
#   filter(!is.na(Courses)),
#   
# 

a <- new_ucv[,1:2] #not nice solution, the emty columns appear as filled with somehting
names(a) <- c("Student", "Courses")
a<-na.omit(a)
a[,"Origin"] <- "UCV"
a[,"Destination"] <- "UCM"
a[,"Year"] <- "2016-2017"

final_names <- names(a)

b <- new_ucv[,4:5]
names(b) <- c("Student", "Courses")
b<-na.omit(b)
b[,"Origin"] <- c("UCM")
b[,"Destination"] <- c("UCV")
b[,"Year"] <- c("2016-2017")

data <- rbind(a,b)


#-------2017/2018 
a <- new_ucv_2017[,1:2] #not nice solution, the emty columns appear as filled with somehting
names(a) <- c("Student", "Courses")
a<-na.omit(a)
a[,"Origin"] <- c("UCV")
a[,"Destination"] <- c("UCM")
a[,"Year"] <- c("2017-2018")

b <- new_ucv_2017[,4:5]
names(b) <- c("Student", "Courses")
b<-na.omit(b)
b[,"Origin"] <- c("UCM")
b[,"Destination"] <- c("UCV")
b[,"Year"] <- c("2017-2018")

data1 <- rbind(a,b)
data <- rbind(data, data1)

#----------------------FORMAT MSP---------------------------------- 

#---------2016/2017
a <- new_msp[,1:2]
names(a) <- c("Student", "Courses")
a<-na.omit(a)
a[,"Origin"] <- c("MSP")
a[,"Destination"] <- c("UCM")
a[,"Year"] <- c("2016-2017")

b <- new_msp[,4:5]
names(b) <- c("Student", "Courses")
b<-na.omit(b)
b[,"Origin"] <- c("UCM")
b[,"Destination"] <- c("MSP")
b[,"Year"] <- c("2016-2017")

data1 <- rbind(a,b)
data <- rbind(data, data1)

#---------2017/2018
a <- new_msp_2017[,1:2] #not nice solution, the emty columns appear as filled with somehting
names(a) <- c("Student", "Courses")
a<-na.omit(a)
a[,"Origin"] <- c("UCV")
a[,"Destination"] <- c("UCM")
a[,"Year"] <- c("2017-2018")

b <- new_msp_2017[,4:5]
names(b) <- c("Student", "Courses")
b<-na.omit(b)
b[,"Origin"] <- c("UCM")
b[,"Destination"] <- c("UCV")
b[,"Year"] <- c("2017-2018")

data1 <- rbind(a,b)
data <- rbind(data, data1)

#-----------------------------------------OLD DATA----------------------------
#*****************************************************************************

old_overview$LAS <- case_when(old_overview$LAS=="BA LAS(UCV)" ~ "UCV",
          old_overview$LAS=="BA LAS(UCM)" ~ "UCM",
          old_overview$LAS=="BA LAS(MSP)" ~ "MSP")

old_overview <-filter(old_overview, old_overview$Status.Request=="APPROVED") %>% 
    select(., "Module", "LAS", "Fac..Module", "Academic.Year")
names(old_overview) <- c("Courses", "Origin", "Destination", "Year")
