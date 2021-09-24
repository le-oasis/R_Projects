#Get & Set Working Directory  
getwd()
#setwd("")


#Read data
util <- read.csv("MU.csv")
install.packages('jsonlite')
#Fix to factor
str(util)
util$Timestamp <- as.factor(util$Timestamp)
util$Machine <- as.factor(util$Machine)
str(util)

#Derive Utilization Column 
util$Utilization = 1 - util$Percent.Idle
head(util,12)

#Handling DateTime 
?POSIXct
util$PosixTime <- as.POSIXct(util$Timestamp, format="%d/%m/%Y  %H:%M")
head(util,12)

#Rearrgange Column 
util$Timestamp <- NULL
head(util,12)
util <- util[,c(4,1,2,3)]
head(util,12)

#We only want to deal with RL1
RL1 <- util[util$Machine== "RL1",]
summary(RL1)
RL1$Machine <- factor(RL1$Machine)
summary(RL1)

#We want Character of Machine name, stats,t/f

#Stats

util_stats_RL1 <- c(min(RL1$Utilization, na.rm = T), #84.9%
                    max(RL1$Utilization, na.rm = T), #was 99.5%, in no given hour was Utilization 100%
                    mean(RL1$Utilization, na.rm = T)) #95.1% 
#Check
util_stats_RL1

#Under90
which(RL1$Utilization < 0.90)
length(which(RL1$Utilization < 0.90)) #27 times Utilization fell below 90%
length(which(RL1$Utilization < 0.90)) > 0
util_under90_flag <- length(which(RL1$Utilization < 0.90)) >0
util_under90_flag

#Construct List. Naming Components
list_RL1 <- list("RL1", util_stats_RL1, util_under90_flag)
list_RL1
names(list_RL1)
names(list_RL1) <- c("Machine", "Stats", "LowThreshold")
list_RL1

#Extracting components from a list 
list_RL1
list_RL1[1]
list_RL1[[1]] #we no longer have $MAchine cause this is now a vector. 
list_RL1$Machine #same answer
list_RL1[2]
typeof(list_RL1[[2]])

#how to access the 3rd element of the vector.
list_RL1
list_RL1$Stats[3]

#Adding / Deleting components 
list_RL1[4] <- "New Information"
list_RL1

#Another way to add a component via the $
#We will add:
#Vector : All hours where Utilization is unknown (NA's)
RL1 # 1-7 unknown 
RL1[is.na(RL1$Utilization),] #subset of our datafram where Utilisation is NA.
RL1[is.na(RL1$Utilization),"PosixTime"]

#Add component via $
list_RL1$Unknown_Hours <- RL1[is.na(RL1$Utilization),"PosixTime"] 
list_RL1

#Remove 
list_RL1[4] <- NULL  #removes  "New Information
list_RL1

#Notice: numeration has shifted
list_RL1[4]

#Add another component 
#Dataframe: for this machine

list_RL1$Data <- RL1
list_RL1

#Summary 
summary(list_RL1)
str(list_RL1)


#Subsetting a list.
list_RL1

#How would you access the first component in $Unknown_Hours
list_RL1$Unknown_Hours[1]

#Let's proceed... 
list_RL1
list_RL1[1:3] # 1 to 3. 
list_RL1[c(1,3)] # 1 and 4
list_RL1[c(1,4)] # 1 and 4

#Subsetting a list properly, giving it a name.
sublist_RL1 <- list_RL1[c("Machine","Stats")]
sublist_RL1
sublist_RL1[[2]][2]
sublist_RL1$Stats[2]


#single squre brackets are for subsetting
#double square brackets are for accessing elements of a list.

#Double Square Brackets are for Subsetting:
#list_RL1[[1:3]]  #ERROR HERE.

#Building a TimeSERIES plot 
#install,packages("ggplot2")

library(ggplot2)
p <- ggplot(data = util)
#geom line is used for the threshold whhic is 0.90
p + geom_line(aes(x=PosixTime, y=Utilization, colour = Machine)
              , size=0.4) + facet_grid(Machine~.) + geom_hline(yintercept = 0.90, colour="Gray")



my_plot <- p + geom_line(aes(x=PosixTime, y=Utilization, colour = Machine), size=0.4) + facet_grid(Machine~.) + geom_hline(yintercept = 0.90, colour="Gray")

list_RL1$Plot <- my_plot
list_RL1
summary(list_RL1) #length 9

str(list_RL1)


list_RL1
              
