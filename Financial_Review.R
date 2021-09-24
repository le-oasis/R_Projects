
getwd()
#setwd()

fin <- read.csv('P2.csv', na.strings =c(""))
fin

fin$Expenses <- gsub(" Dollars", "",fin$Expenses)
fin$Expenses <- gsub(",", "",fin$Expenses)
fin$Revenue <- gsub("\\$", "", fin$Revenue)
fin$Revenue <- gsub(",", "", fin$Revenue)
fin$Growth <- gsub("%", "", fin$Growth)

head(fin)

fin$Expenses <- as.numeric(fin$Expenses)
fin$Revenue  <- as.numeric(fin$Revenue)
fin$Growth   <- as.numeric(fin$Growth)
str(fin)

#Fix
fin$Inception <- as.factor(fin$Inception)
str(fin)
fin$ID <- as.factor(fin$ID)
str(fin)
summary(fin)

# Locate Missing Data 
fin[!complete.cases(fin),]
str(fin)

#Filtering: using which() for non-missing data
fin[fin$Revenue == 9746272,]
which(fin$Revenue == 9746272)
?which
fin[c(3,4,5),]
fin[which(fin$Revenue == 9746272),]
head(fin)
fin[fin$Employees == 45,]
fin[which(fin$Employees ==45),]


#Filtering: using is .na() 
fin[is.na(fin$Expenses),] 
fin[is.na(fin$State),]
fin[is.na(fin$Revenue),]
fin[is.na(fin$Growth),]


#BackUp 
fin_backup <- fin

#Check Messy Data.
fin[!complete.cases(fin),]

#Removing Records With Missing Data
fin[is.na(fin$Industry),] 
fin[!is.na(fin$Industry),] #opposite. .#which rows are not empty in that column.
# notice row 13 and 14 removed.
fin <- fin[!is.na(fin$Industry),] #override. create a subset, then you override the previous dataframe.
fin

fin[!complete.cases(fin),]
fin

#Reset Datafram Index. Fix RowNames. #fin currently has 498 not 500. 
#For Aesthetic Reasons.
rownames(fin) <- NULL

#Replacing Missing Data : Factual Analysis
fin[!complete.cases(fin),] 

fin[is.na(fin$State),] # to find <NA> in State.

fin[!complete.cases(fin),]
fin[is.na(fin$State) & fin$City == 'New York',] 
fin[is.na(fin$State) & fin$City == 'New York', 'State'] <- 'NY'
fin[is.na(fin$State) & fin$City == 'San Francisco',]
fin[is.na(fin$State) & fin$City == 'San Francisco','State'] <- 'CA' #derive what the state is based on the city.

#check
fin[c(11,377),]
fin[c(82,265),]

#Missing Values Left
fin[!complete.cases(fin),] 

#Replace Missing Data: Median Imputation Method (Part 1)
#Median for Retail-Industry
med_employ_retail <- median(fin[fin$Industry == 'Retail','Employees'], na.rm = TRUE)
med_employ_retail
fin[is.na(fin$Employees) & fin$Industry == 'Retail','Employees'] <- med_employ_retail

#check ~ we assigned 28 to the Empty NA in Employees.
fin[3,]

#Median for Financial Services

med_employ_finserv <- median(fin[fin$Industry== "Financial Services", "Employees"], na.rm = TRUE)
med_employ_finserv
fin[is.na(fin$Employees) & fin$Industry== "Financial Services",]
fin[is.na(fin$Employees) & fin$Industry== "Financial Services","Employees"] <- med_employ_finserv
fin[330,]

#Missing Values Left
fin[!complete.cases(fin),] 

#Growth Columns 
med_growth_constr <- median(fin[fin$Industry== "Construction", "Growth"], na.rm = TRUE)
med_growth_constr
fin[is.na(fin$Growth) &fin$Industry== "Construction",]
fin[is.na(fin$Growth) &fin$Industry== "Construction","Growth"] <- med_growth_constr

#check 
fin[8,]

#Missing Values Left
fin[!complete.cases(fin),] 

#Revenue: 
med_rev_constr <- median(fin[fin$Industry== "Construction", "Revenue"], na.rm=TRUE)
med_rev_constr
fin[is.na(fin$Revenue) & fin$Industry== "Construction",]
fin[is.na(fin$Revenue) & fin$Industry== "Construction", "Revenue"] <- med_rev_constr

#Missing Values Left
fin[!complete.cases(fin),] 

#Expenses:
#Row 15 is special - link between revenue and profit so cannot use median.
med_exp_constr <- median(fin[fin$Industry== "Construction", "Expenses"], na.rm=TRUE)
med_exp_constr
fin[is.na(fin$Expenses) & fin$Industry== "Construction" & is.na(fin$Profit),]
fin[is.na(fin$Expenses) & fin$Industry== "Construction" & is.na(fin$Profit), "Expenses"] <- med_exp_constr



#Replacing Missing Data: Deriving Values. 
fin[is.na(fin$Profit),]
#Profit = Revenue - Expenses.
fin[is.na(fin$Profit), "Profit"] <- fin[is.na(fin$Profit), "Revenue"] - fin[is.na(fin$Profit), "Expenses"]
#check
fin[c(8,42),]

#Missing Values Left
fin[!complete.cases(fin),] 
fin[c(15,20),]
#Expenses = Revenue - Profit.
fin[is.na(fin$Expenses),"Expenses"] <- fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses),"Profit"]

fin[!complete.cases(fin),] 

#Done, agreed to keep row 20.

#70% GOES TO DATA PREP
#Visualization
library(ggplot2)

#A scatterplot classified by Industry,Revenue, Expenses, Profit.
p <- ggplot(data = fin)
p
p + geom_point(aes(x=Revenue, y=Expenses, colour=Industry, size=Profit))

#Financial Services - Expenses are quite low.


#A scatterplot that includes Industry trends for the expenses 

d <- ggplot(data=fin, aes(x=Revenue, y=Expenses, colour =Industry))

d + geom_point() + geom_smooth(fill=NA, size =1.2)


#fill = na, so we d not see confidence bans.
#increased the size of the smoother to 1.2.

#Boxplot

f <- ggplot(data = fin, aes(x=Industry, y=Growth, colour =Industry))
f + geom_boxplot(size =1)

f + geom_jitter() + geom_boxplot(size=1, alpha=0.5, outlier.colour = NA)

