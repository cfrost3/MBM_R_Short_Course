



##############################################
#1.1.1 Packages        #######################
##############################################

mean(c(1,2,3))

sum(c(1,2,3))

var(c(1,2,3))

##############################################
#1.1.2 Installing from GitHub  ###############
##############################################

install.packages("devtools")

library(devtools)

install_github("USFWS/AKaerial", ref = "development")

library(AKaerial) 

##############################################
#1.1.3 Functions              ################
##############################################

AdjustCounts

?AdjustCounts

numbers = c(1,2,3)

avg = mean(numbers)

avg


##############################################
#2.1.0 Loading Files        ##################
##############################################

small.data = data.frame(
  name = c("Laura", "Zak", "Chuck"),
  size = c("small", "medium", "large"),
  tenure = c(6, "drifter", 9)
)

small.data

str(small.data)

small.data$tenure[2] = 3

small.data[2,3] = 3

levels(small.data$tenure)

small.data$tenure=as.numeric(as.character(small.data$tenure))

small.data

small.data = data.frame(
  name = c("Laura", "Zak", "Chuck"),
  size = c("small", "medium", "large"),
  tenure = c(6,3,9)
)

str(small.data)


small.data = data.frame(
  name = c("Laura", "Zak", "Chuck"),
  size = c("small", "medium", "large"),
  tenure = c(6,3,9),
  stringsAsFactors = FALSE
)

str(small.data)


small.data$greatness = small.data$tenure^3

small.data

small.data = rbind(small.data, c("Hannah", "small", 1, 1))

str(small.data)

small.data

small.data = cbind(small.data, interests=c("Bedazzling", "K-Pop", "Couponing", "Oranges"))

small.data


##############################################
#2.2.0 Reading in a data file ################
##############################################


aerial=read.csv(
  "https://raw.githubusercontent.com/cfrost3/MBM_R_Short_Course/master/BLSC_2018_RawObs_Fake.csv", 
  header=TRUE,
  stringsAsFactors=FALSE)

str(aerial)


##############################################
#2.3.3 Useful Data Structure Functions #######
##############################################

a = c(1,2,3,4,10.1)

length(a)  #how long is the vector?

mean(a)  #mean value

var(a)  #variance

typeof(a)  #what data type?  (double means floating decimal numeric)

is.na(a)  #any NA or missing values?

summary(a)  #some basic summarizing statistics

max(a)  #maximum value

min(a)  #minimum value

dim(small.data)  #what are the dimensions of a structure?

class(small.data)  #what class of structure is it?

names(small.data)  #what are the names of the objects that make up the structure?

unique(small.data$size)  #what are all of the unique values in a range?


