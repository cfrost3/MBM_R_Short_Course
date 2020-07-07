



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



##############################################
#3.1.1 Values as Column Names  ###############
##############################################

messy1 = data.frame(Species = rep(c("COGO", "BOOW"), each = 3), 
                    Box=c(1:6), 
                    Visit1 = c(5,6,6,4,5,4),
                    Visit2 = c(5,6,5,4,5,7),
                    Visit3 = c(0,6,5,4,5,0),
                    Visit4 = c(NA,0,0,1,0,NA),
                    Visit5 = c(NA,NA,NA,0,NA,NA), 
                    stringsAsFactors = FALSE
)


messy1



#give me the total of row 1 values greater than or equal to 0
v = sum(messy1[1,]>0)  

v

#give me the total of row 1 values greater than or equal to 0, and skip the NAs
v = sum(messy1[1,]>0, na.rm = TRUE)  

v

#give the total of row 1 values >= to 0, skip the NAs, subtract 1 for Box
v = sum(messy1[1,]>0, na.rm = TRUE)-1  

v


install.packages("tidyverse")


library(tidyverse)

aerial=read.csv(
  "https://raw.githubusercontent.com/cfrost3/MBM_R_Short_Course/master/BLSC_2018_RawObs_Fake.csv", 
  header=TRUE,
  stringsAsFactors=FALSE)

#grab Transect, Species, and Num only from aerial
aerial.subset = select(aerial, Transect, Species, Num)  

head(aerial.subset)  #only show me the first few rows

aerial.subset2 = filter(aerial.subset, Num > 2) #give me observations greater than 2

head(aerial.subset2)

obs.by.species = aerial %>%
  
  #grab Transect, Species, and Num only from aerial
  select(Transect, Species, Num)  %>% 
  
  #Num greater than 2, but remove START and ENDPT observations
  filter(Num > 2 & !(Species %in% c("START", "ENDPT")) ) %>% 
  
  
  aggregate(Num~Species, .,FUN=length)

names(obs.by.species)[2]="N.obs"

obs.by.species

messy1

tidy1 = messy1 %>%
  pivot_longer(c("Visit1", "Visit2","Visit3","Visit4","Visit5",), 
               names_to = "Visit", 
               values_to = "eggs")

tidy1

tidy1 = messy1 %>%
  pivot_longer(
    cols = starts_with("Visit"),
    names_to = "Visit",
    names_prefix = "Visit",
    values_to = "Eggs",
    values_drop_na = TRUE
  )

head(tidy1,10)



vis = tidy1 %>% 
  aggregate(Visit~Species + Box, ., FUN=max)
names(vis)[3] = "Num.Visits"

vis

avg.egg = tidy1 %>%
  filter(Visit == 1) %>%
  aggregate(Eggs~Species, ., FUN=mean)
names(avg.egg)[2] = "Avg.Eggs"

avg.egg

#Or using just tidyverse

tidy1 %>% 
  filter(Visit == 1) %>%
  group_by(Species) %>%
  summarize(Avg.Eggs=mean(Eggs))


##############################################
#3.1.2 Multiple Variables in One Column ######
##############################################

messy2 = data.frame(monthday=paste(rep(6,10), c(1:10), sep="/"), Grade=rep(c("A", "F"), 5))

messy2

messy2 %>%
  separate(monthday, into=c("Month", "Day"), sep="/")


unique(aerial$Species)

aerial.tidy = aerial %>% 
  filter(!(Species %in% c("START", "ENDPT"))) %>%  #remove start and end points
  select(Lat, Long, Species, Num) %>% #filter to just 4 columns for illustration
  separate(Species, into=c("Species", "Distance"), sep = -1) %>% #take away the rightmost string value
  separate(Species, into=c("Species", "Behavior"), sep = 4)  #take away the first 4 string values


head(aerial.tidy)

unique(aerial.tidy$Species)
