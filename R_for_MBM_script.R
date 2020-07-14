



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


##############################################
#4.0.0 Tables and Figures  ###################
##############################################

##############################################
#4.1.1 Base R Tables       ###################
##############################################


library(tidyverse)

aerial=read.csv(
  "https://raw.githubusercontent.com/cfrost3/MBM_R_Short_Course/master/BLSC_2018_RawObs_Fake.csv", 
  header=TRUE,
  stringsAsFactors=FALSE)

aerial.tidy = aerial %>% 
  filter(!(Species %in% c("START", "ENDPT"))) %>%  #remove start and end points
  select(Lat, Long, Species, Num) %>% #filter to just 4 columns for illustration
  separate(Species, into=c("Species", "Distance"), sep = -1) %>% #take away the rightmost string value
  separate(Species, into=c("Species", "Behavior"), sep = 4)  #take away the first 4 string values


head(aerial.tidy)

table(aerial.tidy$Species,aerial.tidy$Distance)



##########################################
##########################################

table(aerial.tidy$Species,aerial.tidy$Distance, aerial.tidy$Behavior)

##########################################
##########################################

prop.table(table(aerial.tidy$Species,aerial.tidy$Distance))

max(prop.table(table(aerial.tidy$Species,aerial.tidy$Distance)))


##########################################
##########################################

table(aerial.tidy$Species,aerial.tidy$Num)

str(aerial.tidy)



which(aerial.tidy$Num %in% c("d", "t"))  #what positions are the d and t in?

aerial.tidy$Num[427] #verify 

aerial.tidy[427,] #show me that whole row 427 and all of its columns

aerial.tidy[c(427,438),] #show me both row 427 and 438 and all of their columns



aerial.tidy$Num[427] = 2

aerial.tidy$Num[aerial.tidy$Num == "t"] = 1 

#find any values of Num == "t" and replace with 1


table(aerial.tidy$Species,aerial.tidy$Num)

str(aerial.tidy)



aerial.tidy$Num = as.numeric(aerial.tidy$Num)

str(aerial.tidy)

table(aerial.tidy$Species,aerial.tidy$Num)


##########################################
##########################################


#The 1 specifies the first (row) dimension (how many times did we observe BLSC?)
margin.table(table(aerial.tidy$Species,aerial.tidy$Num), 1)

#The 2 specifies the second (column) dimension (how many times did we see only 1 of something?)
margin.table(table(aerial.tidy$Species,aerial.tidy$Num), 2)

#No dimension argument gives us the sum over both rows and columns (how many total observations were there?)
margin.table(table(aerial.tidy$Species,aerial.tidy$Num))




##############################################
#4.1.2 Tables in Contributed Packages ########
##############################################

install.packages(c("DT", "kableExtra", "htmltools", "zoo"))


library(DT)
library(kableExtra)
library(AKaerial)
library(tidyverse)
library(htmltools)
library(zoo)



##########################################
##########################################

ACPHistoric$combined %>% 
  filter(Species=="SPEI") %>% 
  select(Year, itotal, itotal.se) %>% 
  datatable()

##########################################
##########################################


ACPHistoric$combined %>% 
  filter(Species=="SPEI") %>% 
  select(Year, itotal, itotal.se) %>% 
  mutate_at("itotal", ~round(., 0)) %>%
  mutate_at("itotal.se", ~round(., 2)) %>%
  datatable(rownames=FALSE,  #cut out those row numbers showing
            fillContainer = TRUE,  #auto-size the column widths
            colnames=c("Year","Indicated Total","SE"),  #change column names
            #set the caption obtusely using html styling
            caption = htmltools::tags$caption( style = 'caption-side: top; 
                                                text-align: center; 
                                                color:black; 
                                                font-size:100% ;',
                                               'Table 1: Indicated Total SPEI on the ACP, 2007-2019.'),
            options=list(
              autoWidth=TRUE,
              #center the column indexed 1 (starts with 0)
              columnDefs = list(list(className = 'dt-center', targets = 1)),
              pageLength=length(.[,1]),  #display all of the data points
              dom=""))   #take away the search and page functionality

##########################################
##########################################

YKGHistoric$combined %>% 
  filter(Species=="BRAN") %>% 
  select(Year, itotal, itotal.se) %>%
  kable()

##########################################
##########################################

YKGHistoric$combined %>% 
  filter(Species=="EMGO") %>% 
  
  select(Year, itotal, itotal.se) %>%
  
  #rollapply in zoo rolls through a vector and applies a function to the segments
  
  mutate(avg3=rollapply(itotal,3,mean,align='right',fill=NA)) %>% 
  
  mutate(avg10=rollapply(itotal,10,mean,align='right',fill=NA)) %>%
  
  mutate_at(c("itotal", "avg3", "avg10"), ~round(., 0)) %>%
  
  mutate_at("itotal.se", ~round(., 2)) %>%
  
  #apply our conditional population objective coloring
  mutate_at("itotal", function(x) { 
    cell_spec(x, "html", color = ifelse(x > 26000, "green", "red"))
  }) %>%
  
  kable(format="html",
        escape = F, #html scheme requirement to make the color statement work
        #now we paste in the footnote denotation on the indicated total column
        col.names = c("Year", "Indicated Total*", 
                      "SE", "3-year Avg", "10-year Avg"),
        #and add the top caption
        caption = "Table 2: Indicated Total EMGO on the YK-Delta, 1985-2019, including 3- and 10-year Averages.") %>%
  
  #adding the footnote is done in its own function so we pipe the kable to it
  
  footnote(symbol= "Indicated Total is used by the AMBCC to regulate harvest. Values of 26000 and above result in an open harvest.") %>%
  
  #this tells us to use the default bordered style 
  kable_styling("bordered",
                full_width=FALSE, 
                font_size = 14)


##############################################
#4.2.1 Plotting in Base R    #################
##############################################

hist(aerial.tidy$Num)


hist(aerial.tidy$Num,
     xlab = "Number observed", 
     ylab = "Frequency of observation",
     main = "Observations by size and frequency")



##########################################
##########################################

counts=table(aerial$Obs_Type)

barplot(counts, 
        xlab="Observation Type",
        ylab="Frequency",
        main="Frequency of Observation Types")




aerial$Obs_Type[aerial$Obs_Type %in% c("2pair", "pair ")] = "pair"

aerial$Obs_Type[aerial$Obs_Type == "shingle"] = "single"

unique(aerial$Obs_Type)



counts=table(aerial$Obs_Type)

barplot(counts, 
        xlab="Observation Type",
        ylab="Frequency",
        main="Frequency of Observation Types")






##########################################
##########################################

counts=table(aerial.tidy$Distance, aerial.tidy$Species)

barplot(counts, 
        xlab="Species",
        ylab="Frequency",
        main="Frequency of Distance Bins by Species",
        legend=rownames(counts),
        col=c("red","white","yellow","blue"))




counts=table(aerial.tidy$Distance, aerial.tidy$Species)

barplot(counts, 
        xlab="Species",
        ylab="Frequency",
        main="Frequency of Distance Bins by Species",
        legend=rownames(counts),
        col=c("red","white","yellow","blue"),
        beside=TRUE)

##########################################
##########################################

par(mfrow = c(2,2)) #give me a 2x2 matrix graphical display

#R will fill the 4 spaces with my next 4 plot calls

hist(aerial.tidy$Num,
     xlab = "Number observed", 
     ylab = "Frequency of observation",
     main = "Observations by size and frequency")


counts=table(aerial$Obs_Type)

barplot(counts, 
        xlab="Observation Type",
        ylab="Frequency",
        main="Frequency of Observation Types")


counts=table(aerial.tidy$Distance, aerial.tidy$Species)

barplot(counts, 
        xlab="Species",
        ylab="Frequency",
        main="Frequency of Distance Bins by Species",
        legend=rownames(counts),
        col=c("red","white","yellow","blue"))


counts=table(aerial.tidy$Distance, aerial.tidy$Species)

barplot(counts, 
        xlab="Species",
        ylab="Frequency",
        main="Frequency of Distance Bins by Species",
        legend=rownames(counts),
        col=c("red","white","yellow","blue"),
        beside=TRUE)

#don't forget to change it back!
par(mfrow=c(1,1))

##########################################
##########################################


ltdu = YKDHistoric$combined %>% 
  filter(Species == "LTDU")


plot(total~Year, data = ltdu, 
     type = 'l', #connect the dots
     lwd = 3, #thicken the line
     ylim = c(0,10000), #define the y range instead of letting R calculate it
     ylab = "Total Birds Index", 
     main = "LTDU Total Birds Index on the YK Delta, 1986-2019") 

lines((total-1.96*total.se)~Year, data = ltdu, lty = 2)

lines((total+1.96*total.se)~Year, data = ltdu, lty = 2)

points(total~Year, data = ltdu, 
       pch = 2)  #plot character 2, triangles

##########################################
##########################################

install.packages(c("rgdal", "sp", "geojsonio", "ggplot2", "leaflet"))

library(rgdal)
library(sp)
library(geojsonio)
library(ggplot2)
library(leaflet)


##########################################
##########################################

download.file("http://github.com/cfrost3/MBM_R_Short_Course/raw/master/PlotData/PlotData.zip", 
              destfile = "PlotData.zip" , mode='wb')
unzip("PlotData.zip", exdir = "./Plot")
file.remove("PlotData.zip")


map= readOGR("./Plot/ACP_2007to2019_StudyArea.shp",
             layer = "ACP_2007to2019_StudyArea",
             verbose=FALSE)

map.proj <- spTransform(map, "+proj=longlat +ellps=WGS84 +datum=WGS84")

plot(map.proj)





##############################################
#4.2.2 Plotting in Contributed Packages ######
##############################################

YKDHistoric$combined %>% 
  filter(Species == "LTDU") %>%
  ggplot(aes( x = Year, y = itotal))




YKDHistoric$combined %>% 
  filter(Species == "LTDU") %>%
  ggplot(aes( x = Year, y = itotal)) +
  geom_point() +
  geom_line()


##########################################
##########################################

duck.plot = 
  YKDHistoric$combined %>% 
  filter(Species == "LTDU") %>%
  ggplot(aes( x = Year, y = total)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = total - 1.96 * total.se, ymax = total + 1.96 * total.se), 
              fill = "blue", 
              alpha = 0.2) +
  coord_cartesian(xlim=c(1987,2020), ylim=c(0, 10000)) +
  labs(title="LTDU Total Birds Index on the YK Delta, 1986-2019", x="Year", y="Total Birds Index")

print(duck.plot)

##########################################
##########################################

duck.plot + geom_smooth(method='lm')

##########################################
##########################################


emgo = 
  
  #tidy it up and pivot the estimates out
  
  YKGHistoric$combined %>% 
  
  filter(Species=="EMGO") %>% 
  
  select(Year, itotal, itotal.se) %>%
  
  mutate(avg3=rollapply(itotal,3,mean,align='right',fill=NA)) %>% 
  
  mutate(avg10=rollapply(itotal,10,mean,align='right',fill=NA)) %>%
  
  pivot_longer(cols=c("itotal","avg3", "avg10"), names_to = "index", values_to = "estimate")

#now plot it

emgo.base = 
  
  ggplot(emgo, aes(x = Year)) +
  
  #change the linetype (dash spacing) by index type  
  
  geom_line(aes(y = estimate, linetype=index), size = 1) +
  
  #add the confidence intervals
  #note these are specific to itotal, so we have to filter what we pass
  
  geom_ribbon(data = filter(emgo, index=="itotal"), aes(ymin = estimate - 1.96 * itotal.se, ymax = estimate + 1.96 * itotal.se), 
              fill = "blue", 
              alpha = 0.2) +
  
  coord_cartesian(xlim=c(1985,2020), ylim=c(0, 40000)) +
  
  labs(title="Indicated Total EMGO on the YK Delta, 1986-2019", x="Year", y="Indicated Total") +
  
  #make a keen legend to sort out the mess  
  
  scale_linetype_manual(name = "Estimate", 
                        values = c(1,2,3), 
                        labels = c("Annual Estimate", "3-year Avg", "10-year Avg"),
                        limits = c("itotal", "avg3", "avg10")) +
  
  #take the gray tiles off of the back  
  
  theme_bw()


print(emgo.base)


##########################################
##########################################

multi = 
  
  
  YKGHistoric$combined %>% 
  
  filter(Species %in% c("EMGO", "BRAN", "CCGO", "TAVS")) %>% 
  
  select(Year, Species, itotal, itotal.se)


multi.plot = 
  ggplot(multi, aes(x = Year, y = itotal, color = Species)) +
  geom_line(size=2) +
  geom_ribbon(aes(ymin = itotal - 1.96 * itotal.se, ymax = itotal + 1.96 * itotal.se, 
                  color=Species, 
                  fill = Species), 
              alpha=.2)  +
  labs(title="Indicated Total Geese on the YK Delta, 1985-2019", x="Year", y="Indicated Total") 


print(multi.plot)


##########################################
##########################################

multi.plot + 
  facet_wrap(~Species, scales = "free")

##########################################
##########################################

ssm = read.csv("./Plot/StateSpaceEst.csv", 
               header = TRUE, 
               stringsAsFactors = FALSE)

ssm = ssm %>%
  pivot_longer(cols=c("N", "index"), 
               names_to = "method",
               values_to = "estimate")

ssm.plot=
  ggplot(ssm, aes(x = Year, y = estimate, fill = method)) +
  geom_line(data = ssm %>% filter(method == "index"), size=1) +
  geom_point(data = ssm %>% filter(method == "index"), size=2) +
  geom_ribbon(data = ssm %>% filter(method == "index"), 
              aes(ymin = l95index, ymax = u95index),
              alpha = 0.6) +
  
  geom_ribbon(data = ssm %>% filter(method == "N"), 
              aes(ymin = lower95, ymax = upper95),
              alpha = 0.3) +
  
  facet_grid(Species~., scales = "free") + 
  
  
  labs(title="Indicated Total on the ACP, 2009-2018,\nIncluding State-Space Model Estimates",
       x="Year", y="Index / Model Estimate") +
  
  scale_fill_manual(name = "Method\n(95% CI)", 
                    values=c("grey", "red") ,
                    labels = c("Index Estimate", "State Space Estimate")) +
  
  scale_x_continuous("Year", labels = as.character(ssm$Year), breaks = ssm$Year) +
  
  theme(axis.text.x = element_text(face="bold", color="black")) +
  
  theme_bw()


print(ssm.plot)


##########################################
##########################################

ssm.single = ssm %>% filter(Species == "BRAN")

ssm.single.plot =
  ggplot(data = ssm.single, 
         aes(x = Year, y = estimate, fill = method)) +
  
  geom_line(data = ssm.single %>% filter(method == "index"), size=1) +
  
  geom_point(data = ssm.single %>% filter(method == "index"), size=2) +
  
  geom_ribbon(data = ssm.single %>% filter(method == "index"), 
              aes(ymin = l95index, ymax = u95index),
              alpha = 0.6) +
  
  geom_ribbon(data = ssm.single %>% filter(method == "N"), 
              aes(ymin = lower95, ymax = upper95),
              alpha = 0.3) +
  
  labs(title="Indicated Total Brant on the ACP, 2009-2018,\nIncluding State-Space Model Estimates",
       x="Year", y="Index / Model Estimate") +
  
  scale_fill_manual(name = "Method\n(95% CI)", 
                    values=c("grey", "red") ,
                    labels = c("Index Estimate", "State Space Estimate")) +
  
  scale_x_continuous("Year", 
                     labels = as.character(ssm.single$Year), 
                     breaks = ssm.single$Year) +
  
  theme(axis.text.x = element_text(face="bold", color="black")) +
  
  coord_cartesian(ylim=c(0, 1.1 * max(ssm.single$u95index))) +
  
  geom_text(x=min(ssm.single$Year + 1), y=max(ssm.single$u95index),
            label=paste0("Mean r = ", round(1 + ssm.single$mean.r[1], 2))) +
  
  geom_text(x=min(ssm.single$Year + 1), y=.95*max(ssm.single$u95index),
            label=paste0("Pr(r > 0) = ", round(ssm.single$p.r, 2))) +
  
  theme_bw()

print(ssm.single.plot)

##########################################
##########################################

ssm = read.csv("./Plot/StateSpaceEst.csv", 
               header = TRUE, 
               stringsAsFactors = FALSE)

ssm %>%
  
  filter(Species == "BRAN") %>%
  
  select(-sd, -c(9:14)) %>%
  
  mutate_at(c(2:7), ~round(., 0)) %>%
  
  select(c(1,5,6,7,2,3,4)) %>%
  
  kable(format="html",
        escape = F, 
        col.names = c("Year", "Indicated Total*", 
                      "Lower", "Upper", 
                      "State-Space Estimate", 
                      "Lower", "Upper"),
        
        caption = "Table 3: Indicated Total Brant on the ACP, 2009-2018, including state-space model estimates.") %>%
  
  
  footnote(symbol= "Indicated Total is calculated as 2(singles + pairs) + flocked birds.") %>%
  
  
  kable_styling("bordered",
                full_width=FALSE, 
                font_size = 14) %>%
  
  add_header_above(c(" " = 2, "95% CI" = 2, " " = 1, "95% CI" = 2))

##########################################
##########################################

map.df = AKaerial::LoadMap("./Plot/ACP_2007to2019_StudyArea.shp")

ggplot() +
  
  #geom_path here tells ggplot to connect the dots of my spatial file,
  #since polygons in their simplest form a just a series of vertices. 
  
  geom_path(data=map.df, aes(long,lat,group=group)  ) +
  
  geom_path(color="black") +
  
  #now set a reasonable bounding box  
  coord_map(xlim=c(min(map.df$long), max(map.df$long)), 
            ylim=c(min(map.df$lat), max(map.df$lat)))

##########################################
##########################################


leaflet() %>%
  addTiles() %>%  # use the default base map tiles
  addMarkers(lng=-87.6553, lat=41.9484,
             popup="Wrigley Field")

##########################################
##########################################


leaflet() %>%
  addTiles() %>%
  addPolygons(data=map.proj,
              fillColor = "red",
              fillOpacity = .5)

##########################################
##########################################


#read and project our lines shp file

lines= readOGR("./Plot/ACP_2019_Transects.shp",
               layer = "ACP_2019_Transects",
               verbose=FALSE)

lines.proj <- spTransform(lines, "+proj=longlat +ellps=WGS84 +datum=WGS84")

#map it

leaflet() %>%
  
  addPolygons(data=map.proj,
              color = "yellow",
              fill = FALSE,
              fillOpacity = .5) %>%
  
  #add our lines here
  addPolylines(data=lines.proj, 
               color="white",
               weight=4,
               opacity=.9,
               label=~TRANSID,
               popup = paste("Transect: ", lines.proj$TRANSID, "<br>",
                             "Length: ", lines.proj$LENGTH)) %>%
  
  #scale for...scale
  addScaleBar() %>%
  
  #cool satellite imagery base map
  addProviderTiles("Esri.WorldImagery") %>%
  
  #add labels
  addLabelOnlyMarkers(data = fortify(lines.proj) %>% 
                        filter(order == 1) %>%
                        mutate(new.id = as.numeric(id) + 1),
                      label = ~as.character(new.id),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T,
                                                  style = list(
                                                    "color" = "white",
                                                    "font-size" = "12px")))



##########################################
##########################################

#read our observations

obs = read.csv("./Plot/ACP_2019_QCObs_HWilson.csv",
               header=TRUE,
               stringsAsFactors = FALSE)

#define the spatial x,y for plotting
coordinates(obs)=~Lon+Lat

#map it

leaflet() %>%
  
  addPolygons(data=map.proj,
              color = "yellow",
              fill = FALSE,
              fillOpacity = .5) %>%
  
  #add our lines here
  addPolylines(data=lines.proj, 
               color="white",
               weight=4,
               opacity=.9,
               label=~TRANSID,
               popup = paste("Transect: ", lines.proj$TRANSID, "<br>",
                             "Length: ", lines.proj$LENGTH)) %>%
  
  #scale for...scale
  addScaleBar() %>%
  
  #cool satellite imagery base map
  addProviderTiles("Esri.WorldImagery") %>%
  
  #add labels
  addLabelOnlyMarkers(data = fortify(lines.proj) %>% 
                        filter(order == 1) %>%
                        mutate(new.id = as.numeric(id) + 1),
                      label = ~as.character(new.id),
                      labelOptions = labelOptions(noHide = T, 
                                                  direction = 'top', 
                                                  textOnly = T,
                                                  style = list(
                                                    "color" = "white",
                                                    "font-size" = "12px"))) %>%
  
  #add the point data
  addCircleMarkers(data=obs,
                   radius = 5,
                   color = "green",
                   stroke = FALSE,
                   fillOpacity = 1,
                   popup= paste(obs$Observer, "<br>", obs$Species,
                                "<br>", obs$Obs_Type, "<br>", "n = ",obs$Num,
                                "<br>", "Transect ", obs$Transect, "<br>"))


##########################################
##########################################



ykdair <- geojson_read("./Plot/YKD_DesignStrata.geojson",  what = "sp")
ykdnest <-  geojson_read("./Plot/NestPlotStudyAreaBoundary.geojson",  what = "sp")

leaflet() %>%
  addTiles() %>%
  addPolygons(data=ykdair,
              fillColor="blue",
              fillOpacity=0,
              stroke=TRUE,
              color="white",
              opacity=1,
              weight=2) %>%
  addPolygons(data=ykdnest,
              fillOpacity=0,
              stroke=TRUE,
              color="red",
              opacity=1,
              weight=2) %>%
  addScaleBar() %>%
  addProviderTiles("Esri.WorldImagery")

##########################################
##########################################









