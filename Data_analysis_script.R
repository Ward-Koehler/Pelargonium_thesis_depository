#Check directory
getwd()
#Output should be "C:/Users/wardk/Documents/MSc Biology/MSc Thesis/R/Data_analysis"
#Otherwise set the correct directory

#Import data
library(readxl)
Spurs_field <- read_excel("C:/Users/wardk/OneDrive - Wageningen University & Research/Spurs_field.xlsx")
Spurs_field_only_species <- read_excel("C:/Users/wardk/OneDrive - Wageningen University & Research/Spurs_field_only_species.xlsx")

#Open data in a separate tab
View(Spurs_field)

#####Exploratory visualization
{
#Import visualization program
library(ggplot2)
#Histogram length
hist(Spurs_field_only_species$Spur_length)

#Quick overview of the data in the console
summary(Spurs_field_only_species$Spur_length)

#Boxplot length vs species
boxplot(Spur_length ~ Species, data = Spurs_field)

#Boxplot length vs location
boxplot(Spur_length ~ (floor(Location)), data = Spurs_field)

#Histogram per species
ggplot(data = Spurs_field, aes(x= Spur_length, fill=Species))+
  facet_wrap(~Species)+
  geom_histogram()
}

#####Statistical Analysis
{#We need to check for normality of the data for any statistical analysis
library(car)

#Histogram & QQ-Plot Anova residuals
res_aov <- aov(Spur_length ~ Species,
               data = Spurs_field_only_species)
hist(res_aov$residuals)
qqPlot(res_aov$residuals,
       id = FALSE)

#Test the normality
leveneTest(Spur_length ~ Species, data = Spurs_field_only_species)

#Not normally distributed so we can't use ANOVA

#Let's try Kruskal Wallace 
kruskal.test(Spur_length ~ Species,
             data = Spurs_field_only_species)

#Very significant! At least 2 groups (Species in this case) differ significantly from each other.

#Which groups though? Let's have a look.
#We need a new test for this: the Dunn test
#For this test we need a new package "FSA"
install.packages("FSA")
library(FSA)

#Now let's do the Dunn test
Phocdunn <- dunnTest(Spur_length ~ Species,
         data = Spurs_field_only_species,
         method = "holm")
#This is giving us way too much output because we're comparing every species with every other species.

Phocdunns <- Phocdunn$res

#We could use a Compact letter display (CLD)
#We can use the rcompanion package for this
install.packages("rcompanion")
library(rcompanion)


cld <- cldList(comparison = Phocdunns$Comparison,
               p.value    = Phocdunns$P.adj,
               threshold  = 0.05)[1:2]

names(cld)[1]<-"Species" # change the name of grouping factor according to the dataset (df)
cld

install.packages("tidyverse")
library(tidyverse)
ggplot(data=Spurs_field_only_species, aes(x=Species, y=Spur_length,col=Species))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.5), size = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_text(data = cld, aes(label = Letter, y = 6, x =Species, ), 
            vjust = -0.5,
            hjust= 0.5,
            fontface = "bold",
            size=3.5,
            check_overlap = F)+
  theme(legend.position = "none")+
 ylim(0, NA)
}

#####Produce a table of observation counts per species.
{#We need this to determine if it is (statistically) relevant to include all species.

#dplyr has a lot of useful tools
install.packages("dplyr")
library(dplyr)

#This creates a table with dplyr, but how do I use it/save it
d <- Spurs_field_only_species %>%
  group_by(Species, floor(Location)) %>%
  summarise(count = n())
#
#Why is this here again?
filter_all(Freq_Loc_Spec, any_vars(. != 0))

#This creates a table in the command line
Obs_per_species <- table(Spurs_field_only_species$Species)

#A table is like a data frame but it only has a dimension for the data
dim(Obs_per_species) # This should return a single dimension with 31 length (species count)
#Names for this dimension can be accessed with dimnames()
dimnames(Obs_per_species) #This should return an empty name for the species dimension [[1]]

#We can change this empty name using the names() function
names(dimnames(Obs_per_species)) <- c("Species")

#We can also use a different package called stats
library(stats)

#In this package we have the function xtabs()
Freq_per_species <- xtabs(~Species, data = Spurs_field_only_species)
View(Freq_per_species)
#With xtabs() we don't need to manually add the column label
dimnames(Freq_per_species) 

#Now make a table for observations per location
Freq_per_location <- xtabs(~floor(Location), data = Spurs_field_only_species)
View(Freq_per_location)

#This makes a combined table with the observations of each species per location
Freq_Loc_Spec <- xtabs(~floor(Location)+Species, data = Spurs_field_only_species) #This makes the location the first column
View(Freq_Loc_Spec)
Freq_Spec_Loc <- xtabs(~floor(Location)+Species, data = Spurs_field_only_species) #This makes the location the second column
View(Freq_Spec_Loc)
#Because most locations only contain a handful of species we end up with a lot of zeroes, making the table hard to parse.

#How do we remove the zeroes?
Freq_Spec_Loc <- xtabs(~floor(Location)+Species, data = Spurs_field_only_species, exclude = c(0)) #This makes the location the second column
View(Freq_Spec_Loc)

#This is still not ideal for checking presence and absence.
#We could make a matrix/table with species on the y-axis and locations on the x-axis.
table(Spurs_field_only_species$Species, floor(Spurs_field_only_species$Location))

#Now lets try it with dplyr
freq_table <- Spurs_field_only_species %>%
    count(Species, Location) %>%      # count observations per species-location pair
  pivot_wider(names_from = Location, values_from = n, values_fill = 0)
#This way we get al the sublocations, which we don't want.

#Let's try it with the floor of the location.
freq_table <- Spurs_field_only_species %>%
  count(Species, floor(Location)) %>%      # count observations per species-location pair
  pivot_wider(names_from = floor(Location), values_from = n, values_fill = 0)
#This does not work unfortunately

#I have two options left after the current location column by taking the floor of all the values,
#or add a new column with this floor value.
#The second option seems better, so lets do that.
Spurs_field_only_species$floor_location <- floor(Spurs_field_only_species$Location)

freq_table <- Spurs_field_only_species %>%
  count(Species, floor_location) %>%      # count observations per species-location pair
  pivot_wider(names_from = floor_location, values_from = n, values_fill = 0)

View(freq_table)
}
#So far we've been using the number codes for the locations.
#This works and is easier for the mathematics, but not very informative for a reader.
#So we should add the actual location names to the file.
#For legibility and visualization it is also useful to have a short abbreviation or code for each location.
#I made a code for each location using the first for letters of the location and capitalizing the first. 
#Two locations (i.e. Cape Point 10 & Cape Town 17) did not have unique starting symbols, so instead the first 2 letters of each word were used and capitalized (i.e. CaPo & CaTo).
#I made a new excel file with the location number, name and abbreviation.

#Lets load this new excel into R
library(readxl)
Jens_locations <- read_excel("C:/Users/wardk/OneDrive - Wageningen University & Research/Jens_locations.xlsx")

#Check if its loaded correctly
#View(Jens_locations)

#Do we append this data frame to the main data frame or do we use it as a separate table?
#Let's try appending first.

merged_data_test <- Spurs_field_only_species %>%
  left_join(Jens_locations, by = c("floor_location" = "Number"))

#Check if its appended correctly
#view(merged_data_test)

#Testing some simple boxplots with this
boxplot(Spur_length ~ floor_location, data = merged_data_test, names = 'Name')
boxplot(Spur_length ~ Name, data = merged_data_test)
boxplot(Spur_length ~ Code, data = merged_data_test)


#Creating

#Comparing the observations of a single location with a random sampling of all observations.

