##### Check directory
getwd()
#Output should be "C:/Users/wardk/Documents/MSc Biology/MSc Thesis/R/Data_analysis"
#Otherwise set the correct directory
#setwd("C:/Users/wardk/Documents/MSc Biology/MSc Thesis/R/Data_analysis"


##### Load libraries
{library(readxl)
library(tidyverse)
library(car)
library(FSA)
library(rcompanion)
}
##### Import data
#library(readxl)
#github depository importing
{#Spurs_field <- read_excel("./Spurs_field.xlsx")
#Spurs_field_only_species <- read_excel("./Spurs_field_only_species.xlsx")
  }
#one drive importing
{#Spurs_field <- read_excel("C:/Users/wardk/OneDrive - Wageningen University & Research/Spurs_field.xlsx")
Spurs_field_only_species <- read_excel("C:/Users/wardk/OneDrive - Wageningen University & Research/Spurs_field_only_species.xlsx")
Jens_locations <- read_excel("C:/Users/wardk/OneDrive - Wageningen University & Research/Jens_locations.xlsx")
  }

#Check if data is loaded correctly: Should be 1526 obs of 3 variables
#Open data in a separate tab
#View(Spurs_field_only_species)


##### Exploratory visualization
{
  #Import visualization program
  library(ggplot2)
  #Histogram length
  hist(Spurs_field_only_species$Spur_length)

  #Quick overview of the data in the console
  summary(Spurs_field_only_species$Spur_length)

  #Boxplot length vs species
  #boxplot(Spur_length ~ Species, data = Spurs_field)

  #Boxplot length vs location
  #boxplot(Spur_length ~ (floor(Location)), data = Spurs_field)

  #Histogram per species
  # ggplot(data = Spurs_field, aes(x= Spur_length, fill=Species))+
  #   facet_wrap(~Species)+
  #   geom_histogram()
}


##### Statistical Analysis
{
  #We need to check for normality of the data for any statistical analysis
  library(car)

  #Histogram & QQ-Plot Anova residuals
  res_aov <- aov(Spur_length ~ Species,
                data = Spurs_field_only_species)
  #Visualizations
  hist(res_aov$residuals)
  qqPlot(res_aov$residuals,
        id = FALSE)

  #Test the normality
  #This doesn't check normality. It checks whether variances are equal between species.
  leveneTest(Spur_length ~ Species, data = Spurs_field_only_species)

  #Not normally distributed so we can't use ANOVA

  #Let's try Kruskal Wallace 
  kruskal.test(Spur_length ~ Species,
              data = Spurs_field_only_species)

  #Very significant! At least 2 groups (Species in this case) differ significantly from each other.

  #Which groups though? Let's have a look.
  #We need a new test for this: the Dunn test
  #For this test we need a new package "FSA"
  #install.packages("FSA")
  library(FSA)

  #Now let's do the Dunn test
  Phocdunn <- dunnTest(Spur_length ~ Species,
          data = Spurs_field_only_species,
          method = "holm")
  #This is giving us way too much output because we're comparing every species with every other species.

  Phocdunns <- Phocdunn$res

  #We could use a Compact letter display (CLD)
  #We can use the rcompanion package for this
  #install.packages("rcompanion")
  library(rcompanion)

  cld <- cldList(comparison = Phocdunns$Comparison,
                p.value    = Phocdunns$P.adj,
                threshold  = 0.05)[1:2]

  names(cld)[1]<-"Species" # change the name of grouping factor according to the dataset (df)
  
  #install.packages("tidyverse")
  library(tidyverse)
  
  
  #We might want the labels to be closer to the data points.
  #We could grab the max length measurement of each species and use these.
  max_len_group<- Spurs_field_only_species %>% group_by(Species) %>% summarise(max_spur_length = max(Spur_length, na.rm = TRUE))
  group_label <- left_join(cld, max_len_group, by = "Species") 
  
#windows()

#My version
ggplot(data=Spurs_field_only_species, aes(x=Species, y=Spur_length,col=Species))+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.5), size = 0.5)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_text(data = cld, aes(label = Letter, y = 6, x =Species), 
            position = position_jitterdodge(jitter.width=0, jitter.height = 0),
            #jitter here allows for the labels not to overlap, there is however
            #no control over how they are placed. So you just need to pick the
            #right iteration.
            vjust = -0.5,
            hjust= 0.5,
            fontface = "bold",
            size=3.5,
            check_overlap = F)+
  theme(legend.position = "none")+
  ylim(0, NA)

#The main problem of this graph is the overlap in the cld labels

#Jasper's version
  ggplot(data=Spurs_field_only_species, aes(x=Species, y=Spur_length,col=Species))+
    geom_jitter(position = position_jitterdodge(jitter.width = 0.3), size = 0.3)+
#   geom_violin()+#position = position_jitterdodge(jitter.width = 0.5), size = 0.5)+    theme_bw()+
    geom_text(data = group_label, aes(label = Letter, y =  max_spur_length, x =Species),#, colour = "black"), 
              vjust = 0,
              hjust = -0.75,
              fontface = "bold",
              size=3.5,
              check_overlap = F,
              angle = 90
              #position = position_dodge(width = 0.9)
              )+
    theme_bw()+
    theme(legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust=1))+
    scale_y_continuous(limits= c(0, 7))
  
  #Distance to the labels appears to be an extra millimeter per extra letter, simple addition.
  #We don't want this, we would like the distance to be equal no matter the amount of letters
  #Fonts and points need to be somewhat bigger as well
  #Remove colour of cld labels?
  #This fixes it

  #Now we want a table with species on the y-axis and letter on the x-axis.
  }

##### Produce a table of observation counts per species.
{#We need this to determine if it is (statistically) relevant to include all species.

  {#dplyr has a lot of useful tools
  #install.packages("dplyr")
  library(dplyr)

  #This creates a table with dplyr, but how do I use it/save it
  d <- Spurs_field_only_species %>%
    group_by(Species, floor(Location)) %>%
    summarise(count = n())
  
  #Why is this here again?
  #filter_all(Freq_Loc_Spec, any_vars(. != 0))

  #This creates a table in the command line
  Obs_per_species <- table(Spurs_field_only_species$Species)

  #A table is like a data frame but it only has a dimension for the data
  dim(Obs_per_species) # This should return a single dimension with 31 length (species count)
  #Names for this dimension can be accessed with dimnames()
  dimnames(Obs_per_species) #This should return an empty name for the species dimension [[1]]

  #We can change this empty name using the names() function
  names(dimnames(Obs_per_species)) <- c("Species")}

  #####stats method
    {#We can also use a different package called stats
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
  }
  #This is still not ideal for checking presence and absence.
  
  #We could make a matrix/table with species on the y-axis and locations on the x-axis.
  {table(Spurs_field_only_species$Species, floor(Spurs_field_only_species$Location))
  #This outputs into the commandline
}
  
  #####dplyr method
  {#Now lets try it with dplyr
  {freq_table <- Spurs_field_only_species %>%
      count(Species, Location) %>%      # count observations per species-location pair
    pivot_wider(names_from = Location, values_from = n, values_fill = 0)
  #This way we get all the sub locations, which we don't want.

  #Let's try it with the floor of the location.
  freq_table <- Spurs_field_only_species %>%
    count(Species, floor(Location)) %>%      # count observations per species-location pair
    pivot_wider(names_from = floor(Location), values_from = n, values_fill = 0)
  #This does not work unfortunately
}
  #I have two options left after the current location column by taking the floor of all the values,
  #or add a new column with this floor value.
  #The second option seems better, so lets do that.
  Spurs_field_only_species$floor_location <- floor(Spurs_field_only_species$Location)

  freq_table <- Spurs_field_only_species %>%
    count(Species, floor_location) %>%      # count observations per species-location pair
    pivot_wider(names_from = floor_location, values_from = n, values_fill = 0)

  View(freq_table)
  }
}


##### Location names intermezzo
{#So far we've been using the number codes for the locations.
#This works and is easier for the mathematics, but not very informative for a reader.
#So we should add the actual location names to the file.
#For legibility and visualization it is also useful to have a short abbreviation or code for each location.
#I made a code for each location using the first for letters of the location and capitalizing the first. 
#Two locations (i.e. Cape Point 10 & Cape Town 17) did not have unique starting symbols, so instead the first 2 letters of each word were used and capitalized (i.e. CaPo & CaTo).
#I made a new excel file with the location number, name and abbreviation.

#Lets load this new excel into R
#library(readxl)
#Jens_locations <- read_excel("C:/Users/wardk/OneDrive - Wageningen University & Research/Jens_locations.xlsx")

#Check if its loaded correctly
#View(Jens_locations)

#Do we append this data frame to the main data frame or do we use it as a separate table?
#Let's try appending first.

#Test
#merged_data_test <- Spurs_field_only_species %>%
#  left_join(Jens_locations, by = c("floor_location" = "Number"))

#Check if its appended correctly
#view(merged_data_test)


#Testing some simple boxplots with this
#boxplot(Spur_length ~ floor_location, data = merged_data_test, names = 'Name')
#boxplot(Spur_length ~ Name, data = merged_data_test)
#boxplot(Spur_length ~ Code, data = merged_data_test)

#This works so let's do it to the main set
Spurs_field_only_species <- Spurs_field_only_species %>%
  left_join(Jens_locations, by = c("floor_location" = "Number"))

}

##### Creating a CLD table
{
#library(tidyverse)

# Suppose cld has columns: Species, Letter (e.g. "a", "ab", "b", etc.)
cld_long <- cld %>%
  mutate(Letter = strsplit(as.character(Letter), "")) %>% # split letters into list
  unnest(Letter) # one row per letter per species
# This is now a table (tibble) with 2 columns and 132 rows.

# To improve legibility we want transform this table into a "square" shape.
cld_spread <- cld_long %>%
  mutate(value = TRUE)  %>% 
  pivot_wider(names_from = Letter, values_from = value, values_fill = FALSE)

#View(cld_spread)

#Old
{matches_df <-data.frame()
for(species_i in unique(cld_long$Species)[2]){
  uniq_letters <- cld_long %>% filter(Species == species_i) %>% pull(Letter) %>% unique()
  for(letter_i in uniq_letters){
    matches <- cld_long %>% filter(Letter == letter_i) %>% pull(Species) %>% unique()
    matches_df <- rbind(matches_df, data.frame(Species = species_i, Letter = letter_i, Matches = paste(matches, collapse = ", ")))
  }
}  

matches_df
}

##### Pair selection
{
#
cld_letters <- unique(cld_long$Letter)

#Creating the data frame
Pairs_cld_loc <- matrix(nrow=0, ncol = 17)
Pairs_cld_loc <- as.data.frame(Pairs_cld_loc)
colnames(Pairs_cld_loc) <- c("Species_1", "Species_2", "Location_overlap", "Spec_1_ove_mean", "Spec_1_ove_SD", "Spec_1_ove_median", "Spec_2_ove_mean", "Spec_2_ove_SD", "Spec_2_ove_med", 'Locations_spec_1_nov', 'Spec_1_nov_mean', 'Spec_1_nov_SD', "Spec_1_nov_med", "Locations_spec_2_nov", 'Spec_2_nov_mean', 'Spec_2_nov_SD', 'Spec_2_nov_med') #create a data frame

#Different way 
Pairs_frame_names <- c("Species_1", "Species_2", "Location_overlap", "Spec_1_ove_mean", "Spec_1_ove_SD", "Spec_1_ove_median", "Spec_1_ove_count", "Spec_2_ove_mean", "Spec_2_ove_SD", "Spec_2_ove_med", 'Spec_2_ove_count', 'Locations_spec_1_nov', 'Spec_1_nov_mean', 'Spec_1_nov_SD', "Spec_1_nov_med", 'Spec_1_nov_count', "Locations_spec_2_nov", 'Spec_2_nov_mean', 'Spec_2_nov_SD', 'Spec_2_nov_med', 'Spec_2_nov_count')

abs <- as.data.frame(matrix(nrow = 0, ncol = length(Pairs_frame_names)))
colnames(abs) <- Pairs_frame_names


#Filling the data frame
#Old script, had a lot of errors
{
for(i in length(cld_letters)){ #cycle through each letter once
    spec_i <- cld_long[cld_long$Letter==cld_letters[i], #for each letter in the second column select all rows that match this letter and only give us the species names (1st column)
                       "Species"]
    spec_i <- as.vector(spec_i)
      if (length(spec_i)>1){ #only continue if we have more than one species
        for (j in length(spec_i)){ #for each species in the list of species with the same letter
          species_1 <- spec_i$Species[j] #select the first species to compare
            for (k in (length(spec_i)-j)){ #for each species minus the ones we have already selected
              species_2 <- spec_i$Species[k] #select the second species to compare with
                loc_vect <- freq_table[freq_table$Species==species_1,-1] #vector of the whole row of species_1
                loc_spec_1 <- colnames(loc_vect[which(loc_vect>0)]) #select the column names of all the columns with a value larger than 1
                loc_vect <- freq_table[freq_table$Species==species_2,-1] #vector of the whole row of species_2
                loc_spec_2 <- colnames(loc_vect[which(loc_vect>0)])
                if (any(loc_spec_1 %in% loc_spec_2)){ #check if there are overlapping locations
                  if (length(setdiff(loc_spec_1, loc_spec_2)) > 0){
                   if (length(setdiff(loc_spec_2, loc_spec_1)) > 0){
                  #if (any(!loc_spec_1 %in% loc_spec_2)){ #also check if there are locations without overlap by inverting the true/false of one species, this doesn't work because it gives a false TRUE statement
                    loc_overlap <- loc_spec_1[loc_spec_1 %in% loc_spec_2]
                      for(l in length(loc_overlap)){
                        Temporary_data_frame <- matrix(nrow=1, ncol = 17) #Create a temporary data frame with a single row
                        #We can fill our data into this temporary data frame and then append it to the main one
                        Temporary_data_frame <- as.data.frame(Temporary_data_frame)
                        colnames(Temporary_data_frame) <- c("Species_1", "Species_2", "Location_overlap", "Spec_1_ove_mean", "Spec_1_ove_SD", "Spec_1_ove_median", "Spec_2_ove_mean", "Spec_2_ove_SD", "Spec_2_ove_median", 'Locations_spec_1_nov', 'Spec_1_nov_mean', 'Spec_1_nov_SD', "Spec_1_nov_median", "Locations_spec_2_nov", 'Spec_2_nov_mean', 'Spec_2_nov_SD', 'Spec_2_nov_median') 
                        Temporary_data_frame[1,]$Species_1 <- species_1 #Write the species name to the correct field
                        Temporary_data_frame[1,]$Species_2 <- species_2 
                        Temporary_data_frame[1,]$Location_overlap <- loc_overlap[l] #Take the current location 'l' and write it to the correct field
                          mean_1 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_1,]
                          #select the rows of species 1
                          mean_1 <- mean_1[mean_1$floor_location==loc_overlap[l],]
                          #select the rows corresponding to the overlapping location
                          mean_1 <- mean_1[,mean_1$Spur_length]
                          #select the spur length column
                        Temporary_data_frame[1,]$Spec_1_ove_mean <- mean(mean_1)
                        #Calculate the mean tube length for species 1 at the overlapping location
                        Temporary_data_frame[1,]$Spec_1_ove_SD <- sd(mean_1)
                        #Calculate the standard deviation of the tube length for species 1 at the overlapping location
                        Temporary_data_frame[1,]$Spec_1_ove_median <- median(mean_1)
                        #Calculate the median tube length for species 1 at the overlapping location
                          mean_2 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_2,]
                          #select the rows of species 2
                          mean_2 <- mean_2[mean_2$floor_location==loc_overlap[l],]
                          #select the rows corresponding to the overlapping location
                          mean_2 <- mean_2[,mean_2$Spur_length]
                          #select the spur length column
                        Temporary_data_frame[1,]$Spec_2_ove_mean <- mean(mean_2)
                        Temporary_data_frame[1,]$Spec_2_ove_SD <- sd(mean_2)
                        Temporary_data_frame[1,]$Spec_2_ove_median <- median(mean_2)
                         loc_nov_1 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_1,]
                         #Select the rows of species 1
                         loc_nov_1 <- unique(loc_nov_1$floor_location)
                         #Select only the unique locations
                         loc_nov_1 <- loc_nov_1[!loc_nov_1 %in% loc_overlap]
                         #Select the rows of the non overlapping locations
                        Temporary_data_frame[1,]$Locations_spec_1_nov <- paste(loc_nov_1, collapse = ",")
                        #Write the location(s) to the correct field
                          mean_nov_1 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_1,]
                          #select the rows of species 1
                          mean_nov_1 <- mean__nov_1[mean_nov_1$floor_location %in% loc_nov_1,]
                          #select the rows corresponding to the non-overlapping location(s)
                          mean_nov_1 <- mean_nov_1[,mean_nov_1$Spur_length]
                          #select the spur length column
                        Temporary_data_frame[1,]$Spec_1_nov_mean <- mean(mean_nov_1)
                        Temporary_data_frame[1,]$Spec_1_nov_SD <- sd(mean_nov_1)
                        Temporary_data_frame[1,]$Spec_1_nov_median <- median(mean_nov_1)
                          loc_nov_2 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_2,]
                          loc_nov_2 <- unique(loc_nov_2$floor_location)
                          loc_nov_2 <- loc_nov_2[!loc_nov_2 %in% loc_overlap]
                        Temporary_data_frame[1,]$Locations_spec_2_nov <- paste(loc_nov_2, collapse = ",")
                          mean_nov_2 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_2,]
                          mean_nov_2 <- mean__nov_2[mean_nov_2$floor_location %in% loc_nov_2,]
                          mean_nov_2 <- mean_nov_2[,mean_nov_2$Spur_length]
                        Temporary_data_frame[1,]$Spec_2_nov_mean <- mean(mean_nov_2)
                        Temporary_data_frame[1,]$Spec_2_nov_SD <- sd(mean_nov_2)
                        Temporary_data_frame[1,]$Spec_2_nov_median <- median(mean_nov_2)
                      #Pairs_cld_loc <-  bind_rows(Pairs_cld_loc, Temporary_data_frame)  
                      }
                   }
                        }
                      }
                } 
            }
        }
      }

}
#test, redundant now  
#for(m in length(Spurs_field_only_species)){
#  mean_1 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_1,]
#  mean_1 <- mean_1[mean_1$floor_location==loc_overlap[l],]
#  mean_1 <- mean_1[,mean_1$Spur_length]
#  mean_1 <- mean(mean_1)
#}


#Second try
#This one does seem to work
for(i in 1:length(cld_letters)){ #cycle through each letter once
  spec_i <- cld_long[cld_long$Letter==cld_letters[i], "Species"] #for each letter in the second column select all rows that match this letter and only give us the species names (1st column)
  spec_i <- as.vector(spec_i)
  if (length(spec_i$Species)>1){
    for (j in 1:(length(spec_i$Species)-1)){ #for each species in the list of species with the same letter
      species_1 <- spec_i$Species[j]
      for (k in (1+j):length(spec_i$Species)){ #for each species minus the ones we have already selected
        species_2 <- spec_i$Species[k]
        if (!is.na(species_2)){
          loc_vect <- freq_table[freq_table$Species==species_1,-1] #vector of the whole row of species_1
          loc_spec_1 <- colnames(loc_vect[which(loc_vect>0)]) #select the column names of all the columns with a value larger than 1
          loc_vect <- freq_table[freq_table$Species==species_2,-1] #vector of the whole row of species_2
          loc_spec_2 <- colnames(loc_vect[which(loc_vect>0)])
          if (any(loc_spec_1 %in% loc_spec_2)){
            if (length(setdiff(loc_spec_1, loc_spec_2)) > 0){
              if (length(setdiff(loc_spec_2, loc_spec_1)) > 0){
                loc_overlap <- loc_spec_1[loc_spec_1 %in% loc_spec_2]
                for(l in 1:length(loc_overlap)){
                  Temporary_data_frame <- matrix(nrow=1, ncol = 17) #Create a temporary data frame with a single row
                  #We can fill our data into this temporary data frame and then append it to the main one
                  Temporary_data_frame <- as.data.frame(Temporary_data_frame)
                  colnames(Temporary_data_frame) <- c("Species_1", "Species_2", "Location_overlap", "Spec_1_ove_mean", "Spec_1_ove_SD", "Spec_1_ove_median", "Spec_2_ove_mean", "Spec_2_ove_SD", "Spec_2_ove_median", 'Locations_spec_1_nov', 'Spec_1_nov_mean', 'Spec_1_nov_SD', "Spec_1_nov_median", "Locations_spec_2_nov", 'Spec_2_nov_mean', 'Spec_2_nov_SD', 'Spec_2_nov_median') 
                  Temporary_data_frame[1,]$Species_1 <- species_1 #Write the species name to the correct field
                  Temporary_data_frame[1,]$Species_2 <- species_2 
                  Temporary_data_frame[1,]$Location_overlap <- loc_overlap[l] #Take the current location 'l' and write it to the correct field
                  mean_1 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_1,]
                  #select the rows of species 1
                  mean_1 <- mean_1[mean_1$floor_location==loc_overlap[l],]
                  #select the rows corresponding to the overlapping location
                  mean_1 <- mean_1$Spur_length
                  #select the spur length column
                  Temporary_data_frame[1,]$Spec_1_ove_mean <- mean(mean_1)
                  #Calculate the mean tube length for species 1 at the overlapping location
                  Temporary_data_frame[1,]$Spec_1_ove_SD <- sd(mean_1)
                  #Calculate the standard deviation of the tube length for species 1 at the overlapping location
                  Temporary_data_frame[1,]$Spec_1_ove_median <- median(mean_1)
                  #Calculate the median tube length for species 1 at the overlapping location
                  mean_2 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_2,]
                  #select the rows of species 2
                  mean_2 <- mean_2[mean_2$floor_location==loc_overlap[l],]
                  #select the rows corresponding to the overlapping location
                  mean_2 <- mean_2$Spur_length
                  #select the spur length column
                  Temporary_data_frame[1,]$Spec_2_ove_mean <- mean(mean_2)
                  Temporary_data_frame[1,]$Spec_2_ove_SD <- sd(mean_2)
                  Temporary_data_frame[1,]$Spec_2_ove_median <- median(mean_2)
                  loc_nov_1 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_1,]
                  #Select the rows of species 1
                  loc_nov_1 <- unique(loc_nov_1$floor_location)
                  #Select only the unique locations
                  loc_nov_1 <- loc_nov_1[!loc_nov_1 %in% loc_overlap]
                  #Select the rows of the non overlapping locations
                  Temporary_data_frame[1,]$Locations_spec_1_nov <- paste(loc_nov_1, collapse = ",")
                  #Write the location(s) to the correct field
                  mean_nov_1 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_1,]
                  #select the rows of species 1
                  mean_nov_1 <- mean_nov_1[mean_nov_1$floor_location %in% loc_nov_1,]
                  #select the rows corresponding to the non-overlapping location(s)
                  mean_nov_1 <- mean_nov_1$Spur_length
                  #select the spur length column
                  Temporary_data_frame[1,]$Spec_1_nov_mean <- mean(mean_nov_1)
                  Temporary_data_frame[1,]$Spec_1_nov_SD <- sd(mean_nov_1)
                  Temporary_data_frame[1,]$Spec_1_nov_median <- median(mean_nov_1)
                  loc_nov_2 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_2,]
                  loc_nov_2 <- unique(loc_nov_2$floor_location)
                  loc_nov_2 <- loc_nov_2[!loc_nov_2 %in% loc_overlap]
                  Temporary_data_frame[1,]$Locations_spec_2_nov <- paste(loc_nov_2, collapse = ",")
                  mean_nov_2 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_2,]
                  mean_nov_2 <- mean_nov_2[mean_nov_2$floor_location %in% loc_nov_2,]
                  mean_nov_2 <- mean_nov_2$Spur_length
                  Temporary_data_frame[1,]$Spec_2_nov_mean <- mean(mean_nov_2)
                  Temporary_data_frame[1,]$Spec_2_nov_SD <- sd(mean_nov_2)
                  Temporary_data_frame[1,]$Spec_2_nov_median <- median(mean_nov_2)
                  Pairs_cld_loc <-  rbind(Pairs_cld_loc, Temporary_data_frame)
                  #Adds the new row to the rest of the data set.
                  print(paste(species_1, species_2, loc_overlap[l], cld_letters[i]))
                  }
                
              }
            }
          }
        } 
      }
    }
  }
}

#Because species we cycle through every cld letter, species that share more than one letter will be added seperately for each letter.
#These are exact duplicates, so we would like to filter them out.
Pairs_cld_loc <- Pairs_cld_loc %>% distinct()


#Doing the same thing but without the cld loop, so just occurring together or not. This would be biologically very relevant.

}


##### Statistical analysis of the pairs

# We are interested in the  difference between species means when they occur together compared with the difference when they do not occur together.

### Normality
# We first need to check for the normality of the data.

#Shapiro-Wilk's test
shapiro.test(c(abs(Pairs_cld_loc$Spec_1_ove_mean - Pairs_cld_loc$Spec_2_ove_mean), abs(Pairs_cld_loc$Spec_1_nov_mean - Pairs_cld_loc$Spec_2_nov_mean)))

qqPlot(c(abs(Pairs_cld_loc$Spec_1_ove_mean - Pairs_cld_loc$Spec_2_ove_mean), abs(Pairs_cld_loc$Spec_1_nov_mean - Pairs_cld_loc$Spec_2_nov_mean)))

#The data seems to be normally distributed. Interesting.

#To analyse the difference in means we need to calculate these
#Make a data frame of the absolute differences
Pairs_abs_means <- tibble(abs(Pairs_cld_loc$Spec_1_ove_mean - Pairs_cld_loc$Spec_2_ove_mean), abs(Pairs_cld_loc$Spec_1_nov_mean - Pairs_cld_loc$Spec_2_nov_mean))

colnames(Pairs_abs_means) <- c("Sympatric_mean", "Allopatric_mean")

#Scatter plot difference sympatric vs allopatric tube lengths of pairs
ggplot(Pairs_abs_means) +
  geom_abline() +
  geom_point(aes(x= Allopatric_mean, y = Sympatric_mean))

#Paired t-test
t.test(x = Pairs_abs_means$Sympatric_mean, y =Pairs_abs_means$Allopatric_mean,
       alternative = c("greater"),
       mu=0,
       paired = F
)

#t = 0.91085, df = 25.638, p-value = 0.1854
#This isn't significant yet, but it shows promise.
#We haven't performed any corrections yet.
#The two main factors that should be corrected for are sample size and tube length.


#Weighted_pairs_means <- tibble(Pairs_cld_loc$Spec_1_ove_mean / (Pairs_cld_loc$Spec_1_ove_SD)^2, Pairs_cld_loc$Spec_2_ove_mean / (Pairs_cld_loc$Spec_2_ove_SD)^2)

#Let's first correct for sample size
Sample_weighted_pairs_means <- Pairs_abs_means*((Pairs_cld_loc$Spec_1_ove_count + Pairs_cld_loc$Spec_2_ove_count + Pairs_cld_loc$Spec_1_nov_count + Pairs_cld_loc$Spec_2_nov_count)/2625)

t.test(x = Sample_weighted_pairs_means$Sympatric_mean, y =Sample_weighted_pairs_means$Allopatric_mean,
       alternative = c("greater"),
       mu=0,
       paired = F
)

ggplot(Sample_weighted_pairs_means) +
  geom_abline() +
  geom_point(aes(x= Allopatric_mean, y = Sympatric_mean))

#This seems to improve test by correcting for low sample sizes and their potential effect on mean values.
#data:  Sample_weighted_pairs_means$Sympatric_mean and Sample_weighted_pairs_means$Allopatric_mean
#t = 1.3733, df = 17.678, p-value = 0.09341


Size_weighted_pairs_means <- Pairs_abs_means/medians_aver

t.test(x = Size_weighted_pairs_means$Sympatric_mean, y =Size_weighted_pairs_means$Allopatric_mean,
       alternative = c("greater"),
       mu=0,
       paired = F
)

ggplot(Size_weighted_pairs_means) +
  geom_abline() +
  geom_point(aes(x= Allopatric_mean, y = Sympatric_mean))


#

Combined_weighted_pairs_means <- Size_weighted_pairs_means * (Pairs_cld_loc$Spec_1_ove_count + Pairs_cld_loc$Spec_2_ove_count + Pairs_cld_loc$Spec_1_nov_count + Pairs_cld_loc$Spec_2_nov_count)/2625

t.test(x = Combined_weighted_pairs_means$Sympatric_mean, y =Combined_weighted_pairs_means$Allopatric_mean,
       alternative = c("greater"),
       mu=0,
       paired = F
)

ggplot(Combined_weighted_pairs_means) +
  geom_abline() +
  geom_point(aes(x= Allopatric_mean, y = Sympatric_mean))


#correcting for sample size

#correcting for size

#doing everything but skipping the cld step to get size matches

#Going through every couple to see what's happening

#####Comparing the observations of a single location with a random sampling of all observations.
#This would be a good bootstrapping (more like verification) method

#Creating a GLM to not only test if there's a an effect but also its size.
#We can also test multiple effects at the same time




