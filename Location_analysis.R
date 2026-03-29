Location_groups <- matrix(nrow = 0, ncol = 7)
Location_groups <- as.data.frame(Location_groups)
colnames(Location_groups) <- c("Loc_nr", "Loc_name", "Species", "CLD", "mean", "median", "count")

length(unique(Spurs_field_only_species[Spurs_field_only_species$floor_location==location_nr,]$Species))

for(i in 1:dim(Jens_locations)[1]) {
  location <- as.character(Jens_locations[i,2])
  location_nr <- as.numeric(Jens_locations[i,1])
  print(paste("location nr=",location_nr))
  species <- unique(Spurs_field_only_species[Spurs_field_only_species$floor_location==location_nr,]$Species)
  print(species)
    for(j in 1:length(species)) {
      species_1 <- species[j]
      print(species_1)
      Temporary_data_frame <- matrix(nrow=1, ncol = 7) #Create a temporary data frame with a single row
      #We can fill our data into this temporary data frame and then append it to the main one
      Temporary_data_frame <- as.data.frame(Temporary_data_frame)
      colnames(Temporary_data_frame) <- c("Loc_nr", "Loc_name", "Species", "CLD", "mean", "median", "count")
      Temporary_data_frame$Loc_nr <- location_nr 
      Temporary_data_frame[1,]$Loc_name <- location #Take the current location and write it to the correct field
      Temporary_data_frame[1,]$Species <- species_1 #Write the species name to the correct field
      Temporary_data_frame[1,]$CLD <- cld[cld$Species==species_1,2]
      mean_1 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_1,]
      #select the rows of species 1
      mean_1 <- mean_1[mean_1$floor_location==location_nr,]
      #select the rows corresponding to the overlapping location
      mean_1 <- mean_1$Spur_length
      #select the spur length column
      Temporary_data_frame[1,]$mean <- mean(mean_1)
      #Calculate the mean tube length for species 1 at the overlapping location
      Temporary_data_frame[1,]$median <- median(mean_1)
      #Calculate the median tube length for species 1 at the overlapping location
      Temporary_data_frame[1,]$count <- length(mean_1)
      print(Temporary_data_frame)
      #Write the observation count at the overlapping location
      Location_groups <-  rbind(Location_groups, Temporary_data_frame)
      
    }
  }      
      


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
                  Temporary_data_frame <- matrix(nrow=1, ncol = 21) #Create a temporary data frame with a single row
                  #We can fill our data into this temporary data frame and then append it to the main one
                  Temporary_data_frame <- as.data.frame(Temporary_data_frame)
                  colnames(Temporary_data_frame) <- Pairs_frame_names
                  #colnames(Temporary_data_frame) <- c("Species_1", "Species_2", "Location_overlap", "Spec_1_ove_mean", "Spec_1_ove_SD", "Spec_1_ove_median", "Spec_1_ove_count", "Spec_2_ove_mean", "Spec_2_ove_SD", "Spec_2_ove_med", 'Spec_2_ove_count', 'Locations_spec_1_nov', 'Spec_1_nov_mean', 'Spec_1_nov_SD', "Spec_1_nov_med", 'Spec_1_nov_count', "Locations_spec_2_nov", 'Spec_2_nov_mean', 'Spec_2_nov_SD', 'Spec_2_nov_med', 'Spec_2_nov_count') 
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
                  Temporary_data_frame[1,]$Spec_1_ove_count <- length(mean_1)
                  #Write the observation count at the overlapping location
                  mean_2 <- Spurs_field_only_species[Spurs_field_only_species$Species==species_2,]
                  #select the rows of species 2
                  mean_2 <- mean_2[mean_2$floor_location==loc_overlap[l],]
                  #select the rows corresponding to the overlapping location
                  mean_2 <- mean_2$Spur_length
                  #select the spur length column
                  Temporary_data_frame[1,]$Spec_2_ove_mean <- mean(mean_2)
                  Temporary_data_frame[1,]$Spec_2_ove_SD <- sd(mean_2)
                  Temporary_data_frame[1,]$Spec_2_ove_median <- median(mean_2)
                  Temporary_data_frame[1,]$Spec_2_ove_count <- length(mean_2)
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
                  Temporary_data_frame[1,]$Spec_1_nov_count <- length(mean_nov_1)
                  #Write the sum of all observations minus the ones at the current overlapping location
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
                  Temporary_data_frame[1,]$Spec_2_nov_count <- length(mean_nov_2)
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