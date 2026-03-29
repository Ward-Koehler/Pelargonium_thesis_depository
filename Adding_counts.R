Pairs_frame_names <- c("Species_1", "Species_2", "Location_overlap", "Spec_1_ove_mean", "Spec_1_ove_SD", "Spec_1_ove_median", "Spec_1_ove_count", "Spec_2_ove_mean", "Spec_2_ove_SD", "Spec_2_ove_median", 'Spec_2_ove_count', 'Locations_spec_1_nov', 'Spec_1_nov_mean', 'Spec_1_nov_SD', "Spec_1_nov_median", 'Spec_1_nov_count', "Locations_spec_2_nov", 'Spec_2_nov_mean', 'Spec_2_nov_SD', 'Spec_2_nov_median', 'Spec_2_nov_count')

abs <- as.data.frame(matrix(nrow = 0, ncol = length(Pairs_frame_names)))
colnames(abs) <- Pairs_frame_names

Pairs_cld_loc <- as.data.frame(matrix(nrow = 0, ncol = length(Pairs_frame_names)))
colnames(Pairs_cld_loc) <- Pairs_frame_names


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

abs <- abs %>% distinct()

Pairs_cld_loc <- Pairs_cld_loc %>% distinct()

sum(abs$Spec_1_ove_count + abs$Spec_2_ove_count + abs$Spec_1_nov_count + abs$Spec_2_nov_count)

(abs$Spec_1_ove_count + abs$Spec_2_ove_count + abs$Spec_1_nov_count + abs$Spec_2_nov_count)/2625


#Intermezzo
medians <- tibble(Pairs_cld_loc$Spec_1_ove_median, Pairs_cld_loc$Spec_2_ove_median, Pairs_cld_loc$Spec_1_nov_median, Pairs_cld_loc$Spec_2_nov_median)

colnames(medians) <- c('Spec_1_ove_median','Spec_2_ove_median', 'Spec_1_nov_median', 'Spec_2_nov_median')

medians_diff <- tibble(abs(medians$Spec_1_ove_median - medians$Spec_1_nov_median), abs(medians$Spec_2_ove_median - medians$Spec_2_nov_median))

colnames(medians_diff) <- c('Spec_1_abs_median_diff', 'Spec_2_abs_median_diff')

medians_diff_relative <- tibble(.rows = 14, .name_repair = c('spec1', 'spec2'))
colnames(medians_diff_relative) <- c('Spec_1_rel_median_diff', 'Spec_2_rel_median_diff')

medians_diff_relative <- tibble(abs(Pairs_cld_loc$Spec_1_ove_median - Pairs_cld_loc$Spec_2_ove_median)/mean(medians$Spec_1_ove_median+ medians$Spec_1_nov_median), abs(Pairs_cld_loc$Spec_1_ove_median - Pairs_cld_loc$Spec_2_ove_median)/mean(medians$Spec_2_ove_median+ medians$Spec_2_nov_median), .name_repair = c('Spec_1_rel_median_diff', 'Spec_2_rel_median_diff'))

vect1 <- abs(Pairs_cld_loc$Spec_1_ove_median - Pairs_cld_loc$Spec_2_ove_median)/mean(medians$Spec_1_ove_median+ medians$Spec_1_nov_median)

vect2 <- abs(Pairs_cld_loc$Spec_1_ove_median - Pairs_cld_loc$Spec_2_ove_median)/mean(medians$Spec_2_ove_median+ medians$Spec_2_nov_median)

medians_diff_relative <- tibble(vect1, vect2)


vect1 <- (Pairs_cld_loc$Spec_1_ove_median+Pairs_cld_loc$Spec_2_ove_median)/2
vect2 <- (Pairs_cld_loc$Spec_1_nov_median+Pairs_cld_loc$Spec_2_nov_median)/2
medians_aver <- tibble(vect1, vect2)


abs((Pairs_cld_loc$Spec_1_ove_median + Pairs_cld_loc$Spec_2_ove_median)/2 - (Pairs_cld_loc$Spec_1_nov_median + Pairs_cld_loc$Spec_2_nov_median)/2) / ((Pairs_cld_loc$Spec_1_ove_median + Pairs_cld_loc$Spec_2_ove_median + Pairs_cld_loc$Spec_1_nov_median + Pairs_cld_loc$Spec_2_nov_median)/4) 
