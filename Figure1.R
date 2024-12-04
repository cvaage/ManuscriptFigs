#Manuscript Figures: Plots 1.1 and 1.2
#Written by C Vaage on 2 December 2024

setwd("/Volumes/LaCie/HexSim Results")

library(sf)
library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(ggspatial)
library(patchwork)

####Read in data ---------------------------------------------------------------
#read simple features or layers from file or database
net <- st_read("/Volumes/LaCie/HexSim Results/HexSimNetwork/network3.26.shp",layer="network3.26")
net <- net[,-c(10:22)] #remove unnecessary columns

#read in HexSim report (ex. individuals per segment)
bass <- read.csv("BC2/BC2_REPORT_indivspersegment_Bass.csv",h=T)
cray <- read.csv("BC2/BC2_REPORT_indivspersegment_Crayfish.csv",h=T)

#reorganize file for bass
bass <- data.frame(t(bass[,-ncol(bass)])) #turn table and remove extra column with NA
bass <- bass[-1,] #remove top row with iteration number
bass$RID <- rownames(bass) #create column with ID
bass$RID <- gsub("Seg.","",bass$RID) #format ID - run through, skip down to line 37
bass_net <- merge(net, bass, by = "RID", all = TRUE) #create table with ID and predictions
bass_net[is.na(bass_net)] <- 0 #replace NA values with 0

#reorganize file for crayfish
cray <- data.frame(t(cray[,-ncol(cray)])) #turn table and remove extra column with NA
cray <- cray[-1,] #remove top row with iteration number
cray$RID <- rownames(cray) #create column with ID
cray$RID <- gsub("Seg.","",cray$RID) #format ID - run through, skip down to line 37
cray_net <- merge(net, cray, by = "RID", all = TRUE) #create table with ID and predictions
cray_net[is.na(cray_net)] <- 0 #replace NA values with 0

##### Map overlap of species occurrence 
full_sequence <- as.character(0:2216) #identify missing values in sequence (1-2217)
reaches <- c(0:2216) #for adding back to cleaned df #necessary to have as integers

bass_allreaches <- data.frame(RID = full_sequence) #Create a data frame with the full sequence
bass_complete <- merge(bass_allreaches, bass, by = "RID", all = TRUE)
bass_complete$RID <- as.numeric(bass_complete$RID)
bass_complete <- bass_complete %>%
  arrange(RID)
bass_complete[is.na(bass_complete)] <- 0 #change NAs to 0s

cray_allreaches <- data.frame(RID = full_sequence) #repeat for crayfish
cray_complete <- merge(cray_allreaches, cray, by = "RID", all = TRUE)
cray_complete$RID <- as.numeric(cray_complete$RID)
cray_complete <- cray_complete %>%
  arrange(RID)
cray_complete[is.na(cray_complete)] <- 0

#change values in bass to 1, crayfish to 2
bass_complete[bass_complete > 0] <- 1
cray_complete[cray_complete > 0] <- 2
bass_complete$RID <- full_sequence
cray_complete$RID <- full_sequence
bass_complete <- bass_complete[,-252] #remove time step 251 for bass, only applicable for BC2 run results

#combine data frames to get values of 1 = only bass, 2 = only cray, 3 = both, 0 = neither
combined_complete <- bass_complete[,c(2:251)] + cray_complete[,c(2:251)]
max(combined_complete) #Check that max value is 3 (co-occupancy)
combined_complete$RID <- reaches #add reaches ID back in
combined_complete_long <- combined_complete %>%
  pivot_longer(cols = starts_with("X"),  # Specify columns to pivot
               names_to = "Date",            # Name of the new column for the old column names
               names_prefix = "X",       # Prefix to remove from names (optional)
               values_to = "Occupancy")          # Name of the new column for the values

combined_char <- combined_complete_long %>%
  mutate(Category = case_when(
    Occupancy == 0 ~ "Unoccupied",
    Occupancy == 1 ~ "Bass Present",
    Occupancy == 2 ~ "Crayfish Present",
    Occupancy == 3 ~ "Co-Occupied"
  ))

combined_char <- combined_char[,-3] #remove Occupancy column
combined_char_wide <- combined_char %>%
  pivot_wider(names_from = Date,     # Column to become new columns
              values_from = Category)    # Column to fill the new columns

combined_char_wide <- combined_char_wide[,-1] #remove RID column
dates <- seq(as.Date('1999-05-01'), by = "month", length.out = 250)
colnames(combined_char_wide) <- dates #replace column headers, steps with dates
combined_char_wide$RID <- reaches #add reaches ID back in

occupancyplot1 <- combined_char_wide[,c("2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "RID")]
occupancyplot1 <- net %>%
  left_join(occupancyplot1, by = "RID")
occupancyplot2 <- combined_char_wide[,c("2020-01-01", "RID")]
occupancyplot2 <- net %>%
  left_join(occupancyplot2, by = "RID")

occupancy1long <- occupancyplot1 %>%
  pivot_longer(cols = c("2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01"), # Specify columns to pivot
               names_to = "Date",            # Name of the new column for the old column names
               values_to = "Occupancy")          # Name of the new column for the values
occupancy2long <- occupancyplot2 %>%
  pivot_longer(cols = c("2020-01-01"), # Specify columns to pivot
               names_to = "Date",            # Name of the new column for the old column names
               values_to = "Occupancy")          # Name of the new column for the values

#Plotting ----------------------------------------------------------------------
custom_colors <- c("Unoccupied" = "lightblue", "Bass Present" = "darkgreen", "Crayfish Present" = "darkorange", "Co-Occupied" = "darkred")

p1 <- ggplot() +
  geom_sf() +
  geom_sf(data = occupancy1long, 
          aes(color = Occupancy)) +
  facet_wrap(~ Date, labeller = labeller(Date = c("2000-01-01" = "2000", 
                                                  "2005-01-01" = "2005", 
                                                  "2010-01-01" = "2010",
                                                  "2015-01-01" = "2015"))) +
  scale_color_manual(values = custom_colors) +
  labs(title = 'Non-Native Species Presence', subtitle = 'Reach-Level Occupancy') + 
  theme(plot.title = element_text(size = 18, color = "grey20"),
        plot.subtitle = element_text(size = 15, color = "grey20")) +
  theme(strip.text = element_text(size = 18, color = "grey40", face = "bold", hjust = 0.9, vjust = 0.5),
        strip.background = element_blank()) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = "none") +
  theme(panel.background = element_blank())
p1

# Create a specific point (longitude, latitude) to add text in correct position
point_data <- data.frame(id = 1, lon = -118.5, lat = 45.9)

# Convert to an sf object with a specific point
point_sf <- st_as_sf(point_data, coords = c("lon", "lat"), crs = "NAD83")

p2 <- ggplot() +
  geom_sf(data = occupancy2long, 
          aes(color = Occupancy)) +
  scale_color_manual(values = custom_colors) +
  labs(x = 'Longitude', y = 'Latitude') +
  theme(axis.title.x = element_text(color = "grey60"),
        axis.title.y = element_text(color = "grey60"),
        axis.text = element_text(color = "grey60"),
        text = element_text(size = 16)) +
  theme(legend.position = c(0.5, 0.85),
        legend.text = element_text(size = 12),  # Legend text size
        legend.title = element_blank(),  # Legend title size
        legend.key.width = unit(1, "cm"), # Legend key size
        legend.key.height = unit(1, "cm")) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank()) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(0.5, "in"), width = unit(0.5, "in"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2) +
  geom_sf(data = point_sf, color = "white", size = 4) +  # Plot the point
  geom_sf_text(data = point_sf, aes(label = "2020"), size = 18, color = "grey40", fontface = "bold") # Add text label
p2

plot1 <- p1 + p2
plot1

ggsave("VaageC_Plot1.pdf", plot = plot1, device="pdf", dpi = 300, width = 16, height = 10) #screen width/height
