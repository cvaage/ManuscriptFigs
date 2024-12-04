#Manuscript Figures: Plots 2.1, 2.2, 2.3
#Written by C Vaage on 3 December 2024

#####PLOT 2.1-------------------------------------------------------------------
setwd("/Volumes/LaCie/HexSim Results")

library(sf)
library(ggplot2)
library(gganimate)
library(dplyr)
library(tidyr)
library(ggspatial)
library(patchwork)

#Read in data-------------------------------------------------------------------
#read simple features or layers from file or database
net <- st_read("/Volumes/LaCie/HexSim Results/HexSimNetwork/network3.26.shp",layer="network3.26")
net <- net[,-c(10:22)] #remove unnecessary columns

#read in HexSim report for solo and combo run
cray1 <- read_csv("CrayfishSolo7/CrayfishSolo7_REPORT_indivspersegment_Crayfish.csv")
cray2 <- read_csv("BC2/BC2_REPORT_indivspersegment_Crayfish.csv")

dates <- seq(as.Date('1999-05-01'), by = "month", length.out = 242) #define date range

#reorganize file for crayfish solo
cray1 <- data.frame(t(cray1[,-ncol(cray1)])) #turn table and remove extra column with NA
cray1 <- cray1[-1,] #remove top row with iteration number
colnames(cray1) <- dates #replace column headers, steps with dates
cray1[cray1 > 0] <- 1 #change all counts greater than 0 to 1 for presence
cray1$RID <- rownames(cray1) #create column with ID
cray1$RID <- gsub("Seg.","",cray1$RID) #format ID 

#reorganize file for crayfish combo
cray2 <- data.frame(t(cray2[,-ncol(cray2)])) #turn table and remove extra column with NA
cray2 <- cray2[-1,] #remove top row with iteration number
colnames(cray2) <- dates #replace column headers, steps with dates
cray2[cray2 > 0] <- 1 #change all counts greater than 0 to 1 for presence
cray2$RID <- rownames(cray2) #create column with ID
cray2$RID <- gsub("Seg.","",cray2$RID) #format ID
cray2 <- cray2[,-c(243:250)] #remove columns to match length of cray1

#Plotting Prep------------------------------------------------------------------
#identify missing values in sequence (1-2217)
full_sequence <- as.character(0:2216) #character sequence
reaches <- 0:2216 #integer sequence

cray1_all <- data.frame(RID = full_sequence) #repeat for crayfish
cray1_complete <- merge(cray1_all, cray1, by = "RID", all = TRUE)
cray1_complete$RID <- as.numeric(cray1_complete$RID)
cray1_complete <- cray1_complete %>%
  arrange(RID)
cray1_complete[is.na(cray1_complete)] <- 0

cray2_all <- data.frame(RID = full_sequence) #repeat for crayfish
cray2_complete <- merge(cray2_all, cray2, by = "RID", all = TRUE)
cray2_complete$RID <- as.numeric(cray2_complete$RID)
cray2_complete <- cray2_complete %>%
  arrange(RID)
cray2_complete[is.na(cray2_complete)] <- 0

#change values in solo cray to 1, combo cray to 2
cray1_complete <- cray1_complete[,-1]
cray2_complete <- cray2_complete[,-1]
cray1_complete[cray1_complete > 0] <- 1
cray2_complete[cray2_complete > 0] <- 2

combined_complete <- cray1_complete + cray2_complete
max(combined_complete)
combined_complete$RID <- reaches #add reaches ID back in
combined_complete_long <- combined_complete %>%
  pivot_longer(cols = c(1:242),  # Specify columns to pivot
               names_to = "Date",            # Name of the new column for the old column names
               values_to = "Occupancy")          # Name of the new column for the values

combined_char <- combined_complete_long %>%
  mutate(Category = case_when(
    Occupancy == 0 ~ "Unoccupied",
    Occupancy == 1 ~ "Crayfish.Solo",
    Occupancy == 2 ~ "Crayfish.Combo",
    Occupancy == 3 ~ "Model.Match"
  ))

combined_char <- combined_char[,-3] #remove Occupancy column
combined_char_wide <- combined_char %>%
  pivot_wider(
    names_from = Date,     # Column to become new columns
    values_from = Category    # Column to fill the new columns
  )
combined_char_wide <- combined_char_wide[,c("2019-06-01", "RID")]

#merge with sf object
occupancy <- net %>%
  left_join(combined_char_wide, by = "RID")
occupancy <- occupancy %>% 
  pivot_longer(cols = c("2019-06-01"), # Specify columns to pivot
               names_to = "Date",            # Name of the new column for the old column names
               values_to = "Occupancy")

#Plot---------------------------------------------------------------------------
custom_colors2 <- c("Unoccupied" = "lightblue", "Crayfish.Solo" = "rosybrown1",
                    "Crayfish.Combo" = "tomato4", "Model.Match" = "salmon2")

p1.3 <- ggplot() +
  geom_sf() +
  geom_sf(data = occupancy, 
          aes(color = Occupancy)) +
  scale_color_manual(values = custom_colors2) +
  labs(x = 'Longitude', y = 'Latitude',
       title = 'Rusty Crayfish Occupancy by Reach', subtitle = '2020-01-01') +
  theme(axis.title.x = element_text(color = "grey60"),
        axis.title.y = element_text(color = "grey60"),
        axis.text = element_text(color = "grey60"),
        text = element_text(size = 16)) +
  theme(legend.position = c(0.84, 0.85),
        legend.text = element_text(size = 12),  # Legend text size
        legend.title = element_blank(),  # Legend title size
        legend.key.size = unit(0.75, "cm")) + # Legend key size
  theme(panel.background = element_blank(),
        axis.ticks = element_blank()) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         height = unit(0.5, "in"), width = unit(0.5, "in"),
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.2)
p1.3
ggsave("/Volumes/LaCie/HexSim Results/VaageC_Plot2.3.pdf", plot = p2.1, device="pdf", dpi=300, width = 8, height = 4)

#####PLOT 2.2-------------------------------------------------------------------
setwd("/Volumes/LaCie/HexSim Results")

library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(sf)

#Read in and clean data---------------------------------------------------------
net <- st_read("/Volumes/LaCie/HexSim Results/HexSimNetwork/network_sections.shp",layer="network_sections")
net <- net[,c("RID","shape_leng")] #pull in network with river sections
craysolo <- read_csv("CrayfishSolo269/Data Probe/Segment Manager-[Movement [ crayfish ]] Density Check [ Segment Manager ]-accumulators.csv")
craysolo <- craysolo %>% 
  group_by(Step, `Reach ID`) %>% 
  summarise(density = sum(`Occupants [ cray ]`) / `Reach Area`)
craysolo <- craysolo %>%
  mutate(density.bin = cut(density,
                           breaks = c(-Inf, 1, 5, 10, 15, 20, 25, Inf),  # Define the break points
                           labels = c("Unoccupied", "Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High", "Extreme"),  # Labels for each range
                           right = FALSE))  # Make the intervals left-closed
any(is.na(craysolo)) #QA/QC
colnames(craysolo)[2] <- c("RID")

craysolo_occ <- merge(net, craysolo, by = "RID", all = TRUE) #merge with network object and remove geometry
craysolo_occ <- st_drop_geometry(craysolo_occ)
class(craysolo_occ)

craysolo_plot <- craysolo_occ %>% 
  group_by(Step, density.bin) %>% 
  summarise(RKM = sum(shape_leng))
craysolo_plot <- craysolo_plot %>% 
  mutate(RKM = RKM / 1000)

#Plot---------------------------------------------------------------------------
craysolo_plot$density.bin <- factor(craysolo_plot$density.bin, levels = c("Unoccupied", "Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High", "Extreme"))

custom_colors <- c("Unoccupied" = "white", "Low" = "rosybrown1","ModeratelyLow" = "lightsalmon","Moderate" = "salmon2","ModeratelyHigh" = "indianred","High" = "maroon","Extreme" = "darkred")
bins <- c("Unoccupied", "Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High", "Extreme")


p2.2 <- ggplot(craysolo_plot, aes(x = Step, y = RKM, fill = density.bin)) +
    geom_area(position = "stack", alpha = 0.65) +  # Stack the areas
  labs(title = "Density of Rusty Crayfish by Reach", subtitle = "1999-2020",
       x = "Time (Months)", y = "RKM") +
  scale_fill_manual(values = custom_colors, breaks = bins) +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 6)) +
  theme(axis.title.x = element_text(color = "grey60"),
        axis.title.y = element_text(color = "grey60"),
        axis.text = element_text(color = "grey60"),
        text = element_text(size = 16)) +
  scale_y_continuous(limits = c(0, 1000)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())
p2.2

#PLOT2.3------------------------------------------------------------------------
#Read in and clean data---------------------------------------------------------
net <- st_read("/Volumes/LaCie/HexSim Results/HexSimNetwork/network_sections.shp",layer="network_sections")
net <- net[,c("RID","shape_leng")] #pull in network with river sections
craycombo <- read_csv("BC2/Crayfish/Segment Manager-[Movement [ crayfish ]] Density Check [ Segment Manager ]-accumulators.csv")
craycombo <- craycombo %>% 
  group_by(Step, `Reach ID`) %>% 
  summarise(density = sum(`Occupants [ cray ]`) / `Reach Area`)
craycombo <- craycombo %>%
  mutate(density.bin = cut(density,
                           breaks = c(-Inf, 1, 5, 10, 15, 20, 25, Inf),  # Define the break points
                           labels = c("Unoccupied", "Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High", "Extreme"),  # Labels for each range
                           right = FALSE))  # Make the intervals left-closed
any(is.na(craycombo)) #QA/QC
colnames(craycombo)[2] <- c("RID")

craycombo_occ <- merge(net, craycombo, by = "RID", all = TRUE) #merge with network object and remove geometry
craycombo_occ <- st_drop_geometry(craycombo_occ)
class(craycombo_occ)

craycombo_plot <- craycombo_occ %>% 
  group_by(Step, density.bin) %>% 
  summarise(RKM = sum(shape_leng))
craycombo_plot <- craycombo_plot %>% 
  mutate(RKM = RKM / 1000)

#Plot---------------------------------------------------------------------------
craycombo_plot$density.bin <- factor(craycombo_plot$density.bin, levels = c("Unoccupied", "Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High", "Extreme"))

custom_colors <- c("Unoccupied" = "white", "Low" = "rosybrown1","ModeratelyLow" = "lightsalmon","Moderate" = "salmon2","ModeratelyHigh" = "indianred","High" = "maroon","Extreme" = "darkred")
bins <- c("Unoccupied", "Low", "ModeratelyLow", "Moderate", "ModeratelyHigh", "High", "Extreme")


p2.3 <- ggplot(craycombo_plot, aes(x = Step, y = RKM, fill = density.bin)) +
  geom_area(position = "stack", alpha = 0.65) +  # Stack the areas
  labs(title = "Density of Rusty Crayfish by Reach", subtitle = "1999-2020",
       x = "Time (Months)", y = "RKM") +
  scale_fill_manual(values = custom_colors, breaks = bins) +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 6)) +
  theme(axis.title.x = element_text(color = "grey60"),
        axis.title.y = element_text(color = "grey60"),
        axis.text = element_text(color = "grey60"),
        text = element_text(size = 16)) +
  scale_y_continuous(limits = c(0, 1000)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())
p2.3

#Original Plot------------------------------------------------------------------
craycombo <- read.csv("BC2/Interactions/BC2.7.csv", header = TRUE)
craycombo <- craycombo[,c(2,7:13)]
craycombo <- craycombo %>% #rename columns to named category
  rename(
    Xzero = Trait.Index..0,
    Xfive = Trait.Index..1,
    Xten = Trait.Index..2,
    Xfifteen = Trait.Index..3,
    Xtwenty = Trait.Index..4,
    Xtwentyfive = Trait.Index..5,
    Xplus = Trait.Index..6
  )

craycombo <- craycombo[,-c(1,2)]
craycombo$sum <- rowSums(craycombo)

col1 <- craycombo[,1] / craycombo$sum
col2 <- craycombo[,2] / craycombo$sum
col3 <- craycombo[,3] / craycombo$sum
col4 <- craycombo[,4] / craycombo$sum
col5 <- craycombo[,5] / craycombo$sum
col6 <- craycombo[,6] / craycombo$sum

prop <- cbind(col1, col2, col3, col4, col5, col6)
prop[is.nan(prop)] <- 0.00
prop <- as.data.frame(prop)

dates <- seq(as.Date('1999-05-01'), by = "month", length.out = 251)
prop$Time.Step <- dates

propplot <- prop %>% #rename columns to named category
  rename(
    Low = col1,
    ModeratelyLow = col2,
    Moderate = col3,
    ModeratelyHigh = col4,
    High = col5,
    Extreme = col6
  )

start <- which(propplot$Time.Step == "2010-01-01")
end <- which(propplot$Time.Step == "2020-03-01")
propplot <- propplot[c(start:end),]

proplong <- propplot %>%
  pivot_longer(cols = 1:6,  # Specify columns to pivot
               names_to = "Density.Bin",            # Name of the new column for the old column names
               values_to = "Density")          # Name of the new column for the values

proplong$Density.Bin <- factor(proplong$Density.Bin, levels = c("Low", "ModeratelyLow","Moderate","ModeratelyHigh","High","Extreme"))

custom_colors <- c("Low" = "rosybrown1","ModeratelyLow" = "lightsalmon","Moderate" = "salmon2","ModeratelyHigh" = "indianred","High" = "maroon","Extreme" = "darkred")
bins <- c("Low", "ModeratelyLow","Moderate","ModeratelyHigh","High","Extreme")

p2.1 <- ggplot(proplong, aes(x = Time.Step, y = Density, fill = Density.Bin)) +
  geom_area(position = "stack", alpha = 0.65) +  # Stack the areas
  labs(title = "Density of Rusty Crayfish by Reach", subtitle = "2010-2021",
       x = "Time (Months)", y = "Proportion of Occupied Reaches") +
  scale_fill_manual(values = custom_colors, breaks = bins) +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  guides(fill = guide_legend(ncol = 6)) +
  theme(axis.title.x = element_text(color = "grey60"),
        axis.title.y = element_text(color = "grey60"),
        axis.text = element_text(color = "grey60"),
        text = element_text(size = 16)) +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank())
p2.1
ggsave("/Volumes/LaCie/HexSim Results/VaageC_Plot2.2.pdf", plot = p2.1, device="pdf", dpi=300, width = 4, height = 4)

#EXTRA


craysolo_wide <- craysolo %>% 
  pivot_wider(names_from = Step, 
              values_from = density.bin)
craysolo_wide <- craysolo_wide[,-c(1,2)]
start <- which(names(craysolo_wide) == 1)
end <- ncol(craysolo_wide)
dates <- seq(as.Date('1999-05-01'), by = "month", length.out = end - start)
colnames(craysolo_wide) <- dates
craysolo_wide$RID <- c(0:2216)