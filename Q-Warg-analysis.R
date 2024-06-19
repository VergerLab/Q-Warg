# Library needed to run the code: (need to be installed beforehand)
library(data.table)
library(rlist)
library(hrbrthemes)
library(viridis)
library(tidyverse)

# choose working directory where the subfolders containing data tables generated with ImageJ are:
work_folder <- choose.dir()
setwd(work_folder)

# take all the csv files in the working directory, searching in the subfolders too
# make a list of all csv files and create a data frame containing morphometry and fluorescence intensities
all_files <- list.files(path = work_folder, recursive = TRUE, full.names = TRUE)
file_list <- all_files[grepl("\\.csv$", all_files)]
csv_list <- lapply(file_list, function(file) {
  data <- read.csv(file)
  data$filename <- basename(file)
  return(data)
})
merged_csv <- bind_rows(csv_list)
merged_csv[c('condition', 'staining', 'other')] <- str_split_fixed(merged_csv$filename, '-', 3)

# Subset the raw data to take only the columns of interest
# rename columns for coordinates
df <- subset(merged_csv, select = c("condition", "Label", "Area", "Perimeter", "Circularity", "staining", "Mean", "OBox.Center.X", "OBox.Center.Y"))
df$cooX <- df$OBox.Center.X
df$cooY <- df$OBox.Center.Y

# create a new column to state the staining corresponding to the fluorescence intensity value
df$meanCW <- ifelse(df$staining == "CW", df$Mean, NA)
df$meanViability <- ifelse(df$staining == "Viability", df$Mean, NA)

# convert pixel size into µm: to have diameter of each object.
df <- df %>%
  mutate(diameter = (Perimeter / pi))

# create a new column with the name of each cell based on condition and label: 
df$medium <- df$condition
df <- unite(df, col='cell', c('condition', 'Label'), sep='-')

# subset the data frame
dta <- subset(df, select = c("medium","cell", "diameter", "Circularity", "meanCW", "meanViability", "cooX", "cooY"))

# new data frame with each row corresponding to one cell:
# associate CW and Viability fluorescence intensities with the morphometry measurements
wdta <- dta %>%
  group_by(cell) %>%
  summarise_all(funs(ifelse(all(is.na(.)), NA, first(na.omit(.)))))

# filter out non Viability positive cells and non circular elements (that would not be healthy protoplasts/cells) 
########### Depending on the staining, the Viability mean might need to be adjusted. #####################################################
wdta$cellalive <- "no"
wdta$cellalive[wdta$Circularity >= 0.8 & wdta$meanViability >= 500] <- "yes"

# Working data frame with only living cells: 
livedta <- wdta %>%
  filter(cellalive != "no") 

# Save usable data frame as csv file
# Define the file name
file_name <- "livedta-saved.csv"
# Combine the working directory and file name to get the full file path
file_path <- file.path(work_folder, file_name)
# Save the CSV file
write.csv(livedta, file = file_path, row.names = FALSE, quote = FALSE)

# to add the n of each condition on top of the violin plot
n_counts <- table(livedta$medium)

# Violin plot representing the cell wall staining intensity 
# the color gradient of the points represents the cell size
# The plot is both printed in Rstudio and saved as PDF
# can also be saved as SVG
CW_intensity_plot <- livedta %>%
  ggplot(aes(x = medium, y = meanCW)) +
  geom_jitter(aes(color = diameter), size = 0.3, alpha = 0.5) +
  scale_color_gradientn(colors= c("yellow", "red", "purple", "blue")) +
  geom_violin(color = 'black', scale = "area", draw_quantiles = c(0.5), alpha=0) +
  theme_bw() +
  ggtitle("CW intensity") +
  xlab("Condition") +
  ylab("Cell wall staining")+
  geom_text(data = data.frame(medium = names(n_counts), Value = as.numeric(n_counts)),
            aes(x = medium, y = max(livedta$meanCW) + 0.2, label = as.character(Value)),
            color = "black", size = 4, vjust = -0.5) +
  scale_y_log10() 

print(CW_intensity_plot)
# save as PDF file: the width and height can be adjusted ######################################################################
ggsave("CW_intensity_plot.pdf", plot = CW_intensity_plot, device = "pdf", width=12, height=6)
#ggsave("CW_intensity_plot.svg", plot = CW_intensity_plot, device = "svg")


# Violin plot representing the cell size
# the color gradient of the points represents the cell wall staining intensity 
# The plot is both printed in Rstudio and saved as PDF
# can also be saved as SVG
CellSize_plot <- livedta %>%
  ggplot(aes(x = medium, y = diameter)) +
  geom_jitter(aes(color = meanCW), size = 0.3, alpha = 0.5) +
  scale_color_gradient(low = "darkblue", high = "cyan") +
  geom_violin(color = 'black', scale = "area", draw_quantiles = c(0.5), alpha=0) +
  theme_bw() +
  ggtitle("Cell size distribution") +
  xlab("Condition") +
  ylab("Cell diameter")

print(CellSize_plot)
# save as PDF file: the width and height can be adjusted ######################################################################
ggsave("CellSize_plot.pdf", plot = CellSize_plot, device = "pdf", width=12, height=6)
#ggsave("CellSize_plot.svg", plot = CellSize_plot, device = "svg")

# Proportion of living cells from the number of segmented objects
# The plot is both printed in Rstudio and saved as PDF
# can also be saved as SVG
LivingCells_proportion_plot <- wdta %>%
  group_by(medium, cellalive) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100) %>%
  ggplot(aes(x = medium, y = count, fill = cellalive)) +
  geom_bar(stat = "identity") +
  scale_fill_manual("cellalive", values = c("no" = "black", "yes" = "orange")) +
  geom_text(aes(label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5), size = 3, color="white") +
  labs(title = "Living cells %") +
  theme_minimal() 

print(LivingCells_proportion_plot)
# save as PDF file: the width and height can be adjusted ######################################################################
ggsave("LivingCells_proportion_plot.pdf", plot = LivingCells_proportion_plot, device = "pdf", width=6, height=4)
#ggsave("LivingCells_proportion_plot.svg", plot = LivingCells_proportion_plot, device = "svg")


print("######################################################################### Analysis and plots done ###############################################################")

