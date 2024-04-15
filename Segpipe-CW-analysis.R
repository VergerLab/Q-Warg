require(data.table)
library(rlist)
library(plyr)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(stringr)
library(scales)
library(tidyverse)

# choose working directory:
work_folder <- choose.dir()
setwd(work_folder)

all_files <- list.files(path = work_folder, recursive = TRUE, full.names = TRUE)

# take all the csv files in the working directory, searching in the subfolders too
file_list <- all_files[grepl("\\.csv$", all_files)]

# make a list of all csv files
csv_list <- lapply(file_list, function(file) {
  data <- read.csv(file)
  data$filename <- basename(file)
  return(data)
})

# dataframe from all csv files
merged_csv <- bind_rows(csv_list)
merged_csv[c('condition', 'staining', 'other')] <- str_split_fixed(merged_csv$filename, '-', 3)

# take only the columns of interest
df <- subset(merged_csv, select = c("condition", "Label", "Area", "Perimeter", "Circularity", "staining", "Mean", "OBox.Center.X", "OBox.Center.Y"))

df$cooX <- df$OBox.Center.X
df$cooY <- df$OBox.Center.Y

df$meanCW <- ifelse(df$staining == "CW", df$Mean, NA)
df$meanViability <- ifelse(df$staining == "Viability", df$Mean, NA)

df$medium <- df$condition

df <- unite(df, col='cell', c('condition', 'Label'), sep='-')

# convert pixel size into µm: to have diameter of each object.
df <- df %>%
  mutate(diameter = (Perimeter / pi)/1.5408)

dta <- subset(df, select = c("medium","cell", "diameter", "Circularity", "meanCW", "meanViability", "cooX", "cooY"))

wdta <- dta %>%
  group_by(cell) %>%
  summarise_all(funs(ifelse(all(is.na(.)), NA, first(na.omit(.)))))

#filter out non fda positive cells and non circular elements 
#filter out all debris by size /!\ here it is for pixel size: to modify once the labels are in micrometers. 39pixel ~16µm based on area.
wdta$cellalive <- "no"
#wdta$cellalive[wdta$Circularity >= 0.8 & wdta$meanViability >= 500 & wdta$diameter >= 15] <- "yes"
wdta$cellalive[wdta$Circularity >= 0.8 & wdta$meanViability >= 500] <- "yes"

livedta <- wdta %>%
  filter(cellalive != "no") 

#save usable data frame as csv file
# Define the file name
file_name <- "livedta-saved.csv"
# Combine the working directory and file name to get the full file path
file_path <- file.path(work_folder, file_name)
# Save the CSV file
write.csv(livedta, file = file_path, row.names = FALSE, quote = FALSE)

#save(livedta, file = "dfsaved.Rda")

# to add the n of each medium on top of the violin plot
n_counts <- table(livedta$medium)

#Violin plot with each point and color gradient depending on the cell size
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
ggsave("CW_intensity_plot.pdf", plot = CW_intensity_plot, device = "pdf", width=7, height=6)
#ggsave("CW_intensity_plot.svg", plot = CW_intensity_plot, device = "svg")


## size of the cells
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
ggsave("CellSize_plot.pdf", plot = CellSize_plot, device = "pdf", width=7, height=6)
#ggsave("CellSize_plot.svg", plot = CellSize_plot, device = "svg")

## to see the number of living cells per condition
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
ggsave("LivingCells_proportion_plot.pdf", plot = LivingCells_proportion_plot, device = "pdf", width=6, height=4)
#ggsave("LivingCells_proportion_plot.svg", plot = LivingCells_proportion_plot, device = "svg")


print("######################################################################### Analysis and plots done ###############################################################")
