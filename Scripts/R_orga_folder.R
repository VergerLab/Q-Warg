og_files <- file.choose()
og_dir <- dirname(og_files)

# Set your working directory
setwd(og_dir)

# List all files 
tif_files <- list.files(pattern = "\\.tif$", full.names = TRUE)
png_files <- list.files(og_dir, pattern = "\\.png$", full.names = TRUE)

# Loop through each TIFF file
for (t_file in tif_files) {
  # Get the base name of the file (without the path)
  t_file_name <- basename(t_file)
  
  # Check if the file name does not start with "C1-"
  if (!grepl("^C1-", t_file_name)) {
    # Create a folder with the name of the file (without the extension)
    folder_name <- file.path(og_dir, tools::file_path_sans_ext(t_file_name))
    dir.create(folder_name, showWarnings = FALSE)  # Create folder if it doesn't exist
    
    # Move the TIFF file into the newly created folder
    file.copy(t_file, folder_name)
  }
}

# Loop through each file and rename it
for (png_file in png_files) {
  # Create the new filename by removing 'C1-'
  new_file <- file.path(og_dir, sub("^C1-", "", basename(png_file)))
  
  # Rename the file
  file.rename(png_file, new_file)
  
}

folders <- list.dirs(og_dir, full.names = FALSE, recursive = FALSE)
png_renamed_files <- list.files(og_dir, pattern = "\\.png$", full.names = TRUE)

# Loop through each folder
for (folder in folders) {
  # Construct the folder path
  folder_path <- file.path(og_dir, folder)
  
  # Check if the folder exists
  if (dir.exists(folder_path)) {
    # Loop through each file
    for (png_renamed_file in png_renamed_files) {
      # Check if the file starts with the folder name
      if (startsWith(basename(png_renamed_file), folder)) {
        # Construct the new file path in the target folder
        new_png_path <- file.path(folder_path, basename(png_renamed_file))
        
        # Copy the file to the target folder
        file.copy(png_renamed_file, new_png_path)
        
        # Print the action
        cat("Copied:", png_renamed_file)
      }
    }
  } else {
    cat("Folder does not exist:", folder, "\n")
  }
}

# Loop through each folder
for (folder in folders) {
  # List all PNG files in the folder
  png_files <- list.files(folder, pattern = "\\.png$", full.names = TRUE)
  
  # Loop through each PNG file
  for (png_file in png_files) {
    # Get the base name of the PNG file (without extension)
    png_base_name <- tools::file_path_sans_ext(basename(png_file))
    
    # Find the corresponding TIF file by checking for common prefix
    tif_files <- list.files(folder, pattern = "\\.tif$", full.names = TRUE)
    
    # Initialize a variable to hold the common prefix
    common_prefix <- NULL
    
    # Loop through each TIF file to find the common prefix
    for (tif_file in tif_files) {
      tif_base_name <- tools::file_path_sans_ext(basename(tif_file))
      
      # Check if the PNG base name starts with the TIFF base name
      if (startsWith(png_base_name, tif_base_name)) {
        common_prefix <- tif_base_name
        break  # Exit the loop once we find a match
      }
    }
    
    # If a common prefix was found, rename the PNG file
    if (!is.null(common_prefix)) {
      # Create the new PNG file name by appending "-labels"
      new_png_name <- paste0(common_prefix, "-labels.png")
      new_png_path <- file.path(folder, new_png_name)
      
      # Rename the PNG file
      file.rename(png_file, new_png_path)
      cat("Renamed:", png_file, "to", new_png_path, "\n")
    } else {
      cat("No corresponding TIF file found for:", png_file, "\n")
    }
  }
}

# Create the new folder for analysis
dir.create("ANALYSIS", showWarnings = FALSE)

# Set the path for the new ANALYSIS folder
analysis_dir <- file.path(og_dir, "ANALYSIS")

folders <- folders[folders != analysis_dir]
# Move each folder to the ANALYSIS folder
for (folder in folders) {
  # Get the folder name
  folder_name <- basename(folder)
  
  # Define the new path for the folder
  new_path <- file.path(analysis_dir, folder_name)
  
  # Move the folder
  file.rename(folder, new_path)
}

dir.create("RAW_DATA", showWarnings = FALSE)
raw_data_dir <- file.path(og_dir, "RAW_DATA")

# List all files in the og_dir (excluding folders)
all_files <- list.files(og_dir, full.names = TRUE, recursive = FALSE)

# Move each file to the RAW_DATA folder
for (all_file in all_files) {
  # Check if the item is a file (not a directory)
  if (file.info(all_file)$isdir == FALSE) {
    # Define the new path for the file
    new_path <- file.path(raw_data_dir, basename(all_file))
    
    # Move the file
    file.rename(all_file, new_path)
  }
}