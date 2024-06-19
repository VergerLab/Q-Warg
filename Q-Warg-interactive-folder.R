# code to move the jpg and livedta-saved into a new folder to be used in the interactive plot app.
interactive_files <- choose.dir()

# Set your working directory
setwd(interactive_files)

# List all jpg files recursively
jpg_files <- list.files(pattern = "\\.jpg$", recursive = TRUE, full.names = TRUE)

# Create the new folder "interactive" if it doesn't exist
dir.create("interactive", showWarnings = FALSE)

# Move jpg files to the "interactive" folder
file.rename(jpg_files, file.path("interactive", basename(jpg_files)))

donnees_to_use <- list.files(pattern = "\\livedta-saved.csv$")
file.copy(donnees_to_use, "interactive")

print("Data ready to be used in Shiny App")
