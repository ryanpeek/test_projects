# Copy lots of pdfs from multiple folders to a single location: (Endnote Export)

# One of the main issues with Endnote is it stores PDFs in multiple folders.
# Difficult to export pdfs to a single location.


# USING R -----------------------------------------------------------------

# If you want to programmatically find each of the sub_task dirs
my_dirs <- list.dirs(path = "~/Downloads/pdf_export_peek.Data/PDF/",full.names = T)

my_files <- list.files(., pattern = "*.pdf", recursive = TRUE)

# Grab all files from the directories using list.files in sapply
files <- sapply(my_dirs, list.files, pattern="*.pdf", full.names = T, include.dirs = T)
files

# Your output directory to copy files to
new_dir <- "all_sub_task"
# Make sure the directory exists
dir.create(new_dir, recursive = TRUE)

# Copy the files
for(file in files) {
  # See ?file.copy for more options
  file.copy(file, new_dir)
}


copyEverything <- function(from, to){
  # We search all the files and directories
  files <- list.files(from, r = T)
  dirs  <- list.dirs(from, r = T, f = F)    
  
  
  # We create the required directories
  dir.create(to)
  sapply(paste(to, dirs, sep = '/'), dir.create)
  
  # And then we copy the files
  file.copy(paste(from, files, sep = '/'), paste(to, files, sep = '/'))
}


# USING SHELL -------------------------------------------------------------

# mkdir pdf_outfolder
# navigate to main folder with lots of subfolders:
# run this: 
# find . -name "*pdf" -exec mv {} ../../pdf_outfolder \;