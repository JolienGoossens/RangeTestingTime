#############################
# Make folders #
#############################

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1



#### Function to create directory ####
mkdirs <- function(fp) {
  if(!dir.exists(fp)) {
    dir.create(fp)
  }
} 

#### Create directories ####
mkdirs("data")
mkdirs("data/external")
mkdirs("data/interim")
mkdirs("data/processed")
mkdirs("data/raw")
mkdirs("reports")
mkdirs("reports/figures")
