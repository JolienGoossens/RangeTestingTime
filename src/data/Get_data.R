#############
# Get data  #
#############

# Jolien Goossens -  Marine Biology Research Group, UGent / VLIZ / ILVO
# R version 4.1.1


#### Get detection data, tilt and noise measurements #####
# Export from VUE software (InnovaSea Systems Inc., USA)
# Detection data in "data/raw/Belwind_oct2020_detections_builtin.csv"
# Event metadata in "data/raw/Belwind_oct2020_event.csv"

#### Environmental data wind ####
# Data retrieved from MDK at station Westhinder via EMODnet: 
# https://www.emodnet-physics.eu/Map/platinfo/PIROOSDownload.aspx?PlatformID=9035

#### Environmental data currents ####
# Data retrieved from 
# https://erddap.naturalsciences.be/erddap/griddap/BCZ_HydroState_V1.html

#### Get deployment and receiver metadata #####
# Needs to be performed on LifeWatch RStudio
library(etn)
my_con <- connect_to_etn(Sys.getenv("username"),
                         Sys.getenv("password"))

deploy <- etn::get_deployments(my_con, network_project = "JJ_Belwind", open_only = FALSE)
rec <- etn::get_receivers(my_con, receiver_id = unique(deploy$receiver_id))

#### Save data ####
write.csv(deploy, "data/raw/deploy_read20201026.csv", row.names = F)
write.csv(rec, "data/raw/rec_read20201026.csv", row.names = F)