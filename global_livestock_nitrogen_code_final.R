####################################
## GLOBAL LIVESTOCK SUPPLY CHAINS  #
####################################

#########################################
## AGGREGATION OF DATA AT GLOBAL LEVEL ##
#########################################


################
### Library   ##
################
library("Matrix")
library("MASS")
library("ggplot2")
library("gapminder")
library("grid")
library("dplyr")  					# Data management
library("RColorBrewer")
library("reshape2")


# Notes

#################
#  IMPORT DATA  #
#################

setwd("C:/~/16.Modelling")

##################
# I. Feed Module #
##################

###################
# 1. MONOGASTRIC  #
###################

feed_chi_back <- read.table("1.Data_crop/1.Monogastric/chicken_backyard.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back

feed_chi_broi <- read.table("1.Data_crop/1.Monogastric/chicken_broiler.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # broiler
feed_chi_layr <- read.table("1.Data_crop/1.Monogastric/chicken_layer.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back

feed_pig_back <- read.table("1.Data_crop/1.Monogastric/pig_backyard.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back
feed_pig_inte <- read.table("1.Data_crop/1.Monogastric/pig_intermediate.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Inte
feed_pig_indu <- read.table("1.Data_crop/1.Monogastric/pig_industrial.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Indu


################
# 2. RUMINANTS #
################
feed_cat_flot <- read.table("1.Data_crop/2.Ruminant/cattle_feedlot.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Feedlot

feed_buf_beef_grass <- read.table("1.Data_crop/2.Ruminant/buffalo_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo beef grass
feed_buf_beef_mixed <- read.table("1.Data_crop/2.Ruminant/buffalo_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo beef mixed

feed_buf_dair_grass <- read.table("1.Data_crop/2.Ruminant/buffalo_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo dairy grass
feed_buf_dair_mixed <- read.table("1.Data_crop/2.Ruminant/buffalo_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo dairy mixed

feed_cat_beef_grass <- read.table("1.Data_crop/2.Ruminant/cattle_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle Beef grass
feed_cat_beef_mixed <- read.table("1.Data_crop/2.Ruminant/cattle_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle Beef mixed

feed_cat_dair_grass <- read.table("1.Data_crop/2.Ruminant/cattle_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle dairy grass
feed_cat_dair_mixed <- read.table("1.Data_crop/2.Ruminant/cattle_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle dairy mixed

feed_goa_beef_grass <- read.table("1.Data_crop/2.Ruminant/goat_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat beef grass
feed_goa_beef_mixed <- read.table("1.Data_crop/2.Ruminant/goat_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat beef mixed

feed_goa_dair_grass <- read.table("1.Data_crop/2.Ruminant/goat_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat dairy grass
feed_goa_dair_mixed <- read.table("1.Data_crop/2.Ruminant/goat_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat dairy mixed

feed_she_beef_grass <- read.table("1.Data_crop/2.Ruminant/sheep_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep beef grass
feed_she_beef_mixed <- read.table("1.Data_crop/2.Ruminant/sheep_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep beef mixed

feed_she_dair_grass <- read.table("1.Data_crop/2.Ruminant/sheep_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep dairy grass
feed_she_dair_mixed <- read.table("1.Data_crop/2.Ruminant/sheep_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep dairy mixed

nox_trade <- read.table("8.Transport/emission_trade_system.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep dairy mixed

feed_chi_broi[,22] <- nox_trade$CHK_BRL
feed_chi_layr[,22] <- nox_trade$CHK_LYR
feed_pig_inte[,22] <- nox_trade$PIGS_MED
feed_pig_indu[,22] <- nox_trade$PIGS_IND
feed_cat_flot[,22] <- nox_trade$C_FDLTS
feed_buf_beef_grass[,22] <- nox_trade$BB_GRASS
feed_buf_beef_mixed[,22] <- nox_trade$BB_MIXED
feed_buf_dair_grass[,22] <- nox_trade$BD_GRASS
feed_buf_dair_mixed[,22] <- nox_trade$BD_MIXED
feed_cat_beef_grass[,22] <- nox_trade$CB_GRASS
feed_cat_beef_mixed[,22] <- nox_trade$CB_MIXED
feed_cat_dair_grass[,22] <- nox_trade$CD_GRASS
feed_cat_dair_mixed[,22] <- nox_trade$CD_MIXED
feed_goa_beef_grass[,22] <- nox_trade$GB_GRASS
feed_goa_beef_mixed[,22] <- nox_trade$GB_MIXED
feed_goa_dair_grass[,22] <- nox_trade$GD_GRASS
feed_goa_dair_mixed[,22] <- nox_trade$GD_MIXED
feed_she_beef_grass[,22] <- nox_trade$SB_GRASS
feed_she_beef_mixed[,22] <- nox_trade$SB_MIXED
feed_she_dair_grass[,22] <- nox_trade$SD_GRASS
feed_she_dair_mixed[,22] <- nox_trade$SD_MIXED

########################################
# II. Import feed data with allocation #
########################################

###################
# 1. MONOGASTRIC  #
###################

feed_chi_back_allo <- read.table("1.Data_crop/1.1.Monogastric_allocation/chicken_backyard.csv", 
                                 header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back
feed_chi_broi_allo <- read.table("1.Data_crop/1.1.Monogastric_allocation/chicken_broiler.csv", 
                                 header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # broiler
feed_chi_layr_allo <- read.table("1.Data_crop/1.1.Monogastric_allocation/chicken_layer.csv", 
                                 header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back

feed_pig_back_allo <- read.table("1.Data_crop/1.1.Monogastric_allocation/pig_backyard.csv", 
                                 header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back
feed_pig_inte_allo <- read.table("1.Data_crop/1.1.Monogastric_allocation/pig_intermediate.csv", 
                                 header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Inte
feed_pig_indu_allo <- read.table("1.Data_crop/1.1.Monogastric_allocation/pig_industrial.csv", 
                                 header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Indu


################
# 2. RUMINANTS #
################

feed_buf_beef_grass_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/buffalo_beef_grass.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo beef grass
feed_buf_beef_mixed_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/buffalo_beef_mixed.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo beef mixed

feed_buf_dair_grass_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/buffalo_dairy_grass.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo dairy grass
feed_buf_dair_mixed_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/buffalo_dairy_mixed.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo dairy mixed

feed_cat_flot_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/cattle_feedlot.csv", 
                                 header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle Beef grass


feed_cat_beef_grass_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/cattle_beef_grass.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle Beef grass
feed_cat_beef_mixed_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/cattle_beef_mixed.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle Beef mixed

feed_cat_dair_grass_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/cattle_dairy_grass.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle dairy grass
feed_cat_dair_mixed_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/cattle_dairy_mixed.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle dairy mixed

feed_goa_beef_grass_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/goat_beef_grass.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat beef grass
feed_goa_beef_mixed_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/goat_beef_mixed.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat beef mixed

feed_goa_dair_grass_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/goat_dairy_grass.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat dairy grass
feed_goa_dair_mixed_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/goat_dairy_mixed.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat dairy mixed

feed_she_beef_grass_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/sheep_beef_grass.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep beef grass
feed_she_beef_mixed_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/sheep_beef_mixed.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep beef mixed

feed_she_dair_grass_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/sheep_dairy_grass.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep dairy grass
feed_she_dair_mixed_allo <- read.table("1.Data_crop/2.1.Ruminant_allocation/sheep_dairy_mixed.csv", 
                                       header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep dairy mixed

feed_chi_broi_allo[,22] <- nox_trade$CHK_BRL
feed_chi_layr_allo[,22] <- nox_trade$CHK_LYR
feed_pig_inte_allo[,22] <- nox_trade$PIGS_MED
feed_pig_indu_allo[,22] <- nox_trade$PIGS_IND
feed_cat_flot_allo[,22] <- nox_trade$C_FDLTS
feed_buf_beef_grass_allo[,22] <- nox_trade$BB_GRASS
feed_buf_beef_mixed_allo[,22] <- nox_trade$BB_MIXED
feed_buf_dair_grass_allo[,22] <- nox_trade$BD_GRASS
feed_buf_dair_mixed_allo[,22] <- nox_trade$BD_MIXED
feed_cat_beef_grass_allo[,22] <- nox_trade$CB_GRASS
feed_cat_beef_mixed_allo[,22] <- nox_trade$CB_MIXED
feed_cat_dair_grass_allo[,22] <- nox_trade$CD_GRASS
feed_cat_dair_mixed_allo[,22] <- nox_trade$CD_MIXED
feed_goa_beef_grass_allo[,22] <- nox_trade$GB_GRASS
feed_goa_beef_mixed_allo[,22] <- nox_trade$GB_MIXED
feed_goa_dair_grass_allo[,22] <- nox_trade$GD_GRASS
feed_goa_dair_mixed_allo[,22] <- nox_trade$GD_MIXED
feed_she_beef_grass_allo[,22] <- nox_trade$SB_GRASS
feed_she_beef_mixed_allo[,22] <- nox_trade$SB_MIXED
feed_she_dair_grass_allo[,22] <- nox_trade$SD_GRASS
feed_she_dair_mixed_allo[,22] <- nox_trade$SD_MIXED


########################
# III. ANIMAL MODULE  ##
########################

###################
# 1. MONOGASTRIC  #
###################

animal_chi_back <- read.table("2.Data_animal/1.Monogastric/animal_chicken_backyard.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back
animal_chi_broi <- read.table("2.Data_animal/1.Monogastric/animal_chicken_broilers.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # broiler
animal_chi_layr <- read.table("2.Data_animal/1.Monogastric/animal_chicken_layers.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back

animal_pig_back <- read.table("2.Data_animal/1.Monogastric/animal_pig_backyard.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back
animal_pig_inte <- read.table("2.Data_animal/1.Monogastric/animal_pig_intermediate.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Inte
animal_pig_indu <- read.table("2.Data_animal/1.Monogastric/animal_pig_industrial.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Indu

################
# 2. RUMINANTS #
################

animal_cat_flot <- read.table("2.Data_animal/2.Ruminant/animal_cattle_feedlot.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)

animal_buf_beef_grass <- read.table("2.Data_animal/2.Ruminant/animal_buffalo_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo beef grass
animal_buf_beef_mixed <- read.table("2.Data_animal/2.Ruminant/animal_buffalo_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo beef mixed

animal_buf_dair_grass <- read.table("2.Data_animal/2.Ruminant/animal_buffalo_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo dairy grass
animal_buf_dair_mixed <- read.table("2.Data_animal/2.Ruminant/animal_buffalo_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo dairy mixed

animal_cat_beef_grass <- read.table("2.Data_animal/2.Ruminant/animal_cattle_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle Beef grass
animal_cat_beef_mixed <- read.table("2.Data_animal/2.Ruminant/animal_cattle_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle Beef mixed

animal_cat_dair_grass <- read.table("2.Data_animal/2.Ruminant/animal_cattle_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle dairy grass
animal_cat_dair_mixed <- read.table("2.Data_animal/2.Ruminant/animal_cattle_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle dairy mixed

animal_goa_beef_grass <- read.table("2.Data_animal/2.Ruminant/animal_goat_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat beef grass
animal_goa_beef_mixed <- read.table("2.Data_animal/2.Ruminant/animal_goat_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat beef mixed

animal_goa_dair_grass <- read.table("2.Data_animal/2.Ruminant/animal_goat_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat dairy grass
animal_goa_dair_mixed <- read.table("2.Data_animal/2.Ruminant/animal_goat_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat dairy mixed

animal_she_beef_grass <- read.table("2.Data_animal/2.Ruminant/animal_sheep_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep beef grass
animal_she_beef_mixed <- read.table("2.Data_animal/2.Ruminant/animal_sheep_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep beef mixed

animal_she_dair_grass <- read.table("2.Data_animal/2.Ruminant/animal_sheep_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep dairy grass
animal_she_dair_mixed <- read.table("2.Data_animal/2.Ruminant/animal_sheep_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep dairy mixed


##################
# IV. PROCESSING #
##################

###################
# 1. MONOGASTRIC  #
###################

processing_chi_back <- read.table("3.Data_processing/1.Monogastric/processing_chicken_backyard.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back
processing_chi_broi <- read.table("3.Data_processing/1.Monogastric/processing_chicken_broilers.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # broiler
processing_chi_layr <- read.table("3.Data_processing/1.Monogastric/processing_chicken_layers.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back

processing_pig_back <- read.table("3.Data_processing/1.Monogastric/processing_pig_backyard.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # back
processing_pig_inte <- read.table("3.Data_processing/1.Monogastric/processing_pig_intermediate.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Inte
processing_pig_indu <- read.table("3.Data_processing/1.Monogastric/processing_pig_industrial.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Indu


################
# 2. RUMINANTS #
################

processing_cat_flot <- read.table("3.Data_processing/2.Ruminant/processing_feedlot.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo beef grass


processing_buf_beef_grass <- read.table("3.Data_processing/2.Ruminant/processing_buffalo_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo beef grass
processing_buf_beef_mixed <- read.table("3.Data_processing/2.Ruminant/processing_buffalo_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo beef mixed

processing_buf_dair_grass <- read.table("3.Data_processing/2.Ruminant/processing_buffalo_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo dairy grass
processing_buf_dair_mixed <- read.table("3.Data_processing/2.Ruminant/processing_buffalo_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # buffalo dairy mixed

processing_cat_beef_grass <- read.table("3.Data_processing/2.Ruminant/processing_cattle_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle Beef grass
processing_cat_beef_mixed <- read.table("3.Data_processing/2.Ruminant/processing_cattle_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle Beef mixed

processing_cat_dair_grass <- read.table("3.Data_processing/2.Ruminant/processing_cattle_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle dairy grass
processing_cat_dair_mixed <- read.table("3.Data_processing/2.Ruminant/processing_cattle_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Cattle dairy mixed

processing_goa_beef_grass <- read.table("3.Data_processing/2.Ruminant/processing_goat_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat beef grass
processing_goa_beef_mixed <- read.table("3.Data_processing/2.Ruminant/processing_goat_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat beef mixed

processing_goa_dair_grass <- read.table("3.Data_processing/2.Ruminant/processing_goat_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat dairy grass
processing_goa_dair_mixed <- read.table("3.Data_processing/2.Ruminant/processing_goat_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Goat dairy mixed

processing_she_beef_grass <- read.table("3.Data_processing/2.Ruminant/processing_sheep_beef_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep beef grass
processing_she_beef_mixed <- read.table("3.Data_processing/2.Ruminant/processing_sheep_beef_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep beef mixed

processing_she_dair_grass <- read.table("3.Data_processing/2.Ruminant/processing_sheep_dairy_grass.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep dairy grass
processing_she_dair_mixed <- read.table("3.Data_processing/2.Ruminant/processing_sheep_dairy_mixed.csv", 
								header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)           # Sheep dairy mixed




#############################
# V. Production from GLEAM #
#############################

production <- read.table("2.Data_animal/production_livestock.csv", 
                                  header = T, sep = ",", comment.char = "#", na.strings = "", quote="\"", row.names = NULL)

#########################################
# Add production data on animal module  #
#########################################

animal_chi_back <- data.frame(animal_chi_back, production[,c(9,12)])
animal_chi_broi <- data.frame(animal_chi_broi, production[,c(10)])
animal_chi_layr <- data.frame(animal_chi_layr, production[,c(11,13)])
animal_pig_back <- data.frame(animal_pig_back, production[,c(6)])
animal_pig_inte <- data.frame(animal_pig_inte, production[,c(7)])
animal_pig_indu <- data.frame(animal_pig_indu, production[,c(8)])

animal_buf_beef_grass <- data.frame(animal_buf_beef_grass, production[,19])
animal_buf_beef_mixed <- data.frame(animal_buf_beef_mixed, production[,18])
animal_buf_dair_grass <- data.frame(animal_buf_dair_grass, production[,c(16,17)])
animal_buf_dair_mixed <- data.frame(animal_buf_dair_mixed, production[,c(14,15)])

animal_cat_flot 	  <- data.frame(animal_cat_flot, production[,17])
animal_cat_beef_grass <- data.frame(animal_cat_beef_grass, production[,21])
animal_cat_beef_mixed <- data.frame(animal_cat_beef_mixed, production[,20])
animal_cat_dair_grass <- data.frame(animal_cat_dair_grass, production[,c(24,25)])
animal_cat_dair_mixed <- data.frame(animal_cat_dair_mixed, production[,c(22,23)])

animal_goa_beef_grass <- data.frame(animal_goa_beef_grass, production[,27])
animal_goa_beef_mixed <- data.frame(animal_goa_beef_mixed, production[,26])
animal_goa_dair_grass <- data.frame(animal_goa_dair_grass, production[,c(30,31)])
animal_goa_dair_mixed <- data.frame(animal_goa_dair_mixed, production[,c(28,29)])

animal_she_beef_grass <- data.frame(animal_she_beef_grass, production[,33])
animal_she_beef_mixed <- data.frame(animal_she_beef_mixed, production[,32])
animal_she_dair_grass <- data.frame(animal_she_dair_grass, production[,c(34,35)])
animal_she_dair_mixed <- data.frame(animal_she_dair_mixed, production[,c(36,37)])


## Aggregation sheep and goats

feed_sml_beef_grass = feed_goa_beef_grass[,-c(1:5)] + feed_she_beef_grass[,-c(1:5)]
feed_sml_beef_mixed = feed_goa_beef_mixed[,-c(1:5)] + feed_she_beef_mixed[,-c(1:5)]

feed_sml_dair_grass = feed_goa_dair_grass[,-c(1:5)] + feed_she_dair_grass[,-c(1:5)]
feed_sml_dair_mixed = feed_goa_dair_mixed[,-c(1:5)] + feed_she_dair_mixed[,-c(1:5)]

feed_sml_beef_grass = data.frame(feed_she_beef_grass[,1:5], feed_sml_beef_grass)
feed_sml_beef_mixed = data.frame(feed_she_beef_grass[,1:5], feed_sml_beef_mixed)
feed_sml_dair_grass = data.frame(feed_she_beef_grass[,1:5], feed_sml_dair_grass)
feed_sml_dair_mixed = data.frame(feed_she_beef_grass[,1:5], feed_sml_dair_mixed)

share_trade_beef_grass = (animal_goa_beef_grass$Total_N_intake * animal_goa_beef_grass$share_trade + 
						animal_she_beef_grass$Total_N_intake * animal_she_beef_grass$share_trade)/
						(animal_goa_beef_grass$Total_N_intake + animal_she_beef_grass$Total_N_intake)

share_trade_beef_mixed = (animal_goa_beef_mixed$Total_N_intake * animal_goa_beef_mixed$share_trade + 
						animal_she_beef_mixed$Total_N_intake * animal_she_beef_mixed$share_trade)/
						(animal_goa_beef_mixed$Total_N_intake + animal_she_beef_mixed$Total_N_intake)

share_trade_dair_grass = (animal_goa_dair_grass$Total_N_intake * animal_goa_dair_grass$share_trade + 
						animal_she_dair_grass$Total_N_intake * animal_she_dair_grass$share_trade)/
						(animal_goa_dair_grass$Total_N_intake + animal_she_dair_grass$Total_N_intake)

share_trade_dair_mixed = (animal_goa_dair_mixed$Total_N_intake * animal_goa_dair_mixed$share_trade + 
						animal_she_dair_mixed$Total_N_intake * animal_she_dair_mixed$share_trade)/
						(animal_goa_dair_mixed$Total_N_intake + animal_she_dair_mixed$Total_N_intake)

share_trade_beef_grass[is.na(share_trade_beef_grass)| share_trade_beef_grass == NaN] = 0
share_trade_beef_mixed[is.na(share_trade_beef_mixed)| share_trade_beef_mixed == NaN] = 0
share_trade_dair_grass[is.na(share_trade_dair_grass)| share_trade_dair_grass == NaN] = 0
share_trade_dair_mixed[is.na(share_trade_dair_mixed)| share_trade_dair_mixed == NaN] = 0

animal_sml_beef_grass = animal_goa_beef_grass[,-c(1:5)] + animal_she_beef_grass[,-c(1:5)]
animal_sml_beef_mixed = animal_goa_beef_mixed[,-c(1:5)] + animal_she_beef_mixed[,-c(1:5)]

animal_sml_dair_grass = animal_goa_dair_grass[,-c(1:5)] + animal_she_dair_grass[,-c(1:5)]
animal_sml_dair_mixed = animal_goa_dair_mixed[,-c(1:5)] + animal_she_dair_mixed[,-c(1:5)]

animal_sml_beef_grass[, 19] = share_trade_beef_grass
animal_sml_beef_mixed[, 19] = share_trade_beef_mixed
animal_sml_dair_grass[, 19] = share_trade_dair_grass
animal_sml_dair_mixed[, 19] = share_trade_dair_mixed

animal_sml_beef_grass = data.frame(animal_she_beef_grass[,1:5], animal_sml_beef_grass)
animal_sml_beef_mixed = data.frame(animal_she_beef_mixed[,1:5], animal_sml_beef_mixed)
animal_sml_dair_grass = data.frame(animal_she_dair_grass[,1:5], animal_sml_dair_grass)
animal_sml_dair_mixed = data.frame(animal_she_dair_mixed[,1:5], animal_sml_dair_mixed)

processing_sml_beef_grass = processing_goa_beef_grass[,c(6:8)] + processing_she_beef_grass[,c(6:8)]
processing_sml_beef_mixed = processing_goa_beef_mixed[,c(6:8)] + processing_she_beef_mixed[,c(6:8)]
processing_sml_dair_grass = processing_goa_dair_grass[,c(6:8)] + processing_she_dair_grass[,c(6:8)]
processing_sml_dair_mixed = processing_goa_dair_mixed[,c(6:8)] + processing_she_dair_mixed[,c(6:8)]

processing_sml_beef_grass = data.frame(processing_she_beef_grass[,1:5], processing_sml_beef_grass)
processing_sml_beef_mixed = data.frame(processing_she_beef_mixed[,1:5], processing_sml_beef_mixed)
processing_sml_dair_grass = data.frame(processing_she_dair_grass[,1:5], processing_sml_dair_grass)
processing_sml_dair_mixed = data.frame(processing_she_dair_mixed[,1:5], processing_sml_dair_mixed)

##############################################
## Remove the countries without feed ration ##
##############################################

# There are countries with animals but without any feed composition defined. 
# These countries are excluded from the assessment.

#################
## Monogastric ##
#################

trim_feed_chi_back <- filter (feed_chi_back, Area>0)
trim_feed_chi_broi <- filter (feed_chi_broi, Area>0)
trim_feed_chi_layr <- filter (feed_chi_layr, Area>0)

trim_feed_pig_back <- filter (feed_pig_back, Area>0)
trim_feed_pig_inte <- filter (feed_pig_inte, Area>0)
trim_feed_pig_indu <- filter (feed_pig_indu, Area>0)

trim_feed_buf_beef_grass <- filter (feed_buf_beef_grass, Area>0)
trim_feed_buf_beef_mixed <- filter (feed_buf_beef_mixed, Area>0)
trim_feed_buf_dair_grass <- filter (feed_buf_dair_grass, Area>0)
trim_feed_buf_dair_mixed <- filter (feed_buf_dair_mixed, Area>0)

trim_feed_cat_flot       <- filter (feed_cat_flot, Area>0)

trim_feed_cat_beef_grass <- filter (feed_cat_beef_grass, Area>0)
trim_feed_cat_beef_mixed <- filter (feed_cat_beef_mixed, Area>0)
trim_feed_cat_dair_grass <- filter (feed_cat_dair_grass, Area>0)
trim_feed_cat_dair_mixed <- filter (feed_cat_dair_mixed, Area>0)

trim_feed_goa_beef_grass <- filter (feed_goa_beef_grass, Area>0)
trim_feed_goa_beef_mixed <- filter (feed_goa_beef_mixed, Area>0)
trim_feed_goa_dair_grass <- filter (feed_goa_dair_grass, Area>0)
trim_feed_goa_dair_mixed <- filter (feed_goa_dair_mixed, Area>0)

trim_feed_she_beef_grass <- filter (feed_she_beef_grass, Area>0)
trim_feed_she_beef_mixed <- filter (feed_she_beef_mixed, Area>0)
trim_feed_she_dair_grass <- filter (feed_she_dair_grass, Area>0)
trim_feed_she_dair_mixed <- filter (feed_she_dair_mixed, Area>0)

trim_feed_sml_beef_grass <- filter (feed_sml_beef_grass, Area > 0)
trim_feed_sml_beef_mixed <- filter (feed_sml_beef_mixed, Area > 0)
trim_feed_sml_dair_grass <- filter (feed_sml_dair_grass, Area > 0)
trim_feed_sml_dair_mixed <- filter (feed_sml_dair_mixed, Area > 0)


# Remove countries with 0 values
trim_animal_chi_back <- animal_chi_back[match(trim_feed_chi_back$ADM0_CODE, animal_chi_back$ADM0_CODE), ]
trim_animal_chi_broi <- animal_chi_broi[match(trim_feed_chi_broi$ADM0_CODE, animal_chi_broi$ADM0_CODE), ]
trim_animal_chi_layr <- animal_chi_layr[match(trim_feed_chi_layr$ADM0_CODE, animal_chi_layr$ADM0_CODE), ]

trim_animal_pig_back <- animal_pig_back[match(trim_feed_pig_back$ADM0_CODE, animal_pig_back$ADM0_CODE), ]
trim_animal_pig_inte <- animal_pig_inte[match(trim_feed_pig_inte$ADM0_CODE, animal_pig_inte$ADM0_CODE), ]
trim_animal_pig_indu <- animal_pig_indu[match(trim_feed_pig_indu$ADM0_CODE, animal_pig_indu$ADM0_CODE), ]

trim_animal_buf_beef_grass <- animal_buf_beef_grass[match(trim_feed_buf_beef_grass$ADM0_CODE, animal_buf_beef_grass$ADM0_CODE), ]
trim_animal_buf_beef_mixed <- animal_buf_beef_mixed[match(trim_feed_buf_beef_mixed$ADM0_CODE, animal_buf_beef_mixed$ADM0_CODE), ]
trim_animal_buf_dair_grass <- animal_buf_dair_grass[match(trim_feed_buf_dair_grass$ADM0_CODE, animal_buf_dair_grass$ADM0_CODE), ]
trim_animal_buf_dair_mixed <- animal_buf_dair_mixed[match(trim_feed_buf_dair_mixed$ADM0_CODE, animal_buf_dair_mixed$ADM0_CODE), ]

trim_animal_cat_flot <- animal_cat_flot[match(trim_feed_cat_flot$ADM0_CODE, animal_cat_flot$ADM0_CODE), ]

trim_animal_cat_beef_grass <- animal_cat_beef_grass[match(trim_feed_cat_beef_grass$ADM0_CODE, animal_cat_beef_grass$ADM0_CODE), ]
trim_animal_cat_beef_mixed <- animal_cat_beef_mixed[match(trim_feed_cat_beef_mixed$ADM0_CODE, animal_cat_beef_mixed$ADM0_CODE), ]
trim_animal_cat_dair_grass <- animal_cat_dair_grass[match(trim_feed_cat_dair_grass$ADM0_CODE, animal_cat_dair_grass$ADM0_CODE), ]
trim_animal_cat_dair_mixed <- animal_cat_dair_mixed[match(trim_feed_cat_dair_mixed$ADM0_CODE, animal_cat_dair_mixed$ADM0_CODE), ]

trim_animal_goa_beef_grass <- animal_goa_beef_grass[match(trim_feed_goa_beef_grass$ADM0_CODE, animal_goa_beef_grass$ADM0_CODE), ]
trim_animal_goa_beef_mixed <- animal_goa_beef_mixed[match(trim_feed_goa_beef_mixed$ADM0_CODE, animal_goa_beef_mixed$ADM0_CODE), ]
trim_animal_goa_dair_grass <- animal_goa_dair_grass[match(trim_feed_goa_dair_grass$ADM0_CODE, animal_goa_dair_grass$ADM0_CODE), ]
trim_animal_goa_dair_mixed <- animal_goa_dair_mixed[match(trim_feed_goa_dair_mixed$ADM0_CODE, animal_goa_dair_mixed$ADM0_CODE), ]

trim_animal_she_beef_grass <- animal_she_beef_grass[match(trim_feed_she_beef_grass$ADM0_CODE, animal_she_beef_grass$ADM0_CODE), ]
trim_animal_she_beef_mixed <- animal_she_beef_mixed[match(trim_feed_she_beef_mixed$ADM0_CODE, animal_she_beef_mixed$ADM0_CODE), ]
trim_animal_she_dair_grass <- animal_she_dair_grass[match(trim_feed_she_dair_grass$ADM0_CODE, animal_she_dair_grass$ADM0_CODE), ] 
trim_animal_she_dair_mixed <- animal_she_dair_mixed[match(trim_feed_she_dair_mixed$ADM0_CODE, animal_she_dair_mixed$ADM0_CODE), ]

trim_animal_sml_beef_grass <- animal_sml_beef_grass[match(trim_feed_sml_beef_grass$ADM0_CODE, animal_sml_beef_grass$ADM0_CODE), ]
trim_animal_sml_beef_mixed <- animal_sml_beef_mixed[match(trim_feed_sml_beef_mixed$ADM0_CODE, animal_sml_beef_mixed$ADM0_CODE), ]
trim_animal_sml_dair_grass <- animal_sml_dair_grass[match(trim_feed_sml_dair_grass$ADM0_CODE, animal_sml_dair_grass$ADM0_CODE), ] 
trim_animal_sml_dair_mixed <- animal_sml_dair_mixed[match(trim_feed_sml_dair_mixed$ADM0_CODE, animal_sml_dair_mixed$ADM0_CODE), ]



# Processing
trim_processing_chi_back <- processing_chi_back[match(trim_feed_chi_back$ADM0_CODE, processing_chi_back$ADM0_CODE), ]
trim_processing_chi_broi <- processing_chi_broi[match(trim_feed_chi_broi$ADM0_CODE, processing_chi_broi$ADM0_CODE), ]
trim_processing_chi_layr <- processing_chi_layr[match(trim_feed_chi_layr$ADM0_CODE, processing_chi_layr$ADM0_CODE), ]

trim_processing_pig_back <- processing_pig_back[match(trim_feed_pig_back$ADM0_CODE, processing_pig_back$ADM0_CODE), ]
trim_processing_pig_inte <- processing_pig_inte[match(trim_feed_pig_inte$ADM0_CODE, processing_pig_inte$ADM0_CODE), ]
trim_processing_pig_indu <- processing_pig_indu[match(trim_feed_pig_indu$ADM0_CODE, processing_pig_indu$ADM0_CODE), ]

trim_processing_buf_beef_grass <- processing_buf_beef_grass[match(trim_feed_buf_beef_grass$ADM0_CODE, processing_buf_beef_grass$ADM0_CODE), ]
trim_processing_buf_beef_mixed <- processing_buf_beef_mixed[match(trim_feed_buf_beef_mixed$ADM0_CODE, processing_buf_beef_mixed$ADM0_CODE), ]
trim_processing_buf_dair_grass <- processing_buf_dair_grass[match(trim_feed_buf_dair_grass$ADM0_CODE, processing_buf_dair_grass$ADM0_CODE), ]
trim_processing_buf_dair_mixed <- processing_buf_dair_mixed[match(trim_feed_buf_dair_mixed$ADM0_CODE, processing_buf_dair_mixed$ADM0_CODE), ]

trim_processing_cat_flot <- processing_cat_flot[match(trim_feed_cat_flot$ADM0_CODE, processing_cat_flot$ADM0_CODE), ]

trim_processing_cat_beef_grass <- processing_cat_beef_grass[match(trim_feed_cat_beef_grass$ADM0_CODE, processing_cat_beef_grass$ADM0_CODE), ]
trim_processing_cat_beef_mixed <- processing_cat_beef_mixed[match(trim_feed_cat_beef_mixed$ADM0_CODE, processing_cat_beef_mixed$ADM0_CODE), ]
trim_processing_cat_dair_grass <- processing_cat_dair_grass[match(trim_feed_cat_dair_grass$ADM0_CODE, processing_cat_dair_grass$ADM0_CODE), ]
trim_processing_cat_dair_mixed <- processing_cat_dair_mixed[match(trim_feed_cat_dair_mixed$ADM0_CODE, processing_cat_dair_mixed$ADM0_CODE), ]

trim_processing_goa_beef_grass <- processing_goa_beef_grass[match(trim_feed_goa_beef_grass$ADM0_CODE, processing_goa_beef_grass$ADM0_CODE), ]
trim_processing_goa_beef_mixed <- processing_goa_beef_mixed[match(trim_feed_goa_beef_mixed$ADM0_CODE, processing_goa_beef_mixed$ADM0_CODE), ]
trim_processing_goa_dair_grass <- processing_goa_dair_grass[match(trim_feed_goa_dair_grass$ADM0_CODE, processing_goa_dair_grass$ADM0_CODE), ]
trim_processing_goa_dair_mixed <- processing_goa_dair_mixed[match(trim_feed_goa_dair_mixed$ADM0_CODE, processing_goa_dair_mixed$ADM0_CODE), ]

trim_processing_she_beef_grass <- processing_she_beef_grass[match(trim_feed_she_beef_grass$ADM0_CODE, processing_she_beef_grass$ADM0_CODE), ]
trim_processing_she_beef_mixed <- processing_she_beef_mixed[match(trim_feed_she_beef_mixed$ADM0_CODE, processing_she_beef_mixed$ADM0_CODE), ]
trim_processing_she_dair_grass <- processing_she_dair_grass[match(trim_feed_she_dair_grass$ADM0_CODE, processing_she_dair_grass$ADM0_CODE), ]
trim_processing_she_dair_mixed <- processing_she_dair_mixed[match(trim_feed_she_dair_mixed$ADM0_CODE, processing_she_dair_mixed$ADM0_CODE), ]


trim_processing_sml_beef_grass <- processing_sml_beef_grass[match(trim_feed_sml_beef_grass$ADM0_CODE, processing_sml_beef_grass$ADM0_CODE), ]
trim_processing_sml_beef_mixed <- processing_sml_beef_mixed[match(trim_feed_sml_beef_mixed$ADM0_CODE, processing_sml_beef_mixed$ADM0_CODE), ]
trim_processing_sml_dair_grass <- processing_sml_dair_grass[match(trim_feed_sml_dair_grass$ADM0_CODE, processing_sml_dair_grass$ADM0_CODE), ]
trim_processing_sml_dair_mixed <- processing_sml_dair_mixed[match(trim_feed_sml_dair_mixed$ADM0_CODE, processing_sml_dair_mixed$ADM0_CODE), ]

##############################################
## ALLOCATED FEED LOSSES / EMISSIONS FACTORS ##
###############################################

###########
# TRIM ##
#########

feed_sml_beef_grass_allo = feed_goa_beef_grass_allo[,-c(1:5)] + feed_she_beef_grass_allo[,-c(1:5)]
feed_sml_beef_mixed_allo = feed_goa_beef_mixed_allo[,-c(1:5)] + feed_she_beef_mixed_allo[,-c(1:5)]

feed_sml_dair_grass_allo = feed_goa_dair_grass_allo[,-c(1:5)] + feed_she_dair_grass_allo[,-c(1:5)]
feed_sml_dair_mixed_allo = feed_goa_dair_mixed_allo[,-c(1:5)] + feed_she_dair_mixed_allo[,-c(1:5)]

feed_sml_beef_grass_allo = data.frame(feed_she_beef_grass[,1:5], feed_sml_beef_grass_allo)
feed_sml_beef_mixed_allo = data.frame(feed_she_beef_grass[,1:5], feed_sml_beef_mixed_allo)
feed_sml_dair_grass_allo = data.frame(feed_she_beef_grass[,1:5], feed_sml_dair_grass_allo)
feed_sml_dair_mixed_allo = data.frame(feed_she_beef_grass[,1:5], feed_sml_dair_mixed_allo)



trim_feed_chi_back_allo <- filter (feed_chi_back_allo, Area>0)
trim_feed_chi_broi_allo <- filter (feed_chi_broi_allo, Area>0)
trim_feed_chi_layr_allo <- filter (feed_chi_layr_allo, Area>0)

trim_feed_pig_back_allo <- filter (feed_pig_back_allo, Area>0)
trim_feed_pig_inte_allo <- filter (feed_pig_inte_allo, Area>0)
trim_feed_pig_indu_allo <- filter (feed_pig_indu_allo, Area>0)

trim_feed_buf_beef_grass_allo <- filter (feed_buf_beef_grass_allo, Area>0)
trim_feed_buf_beef_mixed_allo <- filter (feed_buf_beef_mixed_allo, Area>0)
trim_feed_buf_dair_grass_allo <- filter (feed_buf_dair_grass_allo, Area>0)
trim_feed_buf_dair_mixed_allo <- filter (feed_buf_dair_mixed_allo, Area>0)

trim_feed_cat_flot_allo <- filter (feed_cat_flot_allo, Area>0)

trim_feed_cat_beef_grass_allo <- filter (feed_cat_beef_grass_allo, Area>0)
trim_feed_cat_beef_mixed_allo <- filter (feed_cat_beef_mixed_allo, Area>0)
trim_feed_cat_dair_grass_allo <- filter (feed_cat_dair_grass_allo, Area>0)
trim_feed_cat_dair_mixed_allo <- filter (feed_cat_dair_mixed_allo, Area>0)

trim_feed_goa_beef_grass_allo <- filter (feed_goa_beef_grass_allo, Area>0)
trim_feed_goa_beef_mixed_allo <- filter (feed_goa_beef_mixed_allo, Area>0)
trim_feed_goa_dair_grass_allo <- filter (feed_goa_dair_grass_allo, Area>0)
trim_feed_goa_dair_mixed_allo <- filter (feed_goa_dair_mixed_allo, Area>0)

trim_feed_she_beef_grass_allo <- filter (feed_she_beef_grass_allo, Area>0)
trim_feed_she_beef_mixed_allo <- filter (feed_she_beef_mixed_allo, Area>0)
trim_feed_she_dair_grass_allo <- filter (feed_she_dair_grass_allo, Area>0)
trim_feed_she_dair_mixed_allo <- filter (feed_she_dair_mixed_allo, Area>0)

trim_feed_sml_beef_grass_allo <- filter (feed_sml_beef_grass_allo, Area>0)
trim_feed_sml_beef_mixed_allo <- filter (feed_sml_beef_mixed_allo, Area>0)
trim_feed_sml_dair_grass_allo <- filter (feed_sml_dair_grass_allo, Area>0)
trim_feed_sml_dair_mixed_allo <- filter (feed_sml_dair_mixed_allo, Area>0)


################################
##    INDICATORS ESTIMATION  ###
################################
# Use inputs excluding the first column with country name and regions

my_nitrogen_indicator <- function (feed, feed_allo, animal, processing){
	zero_table = data.frame(matrix(0, nrow(feed), 1))
	# 1. Crop residues
	crop_res_recycle  <- feed[,9]

	# 2. Manure recycled
	#manure_recycle <- data.frame(matrix(feed [,3]* animal[,19],nrow(feed), 1)) #animal[,16]
	manure_recycle = data.frame(matrix(0, nrow(feed), 1))
	for (i in 1:nrow(feed)) {
		manure_recycle[i,] <- animal [i,21] #+  animal[i,18] + animal[i,16]
		if (manure_recycle[i,] > (feed[i,8] * (1 - animal[i,24]))) {
			manure_recycle[i,] <- feed[i,8] * (1 - animal[i,24])
		} else {
			manure_recycle[i,] <- manure_recycle[i,]
		}
	}

	# 3. organic waste
	org_waste_recycle <- zero_table
  
	# 4. feed intake
	crop_feed_intake  <- animal[,6] - animal[,23]

	# 5. feed from animal origin
	anim_prod_feed  <- zero_table

	# 6. feed from processing of Animal products
	processed_prod_feed  <- zero_table

	#7. crop_processing
	crop_to_process  <- zero_table

	# 8. Primary animal processing (live animal+milk)
	anim_prod_process  <- processing[,6]

	# 9. Animal product processing (0)
	anim_pro_process <- zero_table

	# 10. Imported crop
	crop_import <- zero_table

	# 11. feed to crop (0)
	ani_to_crop <- zero_table

	# 12. animal products to crop
	processed_to_crop <- zero_table

	# 13. crop to breeding
	crop_to_breed <- zero_table

	# 14. feed to breeding
	anipro_to_breed <- zero_table

	# 15. animal prod to breeding
	ani_to_breed <- zero_table

	# 16. fertilizer to processing =0
	fert_to_process <- zero_table

	# 17. Feed to processing <- 0
	feed_to_process <- zero_table

	#18. animal prod to processing
	ani_to_process <- zero_table
	
	#PRODUCT
	#19.  Crop output (feed)
	crop_output <- feed[,13]

	# 20. animal to crop
	crop_breeding <- zero_table

	# 21. crop_process
	crop_processing <- zero_table

	#22. animal to cropping
	ani_to_cropping <- zero_table

	#23. Animal production output
	anim_output <- animal[,6] - animal[,7] + animal[,21]#+ animal[,18] + animal[,16]

	#24. ani to animal process
	ani_to_processing <- zero_table

	#25. animal product to cropping
	process_ani_crop <- zero_table

	#26. Animal product to breeding
	process_ani_breed <- zero_table

	#27. Animal product to processing
	process_output <- processing[,7]

	#Resources extraction

	#28. Resource to cropping
	resource_crop  <- (feed[,7] - crop_res_recycle - manure_recycle)+ feed[,19] + feed[,20] + feed[,21] + feed[,22]
	
	#29. resource to breeding
	resource_anim <- animal[,23]

	#30. resource to processing
	resource_process <- zero_table

	#Stock change : Accumulation (negative); Remove (positive) - s matrix
	#31. stock change crop
	delta_crop  <- feed[,14] *-1

	#32. stock change animal
	delta_breed <- zero_table

	#33. Stock change processing
	delta_process <- zero_table

	#Wastes
	# 34 cropping
	waste_crop <- feed[,15] + feed[,19] + feed[,20] + feed[,21] + feed[,22]

	# 35. breeding
	waste_anim <-  animal[,20] #- animal[,18] - animal[,16]# Manure not collected added for broilers

	# 36. Processing
	waste_process <- processing[,8]

	country_u   <- data.frame (crop_res_recycle, manure_recycle, org_waste_recycle, 
							   crop_feed_intake, anim_prod_feed, processed_prod_feed,
							   crop_to_process, anim_prod_process, anim_pro_process )

	country_r 	<- data.frame (resource_crop, resource_anim, resource_process)
	country_s   <- data.frame (delta_crop , delta_breed, delta_process)
	country_w   <- data.frame (waste_crop, waste_anim, waste_process)
	country_v   <- data.frame (crop_output, crop_breeding, crop_processing,
							   ani_to_cropping,anim_output, ani_to_processing, 
							   process_ani_crop, process_ani_breed, process_output)

	nox_feed = rowSums(feed_allo[c(19:22)], na.rm = TRUE)
	country_w_allo = data.frame(feed_allo[,c(15:18)], nox_feed)
	country_animal = animal[,c(6:23)]

	output_1 <- data.frame(matrix(0, nrow(feed), 3))
	output_2 <- data.frame(matrix(0, nrow(feed), 3))
	output_3 <- data.frame(matrix(0, nrow(feed), 3))
	output_4 <- data.frame(matrix(0, nrow(feed), 1))
	output_5 <- data.frame(matrix(0, nrow(feed), 1))
	output_6 <- data.frame(matrix(0, nrow(feed), 1))
	output_7 <- data.frame(matrix(0, nrow(feed), 3))
	output_8 <- data.frame(matrix(0, nrow(feed), 1))
	output_9 <- data.frame(matrix(0, nrow(feed), 18))

	af_feed <- data.frame(matrix(0, nrow(feed), 3))
	af_animal <- data.frame(matrix(0, nrow(feed), 3))

	names(output_1) <- c("Verfc", "Verfa", "Verfp" )
	names(output_2) <- c("Nuec", "nuea", "nuep" )
	names(output_3) <- c("r1", "r2", "r3" )
	names(output_4) <- c("LCNUE")
	names(output_5) <- c("LCNNB")
	names(output_6) <- c("NHI")
	names(output_7) <- c("LossF", "LossAn", "LossProc" )
	names(output_9) <- c("Total_N_intake", "Total_N_excreted", "TAN", "NH3", 
		"N2O", "NOx", "Nox_burned", "N2", "NO3_leach", "NH3_daily_spread",
		"N_discharged" , "Npublsew", "N_not_collected", "Nox_incinerated",
		"N_losses", "N_recycled","Balance","Intake_non_crop")

		for(i in 1:nrow(feed)) {
			u = matrix(as.numeric(country_u[i,]), nrow=3, ncol=3, byrow=F)

			#Resource
			r = matrix(as.numeric(country_r[i,]), nrow=3, ncol=1, byrow=F)
			#change in stock
			s = matrix(as.numeric(country_s[i,] * -1), nrow=3, ncol=1, byrow=F) #-1 because in the table are -s'
			#waste
			w = matrix(as.numeric(country_w[i,]), nrow=3, ncol=1) #-1 because in the table are -w'
			#Product
			v = matrix(as.numeric(country_v[i,]), nrow=3, ncol=3, byrow=F)

			#Check validity
			ut = t(u)

			#Verification
			verf_1 = rowSums(ut, na.rm = F) + t(r)
			verf_2 = rowSums(v, na.rm = F) + t(s) + t(w)

			output_1[i,] = verf_1 - verf_2

			if(country_u[i,1] ==0){
				next
			}
				#NUE
				nue = (rowSums(v, na.rm = F) + t(s)) / (rowSums(ut, na.rm = F) +  t(r))
				output_2[i,] = nue * 100
				gs = matrix (c(country_s[i,1] * -1, 1:8 * 0),nrow = 3, ncol = 3, byrow = T)

				# LCNUE
				rf = t(r) %*% ginv (t(v) - u + gs ) # Calculation of RES* #Always use mass package to get ginv function
				output_3[i,] = rf
					
				if(output_3[i,3] == 0){
					next
				}
					lcnue = (1 / output_3[i,3])  * 100
					
					output_4[i,] = abs(lcnue)

			# Allocation faction
			
			af_animal_1 <- (manure_recycle[i,1] + anim_prod_process[i]) / anim_output[i]
			af_animal[i,] <- af_animal_1
			af_animal [is.na(af_animal)|af_animal ==NaN ]  <- 0
			af_animal [is.na(af_animal)|af_animal ==NA ]   <- 0
			af_animal [is.na(af_animal)|af_animal ==Inf ]  <- 0

			if (af_animal[i,1] >1){
				  af_animal[i,1] = 1
				}else{
				  af_animal[i,1] = af_animal[i,1]
				}
    
			output_8[i,] = af_animal[i,1]
    
			#LCNNB
			#net_loss  <- (waste_crop[i] * af_feed[i,1] + waste_anim[i] * af_animal[i,1] + waste_process[i])/feed[i,6]
#
			net_loss  <- (feed_allo[i,15]  + (waste_anim[i] - animal[i,13])* af_animal[i,1]  + waste_process[i])/feed_allo[i,6]
			net_loss [is.na(net_loss)|net_loss ==NaN ]  <- 0
			net_loss [is.na(net_loss)|net_loss ==NA ]   <- 0
			net_loss [is.na(net_loss)|net_loss ==Inf ]  <- 0

			output_5[i,] = net_loss
			
			# Loss animal production 
			#]
			loss_animal <- country_animal[i,]* af_animal[i,1] /1000000
			
			output_9[i,] <-	loss_animal 

			#NHI
			#allo_loss_feed = waste_crop[i] * af_feed[i,1]
			allo_loss_feed = feed_allo[i,15]
			allo_loss_anim = (waste_anim[i] - animal[i,13]) * af_animal[i,1]
			allo_loss_proc = waste_process[i]
			output_7[i,] = c(allo_loss_feed,allo_loss_anim,allo_loss_proc)/1000000 #in Gg N

			nhi <- (sd(c(allo_loss_feed, allo_loss_anim, allo_loss_proc))/
				  mean(c(allo_loss_feed, allo_loss_anim, allo_loss_proc)))*100
			nhi [is.na(nhi)|nhi ==NaN ]  <- 0
			nhi [is.na(nhi)|nhi ==NA ]   <- 0
			nhi [is.na(nhi)|nhi ==Inf ]  <- 0

			output_6[i,] <- nhi		
		}

	output_all = data.frame(output_1, output_2, output_3, output_4, output_5, output_6, output_7, country_w_allo/1000000, output_9)
	
	return (output_all)
	verf_input = data.frame (u,r,s,w,v)
	#return (output_8)
}

#country_cat_flot <- my_nitrogen_indicator (feed = trim_feed_cat_flot[12,], feed_allo = trim_feed_cat_flot_allo[12,], animal = trim_animal_cat_flot[12,], processing = trim_processing_cat_flot[12,])

# all production systems by country
country_chi_back       <- my_nitrogen_indicator (feed = trim_feed_chi_back, feed_allo = trim_feed_chi_back_allo, animal = trim_animal_chi_back, processing = trim_processing_chi_back)
country_chi_broi       <- my_nitrogen_indicator (feed = trim_feed_chi_broi, feed_allo = trim_feed_chi_broi_allo, animal = trim_animal_chi_broi, processing = trim_processing_chi_broi)
country_chi_layr       <- my_nitrogen_indicator (feed = trim_feed_chi_layr, feed_allo = trim_feed_chi_layr_allo, animal = trim_animal_chi_layr, processing = trim_processing_chi_layr)

country_pig_back       <- my_nitrogen_indicator (feed = trim_feed_pig_back, feed_allo = trim_feed_pig_back_allo, animal = trim_animal_pig_back, processing = trim_processing_pig_back)
country_pig_inte       <- my_nitrogen_indicator (feed = trim_feed_pig_inte, feed_allo = trim_feed_pig_inte_allo, animal = trim_animal_pig_inte, processing = trim_processing_pig_inte)
country_pig_indu       <- my_nitrogen_indicator (feed = trim_feed_pig_indu, feed_allo = trim_feed_pig_indu_allo, animal = trim_animal_pig_indu, processing = trim_processing_pig_indu)

country_buf_beef_grass <- my_nitrogen_indicator (feed = trim_feed_buf_beef_grass, feed_allo = trim_feed_buf_beef_grass_allo , animal = trim_animal_buf_beef_grass, processing = trim_processing_buf_beef_grass)
country_buf_beef_mixed <- my_nitrogen_indicator (feed = trim_feed_buf_beef_mixed, feed_allo = trim_feed_buf_beef_mixed_allo , animal = trim_animal_buf_beef_mixed, processing = trim_processing_buf_beef_mixed)
country_buf_dair_grass <- my_nitrogen_indicator (feed = trim_feed_buf_dair_grass, feed_allo = trim_feed_buf_dair_grass_allo , animal = trim_animal_buf_dair_grass, processing = trim_processing_buf_dair_grass)
country_buf_dair_mixed <- my_nitrogen_indicator (feed = trim_feed_buf_dair_mixed, feed_allo = trim_feed_buf_dair_mixed_allo , animal = trim_animal_buf_dair_mixed, processing = trim_processing_buf_dair_mixed)

country_cat_flot <- my_nitrogen_indicator (feed = trim_feed_cat_flot, feed_allo = trim_feed_cat_flot_allo, animal = trim_animal_cat_flot, processing = trim_processing_cat_flot)

country_cat_beef_grass <- my_nitrogen_indicator (feed = trim_feed_cat_beef_grass, feed_allo = trim_feed_cat_beef_grass_allo, animal = trim_animal_cat_beef_grass, processing = trim_processing_cat_beef_grass)
country_cat_beef_mixed <- my_nitrogen_indicator (feed = trim_feed_cat_beef_mixed, feed_allo = trim_feed_cat_beef_mixed_allo, animal = trim_animal_cat_beef_mixed, processing = trim_processing_cat_beef_mixed)
country_cat_dair_grass <- my_nitrogen_indicator (feed = trim_feed_cat_dair_grass, feed_allo = trim_feed_cat_dair_grass_allo, animal = trim_animal_cat_dair_grass, processing = trim_processing_cat_dair_grass)
country_cat_dair_mixed <- my_nitrogen_indicator (feed = trim_feed_cat_dair_mixed, feed_allo = trim_feed_cat_dair_mixed_allo, animal = trim_animal_cat_dair_mixed, processing = trim_processing_cat_dair_mixed)

country_goa_beef_grass <- my_nitrogen_indicator (feed = trim_feed_goa_beef_grass, feed_allo = trim_feed_goa_beef_grass_allo, animal = trim_animal_goa_beef_grass, processing = trim_processing_goa_beef_grass)
country_goa_beef_mixed <- my_nitrogen_indicator (feed = trim_feed_goa_beef_mixed, feed_allo = trim_feed_goa_beef_mixed_allo, animal = trim_animal_goa_beef_mixed, processing = trim_processing_goa_beef_mixed)
country_goa_dair_grass <- my_nitrogen_indicator (feed = trim_feed_goa_dair_grass, feed_allo = trim_feed_goa_dair_grass_allo, animal = trim_animal_goa_dair_grass, processing = trim_processing_goa_dair_grass)
country_goa_dair_mixed <- my_nitrogen_indicator (feed = trim_feed_goa_dair_mixed, feed_allo = trim_feed_goa_dair_mixed_allo, animal = trim_animal_goa_dair_mixed, processing = trim_processing_goa_dair_mixed)

country_she_beef_grass <- my_nitrogen_indicator (feed = trim_feed_she_beef_grass, feed_allo = trim_feed_she_beef_grass_allo, animal = trim_animal_she_beef_grass, processing = trim_processing_she_beef_grass)
country_she_beef_mixed <- my_nitrogen_indicator (feed = trim_feed_she_beef_mixed, feed_allo = trim_feed_she_beef_mixed_allo, animal = trim_animal_she_beef_mixed, processing = trim_processing_she_beef_mixed)
country_she_dair_grass <- my_nitrogen_indicator (feed = trim_feed_she_dair_grass, feed_allo = trim_feed_she_dair_grass_allo, animal = trim_animal_she_dair_grass, processing = trim_processing_she_dair_grass)
country_she_dair_mixed <- my_nitrogen_indicator (feed = trim_feed_she_dair_mixed, feed_allo = trim_feed_she_dair_mixed_allo, animal = trim_animal_she_dair_mixed, processing = trim_processing_she_dair_mixed)

country_sml_beef_grass <- my_nitrogen_indicator (feed = trim_feed_sml_beef_grass, feed_allo = trim_feed_sml_beef_grass_allo, animal = trim_animal_sml_beef_grass, processing = trim_processing_sml_beef_grass)
country_sml_beef_mixed <- my_nitrogen_indicator (feed = trim_feed_sml_beef_mixed, feed_allo = trim_feed_sml_beef_mixed_allo, animal = trim_animal_sml_beef_mixed, processing = trim_processing_sml_beef_mixed)
country_sml_dair_grass <- my_nitrogen_indicator (feed = trim_feed_sml_dair_grass, feed_allo = trim_feed_sml_dair_grass_allo, animal = trim_animal_sml_dair_grass, processing = trim_processing_sml_dair_grass)
country_sml_dair_mixed <- my_nitrogen_indicator (feed = trim_feed_sml_dair_mixed, feed_allo = trim_feed_sml_dair_mixed_allo, animal = trim_animal_sml_dair_mixed, processing = trim_processing_sml_dair_mixed)




#################
# Animal all livestock 

my_final_country = function (feed, feed_trim, output){
	final <- data.frame(feed_trim[,c(1:5)], output)
	final_output   <- final [match(feed$ADM0_CODE, final$ADM0_CODE), ]
	final_output[,c(1:5)] <- feed[,c(1:5)]
	final_output[is.na(final_output)| final_output ==NA] <- 0
	final_output$total_losses <- rowSums(final_output[,c(18:20,25)], na.rm = TRUE) #- final_output [,33]

	return(final_output)
}

final_country_chi_back       <- my_final_country(feed_trim = trim_feed_chi_back, feed = feed_chi_back, output = country_chi_back)
final_country_chi_broi       <- my_final_country(feed_trim = trim_feed_chi_broi, feed = feed_chi_broi, output = country_chi_broi)
final_country_chi_layr       <- my_final_country(feed_trim = trim_feed_chi_layr, feed = feed_chi_layr, output = country_chi_layr)

final_country_pig_back       <- my_final_country(feed_trim = trim_feed_pig_back, feed = feed_pig_back, output = country_pig_back)
final_country_pig_inte       <- my_final_country(feed_trim = trim_feed_pig_inte, feed = feed_pig_inte, output = country_pig_inte)
final_country_pig_indu       <- my_final_country(feed_trim = trim_feed_pig_indu, feed = feed_pig_indu, output = country_pig_indu)

final_country_buf_beef_grass <- my_final_country(feed_trim = trim_feed_buf_beef_grass, feed = feed_buf_beef_grass, output = country_buf_beef_grass)
final_country_buf_beef_mixed <- my_final_country(feed_trim = trim_feed_buf_beef_mixed, feed = feed_buf_beef_mixed, output = country_buf_beef_mixed)
final_country_buf_dair_grass <- my_final_country(feed_trim = trim_feed_buf_dair_grass, feed = feed_buf_dair_grass, output = country_buf_dair_grass)
final_country_buf_dair_mixed <- my_final_country(feed_trim = trim_feed_buf_dair_mixed, feed = feed_buf_dair_mixed, output = country_buf_dair_mixed)

final_country_cat_flot <- my_final_country(feed_trim = trim_feed_cat_flot, feed = feed_cat_flot, output = country_cat_flot)

final_country_cat_beef_grass <- my_final_country(feed_trim = trim_feed_cat_beef_grass, feed = feed_cat_beef_grass, output = country_cat_beef_grass)
final_country_cat_beef_mixed <- my_final_country(feed_trim = trim_feed_cat_beef_mixed, feed = feed_cat_beef_mixed, output = country_cat_beef_mixed)
final_country_cat_dair_grass <- my_final_country(feed_trim = trim_feed_cat_dair_grass, feed = feed_cat_dair_grass, output = country_cat_dair_grass)
final_country_cat_dair_mixed <- my_final_country(feed_trim = trim_feed_cat_dair_mixed, feed = feed_cat_dair_mixed, output = country_cat_dair_mixed)

final_country_goa_beef_grass <- my_final_country(feed_trim = trim_feed_goa_beef_grass, feed = feed_goa_beef_grass, output = country_goa_beef_grass)
final_country_goa_beef_mixed <- my_final_country(feed_trim = trim_feed_goa_beef_mixed, feed = feed_goa_beef_mixed, output = country_goa_beef_mixed)
final_country_goa_dair_grass <- my_final_country(feed_trim = trim_feed_goa_dair_grass, feed = feed_goa_dair_grass, output = country_goa_dair_grass)
final_country_goa_dair_mixed <- my_final_country(feed_trim = trim_feed_goa_dair_mixed, feed = feed_goa_dair_mixed, output = country_goa_dair_mixed)

final_country_she_beef_grass <- my_final_country(feed_trim = trim_feed_she_beef_grass, feed = feed_she_beef_grass, output = country_she_beef_grass)
final_country_she_beef_mixed <- my_final_country(feed_trim = trim_feed_she_beef_mixed, feed = feed_she_beef_mixed, output = country_she_beef_mixed)
final_country_she_dair_grass <- my_final_country(feed_trim = trim_feed_she_dair_grass, feed = feed_she_dair_grass, output = country_she_dair_grass)
final_country_she_dair_mixed <- my_final_country(feed_trim = trim_feed_she_dair_mixed, feed = feed_she_dair_mixed, output = country_she_dair_mixed)

final_country_sml_beef_grass <- my_final_country(feed_trim = trim_feed_sml_beef_grass, feed = feed_sml_beef_grass, output = country_sml_beef_grass)
final_country_sml_beef_mixed <- my_final_country(feed_trim = trim_feed_sml_beef_mixed, feed = feed_sml_beef_mixed, output = country_sml_beef_mixed)
final_country_sml_dair_grass <- my_final_country(feed_trim = trim_feed_sml_dair_grass, feed = feed_sml_dair_grass, output = country_sml_dair_grass)
final_country_sml_dair_mixed <- my_final_country(feed_trim = trim_feed_sml_dair_mixed, feed = feed_sml_dair_mixed, output = country_sml_dair_mixed)



# Comparing emissions from feed and manure 

fe_vs_man_country_chi_back       <- final_country_chi_back      [,18]/ final_country_chi_back      [,44] * 100 
fe_vs_man_country_chi_broi       <- final_country_chi_broi      [,18]/ final_country_chi_broi      [,44] * 100 
fe_vs_man_country_chi_layr       <- final_country_chi_layr      [,18]/ final_country_chi_layr      [,44] * 100 
fe_vs_man_country_pig_back       <- final_country_pig_back      [,18]/ final_country_pig_back      [,44] * 100 
fe_vs_man_country_pig_inte       <- final_country_pig_inte      [,18]/ final_country_pig_inte      [,44] * 100 
fe_vs_man_country_pig_indu       <- final_country_pig_indu      [,18]/ final_country_pig_indu      [,44] * 100 
fe_vs_man_country_buf_beef_grass <- final_country_buf_beef_grass[,18]/ final_country_buf_beef_grass[,44] * 100 
fe_vs_man_country_buf_beef_mixed <- final_country_buf_beef_mixed[,18]/ final_country_buf_beef_mixed[,44] * 100 
fe_vs_man_country_buf_dair_grass <- final_country_buf_dair_grass[,18]/ final_country_buf_dair_grass[,44] * 100 
fe_vs_man_country_buf_dair_mixed <- final_country_buf_dair_mixed[,18]/ final_country_buf_dair_mixed[,44] * 100 
fe_vs_man_country_cat_flot       <- final_country_cat_flot      [,18]/ final_country_cat_flot      [,44] * 100  
fe_vs_man_country_cat_beef_grass <- final_country_cat_beef_grass[,18]/ final_country_cat_beef_grass[,44] * 100 
fe_vs_man_country_cat_beef_mixed <- final_country_cat_beef_mixed[,18]/ final_country_cat_beef_mixed[,44] * 100 
fe_vs_man_country_cat_dair_grass <- final_country_cat_dair_grass[,18]/ final_country_cat_dair_grass[,44] * 100 
fe_vs_man_country_cat_dair_mixed <- final_country_cat_dair_mixed[,18]/ final_country_cat_dair_mixed[,44] * 100 
fe_vs_man_country_sml_beef_grass <- final_country_sml_beef_grass[,18]/ final_country_sml_beef_grass[,44] * 100 
fe_vs_man_country_sml_beef_mixed <- final_country_sml_beef_mixed[,18]/ final_country_sml_beef_mixed[,44] * 100 
fe_vs_man_country_sml_dair_grass <- final_country_sml_dair_grass[,18]/ final_country_sml_dair_grass[,44] * 100 
fe_vs_man_country_sml_dair_mixed <- final_country_sml_dair_mixed[,18]/ final_country_sml_dair_mixed[,44] * 100 

fe_vs_man = data.frame(final_country_chi_back[,1:2],fe_vs_man_country_chi_back ,     
		fe_vs_man_country_chi_broi      ,
		fe_vs_man_country_chi_layr      ,
		fe_vs_man_country_pig_back      ,
		fe_vs_man_country_pig_inte      ,
		fe_vs_man_country_pig_indu      ,
		fe_vs_man_country_buf_beef_grass,
		fe_vs_man_country_buf_beef_mixed,
		fe_vs_man_country_buf_dair_grass,
		fe_vs_man_country_buf_dair_mixed,
		fe_vs_man_country_cat_flot      ,
		fe_vs_man_country_cat_beef_grass,
		fe_vs_man_country_cat_beef_mixed,
		fe_vs_man_country_cat_dair_grass,
		fe_vs_man_country_cat_dair_mixed,
		fe_vs_man_country_sml_beef_grass,
		fe_vs_man_country_sml_beef_mixed,
		fe_vs_man_country_sml_dair_grass,
		fe_vs_man_country_sml_dair_mixed)

write.table (fe_vs_man, "5.Outputs/2.country/country_fe_vs_man.csv", sep = ",", row.names = FALSE)

nue_stage = data.frame(final_country_chi_back[,1:2],final_country_chi_back      [,9:11], 
		final_country_chi_broi      [,9:11],
		final_country_chi_layr      [,9:11],
		final_country_pig_back      [,9:11],
		final_country_pig_inte      [,9:11],
		final_country_pig_indu      [,9:11],
		final_country_buf_beef_grass[,9:11],
		final_country_buf_beef_mixed[,9:11],
		final_country_buf_dair_grass[,9:11],
		final_country_buf_dair_mixed[,9:11],
		final_country_cat_flot      [,9:11],
		final_country_cat_beef_grass[,9:11],
		final_country_cat_beef_mixed[,9:11],
		final_country_cat_dair_grass[,9:11],
		final_country_cat_dair_mixed[,9:11],
		final_country_sml_beef_grass[,9:11],
		final_country_sml_beef_mixed[,9:11],
		final_country_sml_dair_grass[,9:11],
		final_country_sml_dair_mixed[,9:11])
write.table (nue_stage, "5.Outputs/2.country/nue_stage.csv", sep = ",", row.names = FALSE)


####################################
## Analysis of nitrogen emissions  ##
####################################

regional_aggregation = function(table){
	region_annex = data.frame(matrix(c( "North America", "10",
					"Central & South America","20",
					"South Asia", "30",
					"East Asia", "40",
					"Western Europe", "50",
					"Eastern Europe", "60",
					"Oceania", "70",
					"Russian Federation", "80",
					"Sub-Saharan Africa", "90",
					"West Asia & Northern Africa", "91"), 10, 2, byrow=TRUE))
	names(region_annex) = c("REG_ANNEX5", "CATEG")

	table$CATEG <- region_annex$CATEG[match(table$REG_ANNEX5, region_annex$REG_ANNEX5)]

	table$CATEG <- as.numeric(paste(table$CATEG))

		my_feby <- function (tab, fact, fonction) {
					aaa = by(tab, fact, fonction)
					aaa = do.call(rbind, aaa)
					aaa = data.frame(row.names(aaa), aaa)
					nnn = strsplit(deparse(substitute(fact)), "$", fixed = T)
					nnn = nnn[[1]][2]
					names(aaa)[1] = nnn
					aaa
					return (aaa)
			  
			}

			regional_table <- my_feby(table[,-c(1:5)], table$CATEG, colSums)
			regional_table <- regional_table[,-(ncol(regional_table))]
			return(regional_table)
}


# Emissions by country
my_emission_total <- function (data){
	n_emission <- data.frame(matrix(0, nrow(data), ncol = 4))
	n_emission[,1] <- rowSums(data[,c(22, 29, 35)]) #NH3
	n_emission[,2] <- rowSums(data[,c(25, 31, 32, 39)]) #NOx
	n_emission[,3] <- rowSums(data[,c(23,30)]) #N2O
	n_emission[,4] <- rowSums(data[,c(24,34,36:38,20)]) # NO3
	n_total = rowSums(n_emission, na.rm=TRUE)
	return(n_emission)
}
emission_country = data.frame(my_emission_total (data = final_country_chi_back ),
		 my_emission_total (data = final_country_chi_broi      ),
		 my_emission_total (data = final_country_chi_layr      ),
		 my_emission_total (data = final_country_pig_back      ),
		 my_emission_total (data = final_country_pig_inte      ),
		 my_emission_total (data = final_country_pig_indu      ),
		 my_emission_total (data = final_country_buf_beef_grass),
		 my_emission_total (data = final_country_buf_beef_mixed),
		 my_emission_total (data = final_country_buf_dair_grass),
		 my_emission_total (data = final_country_buf_dair_mixed),
		 my_emission_total (data = final_country_cat_flot),
		 my_emission_total (data = final_country_cat_beef_grass),
		 my_emission_total (data = final_country_cat_beef_mixed),
		 my_emission_total (data = final_country_cat_dair_grass),
		 my_emission_total (data = final_country_cat_dair_mixed),
		 my_emission_total (data = final_country_sml_beef_grass),
		 my_emission_total (data = final_country_sml_beef_mixed),
		 my_emission_total (data = final_country_sml_dair_grass),
		 my_emission_total (data = final_country_sml_dair_mixed))

emission_country_chi_back =  my_emission_total (data = final_country_sml_dair_mixed)

# Emissions by species

emission_total_chi = my_emission_total (data = final_country_chi_back ) +
						 my_emission_total (data = final_country_chi_broi) +
						 my_emission_total (data = final_country_chi_layr) 

emission_total_chi = data.frame(final_country_chi_back[,1:5], emission_total_chi)
regional_emission_total_chi  = regional_aggregation(table= emission_total_chi)

emission_total_pig = my_emission_total (data = final_country_pig_back) +
						 my_emission_total (data = final_country_pig_inte) +
						 my_emission_total (data = final_country_pig_indu) 

emission_total_pig = data.frame(final_country_chi_back[,1:5], emission_total_pig)
regional_emission_total_pig = regional_aggregation(table= emission_total_pig)

emission_total_buf = my_emission_total (data = final_country_buf_beef_grass) +
						 my_emission_total (data = final_country_buf_beef_mixed) +
						 my_emission_total (data = final_country_buf_dair_grass) +
						 my_emission_total (data = final_country_buf_dair_mixed) 

emission_total_buf = data.frame(final_country_chi_back[,1:5], emission_total_buf)
regional_emission_total_buf  = regional_aggregation(table= emission_total_buf)

emission_total_cat = my_emission_total (data = final_country_cat_flot) +
						 my_emission_total (data = final_country_cat_beef_grass) +
						 my_emission_total (data = final_country_cat_beef_mixed) +
						 my_emission_total (data = final_country_cat_dair_grass) +
						 my_emission_total (data = final_country_cat_dair_mixed) 

emission_total_cat = data.frame(final_country_chi_back[,1:5], emission_total_cat)
regional_emission_total_cat = regional_aggregation(table= emission_total_cat)

emission_total_sml =  my_emission_total (data = final_country_sml_beef_grass) +
						 my_emission_total (data = final_country_sml_beef_mixed) +
						 my_emission_total (data = final_country_sml_dair_grass) +
						 my_emission_total (data = final_country_sml_dair_mixed) 

emission_total_sml = data.frame(final_country_chi_back[,1:5], emission_total_sml)
regional_emission_total_sml = regional_aggregation(table= emission_total_sml)



regional_emission_species = data.frame(regional_emission_total_chi[,1], rowSums(regional_emission_total_chi[,-1], na.rm=TRUE), 
                                       rowSums(regional_emission_total_pig[,-1], na.rm=TRUE), rowSums(regional_emission_total_buf[,-1], na.rm=TRUE),
                                       rowSums(regional_emission_total_cat[,-1], na.rm=TRUE), rowSums(regional_emission_total_sml[,-1], na.rm=TRUE))
names(regional_emission_species) = c("regions", "chicken", "pig", "buffalo", "cattle", "small_ruminant")
write.table(regional_emission_species, "5.Outputs/4.total/regional_emission_species.csv", sep=",", row.names = FALSE)


# Total_emissions_by country
final_emission_country = data.frame(final_country_chi_back[,1:5], rowSums(emission_country, na.rm = TRUE))
write.table(final_emission_country, "5.Outputs/4.total/total_emission_country.csv", sep=",", row.names = FALSE)

regional_emission = regional_aggregation(table= final_emission_country)
write.table(regional_emission, "5.Outputs/4.total/regional_total_emission.csv", sep=",", row.names = FALSE)

# Emission compounds by country
my_emission_compound <- function (data){
  n_emission <- data.frame(matrix(0, nrow(data), ncol = 4))
  n_emission[,1] <- rowSums(data[,c(22, 29, 35)]) #NH3
  n_emission[,2] <- rowSums(data[,c(25, 31, 32, 39)]) #NOx
  n_emission[,3] <- rowSums(data[,c(23,30)]) #N2O
  n_emission[,4] <- rowSums(data[,c(20,24,34,36:38)]) # NO3
  return(n_emission)
}

final_emission_compound = my_emission_compound (data = final_country_chi_back ) +
		 my_emission_compound (data = final_country_chi_broi      ) +
		 my_emission_compound (data = final_country_chi_layr      ) +
		 my_emission_compound (data = final_country_pig_back      ) +
		 my_emission_compound (data = final_country_pig_inte      ) +
		 my_emission_compound (data = final_country_pig_indu      ) +
		 my_emission_compound (data = final_country_buf_beef_grass) +
		 my_emission_compound (data = final_country_buf_beef_mixed) +
		 my_emission_compound (data = final_country_buf_dair_grass) +
		 my_emission_compound (data = final_country_buf_dair_mixed) +
		 my_emission_compound (data = final_country_cat_flot) +
		 my_emission_compound (data = final_country_cat_beef_grass) +
		 my_emission_compound (data = final_country_cat_beef_mixed) +
		 my_emission_compound (data = final_country_cat_dair_grass) +
		 my_emission_compound (data = final_country_cat_dair_mixed) +
		 my_emission_compound (data = final_country_sml_beef_grass) +
		 my_emission_compound (data = final_country_sml_beef_mixed) +
		 my_emission_compound (data = final_country_sml_dair_grass) +
		 my_emission_compound (data = final_country_sml_dair_mixed) 

final_emission_compound = data.frame(final_country_chi_back[,1:5], final_emission_compound)
write.table(final_emission_compound, "5.Outputs/4.total/emissions_country_compound.csv", sep=",", row.names = FALSE)

regional_emission_compound = regional_aggregation(table= final_emission_compound)
write.table(regional_emission_compound, "5.Outputs/4.total/regional_total_emission_compound.csv", sep=",", row.names = FALSE)

# Emission compound by stage
my_emission_compound_stage <- function (data){
	n_emission <- data.frame(matrix(0, nrow(data), ncol = 9))
	n_emission[,1] <- data[,22] #NH3 feed
	n_emission[,2] <- data[,25] #NOx feed
	n_emission[,3] <- data[,23] #N2O feed
	n_emission[,4] <- data[,24] #NO3 feed
	n_emission[,5] <- rowSums(data[,c(29, 35)]) # NH3_animal
	n_emission[,6] <- rowSums(data[,c(31, 32, 39)]) # NOx_animal
	n_emission[,7] <- data[,30] # N2O_animal
	n_emission[,8] <- rowSums(data[,c(34,36:38)]) # NO3_animal
	n_emission[,9] <- data[,20] # NO3_processing
	names(n_emission)= c("NH3_feed", "NOx_feed", "N2O_feed", "NO3_feed", "NH3_animal", "NOx_animal", "N2O_animal", "NO3_animal", "NO3_proces")
	return(n_emission)
}

emission_compound_stage = my_emission_compound_stage (data = final_country_chi_back ) +
		 my_emission_compound_stage (data = final_country_chi_broi      ) +
		 my_emission_compound_stage (data = final_country_chi_layr      ) +
		 my_emission_compound_stage (data = final_country_pig_back      ) +
		 my_emission_compound_stage (data = final_country_pig_inte      ) +
		 my_emission_compound_stage (data = final_country_pig_indu      ) +
		 my_emission_compound_stage (data = final_country_buf_beef_grass) +
		 my_emission_compound_stage (data = final_country_buf_beef_mixed) +
		 my_emission_compound_stage (data = final_country_buf_dair_grass) +
		 my_emission_compound_stage (data = final_country_buf_dair_mixed) +
		 my_emission_compound_stage (data = final_country_cat_flot) +
		 my_emission_compound_stage (data = final_country_cat_beef_grass) +
		 my_emission_compound_stage (data = final_country_cat_beef_mixed) +
		 my_emission_compound_stage (data = final_country_cat_dair_grass) +
		 my_emission_compound_stage (data = final_country_cat_dair_mixed) +
		 my_emission_compound_stage (data = final_country_sml_beef_grass) +
		 my_emission_compound_stage (data = final_country_sml_beef_mixed) +
		 my_emission_compound_stage (data = final_country_sml_dair_grass) +
		 my_emission_compound_stage (data = final_country_sml_dair_mixed) 

emission_compound_stage = data.frame(final_country_chi_back[,1:5], emission_compound_stage)
write.table(emission_compound_stage, "5.Outputs/4.total/emissions_compound_stage.csv", sep=",", row.names = FALSE)

regional_emission_compound_stage = regional_aggregation(table= emission_compound_stage)
write.table(regional_emission_compound_stage, "5.Outputs/4.total/regional_emission_compound_stage.csv", sep=",", row.names = FALSE)

# Emissions compounds by species

emission_compound_stage_chi = my_emission_compound_stage (data = final_country_chi_back ) +
						 my_emission_compound_stage (data = final_country_chi_broi) +
						 my_emission_compound_stage (data = final_country_chi_layr) 

emission_compound_stage_chi = data.frame(final_country_chi_back[,1:5], emission_compound_stage_chi)
regional_emission_compound_stage_chi  = regional_aggregation(table= emission_compound_stage_chi)

emission_compound_stage_pig = my_emission_compound_stage (data = final_country_pig_back) +
						 my_emission_compound_stage (data = final_country_pig_inte) +
						 my_emission_compound_stage (data = final_country_pig_indu) 

emission_compound_stage_pig = data.frame(final_country_chi_back[,1:5], emission_compound_stage_pig)
regional_emission_compound_stage_pig = regional_aggregation(table= emission_compound_stage_pig)

emission_compound_stage_buf = my_emission_compound_stage (data = final_country_buf_beef_grass) +
						 my_emission_compound_stage (data = final_country_buf_beef_mixed) +
						 my_emission_compound_stage (data = final_country_buf_dair_grass) +
						 my_emission_compound_stage (data = final_country_buf_dair_mixed) 

emission_compound_stage_buf = data.frame(final_country_chi_back[,1:5], emission_compound_stage_buf)
regional_emission_compound_stage_buf  = regional_aggregation(table= emission_compound_stage_buf)

emission_compound_stage_cat = my_emission_compound_stage (data = final_country_cat_flot) +
						 my_emission_compound_stage (data = final_country_cat_beef_grass) +
						 my_emission_compound_stage (data = final_country_cat_beef_mixed) +
						 my_emission_compound_stage (data = final_country_cat_dair_grass) +
						 my_emission_compound_stage (data = final_country_cat_dair_mixed) 

emission_compound_stage_cat = data.frame(final_country_chi_back[,1:5], emission_compound_stage_cat)
regional_emission_compound_stage_cat = regional_aggregation(table= emission_compound_stage_cat)

emission_compound_stage_sml =  my_emission_compound_stage (data = final_country_sml_beef_grass) +
						 my_emission_compound_stage (data = final_country_sml_beef_mixed) +
						 my_emission_compound_stage (data = final_country_sml_dair_grass) +
						 my_emission_compound_stage (data = final_country_sml_dair_mixed) 

emission_compound_stage_sml = data.frame(final_country_chi_back[,1:5], emission_compound_stage_sml)
regional_emission_compound_stage_sml = regional_aggregation(table= emission_compound_stage_sml)


#################################################
# Analysis of total losses per livestock system #
#################################################

nom_system <- c("Backyard_chicken",
	    "Broilers_Chicken",
	    "Layers_chicken",
	    "Backyard_pig",
	    "Intermediate_pig",
	    "Industrial_pig",
	    "Grazing_buffalo_meat",
	    "Mixed_buffalo_meat",
	    "Grazing_buffalo_milk",
	    "Mixed_Buffalo_milk",
	    "Beef_cattle_feedlot",
	    "Grazing_beef_cattle",
	    "Mixed_beef_cattle",
	    "Grazing_dairy_cattle",
	    "Mixed_dairy_cattle",
	    "Grazing_small_ruminant_meat",
	    "Mixed_small_ruminant_meat",
	    "Grazing_small_ruminant_milk",
	    "Mixed_small_ruminant_milk")

losses_systems <- data.frame((final_country_chi_back$total_losses),
		(final_country_chi_broi$total_losses),
		(final_country_chi_layr$total_losses),
		(final_country_pig_back$total_losses),
		(final_country_pig_inte$total_losses),
		(final_country_pig_indu$total_losses),
		(final_country_buf_beef_grass$total_losses),
		(final_country_buf_beef_mixed$total_losses),
		(final_country_buf_dair_grass$total_losses),
		(final_country_buf_dair_mixed$total_losses),
		(final_country_cat_flot$total_losses),
		(final_country_cat_beef_grass$total_losses),
		(final_country_cat_beef_mixed$total_losses),
		(final_country_cat_dair_grass$total_losses),
		(final_country_cat_dair_mixed$total_losses),
		(final_country_sml_beef_grass$total_losses),
		(final_country_sml_beef_mixed$total_losses),
		(final_country_sml_dair_grass$total_losses),
		(final_country_sml_dair_mixed$total_losses))/1000
names (losses_systems) = nom_system

regional_emission_system = regional_aggregation(table= data.frame(feed_chi_back[,1:5],losses_systems))
write.table(regional_emission_system, "5.Outputs/4.total/regional_emission_system.csv", sep=",", row.names = FALSE)


################ 
# Representation of losses for all countries 
colourCount = length(unique(aaa$Region))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

aaa = melt(regional_emission_system)

names(aaa) = c("Region", "syst", "losses")

windows(height=18, width=25)
ggplot(aaa, aes(x = reorder(syst, losses), y = losses, fill = Region )) + 
  geom_bar(stat="identity")+
  scale_fill_manual(values = getPalette(colourCount))+
  #scale_fill_brewer(name = "Side~Factor", palette = "Paired")+
  scale_y_continuous(breaks = seq(0, 14, by=2), limits=c(0, 14))+
  coord_flip() +
  theme(axis.text = element_text(size  =17)) +
  theme_bw(base_size = 15, base_family = "") +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(legend.position="right")+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  ylab("N emissions, Tg N y-1")
savePlot("7.Graphs/3.systems/All_system_contribution_by_region.emf",type = "emf")


#####################
## N use indicators #
#####################

# Life-cycle nitrogen use efficiency

lcnue_data = data.frame(final_country_chi_back[,15],
	  final_country_chi_broi[,15],
	  final_country_chi_layr[,15],
	  final_country_pig_back[,15],
	  final_country_pig_inte[,15],
	  final_country_pig_indu[,15],
	  final_country_buf_beef_grass[,15],
	  final_country_buf_beef_mixed[,15],
	  final_country_buf_dair_grass[,15],
	  final_country_buf_dair_mixed[,15],
	  final_country_cat_flot[,15],
	  final_country_cat_beef_grass[,15],
	  final_country_cat_beef_mixed[,15],
	  final_country_cat_dair_grass[,15],
	  final_country_cat_dair_mixed[,15],
	  final_country_sml_beef_grass[,15],
	  final_country_sml_beef_mixed[,15],
	  final_country_sml_dair_grass[,15],
	  final_country_sml_dair_mixed[,15])
lcnue_data[lcnue_data <= 1] = NA
lcnue_data[lcnue_data >= 100] = NA
sapply(lcnue_data,median, na.rm=TRUE)
med_lcnue = sapply(lcnue_data, quantile, na.rm=TRUE)
names(med_lcnue) = nom_system
write.table(med_lcnue, "5.Outputs/5.indicators/quartile_lcnue_data.csv", sep=",", row.names=FALSE)


## Regional analysis for mitigation potential


write.table(data.frame(feed_chi_back[,1:2], lcnue_data), "5.Outputs/5.indicators/lcnue_data.csv", sep=",", row.names=FALSE)

names(lcnue_data ) =  nom_system
lcnue_data = melt(lcnue_data)
lcnue_data = filter(lcnue_data, value>1)
lcnue_data = filter(lcnue_data, value<100)
#####
# Quantiles = 90th for LCNUE
lcnue_data %>% 
  group_by(variable) %>% 
  summarise(value = list(enframe(quantile(value, probs=c(0.90))))) %>% 
  unnest

lcnue_data$syst = lcnue_data$variable

# Basic box plot
windows(height=20, width=25)
ggplot(lcnue_data, aes(x=reorder(variable, value, FUN=median), y=value)) + 
		  geom_boxplot(aes(fill = syst))+ 
		  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE) +
		  scale_y_continuous(breaks = seq(0, 100, by=10), limits=c(0,100))+
		  coord_flip() +
		  theme(axis.text = element_text(size  =15)) +
		  theme_bw(base_size = 15, base_family = "") +
		  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
		  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
		  ylab("Life cycle nitrogen use efficiency, %")+
		  ggtitle("Life cycle nitrogen use efficiency")
savePlot("7.Graphs/5.indicator/LCNUE_boxplot.emf",type = "emf")

# Life-cycle net nitrogen balance

lcnnb_data = data.frame(final_country_chi_back[,16],
		  final_country_chi_broi[,16],
		  final_country_chi_layr[,16],
		  final_country_pig_back[,16],
		  final_country_pig_inte[,16],
		  final_country_pig_indu[,16],
		  final_country_buf_beef_grass[,16],
		  final_country_buf_beef_mixed[,16],
		  final_country_buf_dair_grass[,16],
		  final_country_buf_dair_mixed[,16],
		  final_country_cat_flot[,16],
		  final_country_cat_beef_grass[,16],
		  final_country_cat_beef_mixed[,16],
		  final_country_cat_dair_grass[,16],
		  final_country_cat_dair_mixed[,16],
		  final_country_sml_beef_grass[,16],
		  final_country_sml_beef_mixed[,16],
		  final_country_sml_dair_grass[,16],
		  final_country_sml_dair_mixed[,16])
write.table(data.frame(feed_chi_back[,1:2],lcnnb_data), "5.Outputs/5.indicators/lcnnb_data.csv", sep=",", row.names=FALSE)


lcnnb_data[lcnnb_data <= 0] = NA
lcnnb_data[lcnnb_data >= 300] = NA
sapply(lcnnb_data,median, na.rm=TRUE)

med_lcnnb = sapply(lcnnb_data, quantile, na.rm=TRUE)
names(med_lcnnb) = nom_system
write.table(med_lcnnb, "5.Outputs/5.indicators/quartile_lcnnb_data.csv", sep=",", row.names=FALSE)


write.table(data.frame(feed_chi_back[,1:2], lcnnb_data), "5.Outputs/5.indicators/lcnnb_data.csv", sep=",", row.names=FALSE)
names(lcnnb_data ) =  nom_system

lcnnb_data = melt(lcnnb_data)
lcnnb_data = filter(lcnnb_data, value>0.9)
lcnnb_data = filter(lcnnb_data, value<150)
lcnnb_data %>% 
  group_by(variable) %>% 
  summarise(value = list(enframe(quantile(value, probs=c(0.10))))) %>% 
  unnest

#lcnnb_data[is.na(lcnnb_data) | lcnnb_data == NA] = 0

lcnnb_data$syst = lcnnb_data$variable
#lcnnb_data$variable <- factor(lcnnb_data$variablee, levels = lcnnb_data$variable[order(sapply(lcnnb_data,median, na.rm=TRUE))])

#https://www.r-graph-gallery.com/267-reorder-a-variable-in-ggplot2/

windows (height=20, width=25)
ggplot(lcnnb_data, aes(x=reorder(variable, (value*-1), FUN=median, order = is.ordered(value)), y=value)) + 
  geom_boxplot(aes(fill = syst))+ 
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=18, size=3, show.legend = FALSE) +
#  stat_summary(fun.y=sd, colour="blue", geom="point", shape=18, size=3, show.legend = FALSE) +
  #scale_y_continuous(breaks = seq(0, 300, by=30), limits=c(0,300))+
  coord_flip(clip = "on") +
  theme(axis.text = element_text(size  =15)) +
  theme_bw(base_size = 15, base_family = "") +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  ylab("Life cycle net nitrogen balance, kg ha")+
  ggtitle("Life cycle net nutrient balance")
#ggsave("7.Graphs/5.indicator/LCNNB_boxplot.pdf", width = 20, height = 20, units = "cm")
savePlot("7.Graphs/5.indicator/1LCNNB_boxplot.emf",type = "emf")


######################################
## EMISSION FACTORS TRADED PRODUCTS ##
######################################

ef_pig <- (final_country_pig_inte$total_losses + final_country_pig_indu$total_losses)*1000000 / (animal_pig_inte[,25] + animal_pig_indu[,25])
ef_pig[is.na(ef_pig)|ef_pig ==NaN ]  <- 0

ef_egg <- final_country_chi_layr$total_losses *1000000/animal_chi_layr[,26]
ef_egg[is.na(ef_egg)|ef_egg ==NaN ]  <- 0

ef_chicken <- final_country_chi_broi$total_losses * 1000000 / animal_chi_broi[,25]
ef_chicken[is.na(ef_chicken)|ef_chicken ==NaN ]  <- 0

# Allocation ruminants 
my_allocation = function(table){
  output = data.frame(matrix(0, nrow(table),2))
  names(output) = c("meat", "milk")
  output[,1] = table$OUTPUT_MEAT /(table$OUTPUT_MEAT + table$OUTPUT_MILK)
  output[,2] = table$OUTPUT_MILK /(table$OUTPUT_MEAT + table$OUTPUT_MILK)
  output[is.na(output)|output ==NaN ]  <- 0

  return (output)
}

allo_she_dairy_grass = my_allocation (table = processing_she_dair_grass)
allo_she_dairy_mixed = my_allocation (table = processing_she_dair_mixed)


allo_cat_dairy_grass = my_allocation (table = processing_cat_dair_grass)
allo_cat_dairy_mixed = my_allocation (table = processing_cat_dair_mixed)


ef_sheep_beef <-(final_country_she_beef_grass$total_losses + 
				 final_country_she_beef_mixed$total_losses +
				 final_country_she_dair_grass$total_losses *  allo_she_dairy_grass [,1]+ 
				 final_country_she_dair_mixed$total_losses *  allo_she_dairy_mixed [,1])*1000000 /
				(animal_she_beef_grass[,25] + animal_she_beef_mixed[,25] + animal_she_dair_grass[,25] + animal_she_dair_mixed[,25])
ef_sheep_beef[is.na(ef_sheep_beef)|ef_sheep_beef ==NaN ]  <- 0

ef_sheep_dairy <- (final_country_she_dair_grass$total_losses *  allo_she_dairy_grass [,2]+ 
				 final_country_she_dair_mixed$total_losses *  allo_she_dairy_mixed [,2])*1000000 /
				 (animal_she_dair_grass[,26] + animal_she_dair_mixed[,26])
ef_sheep_dairy[is.na(ef_sheep_dairy)|ef_sheep_dairy ==NaN ]  <- 0

ef_cattle_beef <-(final_country_cat_flot$total_losses +
				 final_country_cat_beef_grass$total_losses + 
				 final_country_cat_beef_mixed$total_losses +
				 final_country_cat_dair_grass$total_losses *  allo_cat_dairy_grass [,1]+ 
				 final_country_cat_dair_mixed$total_losses *  allo_cat_dairy_mixed [,1])*1000000 /
				(animal_cat_beef_grass[,25] + animal_cat_beef_mixed[,25] + animal_cat_dair_grass[,25] + animal_cat_dair_mixed[,25])
ef_cattle_beef[is.na(ef_cattle_beef)|ef_cattle_beef ==NaN ]  <- 0

ef_cattle_dairy <- (final_country_cat_dair_grass$total_losses *  allo_cat_dairy_grass [,2]+ 
				    final_country_cat_dair_mixed$total_losses *  allo_cat_dairy_mixed [,2])*1000000 /
				   (animal_cat_dair_grass[,26] + animal_cat_dair_mixed[,26])
ef_cattle_dairy [is.na(ef_cattle_dairy )|ef_cattle_dairy  ==NaN ]  <- 0

emission_factor_animal = data.frame (animal_chi_back[,1], ef_pig,ef_egg, ef_chicken,ef_sheep_beef,ef_sheep_dairy, ef_cattle_beef, ef_cattle_dairy)
names(emission_factor_animal) = c("ADM0_CODE", "pig_meat", "egg", "chicken_meat", "meat_sheep", "milk_sheep", "meat_cattle", "milk_cattle")
write.table(emission_factor_animal, "emission_factor_animal.csv", sep=",", row.names = FALSE)


# Global Nitrogen flows - Fig. 1
# Sum of the inputs - feed

global_feed = colSums((feed_chi_back_allo[,-c(1:5)] +
		feed_chi_broi_allo[,-c(1:5)] + 
		feed_chi_layr_allo[,-c(1:5)] + 
		feed_pig_back_allo[,-c(1:5)] + 
		feed_pig_inte_allo[,-c(1:5)] + 
		feed_pig_indu_allo[,-c(1:5)] + 
		feed_cat_flot_allo[,-c(1:5)] + 
		feed_buf_beef_grass_allo[,-c(1:5)] +
		feed_buf_beef_mixed_allo[,-c(1:5)] +
		feed_buf_dair_grass_allo[,-c(1:5)] +
		feed_buf_dair_mixed_allo[,-c(1:5)] +
		feed_cat_beef_grass_allo[,-c(1:5)] +
		feed_cat_beef_mixed_allo[,-c(1:5)] +
		feed_cat_dair_grass_allo[,-c(1:5)] +
		feed_cat_dair_mixed_allo[,-c(1:5)] +
		feed_sml_beef_grass_allo[,-c(1:5)] +
		feed_sml_beef_mixed_allo[,-c(1:5)] +
		feed_sml_dair_grass_allo[,-c(1:5)] +
		feed_sml_dair_mixed_allo[,-c(1:5)] )/10^9, na.rm = FALSE)
write.table(global_feed, "global_feed.csv", col.names = FALSE, sep=",")


n_fixation = (global_feed[5]+global_feed[6])



# Global flows 

global_flows = colSums((final_country_chi_back[,-c(1:17)]+
		final_country_chi_broi[,-c(1:17)]+
		final_country_chi_layr[,-c(1:17)]+
		final_country_pig_back[,-c(1:17)]+
		final_country_pig_inte[,-c(1:17)]+
		final_country_pig_indu[,-c(1:17)]+
		final_country_buf_beef_grass[,-c(1:17)]+
		final_country_buf_beef_mixed[,-c(1:17)] +
		final_country_buf_dair_grass[,-c(1:17)] +
		final_country_buf_dair_mixed[,-c(1:17)] +
		final_country_cat_flot[,-c(1:17)] +
		final_country_cat_beef_grass[,-c(1:17)] +
		final_country_cat_beef_mixed[,-c(1:17)] +
		final_country_cat_dair_grass[,-c(1:17)] +
		final_country_cat_dair_mixed[,-c(1:17)] +
		final_country_sml_beef_grass[,-c(1:17)] +
		final_country_sml_beef_mixed[,-c(1:17)] +
		final_country_sml_dair_grass[,-c(1:17)] +
		final_country_sml_dair_mixed[,-c(1:17)])/10^3, na.rm = FALSE)
write.table(global_flows, "global_flows.csv", col.names = FALSE, sep=",")

glo_anim_flows = colSums((animal_chi_back[,c(6:23)]+
		animal_chi_broi[,c(6:23)]+
		animal_chi_layr[,c(6:23)]+
		animal_pig_back[,c(6:23)]+
		animal_pig_inte[,c(6:23)]+
		animal_pig_indu[,c(6:23)]+
		animal_buf_beef_grass[,c(6:23)]+
		animal_buf_beef_mixed[,c(6:23)] +
		animal_buf_dair_grass[,c(6:23)] +
		animal_buf_dair_mixed[,c(6:23)] +
		animal_cat_flot[,c(6:23)] +
		animal_cat_beef_grass[,c(6:23)] +
		animal_cat_beef_mixed[,c(6:23)] +
		animal_cat_dair_grass[,c(6:23)] +
		animal_cat_dair_mixed[,c(6:23)] +
		animal_sml_beef_grass[,c(6:23)] +
		animal_sml_beef_mixed[,c(6:23)] +
		animal_sml_dair_grass[,c(6:23)] +
		animal_sml_dair_mixed[,c(6:23)])/10^9, na.rm = FALSE)
write.table(glo_anim_flows, "global_anim_flows.csv", col.names = FALSE, sep=",")


glo_swill_flows = colSums((animal_chi_back[,c(6:23)]+animal_pig_back[,c(6:23)]), na.rm = FALSE)

glo_proc_flows = colSums((processing_chi_back[,c(6:8)]+
		processing_chi_broi[,c(6:8)]+
		processing_chi_layr[,c(6:8)]+
		processing_pig_back[,c(6:8)]+
		processing_pig_inte[,c(6:8)]+
		processing_pig_indu[,c(6:8)]+
		processing_buf_beef_grass[,c(6:8)]+
		processing_buf_beef_mixed[,c(6:8)] +
		processing_buf_dair_grass[,c(6:8)] +
		processing_buf_dair_mixed[,c(6:8)] +
		processing_cat_flot[,c(6:8)] +
		processing_cat_beef_grass[,c(6:8)] +
		processing_cat_beef_mixed[,c(6:8)] +
		processing_cat_dair_grass[,c(6:8)] +
		processing_cat_dair_mixed[,c(6:8)] +
		processing_sml_beef_grass[,c(6:8)] +
		processing_sml_beef_mixed[,c(6:8)] +
		processing_sml_dair_grass[,c(6:8)] +
		processing_sml_dair_mixed[,c(6:8)])/10^9, na.rm = FALSE)
write.table(glo_proc_flows, "global_proc_flows.csv", col.names = FALSE, sep=",")

