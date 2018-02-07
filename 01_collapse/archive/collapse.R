#### Set Up Environment ####
# Clear environment
rm(list = ls())

# Define if you are running code loally
local <- F

# Set repo & library path 
if(Sys.info()[1]!="Windows") {
  if(!local) {
    root <- "/home/j/"
    package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                          paste0(root,'temp/geospatial/geos_packages'),
                          paste0(root,'temp/geospatial/packages'))
    .libPaths(package_lib)
  } else {
    package_lib <- .libPaths()
    root <- '/home/j/'
  }
} else {
  package_lib <- .libPaths()
  root <- 'J:/'
}

repo <- ifelse(Sys.info()[1]=="Windows", 'C:/Users/adesh/Documents/WASH/wash_code/01_collapse/',
               ifelse(local, '/home/adesh/Documents/wash_mapping/01_collapse',
                '/share/code/geospatial/adesh/wash_mapping/01_collapse/'))

# Detach all but base packages
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices",
                      "package:utils","package:datasets","package:methods",
                      "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) 
    detach(package, character.only=TRUE)
}
detachAllPackages()

# Load and install, if necessary, needed packages
packages <- c('dplyr', 'feather')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = T)

#### Load functions ####
for (data_type in c("pt","poly")){
  message(paste("Loading",data_type, "data"))
  rm(pt_collapse)
  message('Loading Data...')
  # Load data
  if (!("pt_collapse" %in% ls()) & data_type == 'pt') {
    pt_collapse <- read_feather(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_2018_01_02.feather'))
    Encoding(pt_collapse$w_source_drink) <- "UTF-8"
    Encoding(pt_collapse$w_source_other) <- "UTF-8"
    Encoding(pt_collapse$t_type) <- "UTF-8"
    pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    pt_collapse$t_type <- tolower(pt_collapse$t_type)
  } 
    
  if (!("pt_collapse" %in% ls()) & data_type == 'poly') {
    pt_collapse <- read_feather(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly_2018_01_02.feather'))
    Encoding(pt_collapse$w_source_drink) <- "UTF-8"
    Encoding(pt_collapse$w_source_other) <- "UTF-8"
    Encoding(pt_collapse$t_type) <- "UTF-8"
    pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    pt_collapse$t_type <- tolower(pt_collapse$t_type)
  }

  for (indi_fam in c('water','sani')) {
    rm(definitions)

    for (agg_level in c('country','')) {
      message(paste("Collapsing",indi_fam, "with", agg_level, "agg_level"))
      message('Loading Definitions...')
      if (!("definitions" %in% ls())) {
        if (indi_fam == "sani") {
          definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/t_type_defined_2017_12_18.csv'),
                                  encoding="windows-1252", stringsAsFactors = F)
          definitions <- select(definitions, string, sdg)
        } else {
          definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/w_source_defined_2017_12_18.csv'),
                                  encoding="windows-1252", stringsAsFactors = F) 
          definitions2 <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/w_other_defined_2017_12_18.csv'),
                                   encoding="windows-1252", stringsAsFactors = F)
          definitions2 <- rename(definitions2, sdg2 = sdg)
          definitions <- select(definitions, string, sdg, jmp)
        }
      }

      definitions$string <- iconv(definitions$string, 'windows-1252', 'UTF-8')
      definitions$string <- tolower(definitions$string)
      definitions$sdg <- ifelse(definitions$sdg == "", NA, definitions$sdg)
      definitions$string <- ifelse(definitions$sdg == "", NA, definitions$string)
      definitions$sdg <- ifelse(is.na(definitions$string), NA, definitions$sdg)
      if (indi_fam == "water") {
        definitions$jmp <- ifelse(is.na(definitions$string), NA, definitions$jmp)
      }
      definitions <- distinct(definitions)
      
      if (exists('definitions2')) {
        definitions2$string <- iconv(definitions2$string, 'windows-1252', 'UTF-8')
        definitions2$string <- tolower(definitions2$string)
        definitions2 <- distinct(definitions2)
      }

      rm(list = setdiff(ls(),c('definitions','pt_collapse','definitions2','indi_fam',
        'repo','data_type','root','agg_level', 'sdg')))

      message("Importing functions...")
      setwd(repo)
      source('functions/initial_cleaning.R')
      source('functions/hh_cw.R')
      source('functions/address_missing.R')
      source('functions/cw_indi.R')
      source('functions/agg_wash.R')
      source('functions/define_wash.R')

      #### Subset & Shape Data ####
      message("Initial Cleaning...")
      temp_list <- initial_cleaning()
      ptdat <- temp_list[[1]]; ptdat_0 <- temp_list[[2]]; rm(temp_list)

      #### Define Indicator ####
      message("Defining Indicator...")
      ptdat <- define_indi(sdg_indi = T)

      #### Address Missingness ####
      message("Addressing Missingness...")
      
      # Remove clusters with more than 20% weighted missingness
      ptdat <- rm_miss()

      # Remove cluster_ids with missing hhweight or invalid hhs
      miss_wts <- unique(ptdat$id_short[which(is.na(ptdat$hhweight))])
      ptdat <- filter(ptdat, !(id_short %in% miss_wts))

      invalid_hhs <- unique(ptdat$id_short[which(ptdat$hh_size <= 0)])
      ptdat <- filter(ptdat, !(id_short %in% invalid_hhs))

      # Crosswalk missing household size data
      message("Crosswalking HH Sizes...")
      ptdat <- hh_cw_reg(data = ptdat)

      # Calculated household size weighted means for all clusters
      # Assign observations with NA indicator value the weighted average for the cluster
      message("Imputing indicator...")
      ptdat <- impute_indi()

      #### Aggregate Data ####
      # Bookmarking dataset so it can be looped over for conditional switch
      ptdat_preagg <- ptdat
      
      # Conditional switch is to switch collapsing for conditional vs unconditional indicators
      for (conditional in c('unconditional')) {
        # Reseting the dataset to preagregate
        ptdat <- ptdat_preagg
        message(paste("Conditional variables status:",conditional))
        # Aggregate indicator to cluster level
        message("Aggregating Data...")
        ptdat <- agg_indi()

        # Crosswalk indicator data
        message("Crosswalking Indicators...")
        ptdat <- cw_indi_reg_time(data = ptdat)

        # create sdg improved for sdg era
        if(indi_fam == 'water') {ptdat$sdg_imp <- ptdat$piped + ptdat$imp}
        
        #save poly and point collapses
        message("Saving Collapsed Data...")
        today <- gsub("-", "_", Sys.Date())
          
        if (data_type == "poly") {
          polydat <- ptdat
          rm(ptdat)
          write_feather(polydat, paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_",
                        indi_fam, '_', conditional, '_', agg_level, '_', today, ".feather"))
        } else{
          write_feather(ptdat, paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_",
                        indi_fam, '_', conditional, '_', agg_level, '_', today, ".feather"))
        }
      }
    }
  }
}