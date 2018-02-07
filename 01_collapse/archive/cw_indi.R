cw_indi <- function(mydat = ptdat, var_family = indi_fam, agg = agg_level, condition = conditional) {
  if (var_family == 'hw') {
    message('No indi to CW!')
    return(mydat)
  }

  if (var_family == 'water') {
  
    attach(mydat)
      if ((mean(spring_imp, na.rm = T) + mean(spring_unimp, na.rm = T)) == 0) {
        mydat$spring_cw <- 0
        ratio_sp <- 1
      } else {
        ratio_sp <- mean(spring_imp, na.rm = T)/(mean(spring_imp, na.rm = T) + mean(spring_unimp, na.rm = T))
      }
      
      if ((mean(well_imp, na.rm = T) + mean(well_unimp, na.rm = T)) == 0) {
        mydat$well_cw <- 0
        ratio_wl <- 1
      } else {
        ratio_wl <- mean(well_imp, na.rm = T)/(mean(well_imp, na.rm = T) + mean(well_unimp, na.rm = T)) 
      }
    detach(mydat)
  
    if (agg == 'country') {

      # mdg way
      #mydat <- mydat %>%
      #  mutate(unimp = bottled + bottled_sp*(1 - ratio_sp) + bottled_wl*(1 - ratio_wl) +
      #           spring_unimp + spring_cw*(1 - ratio_sp) + well_cw*(1 - ratio_wl) +
      #           well_unimp + unimp,
      #         imp = bottled_sp*(ratio_sp) + bottled_wl*(ratio_wl) + well_imp + well_cw*(ratio_wl) +
      #           spring_cw*(ratio_sp) + spring_imp + imp) %>%
      #  dplyr::select(nid, iso3, survey_series, year_start, total_hh,
      #         piped, surface, imp, unimp)

      # sdg way
      mydat <- mydat %>%
        mutate(unimp = spring_unimp + spring_cw*(1 - ratio_sp) + well_cw*(1 - ratio_wl) +
                well_unimp + unimp,
               imp = well_imp + well_cw*(ratio_wl) + spring_cw*(ratio_sp) + spring_imp + imp) %>%
        dplyr::select(nid, iso3, survey_series, year_start, total_hh,
               piped, surface, imp, unimp)
    } else {
      
      # mdg way
      #mydat <- mydat %>%
      #     mutate(unimp = bottled + bottled_sp*(1 - ratio_sp) + bottled_wl*(1 - ratio_wl) +
      #                    spring_unimp + spring_cw*(1 - ratio_sp) + well_cw*(1 - ratio_wl) +
      #                    well_unimp + unimp,
      #            imp = bottled_sp*(ratio_sp) + bottled_wl*(ratio_wl) + well_imp + well_cw*(ratio_wl) +
      #                  spring_cw*(ratio_sp) + spring_imp + imp) %>%
      #     dplyr::select(id_short, nid, iso3, lat, long, shapefile, location_code, survey_series, urban, year_start, total_hh,
      #            piped, surface, imp, unimp)
     
      # sdg way
       mydat <- mydat %>%
         mutate(unimp = spring_unimp + spring_cw*(1 - ratio_sp) + well_cw*(1 - ratio_wl) +
                        well_unimp + unimp,
                imp = well_imp + well_cw*(ratio_wl) +
                      spring_cw*(ratio_sp) + spring_imp + imp) %>%
         dplyr::select(id_short, nid, iso3, lat, long, shapefile, location_code, survey_series, urban, year_start, total_hh,
                piped, surface, imp, unimp)
    }
    return(mydat)
  } 

  if (var_family == 'sani') {
    if (condition == 'unconditional') {
      attach(mydat)
        if ((mean(latrine_imp, na.rm = T) + mean(latrine_unimp, na.rm = T)) == 0) {
            mydat$latrine_cw <- 0
            ratio_lt <- 1
          } else {
            ratio_lt <- mean(latrine_imp, na.rm = T)/(mean(latrine_imp, na.rm = T) + mean(latrine_unimp, na.rm = T))
          }

      detach(mydat)

      if (agg == 'country') {
        mydat <- mydat %>%
          mutate(unimp = unimp + latrine_unimp + latrine_cw*(1-ratio_lt),
                 imp = imp + latrine_imp + latrine_cw*(ratio_lt)) %>%
          dplyr::select(nid, iso3, survey_series, year_start, total_hh,
                 imp, unimp, od)
      } else {
        mydat <- mydat %>%
             mutate(unimp = unimp + latrine_unimp + latrine_cw*(1-ratio_lt),
                    imp = imp + latrine_imp + latrine_cw*(ratio_lt)) %>%
             dplyr::select(id_short, nid, iso3, lat, long, shapefile, location_code, survey_series, urban, year_start, total_hh,
                    imp, unimp, od)
      }
      return(mydat)
    }

    if (condition == 'conditional') {
      message('No indi to CW!')
      return(mydat)
    }
  }
}


cw_indi_reg <- function(data, var_family = indi_fam) {

  library(dplyr)

  # message('Only African Data is currently CWed by reg')
  message('The regs are sssa_hi, cssa, wsssa, name_hi, and essa_hilo')
  sssa_hi <- c('NAM','BWA','ZAF')
  cssa <- c('CAF','GAB','GNQ','COD','COG','AGO','STP')
  name_hi <- c('MAR','DZA','TUN','LBY','EGY')
  essa_hilo <- c('SDN','ERI','DJI','SOM','ETH','SSD',
                 'SSD','UGA','KEN','RWA','BDI','TZA',
                 'MWI','MOZ','ZMB','MDG','ZWE','SWZ','LSO',
                 'COM')
  wssa <- c('CPV','SEN','GMB','GIN','GNB','SLE','MLI','LBR',
            'CIV','GHA','TGO','BEN','NGA','NER','TCD','CMR',
            'BFA','MRT')
  africa <- c(sssa_hi, cssa, name_hi, essa_hilo, wssa)

  results <- list()
  
  message('sssa_hi')
  mydat <- filter(data, iso3 %in% sssa_hi)
  if (nrow(mydat)>0) {
    results[[1]] <- cw_indi(mydat = mydat, var_family = var_family)
  }

  message('wssa')
  mydat <- filter(data, iso3 %in% wssa)
  if (nrow(mydat)>0) {
    results[[2]] <- cw_indi(mydat = mydat, var_family = var_family)
  }

  message('cssa')
  mydat <- filter(data, iso3 %in% cssa)
  if (nrow(mydat)>0) {
    results[[3]] <- cw_indi(mydat = mydat, var_family = var_family)
  }

  message('essa_hilo')
  mydat <- filter(data, iso3 %in% essa_hilo)
  if (nrow(mydat)>0) {
    results[[4]] <- cw_indi(mydat = mydat, var_family = var_family)
  }

  message('name_hi')
  mydat <- filter(data, iso3 %in% name_hi)
  if (nrow(mydat)>0) {
    results[[5]] <- cw_indi(mydat = mydat, var_family = var_family)
  }

  message('non africa')
  mydat <- filter(data, !(iso3 %in% africa))
  if (nrow(mydat)>0) {
    results[[6]] <- cw_indi(mydat = mydat, var_family = var_family)
  }

  results <- do.call(rbind, results)
  return(results)
}

cw_indi_reg_time <- function(data = ptdat) {
  message('The periods are pre-2000,00-04,05-09,10-15')

  period1 <- 2000:2004
  period2 <- 2005:2009
  results <- list()

  library(dplyr)
  message('Period: pre-2000')
  mydat <- filter(data, year_start < 2000)
  if (nrow(mydat)>0) {
    results[[1]] <- cw_indi_reg(mydat)
  }

  message('Period: 00-04')
  mydat <- filter(data, year_start <= 2004 & year_start >= 2000)
  if (nrow(mydat)>0) {
    results[[2]] <- cw_indi_reg(mydat)
  }

  message('Period: 05-09')
  mydat <- filter(data, year_start <= 2009 & year_start >= 2005)
  if (nrow(mydat)>0) {
    results[[3]] <- cw_indi_reg(mydat)
  }

  message('Period: 10-15')
  mydat <- filter(data, year_start >= 2010)
  if (nrow(mydat)>0) {
    results[[4]] <- cw_indi_reg(mydat)
  }
 results <- do.call(rbind, results)
 return(results)
}