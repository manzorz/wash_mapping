.libPaths('/share/code/geospatial/adesh/r_packages')
rm(list = ls())
indic <- as.character(commandArgs()[4])

if (indic == 'water') {
    run_date <- as.character(commandArgs()[5])
    for (region in c('cssa','essa_hilo','name_hi','sssa_hi','wssa')) {
        for (indicator in c('w_piped', 'w_imp_cr','w_unimp_cr')) {
            message(region); message(indicator); 
            
            message('loading data...')
            setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
            files <- list.files()[grep(pattern = 'cell_draws', x = list.files())]
            preds <- load(files[grep(x = files, pattern = region)])
            del_obj <- preds
            preds <- get(preds)
            assign(indicator, preds); rm(preds)
            rm(list = paste(del_obj)); rm(del_obj);
        }

        message('calculating...')
        w_imp_calc <- (1 - w_piped) * w_imp_cr; rm(w_imp_cr)
        w_unimp_calc <- (1 - w_piped - w_imp_calc) * w_unimp_cr; rm(w_unimp_cr)
        w_surface_calc <- 1 - w_piped - w_imp_calc - w_unimp_calc

        message('saving w_imp_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_imp_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_imp_calc/output/',run_date))
        save(w_imp_calc, file = paste0("w_imp_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_imp_calc)

        message('saving w_unimp_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_unimp_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_unimp_calc/output/',run_date))
        save(w_unimp_calc, file = paste0("w_unimp_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_unimp_calc)

        message('saving w_surface_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_surface_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_surface_calc/output/',run_date))
        save(w_surface_calc, file = paste0("w_surface_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_surface_calc); rm(w_piped)
        
    }    
}

if (indic == 'sani') {
    run_date <- as.character(commandArgs()[5])
    for (region in c('cssa','essa_hilo','name_hi','sssa_hi','wssa')) {
        for (indicator in c('s_imp', 's_unimp_cr')) {
            message(region); message(indicator); 
            
            message('loading data...')
            setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
            files <- list.files()[grep(pattern = 'cell_draws', x = list.files())]
            preds <- load(files[grep(x = files, pattern = region)])
            del_obj <- preds
            preds <- get(preds)
            assign(indicator, preds); rm(preds)
            rm(list = paste(del_obj)); rm(del_obj);
        }

        message('calculating...')
        s_unimp_calc <- (1 - s_imp) * s_unimp_cr; rm(s_unimp_cr)
        s_od_calc <- 1 - s_imp - s_unimp_calc

        message('saving s_unimp_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_unimp_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_unimp_calc/output/',run_date))
        save(s_unimp_calc, file = paste0("s_unimp_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_unimp_calc)

        message('saving s_od_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_od_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_od_calc/output/',run_date))
        save(s_od_calc, file = paste0("s_od_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_od_calc); rm(s_imp)
        
    }
}

if (indic == 'both') {
    run_date <- commandArgs()[5]
    for (region in c('cssa','essa_hilo','name_hi','sssa_hi','wssa')) {
        for (indicator in c('w_piped', 'w_imp_cr','w_unimp_cr')) {
            message(region); message(indicator); 
            
            message('loading data...')
            setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
            files <- list.files()[grep(pattern = 'cell_draws', x = list.files())]
            preds <- load(files[grep(x = files, pattern = region)])
            del_obj <- preds
            preds <- get(preds)
            assign(indicator, preds); rm(preds)
            rm(list = paste(del_obj)); rm(del_obj);
        }

        message('calculating...')
        w_imp_calc <- (1 - w_piped) * w_imp_cr; rm(w_imp_cr)
        w_unimp_calc <- (1 - w_piped - w_imp_calc) * w_unimp_cr; rm(w_unimp_cr)
        w_surface_calc <- 1 - w_piped - w_imp_calc - w_unimp_calc

        message('saving w_imp_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_imp_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_imp_calc/output/',run_date))
        save(w_imp_calc, file = paste0("w_imp_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_imp_calc)

        message('saving w_unimp_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_unimp_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_unimp_calc/output/',run_date))
        save(w_unimp_calc, file = paste0("w_unimp_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_unimp_calc)

        message('saving w_surface_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_surface_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_surface_calc/output/',run_date))
        save(w_surface_calc, file = paste0("w_surface_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_surface_calc); rm(w_piped)
        
    }

    run_date <- as.character(commandArgs()[5])
    for (region in c('cssa','essa_hilo','name_hi','sssa_hi','wssa')) {
        for (indicator in c('s_imp', 's_unimp_cr')) {
            message(region); message(indicator); 
            
            message('loading data...')
            setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
            files <- list.files()[grep(pattern = 'cell_draws', x = list.files())]
            preds <- load(files[grep(x = files, pattern = region)])
            del_obj <- preds
            preds <- get(preds)
            assign(indicator, preds); rm(preds)
            rm(list = paste(del_obj)); rm(del_obj);
        }

        message('calculating...')
        s_unimp_calc <- (1 - s_imp) * s_unimp_cr; rm(s_unimp_cr)
        s_od_calc <- 1 - s_imp - s_unimp_calc

        message('saving s_unimp_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_unimp_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_unimp_calc/output/',run_date))
        save(s_unimp_calc, file = paste0("s_unimp_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_unimp_calc)

        message('saving s_od_calc...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_od_calc/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_od_calc/output/',run_date))
        save(s_od_calc, file = paste0("s_od_calc_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_od_calc); rm(s_imp)
        
    }

}
