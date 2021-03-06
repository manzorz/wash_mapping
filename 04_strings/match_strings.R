#source("/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/04_strings/match_strings.R")
package_lib <- '/snfs1/temp/geospatial/geos_packages'
.libPaths(package_lib)
library(magrittr)
library(data.table)
j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")

message("Rounding up necessary file paths")
most_recent <- list.files(paste0(j, "WORK/11_geospatial/wash/definitions"), full.names = T, pattern=".csv$") %>% grep(value=T, ignore.case=T, pattern="IPUMS", invert=T) %>% grep(value=T, ignore.case=T, pattern="t_|w_")

most_recent_water <- grep(most_recent, pattern="w_source", value=T) %>% tail(1)
most_recent_wother <- grep(most_recent, pattern="w_other", value=T) %>% tail(1)
most_recent_toilet <- grep(most_recent, pattern="t_type", value=T) %>% tail(1)

w <- read.csv(most_recent_water, stringsAsFactors=F, encoding = 'windows-1252')
w_o <- read.csv(most_recent_wother, stringsAsFactors=F, encoding = 'windows-1252')
t <- read.csv(most_recent_toilet, stringsAsFactors=F, encoding = 'windows-1252')

most_recent_extracts <- list.files(paste0(j, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/"), full.names = T, pattern=".Rdata$") %>% grep(value=T, pattern="poly|points", invert=T)
extract_info <- file.info(most_recent_extracts)
extract_info$path <- rownames(extract_info)
extract_info <- data.table(extract_info)
most_recent_extract <- extract_info[order(mtime, decreasing = T), path][1]

message("Loading big extraction .Rdata")
load(most_recent_extract)
#called all. should rename to packaged. subset only to surveys that have data at or beyond 1997
packaged <- all[all$year_end >= 1997, ]
#rm(all)

message("Making data.frames of new strings")
new_w <- packaged$w_source_drink %>% unique
new_wo <- packaged$w_source_other %>% unique
new_t <- packaged$t_type %>% unique

new_w <- new_w[!(new_w %in% w$string)]
new_wo <- new_wo[!(new_wo %in% w_o$string) & !is.na(new_wo)]
new_t <- new_t[!(new_t %in% t$string) & !is.na(new_t)]

new_w <- as.data.frame(new_w, col.names="string", stringsAsFactors=F)
new_wo <- as.data.frame(new_wo, col.names="string", stringsAsFactors=F)
new_t <- as.data.frame(new_t, col.names="string", stringsAsFactors=F)
colnames(new_w)[1] <- "string"
colnames(new_wo)[1] <- "string"
colnames(new_t)[1] <- "string"

message("Attaching new data.frames to old ones")
w <- rbindlist(list(w, new_w), fill=T, use.names=T)
w_o <- rbindlist(list(w_o, new_wo), fill=T, use.names=T)
t <- rbindlist(list(t, new_t), fill=T, use.names=T)

today <- Sys.Date() %>% gsub(pattern="-", replace="_")

message("Writing to J")
write.csv(w, paste0(j, "WORK/11_geospatial/wash/definitions/w_source_defined_", today, ".csv"), row.names=F, na="")
write.csv(w_o, paste0(j, "WORK/11_geospatial/wash/definitions/w_other_defined_", today, ".csv"), row.names=F, na="")
write.csv(t, paste0(j, "WORK/11_geospatial/wash/definitions/t_type_defined_", today, ".csv"), row.names=F, na="")
