# get the coordinates using the package rgbif
# Loop through the museums with more than 1000 records
library(rgbif)
voucher.rgbif <- function(database, threshold){
        DB <- database
        temp_mus_ID <- sort(table(DB$Museum_id), decreasing = T)
        mus_ID <- names(temp_mus_ID)[which(temp_mus_ID > threshold)]
        DB_thold <- DB[which(DB$Museum_id %in% mus_ID ),]
        DB_thold$Sp_bi <- gsub("_", " ", gsub("([A-Za-z]+_[A-Za-z]+).*", "\\1",DB_thold$Species))
        mus_sp <- unique(DB_thold$Sp_bi)
        list_sp_mus <-list()
        for(sp in seq_along(mus_sp)){
                sci_name <- mus_sp[sp]
                tmp_DB <- subset(DB_thold, DB_thold$Sp_bi == sci_name)
                mus_ID_sp <- mus_ID[which(mus_ID %in% unique(tmp_DB$Museum_id))]
                sp_hits <- data.frame(matrix(nrow=0, ncol=5))
                colnames(sp_hits) <- c("Species", "Lat", "Long", "Inst_code",
                                       "Catalog_num")
                tmp_search <- occ_search(scientificName = sci_name, institutionCode = mus_ID_sp)
                if (length(mus_ID_sp) == 1){
                        tmp_names <- mus_ID_sp
                        tmp_count <- temp_search$meta$count
                        if (tmp_count > 0){
                                Catalog_num <- tmp_search$data$catalogNumber
                                tmp_DB_mus <- tmp_DB[tmp_DB$Museum_id == tmp_names[mus],]
                                m <- match(tmp_DB_mus$Number_id, gsub(".*([0-9]*)*.*", "\\1", Catalog_num))
                                m <- as.vector(na.omit(m))
                                coor_logic <- "decimalLatitude" %in% colnames(tmp_data)
                                if(length(m) > 0 && coor_logic == TRUE){
                                        Species <- tmp_search$data$species[m]
                                        Lat <- tmp_search$data$decimalLatitude[m]
                                        Long <- tmp_search$data$decimalLongitude[m]
                                        Inst_code <- tmp_search$data$institutionCode[m]
                                        Catalog_num <- tmp_search$data$catalogNumber[m]
                                        hit_tbl <- cbind(Species, Lat, 
                                                         Long, Inst_code, 
                                                         Catalog_num)
                                        if (dim(hit_tbl)[1] > 0){
                                                sp_hits <- rbind(sp_hits, hit_tbl)
                                        }
                                }
                        }
                        if(dim(sp_hits)[1] > 0){
                                list_sp_mus[[sp]] <- sp_hits
                        }
                }
                if (length(mus_ID_sp) > 1){
                        tmp_names <- names(tmp_search)
                        for (mus in seq_along(tmp_names)){
                        # if the museum has zero hits dont loop through it
                        # Focus now in the hits with lat and long, only chech hits with lat != NA
                        # if the hit has lat and long then match the catalog number and extract the positive hits
                                tmp_count <- tmp_search[mus][[tmp_names[mus]]]$meta$count
                                if (tmp_count > 0){
                                        tmp_data <- tmp_search[mus][[tmp_names[mus]]]$data
                                        coor_logic <- "decimalLatitude" %in% colnames(tmp_data)
                                        if (coor_logic == TRUE){
                                                Catalog_num <- tmp_data$catalogNumber[!is.na(tmp_data$decimalLatitude)]
                                                tmp_DB_mus <- tmp_DB[tmp_DB$Museum_id == tmp_names[mus],]
                                                m <- match(tmp_DB_mus$Number_id, gsub(".*([0-9]*)*.*", "\\1", Catalog_num))
                                                m <- as.vector(na.omit(m))
                                                if(length(m) > 0){
                                                        Species <- tmp_data$species[m]
                                                        Lat <- tmp_data$decimalLatitude[m]
                                                        Long <- tmp_data$decimalLongitude[m]
                                                        Inst_code <- tmp_data$institutionCode[m]
                                                        Catalog_num <- tmp_data$catalogNumber[m]
                                                        hit_tbl <- cbind(Species, Lat, 
                                                                         Long, Inst_code, 
                                                                         Catalog_num)
                                                        if (dim(hit_tbl)[1] > 0){
                                                                sp_hits <- rbind(sp_hits, hit_tbl)
                                                        }
                                                }
                                        }
                                }
                        }
                        if(dim(sp_hits)[1] > 0){
                                list_sp_mus[[sp]] <- sp_hits
                        }
                }
                print(paste(sp, " out of ", length(mus_sp)))
        }
        list_sp_mus
}
rgbif_coor <- voucher.rgbif(database = database, 1000)
