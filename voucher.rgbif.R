# get the coordinates using the package rgbif
# Loop through the museums with more than 1000 records
temp_mus_ID <- sort(table(museum_id$Museum_id), decreasing = T)
Mus_ID <- names(temp_mus_ID)[which(temp_mus_ID > 1000)]

library(rgbif)
voucher.rgbif <- function(database, threshold){
        DB <- database
        temp_mus_ID <- sort(table(DB$Museum_id), decreasing = T)
        mus_ID <- names(temp_mus_ID)[which(temp_mus_ID > threshold)]
        DB_thold <- DB[which(DB$Museum_id %in% mus_ID ),]
        DB_thold$Sp_bi <- gsub("_", " ", gsub("([A-Za-z]+_[A-Za-z]+).*", "\\1",DB_thold$Species))
        mus_sp <- unique(DB_thold$Sp_bi)
        list_sp_mus <-list()
        print(mus_sp)
        for(sp in seq_along(mus_sp)){
                sci_name <- mus_sp[sp]
                tmp_DB <- subset(DB_thold, DB_thold$Sp_bi == sci_name)
                mus_ID_sp <- mus_ID[which(mus_ID %in% unique(tmp_DB$Museum_id))]
                sp_hits <- data.frame(matrix(nrow=0, ncol=7))
                colnames(sp_hits) <- c("Species", "Gbif_ID", "Lat", "Long", "Inst_code",
                                       "Catalog_num", "Verb_Locality")
                for (mus in seq_along(mus_ID_sp)){
                        inst_code <- mus_ID_sp[mus]
                        tmp_search <- occ_search(scientificName = sci_name, 
                                                 institutionCode = inst_code)
                        if (tmp_search$meta$count > 0){
                                Catalog_num <- tmp_search$data$catalogNumber
                                hit <- which(grepl(tmp_DB$Number_id, tmp_catalog_num))
                                Species <- tmp_search$data$species[hit]
                                Gbif_ID <- tmp_search$data$gbifID[hit]
                                Lat <- tmp_search$data$decimalLatitude[hit]
                                Long <- tmp_search$data$decimalLongitude[hit]
                                Inst_code <- tmp_search$data$institutionCode[hit]
                                Catalog_num <- tmp_search$data$catalogNumber[hit]
                                Verb_Locality <- tmp_search$data$locality[hit]
                                hit_tbl <- cbind(Species, Gbif_ID, Lat, 
                                                 Long, Inst_code, 
                                                 Catalog_num, Verb_Locality)
                                sp_hits <- rbind(sp_hits, hit_tbl)
                        }     
                }
                if(dim(sp_hits)[1] > 0){
                        list_sp_mus[[sp]] <- sp_hits
                }
                print(sp)
        }
        list_sp_mus
}
random_rows <- sample(1:dim(database)[1], size = 100, replace = F)
kk <- voucher.rgbif(database = database[random_rows,], 1)
DB <- database[random_rows,]
  