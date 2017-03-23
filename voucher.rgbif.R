# get the coordinates using the package rgbif
# Loop through the museums with more than X amount of records

# Internal functions
# Matches the Genbank vouchers with the 
match.voucher <- function(tible, tmp_DB){
        tmp_mus <- unique(tible$institutionCode)
        Catalog_num <- tible$catalogNumber
        tmp_DB_mus <- tmp_DB[tmp_DB$Museum_id == tmp_mus,]
        m <- match(tmp_DB_mus$Number_id, gsub(".*([0-9]*)*.*", "\\1", Catalog_num))
        m <- as.vector(na.omit(m))
        if (length(m) > 0){
                tmp_out <- tible[m,]
                # include the ID column in the raw data, that will make easy to match them afterwards
                temp_cat_num <- tmp_out$catalogNumber
                m1 <- match(gsub(".*([0-9]*)*.*", "\\1", temp_cat_num), tmp_DB_mus$Number_id)
                ID <- as.vector(tmp_DB_mus$ID[m1])
                tmp_out <- add_column(tmp_out, ID)
        }else{
                tmp_out <- NA
        }
        tmp_out
}

# extract the especific columns for the seq-hits that match the GenBank vouchers
seq.hits.extract <- function(U){
        # test if the variable is in the seq-hit table
        tmp_var <- c("ID", "species","decimalLatitude", "decimalLongitude", 
                     "institutionCode", "catalogNumber", "country",
                     "stateProvince", "county","locality", 
                     "verbatimLocality")
        tmp_U <- U[,na.omit(match(tmp_var, names(U)))]
        tmp_U
}

# Creates a list, each element in the list is a tible with the hits for each ...
# ... museum that more sequences than the theshold value
search_2_list <- function(mus_ID_sp, tmp_search){
        tmp_list <- list()
        if(length(mus_ID_sp) == 1){
                if (tmp_search$meta$count > 0){
                        tmp_data <- tmp_search$data
                        if(dim(tmp_data)[1] > 1){
                                tmp_list[[1]] <- tmp_data
                        }
                }else{
                        tmp_list[[1]] <- NA
                }
        }
        if(length(mus_ID_sp) > 1){
                tmp_names <- names(tmp_search)
                tmp_actual_hits <- 0
                for (mus in  seq_along(tmp_names)){
                        if (tmp_search[mus][[tmp_names[mus]]]$meta$count > 0){
                                tmp_actual_hits <- tmp_actual_hits + 1
                        }
                }
                if (tmp_actual_hits > 0){
                        count <- 1
                        for (mus in  seq_along(tmp_names)){
                                if (tmp_search[mus][[tmp_names[mus]]]$meta$count > 0){
                                        tmp_data <- tmp_search[mus][[tmp_names[mus]]]$data
                                        tmp_list[[count]] <- tmp_data
                                }
                        }
                }else{
                        tmp_list[[1]] <- NA
                }
        }
        tmp_list
}

# DESCRIBE THIS FUNCTION
voucher.rgbif <- function(A, threshold, save_mode=T){
        library(rgbif)
        library(reshape)
        library(tibble)
        DB <- A
        temp_mus_ID <- sort(table(DB$Museum_id), decreasing = T)
        mus_ID <- names(temp_mus_ID)[which(temp_mus_ID > threshold)]
        DB_thold <- DB[which(DB$Museum_id %in% mus_ID),]
        DB_thold$Sp_bi <- gsub("_", " ", gsub("([A-Za-z]+_[A-Za-z]+).*", "\\1",DB_thold$Species))
        mus_sp <- unique(DB_thold$Sp_bi)
        sp_hits <- data.frame(matrix(nrow=0, ncol=11))
        colnames(sp_hits) <- c("ID", "species","decimalLatitude", "decimalLongitude", 
                               "institutionCode", "catalogNumber", "country",
                               "stateProvince", "county","locality", 
                               "verbatimLocality")
        number_file <- 50
        sp_not_found <- 1
        sp_not_matching_records <- 1
        for(sp in seq_along(mus_sp)){
                tmp_DB <- subset(DB_thold, DB_thold$Sp_bi == mus_sp[sp])
                mus_ID_sp <- mus_ID[which(mus_ID %in% unique(tmp_DB$Museum_id))]
                tmp_search <- occ_search(scientificName = mus_sp[sp], institutionCode = mus_ID_sp, limit = 10000)
                # Use search_2_list to create the list of tibles
                tmp_list <- search_2_list(mus_ID_sp, tmp_search)
                # from this point all the seq-hits are in a list << tmp_list >> ... 
                # ... where each element correspond to a tiblefrom each museum
                cond1 <- length(tmp_list) == 1
                cond2 <- is.na(tmp_list[1])
                if (cond1 & cond2){
                        # species with no records in Gbif
                        print(paste(mus_sp[sp], "not found in Gbif", " (", sp_not_found, ")", sep=""))
                        sp_not_found <- sp_not_found + 1
                        next  
                }else{
                        tmp_seq_hits <- lapply(tmp_list,FUN =match.voucher, tmp_DB)
                        tmp_seq_hits <- tmp_seq_hits[which(!is.na(tmp_seq_hits))]
                }
                if (length(tmp_seq_hits) == 0){
                        print(paste(mus_sp[sp], " has not matching records", " (", sp_not_matching_records, ")", sep=""))
                        sp_not_matching_records <- sp_not_matching_records + 1
                }else{
                        tmp_sp_hits <- lapply(tmp_seq_hits, seq.hits.extract)
                        tmp_sp_hits <- tmp_sp_hits[order(sapply(tmp_sp_hits, ncol), decreasing = T)]
                        tmp_hits <- merge_all(tmp_sp_hits)
                        #print(paste(sp, " out of ", length(mus_sp)))
                }
                sp_hits <- merge(sp_hits, tmp_hits , all = T)
                if (save_mode == T & sp == number_file){
                        file_name <- paste(number_file, "_rgbif.txt", sep = "")
                        write.table(file = file_name, sp_hits, sep="\t", row.names = F)
                        number_file <- number_file + 50
                }else{
                        sp_hits
                }
        }
}

setwd("/Users/afr/Desktop/A/Postdoc/Birds_museum_data/BMD_exploratory/rgbif_test_out/")
rgbif_out <- voucher.rgbif(A = test_DB_mus, test_thold, save_mode = T)
