# remove variables in the environment
rm(list=ls())
# create a table for each museum
per_museum_list <- function(db, threshold){
        museums_abbrev <- Museums_list(db, threshold)
        BMD_raw <- read.delim(db, header = F, stringsAsFactors = F)
        colnames(BMD_raw) <- c("ID", "Species", "Coordinates", "Location", 
                               "Voucher","Isolate", "Haplotype")
        new_df_block <- data.frame(matrix(nrow = dim(BMD_raw)[1], ncol = 5))
        colnames(new_df_block) <- c("Before_id", "Museum_id", 
                                    "Pre_number_id", "Number_id", "After_id")
        BMD_raw <- cbind(BMD_raw,new_df_block) 
        BMD_voucher <- BMD_raw[which(BMD_raw$Voucher != "voucher_is_not_available"),]
        BMD_per_museum <- cbind(BMD_voucher[0,], new_df_block[0,])
        for(ab in seq_along(museums_abbrev$Museum_ID)){
                if (!is.element(museums_abbrev$Museum_ID[ab], c("", "U"))){
                        abbrev <- as.character(museums_abbrev$Museum_ID[ab])
                        temp_museum <- BMD_voucher[grep(abbrev, BMD_voucher$Voucher), ]
                        temp_pattern <- paste("(.*)(", abbrev, ")([a-zA-Z_]*)([0-9]*)(.*)",
                                        sep = "")
                        for (clm in 8:12){
                                temp_museum[,clm] <- gsub(temp_pattern, temp_museum$Voucher, 
                                                  replacement = paste("\\", clm-7,
                                                                      sep=""))
                        }
                BMD_per_museum <- rbind(BMD_per_museum, temp_museum)
                }
                print(ab)
        }
        return(BMD_per_museum)
}
kk <- per_museum_list("coordinates.temp", 400)

barplot(sort(table(kk$Museum_id), decreasing = T),
        las=2, 
        ylim = c(0,8500))
barplot(sort(table(kk$Museum_id[kk$Coordinates != "coordinates_are_not_available"]),
             decreasing = T),las=2, ylim = c(0,8500), add = T, col="red")

########## remove below this line ####
table(temp_museum$Before_id)
table(temp_museum$Museum_id)
table(temp_museum$Pre_number_id)
sort(table(temp_museum$Number_id), decreasing = T)
table(temp_museum$After_id)

