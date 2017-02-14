# function to get the museums with more than X amount of sequences
Museums_list <- function(db, threshold){
        BMD_raw <- read.delim(db, header = F, stringsAsFactors = F)
        colnames(BMD_raw) <- c("ID", "Species", "Coordinates", "Location", 
                               "Voucher","Isolate", "Haplotype")
        BMD_voucher <- BMD_raw[which(BMD_raw$Voucher != "voucher_is_not_available"),]
        BMD_voucher$Voucher <- gsub("^_(*.*)_$", "\\1", BMD_voucher$Voucher)
        List_BMD_voucher <- strsplit(BMD_voucher$Voucher, split = "_")
        BMD_voucher$Museum_ID <- unlist(lapply(List_BMD_voucher,
                                               function(l) l[[1]][1]))
        outfile_name <- paste("museums_over_", threshold, sep = "")
        tbl_museum_ID <- table(BMD_voucher$Museum_ID)
        tmp_df <- as.data.frame(tbl_museum_ID[tbl_museum_ID >= threshold])
        colnames(tmp_df) <- c("Museum_ID", "Records")
        assign(outfile_name,tmp_df )
        get(outfile_name)

}
Museums_list("coordinates.temp", 400)
