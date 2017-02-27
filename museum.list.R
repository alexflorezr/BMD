# function to get the museums with more than X amount of sequences
# CHECK: almost 2886 sequences with empty (" ") museum name
museum.list <- function(db, threshold){
        BMD_voucher <- db
        BMD_voucher$Voucher <- gsub("^_(*.*)_$", "\\1", BMD_voucher$Voucher)
        List_BMD_voucher <- strsplit(BMD_voucher$Voucher, split = "_")
        BMD_voucher$Museum_ID <- unlist(lapply(List_BMD_voucher,function(l) l[[1]][1]))
        outfile_name <- paste("museums_over_", threshold, sep = "")
        tbl_museum_ID <- table(BMD_voucher$Museum_ID)
        tmp_df <- as.data.frame(tbl_museum_ID[tbl_museum_ID >= threshold])
        colnames(tmp_df) <- c("Museum_ID", "Records")
        assign(outfile_name,tmp_df )
        get(outfile_name)
}
museum.list(BMD_NO_coor_Genbank, 100)
