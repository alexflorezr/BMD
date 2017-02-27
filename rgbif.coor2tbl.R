# extract all the values in the list rgbif_coor
rgbif_coor_obj <- rgbif_coor
rgbif.coor2tbl <- function(rgbif_coor_obj){
        sp_hits <- data.frame(matrix(nrow=0, ncol=5))
        colnames(sp_hits) <- c("Species", "Lat", "Long", "Inst_code","Catalog_num")
        tmp_nulls <- which(sapply(rgbif_coor_obj, is.null))
        tmp_hits <- rgbif_coor_obj[-tmp_nulls]
        tmp_hits_tbl <- do.call("rbind", tmp_hits)
        tmp_hits_tbl
}
