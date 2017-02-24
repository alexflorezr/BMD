# extract all the values in the list rgbif_coor
rgbif_coor_obj <- rgbif_coor
rgbif.coor2tbl <- function(rgbif_coor_obj){
        sp_hits <- data.frame(matrix(nrow=0, ncol=5))
        colnames(sp_hits) <- c("Species", "Lat", "Long", "Inst_code","Catalog_num")
        tmp_nulls <- which(sapply(rgbif_coor_obj, is.null))
        tmp_hits <- rgbif_coor_obj[-tmp_nulls]
        tmp_hits_tbl <- do.call("rbind", tmp_hits)
}
library(rworldmap)
map <- getMap()
plot(map)
y_coor <- as.numeric(as.character(tmp_hits_tbl$Lat))
x_coor <- as.numeric(as.character(tmp_hits_tbl$Long))
points( x = x_coor, y = y_coor, pch=16, col="#68838B90")
#### records with coordinates in GenBank
x <- database
GB_coor <- x[x$Coordinates != "coordinates_are_not_available",]
table(is.na(GB_coor$Coordinates))
temp_list <- strsplit(GB_coor$Coordinates, split = " ")
GB_coor <- GB_coor[-which(sapply(temp_list, FUN=length) == 6),]
temp_list <- strsplit(GB_coor$Coordinates, split = " ")
table(sapply(temp_list, FUN=length))
GB_lat_long <- unlist(strsplit(GB_coor$Coordinates, split = " "))
GB_lat <- as.numeric(GB_lat_long[seq(1,length(GB_lat_long)*4, 4)])
NS <- GB_lat_long[seq(2,length(GB_lat_long)*4, 4)]
NS_factor <- ifelse(NS=="N", 1, -1)
GB_long <- as.numeric(GB_lat_long[seq(3,length(GB_lat_long)*4, 4)])
EW <- GB_lat_long[seq(4,length(GB_lat_long)*4, 4)]
EW_factor <- ifelse(EW=="E", 1, -1)
points(GB_long*EW_factor, col="#EE760085", GB_lat*NS_factor, pch=4)
