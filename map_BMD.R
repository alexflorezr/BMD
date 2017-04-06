# plot the coordinates found in rgbif
## ---- BMD.map ----
library(rworldmap)
map <- getMap()

# infiles
#### infiles (BMD_all_voucher and rgbif_300) ####
db <- "/Users/afr/Desktop/A/Postdoc/Birds_museum_data/BMD_exploratory/Data/coordinates.temp"
# Read the database
DB_raw <- read.delim(db, header = F, stringsAsFactors = F)
# Add names to the columns
colnames(DB_raw) <- c("ID", "Species", "Coordinates", "Location", "Voucher","Isolate", "Haplotype")
# subset sequences with voucher information
BMD_all_voucher <- DB_raw[DB_raw$Voucher != "voucher_is_not_available",]
# subset the sequences with unique vouchers
BMD_unq_voucher <- BMD_all_voucher[-which(duplicated(BMD_all_voucher$Voucher)),]
# gbif target
target_rgbif_300 <- museum.threshold(BMD_unq_voucher, 300)
target_rgbif_300 <- target_rgbif_300[-which(duplicated(target_rgbif_300$ID)),]
# load the voucher.rgbif output #
rgbif_300 <- read.delim("../rgbif_out3/3600_unq_vou_300_rgbif_out.txt", header = T, sep = "\t")
# match BMD_all_voucher and rgbif_300 by ID
Genbank_Gbif_target <- merge(BMD_unq_voucher, target_rgbif_300[,c(1,8:12)], by.x = "ID", by.y = "ID", all=T)
Genbank_Gbif <- merge(Genbank_Gbif_target, rgbif_300, by.x = "ID", by.y = "ID", all=T)
Genbank_Gbif$unique_museum_id <- Genbank_Gbif$Museum_id
Genbank_Gbif$unique_museum_id[which(Genbank_Gbif$unique_museum_id == "LSUMNS")] <- "LSU"
Genbank_Gbif$unique_museum_id[which(Genbank_Gbif$unique_museum_id == "LSUMZ")] <- "LSU"

### the maps

map.genBank.BMD <- function(A){
        tmp_points <- subset(A, A$Coordinates != "coordinates_are_not_available")
        temp_list <- strsplit(tmp_points$Coordinates, split = " ")
        # remember to add the coordinates in degree
        GB_coor <- tmp_points[-which(sapply(temp_list, FUN=length) == 6),]
        GB_lat_long <- unlist(strsplit(GB_coor$Coordinates, split = " "))
        GB_lat <- as.numeric(GB_lat_long[seq(1,length(GB_lat_long), 4)])
        NS <- GB_lat_long[seq(2,length(GB_lat_long), 4)]
        NS_factor <- ifelse(NS=="N", 1, -1)
        GB_long <- as.numeric(GB_lat_long[seq(3,length(GB_lat_long), 4)])
        EW <- GB_lat_long[seq(4,length(GB_lat_long), 4)]
        EW_factor <- ifelse(EW=="E", 1, -1)
        points_Gbank <- cbind(GB_long*EW_factor, GB_lat*NS_factor)
}
plot(map)
points_gbank <- map.genBank.BMD(Genbank_Gbif)
points(points_gbank, pch=16, col="#68838B90")
mtext(side=3, line=2, "Sequences with coordinates in Genbank")
mtext(side = 3, dim(points_gbank)[1])


map.gbif.BMD <- function(A){
        tmp_points <- subset(A, !is.na(A$decimalLatitude))
        y_coor <- as.numeric(tmp_points$decimalLatitude)
        x_coor <- as.numeric(tmp_points$decimalLongitude)
        points_gbif <- cbind(x_coor, y_coor)
        points_gbif
}
plot(map)
points_gbif <- map.gbif.BMD(Genbank_Gbif)
points(points_gbif, pch=16, col="#68838B90")
mtext(side=3, line=2, "Sequences with coordinates in Genbank")
mtext(side = 3, dim(points_gbank)[1])
       
        



plot(map)
points_B <- map_B(BMD_coor_Genbank)
points(points_B, pch=4, col="#EE760085")
mtext(side=3, line=2, "Sequences with coordinates in GenBank")
mtext(side = 3, dim(points_B)[1])

#plot both A and B
plot(map)
points(points_A, pch=16, col="#68838B90")
points(points_B, pch=4, col="#EE760085")
mtext(side=3, line=2, "Total sequences with coordinates")
mtext(side = 3, dim(points_A)[1]+dim(points_B)[1])



voucher.rgbif.map(tmp_hits_tbl, database)
