# plot the coordinates found in rgbif
## ---- BMD.map ----
# infiles (BMD_all_voucher and rgbif_300) ###
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
# remove coordinates in degrees
temp_list <- strsplit(Genbank_Gbif$Coordinates, split = " ")
Genbank_Gbif <- Genbank_Gbif[-which(sapply(temp_list, FUN=length) == 6),]

## ---- BMD.genbank.and.gbif.map ----
# map for GenBank points ####
library(rworldmap)
library(sp)
map.genBank.BMD <- function(A){
        tmp_points <- subset(A, A$Coordinates != "coordinates_are_not_available")
        GB_lat_long <- unlist(strsplit(tmp_points$Coordinates, split = " "))
        GB_lat <- as.numeric(GB_lat_long[seq(1,length(GB_lat_long), 4)])
        NS <- GB_lat_long[seq(2,length(GB_lat_long), 4)]
        NS_factor <- ifelse(NS=="N", 1, -1)
        GB_long <- as.numeric(GB_lat_long[seq(3,length(GB_lat_long), 4)])
        EW <- GB_lat_long[seq(4,length(GB_lat_long), 4)]
        EW_factor <- ifelse(EW=="E", 1, -1)
        points_Gbank <- cbind(GB_long*EW_factor, GB_lat*NS_factor)
}
map.gbif.BMD <- function(A){
        tmp_points <- subset(A, !is.na(A$decimalLatitude))
        y_coor <- as.numeric(tmp_points$decimalLatitude)
        x_coor <- as.numeric(tmp_points$decimalLongitude)
        points_gbif <- cbind(x_coor, y_coor)
        points_gbif
}
map <- getMap()
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(map)
points(map.genBank.BMD(Genbank_Gbif), pch=21, col="#474747", lwd=0.08, cex=0.7, bg="#B2DFEE98")
points(map.gbif.BMD(Genbank_Gbif), pch=21, col="#474747", lwd=0.08, cex=0.7, bg="#8FBC8F98")
mtext(side=3, line=-2, "Sequences with coordinates in Genbank and GBIF")
total_coor <- sum(dim(map.gbif.BMD(Genbank_Gbif))[1], dim(map.genBank.BMD(Genbank_Gbif))[1]) - 3700
mtext(side = 3 , line=-3, paste(total_coor, "sequences", sep=" "))

#### map for the disjuntive union ####
## ---- BMD.disjuntive.map ----
# plot only the genbank that are not in gbif and the gbif that are not in genbank
only_genbank <- Genbank_Gbif[which(Genbank_Gbif$Coordinates != "coordinates_are_not_available"),]
only_genbank_points <- only_genbank[which(is.na(only_genbank$decimalLatitude)),]
intersection <- only_genbank[!is.na(only_genbank$decimalLatitude),]
only_gbif <- Genbank_Gbif[which(Genbank_Gbif$Coordinates == "coordinates_are_not_available"),]
only_gbif_points <- only_gbif[!is.na(only_gbif$decimalLatitude),]
only_genbank <- only_genbank[-which(!is.na(only_genbank$decimalLatitude)),]
# plot only genbank 
map <- getMap()
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(map)
points(map.genBank.BMD(only_genbank_points), pch=21, col="#474747", lwd=0.08, cex=0.7, bg="#B2DFEE98")
mtext(side=3, line=-2, "Sequences with coordinates ONLY in Genbank")
mtext(side = 3 , line=-3, paste(dim(map.genBank.BMD(only_genbank_points))[1], "sequences", sep=" "))
# plot only gbif
map <- getMap()
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(map)
points(map.gbif.BMD(only_gbif_points), pch=21, col="#474747", lwd=0.08, cex=0.7, bg="#8FBC8F98")
mtext(side=3, line=-2, "Sequences with coordinates ONLY in GBIF")
mtext(side = 3 , line=-3, paste(dim(map.gbif.BMD(only_gbif_points))[1], "sequences", sep=" "))
# plot the intersection

## ---- BMD.intersection.map ----
map <- getMap()
par(mar=c(0,0,0,0),oma=c(0,0,0,0))
plot(map)
points(map.genBank.BMD(intersection), pch=21, col="#474747", lwd=0.08, cex=0.7, bg="#9F79EE98")
mtext(side=3, line=-2, "Sequences with coordinates in BOTH Genbank and GBIF")
mtext(side = 3 , line=-3, paste(dim(map.genBank.BMD(intersection))[1], "sequences", sep=" "))

# estimate the euclidian distance
## ---- BMD.coordinates.diff.map ----
euclidian.dist <- function(x1, x2, y1, y2){sqrt((x2-x1)^2 + (y2-y1)^2)}
tmp_gbank <- map.genBank.BMD(intersection)
tmp_gbif <- map.gbif.BMD(intersection)
#bp_long <- boxplot(tmp_gbank[,1]-tmp_gbif[,1])
#t.test(x = tmp_gbank[,1],y =  tmp_gbif[,1], mu = 0, paired = T)
#t.test(x = tmp_gbank[,2],y =  tmp_gbif[,2], mu = 0, paired = T)
#bp_lat <- boxplot(tmp_gbank[,2]-tmp_gbif[,2])
mean_diff <- NULL
for(r in seq_along(tmp_gbank[,1])){
        x1 <- tmp_gbif[r,1]
        y1 <- tmp_gbif[r,2]
        x2 <- tmp_gbank[r,1]
        y2 <- tmp_gbank[r,2]
        tmp_eucl_dist <- euclidian.dist(x1=x1, y1=y1, x2=x2, y2=y2)
        mean_diff[r] <- euclidian.dist(x1=x1, y1=y1, x2=x2, y2=y2)
       
}
bp <- boxplot(mean_diff, outline = F)
outliers <- length(bp$out)
