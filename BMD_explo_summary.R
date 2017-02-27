db <- "/Users/afr/Desktop/A/Postdoc/Birds_museum_data/BMD_exploratory/Data/coordinates.temp"
BMD_raw <- read.delim(db, header = F, stringsAsFactors = F)
colnames(BMD_raw) <- c("ID", "Species", "Coordinates", "Location", "Voucher","Isolate", "Haplotype")
BMD_coor_Genbank <- BMD_raw[which(BMD_raw$Coordinates != "coordinates_are_not_available"),]
BMD_NO_coor_Genbank <- BMD_raw[which(BMD_raw$Coordinates == "coordinates_are_not_available"),]
dim(BMD_NO_coor_Genbank)
## BMD target are the sequences with no coordinate info, BUT with voucher info
BMD_target_all <- BMD_NO_coor_Genbank[BMD_NO_coor_Genbank$Voucher != "voucher_is_not_available",]
