rm(list=ls())
# read raw database
db <- "/Users/afr/Desktop/A/Postdoc/Birds_museum_data/BMD_exploratory/Data/coordinates.temp"
BMD_raw <- read.delim(db, header = F, stringsAsFactors = F)
colnames(BMD_raw) <- c("ID", "Species", "Coordinates", "Location", "Voucher","Isolate", "Haplotype")
# subset the sequences with coordinates
BMD_coor_Genbank <- BMD_raw[which(BMD_raw$Coordinates != "coordinates_are_not_available"),]
# subset the sequences with locality information
BMD_NO_coor_Genbank <- BMD_raw[which(BMD_raw$Coordinates == "coordinates_are_not_available"),]
BMD_local_Genbank <- BMD_NO_coor_Genbank[BMD_NO_coor_Genbank$Location != "location is not available",] 
# subset sequences with no coordinates information, but with voucher information
# this dataset includes ALL the data without coordinates, BUT with voucher information
BMD_voucher <- BMD_NO_coor_Genbank[BMD_NO_coor_Genbank$Voucher != "voucher_is_not_available",]
# database to include any sequence with voucher info in GenBank
BMD_all_voucher <- BMD_raw[BMD_raw$Voucher != "voucher_is_not_available",]
