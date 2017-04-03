rm(list=ls())
#### read raw database ####
# Location of the the raw database
db <- "/Users/afr/Desktop/A/Postdoc/Birds_museum_data/BMD_exploratory/Data/coordinates.temp"
# Read the database
BMD_raw <- read.delim(db, header = F, stringsAsFactors = F)
# Add names to the columns
colnames(BMD_raw) <- c("ID", "Species", "Coordinates", "Location", "Voucher","Isolate", "Haplotype")

#### subset the database in coor, voucher, etc ####
# Database to include all the sequences with voucher info in GenBank
BMD_all_voucher <- BMD_raw[BMD_raw$Voucher != "voucher_is_not_available",]
# Remove the sequences with duplicated voucher
BMD_unq_voucher <- BMD_all_voucher[-which(duplicated(BMD_all_voucher$Voucher)),]
# Sequences that have coordinates in GenBank
BMD_unq_vou_coo <- BMD_unq_voucher[which(BMD_unq_voucher$Coordinates != "coordinates_are_not_available"),]
# Sequences that DO NOT have coordinates in GenBank
BMD_unq_vou_Nocoo <- BMD_unq_voucher[which(BMD_unq_voucher$Coordinates == "coordinates_are_not_available"),]
dim(BMD_unq_vou_Nocoo[which(BMD_unq_vou_Nocoo$Location == "location is not available"),])

# subset the sequences with coordinates
BMD_coor_Genbank <- BMD_raw[which(BMD_raw$Coordinates != "coordinates_are_not_available"),]
# subset the sequences with locality information
BMD_NO_coor_Genbank <- BMD_raw[which(BMD_raw$Coordinates == "coordinates_are_not_available"),]
BMD_local_Genbank <- BMD_NO_coor_Genbank[BMD_NO_coor_Genbank$Location != "location is not available",] 
# subset sequences with no coordinates information, but with voucher information
# this dataset includes ALL the data without coordinates, BUT with voucher information
BMD_voucher <- BMD_NO_coor_Genbank[BMD_NO_coor_Genbank$Voucher != "voucher_is_not_available",]
BMD_all_voucher[which(BMD_all_voucher$Coordinates != "coordinates_are_not_available"),c(3,5)]
dim(BMD_coor_Genbank)
