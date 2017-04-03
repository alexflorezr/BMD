#remove the sequences that have duplicated voucher (e.g. different loci from the same specimen)
U_voucher_Genbank <- BMD_all_voucher_mus[-which(duplicated(BMD_all_voucher_mus$Voucher)),]
sum(U_voucher_Genbank$Coordinates != "coordinates_are_not_available")
length(unique(U_voucher_Genbank$Voucher))
U_rgbif_out <- read.delim("rgbif.out.txt", header = T, sep="\t")
gbnk_gbif <- merge.default(U_voucher_Genbank, U_rgbif_out, by.x="ID", by.y="ID", all=T)
length(unique(U_rgbif_out$ID))

write.table(kk5, file = "rgbif.out.txt", sep = "\t", row.names = F)


kk <- read.delim("3900_all_voucher_rgbif_out.txt", header = T, sep = "\t")
kk3 <- kk[-which(duplicated(kk$ID)),]
kk4 <- kk[-which(duplicated(kk2$ID)),]
sum(duplicated(kk5$ID))
kk5 <- rbind(kk3, kk4)
sum(duplicated(kk5$catalogNumber))

dim (kk)
length(unique(kk$ID))
setwd("/Users/afr/Desktop/A/Postdoc/Birds_museum_data/BMD_exploratory/rgbif_out")
kk2 <- read.delim("2100_all_voucher_rgbif_out.txt", header = T, sep = "\t")
length(unique(kk2$ID))
rgbif_out  <- rbind(kk2, kk)
gbnk_gbif <- merge.default(BMD_all_voucher_mus, rgbif_out, by.x="ID", by.y="ID", all=T)
dim(BMD_all_voucher_mus)
length(unique(BMD_all_voucher_mus$Voucher))
which(duplicated(BMD_all_voucher_mus$Voucher))
dim(BMD_all_voucher_mus)
which(is.na(match(rgbif_out$ID, BMD_all_voucher_mus$ID)))
length(unique(BMD_all_voucher_mus$ID))
length(unique(rgbif_out$ID))
length(unique(gbnk_gbif$ID))
which(BMD_all_voucher_mus$ID == 3676571)
BMD_all_voucher_mus[48631,]
rgbif_out[rgbif_out$ID == 3676571,]


table(tmp_DB$Museum_id)
gbnk_gbif[which(gbnk_gbif$species == "Aphelocoma californica"),]
