# create a barplot for the number of records per museum 
## ---- barplot.museum ----
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

#### match BMD_all_voucher and rgbif_300 by ID
Genbank_Gbif_target <- merge(BMD_unq_voucher, target_rgbif_300[,c(1,8:12)], by.x = "ID", by.y = "ID", all=T)
Genbank_Gbif <- merge(Genbank_Gbif_target, rgbif_300, by.x = "ID", by.y = "ID", all=T)


#### barplot ####
# Table for the stacked barplot
Genbank_Gbif$unique_museum_id <- Genbank_Gbif$Museum_id
Genbank_Gbif$unique_museum_id[which(Genbank_Gbif$unique_museum_id == "LSUMNS")] <- "LSU"
Genbank_Gbif$unique_museum_id[which(Genbank_Gbif$unique_museum_id == "LSUMZ")] <- "LSU"

unique_museum <- sort(table(Genbank_Gbif$unique_museum_id), decreasing = T)
barplot_tbl <- as.data.frame(matrix(nrow=5, ncol=length(unique_museum)))
colnames(barplot_tbl) <- names(unique_museum)
rownames(barplot_tbl) <- c("coo_Gbank","coo_Gbif", "loc_Gbank", "loc_Gbif", "no_geo_info")
for(mus in seq_along(unique_museum)){
        # Sequences for an specific museum in the unique museum vector
        tmp_mus <- subset(Genbank_Gbif, Genbank_Gbif$unique_museum_id == names(unique_museum)[mus])
        # Sequences with coordinates in genbank
        tmp_mus_coo_gbank <- subset(tmp_mus, tmp_mus$Coordinates != "coordinates_are_not_available")
        barplot_tbl[1,mus] <- dim(tmp_mus_coo_gbank)[1]
        # Sequences WITHOUT coordinates in genbank
        tmp_mus_no_coo_gbank <- subset(tmp_mus, tmp_mus$Coordinates == "coordinates_are_not_available")
        # Sequences with coordinates in GBIF
        tmp_mus_coo_gbif <- subset(tmp_mus_no_coo_gbank, !is.na(tmp_mus_no_coo_gbank$decimalLatitude))
        barplot_tbl[2,mus] <- dim(tmp_mus_coo_gbif)[1]
        # Sequences WITHOUT coordinates in genbank, neither in GBIF
        tmp_mus_no_coo_gbif <- subset(tmp_mus_no_coo_gbank, is.na(tmp_mus_no_coo_gbank$decimalLatitude))
        # Sequences with localities in genbank
        tmp_mus_loc_gbank <- subset(tmp_mus_no_coo_gbif, tmp_mus_no_coo_gbif$Location != "location is not available")
        barplot_tbl[3,mus] <- dim(tmp_mus_loc_gbank)[1]
        # Sequences WITHOUT coordinates nor localities in genbank, neither coordinates in GBIF,
        tmp_mus_no_loc_gbank <- subset(tmp_mus_no_coo_gbif, tmp_mus_no_coo_gbif$Location == "location is not available")
        # Sequences with localities in GBIF
        tmp_mus_loc_gbif <- subset(tmp_mus_no_loc_gbank, !is.na(tmp_mus_no_loc_gbank$verbatimLocality))
        barplot_tbl[4,mus] <- dim(tmp_mus_loc_gbif)[1]
        # Sequences WITHOUT geographical information
        tmp_mus_no_loc_gbif <- subset(tmp_mus_no_loc_gbank, is.na(tmp_mus_no_loc_gbank$verbatimLocality))
        barplot_tbl[5,mus] <- dim(tmp_mus_no_loc_gbif)[1]
}
par(mar=c(6,6,7,1), oma=c(0,0,0,0))
barplot(as.matrix(barplot_tbl), las=2, main = NA,
        names.arg = colnames(barplot_tbl), las=2, col=c( "#B2DFEE","#FFA54F","#4F94CD","#FF7F00", "#EE6363"),
        border = NA,cex.names = 0.8)
mtext(side=3, line=5, "Museums with more than 300 sequences", cex=1.5)
mtext(side=3, line=3, paste(dim(target_rgbif_300)[1], " records")) 
mtext(side=3, line=2, paste(round(dim(target_rgbif_300)[1]*100/dim(BMD_raw)[1], digits = 0),
                                "%", " of all the sequences in Genbank", sep = ""))
mtext(side=3, line=1, paste(round(dim(target_rgbif_300)[1]*100/dim(BMD_unq_voucher)[1], digits = 0), "%",
      " of all sequences with voucher information", sep=""))
mtext(side=1, line=4, cex=1.2, "Museum (abbreviation)")
mtext(side=2, line=4, cex=1.2, "Sequences")
abline(h=seq(0,5000, 1000), lty=3)
# legend
lgnd_1 <- paste("Coordinates in GenBank")
lgnd_2 <- paste("Coordinates in GBIF")
lgnd_3 <- paste("Locality in GenBank")
lgnd_4 <- paste("Locality in GBIF") 
lgnd_5 <- paste("No geographical information") 
legend(19, 5000, legend=rev(c(lgnd_1, lgnd_2, lgnd_3, lgnd_4, lgnd_5)),
       border = "white",fill = rev(c( "#B2DFEE","#FFA54F","#4F94CD","#FF7F00", "#EE6363")),
       box.lwd = 0,box.col = "white",bg = "white", x.intersp = 0.2, y.intersp = 1, cex = 0.7)
