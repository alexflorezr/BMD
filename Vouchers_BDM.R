setwd("/Users/afr/Desktop/Postdoc/Birds_museum_data/BMD_exploratory/Data")
BMD_raw <- read.delim("coordinates.temp", header = F, stringsAsFactors = F)
colnames(BMD_raw) <- c("ID", "Species", "Coordinates", "Location", "Voucher", "Isolate", "Haplotype")
BMD_voucher <- BMD_raw[which(BMD_raw$Voucher != "voucher_is_not_available"),]
BMD_NO_voucher <- BMD_raw[which(BMD_raw$Voucher == "voucher_is_not_available"),]
# percentage of the sequences with voucher names
dim(BMD_voucher)[1]/(dim(BMD_voucher)[1] + dim(BMD_NO_voucher)[1]) * 100
# Back-up BMD voucher
BMD_voucher_bck <- BMD_voucher
table(BMD_voucher$Coordinates == "coordinates_are_not_available")
# replace the "_" at the begginig of the voucher string
BMD_voucher$Voucher <- gsub("^_(*.*)_$", "\\1", BMD_voucher$Voucher)
# separate the museum name and the number
List_BMD_voucher <- strsplit(BMD_voucher$Voucher, split = "_")
BMD_voucher$Museum_ID <- unlist(lapply(List_BMD_voucher, function(l) l[[1]][1]))

# plot a histogram for the museums with more than 500
# IMPROVE: make the threshold flexible
png("Museum_over_500.png")
barplot(over_500, names.arg = names(over_500),
        las=2, 
        ylim=c(0, 5000))
abline(h=seq(0,5000, 500), lty=3)
mtext("Museums with more than 500 records", side=3, cex=2, line=1.5)
mtext(paste(sum(over_500), " among these museums in total", sep = ""), side=3, cex=1, line=.5)
dev.off()
### IDEAS
# Check the proportion of these sequences with coordinates (how much can we actually improve)
# Merge the museums as lousiana
# improve the search (check gbif database for museums ans collections)
# Check the Darwin project

