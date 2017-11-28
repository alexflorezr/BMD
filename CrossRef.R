# crossreference ZMUC data and sequences with ZMUC voucher in GenBank
G_ZMUC <- subset(unique_voucher, unique_voucher$Museum_id == "ZMUC")
M_ZMUC <- read.delim("/Users/afr/Desktop/A/Postdoc/Birds_museum_data/BMD_museums/BMD_mus_data/ZMUC/BIOTA_all data_040417.txt",
                     sep="\t", stringsAsFactors = F, header = T)
cross <- na.exclude(match(G_ZMUC$Number_id, M_ZMUC$SpecimenCode))
cross <- as.numeric(as.character(cross))
points_ZMUC <- cbind(as.numeric(M_ZMUC$LocLongitude[cross]), as.numeric(M_ZMUC$LocLatitude[cross]))
library(rworldmap)
map <- getMap()
plot(map)
points(points_ZMUC, col="lightblue", pch=21, bg="steelblue3")


#### smithsonian #### 
G_USNM <- subset(unique_voucher, unique_voucher$Museum_id == "USNM")
G_STRI <- subset(unique_voucher, unique_voucher$Museum_id == "STRI")
sort(table(G_USNM$Coordinates), decreasing = T)
table(G_STRI$Coordinates)
