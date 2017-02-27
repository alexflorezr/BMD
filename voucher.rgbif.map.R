# plot the coordinates found in rgbif
library(rworldmap)
map <- getMap()
par(mfrow=c(3,1), mar=c(3,1,6,3))

map_A <- function(A){
        y_coor <- as.numeric(as.character(A$Lat))
        x_coor <- as.numeric(as.character(A$Long))
        points_A <- cbind(x_coor, y_coor)
        points_A 
}
plot(map)
points_A <- map_A(tmp_hits_tbl)
points(points, pch=16, col="#68838B90")
mtext(side=3, line=2, "Sequences with coordinates in Gbif (using voucher")
mtext(side = 3, dim(points_A)[1])

map_B <- function(B){
        B <- BMD_coor_Genbank 
        temp_list <- strsplit(B$Coordinates, split = " ")
        GB_coor <- B[-which(sapply(temp_list, FUN=length) == 6),]
        GB_lat_long <- unlist(strsplit(GB_coor$Coordinates, split = " "))
        GB_lat <- as.numeric(GB_lat_long[seq(1,length(GB_lat_long), 4)])
        NS <- GB_lat_long[seq(2,length(GB_lat_long), 4)]
        NS_factor <- ifelse(NS=="N", 1, -1)
        GB_long <- as.numeric(GB_lat_long[seq(3,length(GB_lat_long), 4)])
        EW <- GB_lat_long[seq(4,length(GB_lat_long), 4)]
        EW_factor <- ifelse(EW=="E", 1, -1)
        points_B <- cbind(GB_long*EW_factor, GB_lat*NS_factor)
        points_B
}
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
