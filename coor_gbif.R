# search for coordinates in gbif
library(httr)
library(jsonlite)
library(lubridate)

coor.gbif <- function(x){
        x$gbif_coor_long <- NA
        x$gbif_coor_lat <- NA
        x$gbif_id <- NA
        x$gbif_sp <- NA
        gbif_colnames <- vector()
        for(r in seq_along(x$Museum_id)){
                query <- paste(x$Museum_id[r], x$Number_id[r], sep = "%20")
                URL_start <- "http://bionames.org/~rpage/material-examined/service/api.php?code="
                URL_end <- "&match&extend=10"
                URL_query <- paste(URL_start, query,URL_end, sep = "")
                raw.result <- GET(URL_query)
                this.raw.content <- rawToChar(raw.result$content)
                this.content <- fromJSON(this.raw.content)
                df_hits <- lapply(this.content, as.data.frame)$hits
                tax_class <- which(df_hits$class == "Aves")
                #sp <- paste(unlist(strsplit(x$Species[r], split="_"))[c(1,2)], collapse = " ")
                #m <- which(df_hits$species == sp)
                m <- tax_class
                if(length(m) >= 1){
                        gbif_sample <- df_hits[m,]
                        empty_colums <- as.matrix(is.na(gbif_sample))
                        if (sum(empty_colums) < 1){
                                name_empty_colums <- colnames(empty_colums)[empty_colums == T]
                                found <- gbif_sample[,-c(match(name_empty_colums, colnames(gbif_sample)))]
                        }else{
                                found <- gbif_sample
                        }
                         if(is.element("decimalLongitude", colnames(found))){
                                x$gbif_coor_long[r] <- found$decimalLongitude[m]
                                x$gbif_coor_lat[r] <- found$decimalLatitude[m]
                                x$gbif_id[r] <- found$gbifID[m]
                                if(is.element("species", colnames(found))){
                                        x$gbif_sp[r] <- found$species[m]
                                }else{
                                        x$gbif_sp[r] <- found$scientificName[m]
                                }
                        }
                }
                if(length(m) == 0){
                        x$gbif_coor_long[r] <- "Different species name"
                        x$gbif_coor_lat[r] <- "Different species name"
                }
                gbif_colnames <- c(gbif_colnames, colnames(found)) 
                print(r)
        }
}
coor.gbif(museum_id)
x <- museum_id



#### Trasform the code bellow this line in a mapping function

library(rworldmap)
map <- getMap()
plot(map)
### map samples with coordinates values in GenBank
GB_coor <- x[x$Coordinates != "coordinates_are_not_available",]
GB_coor <- GB_coor[-is.na(GB_coor),]
temp_list <- strsplit(GB_coor$Coordinates, split = " ")
GB_coor <- GB_coor[-which(sapply(temp_list, FUN=length) == 6),]
temp_list <- strsplit(GB_coor$Coordinates, split = " ")
table(sapply(temp_list, FUN=length))
GB_lat_long <- unlist(strsplit(GB_coor$Coordinates, split = " "))
GB_lat <- as.numeric(GB_lat_long[seq(1,length(GB_lat_long)*4, 4)])
NS <- GB_lat_long[seq(2,length(GB_lat_long)*4, 4)]
NS_factor <- ifelse(NS=="N", 1, -1)
GB_long <- as.numeric(GB_lat_long[seq(3,length(GB_lat_long)*4, 4)])
EW <- GB_lat_long[seq(4,length(GB_lat_long)*4, 4)]
EW_factor <- ifelse(EW=="E", 1, -1)
points(GB_long*EW_factor, col="#228B2285", GB_lat*NS_factor, pch=16)
### map samples with coordinates from Gbif using voucher
Gbif_coor <- x[x$Coordinates == "coordinates_are_not_available",]
Gbif_coor <- Gbif_coor[-which(is.na(Gbif_coor$gbif_coor_lat)),]
points(Gbif_coor$gbif_coor_long,Gbif_coor$gbif_coor_lat, col="#FF450085",pch=16)







url  <- "http://api.epdb.eu"
path <- "eurlex/directory_code"
raw.result <- GET("http://bionames.org/~rpage/material-examined/service/api.php?code=MVZ%20172494)")
names(raw.result)
raw.result$status_code
head(raw.result$content)
this.raw.content <- rawToChar(raw.result$content)
nchar(this.raw.content)
this.content <- fromJSON(this.raw.content)
class(this.content)
length(this.content)
df <- lapply(this.content, as.data.frame)
df_hits <- df$hits
paste(df_hits$genus, df_hits$specificEpithet)
