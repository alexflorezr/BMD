# search for coordinates in gbif
library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)

coor.gbif <- function(museum_data){
        x <- museum_data
        f <- list()
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
                if(length(tax_class) >= 1){
                        f[[r]] <- as.data.frame(df_hits[tax_class,])
                }
                print(r)
        }        
        f
}
## the 1B museum name has not been assigned to any known museum
## for the exploratory running I will remove the museum 1B
musuem_id_no_1B <- museum_id[!museum_id$Museum_id == "1B",]
### for through the museum_id_no_1B in order to save some data before it crashes
sets <- rep(1:110, rep(700, 110))
musuem_id_no_1B$sets <- sets[1:dim(musuem_id_no_1B)[1]]
for(set in seq_along(unique(musuem_id_no_1B$sets))){
        temp_museum <- subset(musuem_id_no_1B, musuem_id_no_1B$sets == unique(musuem_id_no_1B$sets)[set])
        temp_list <- coor.gbif(temp_museum)
        assign(paste("set_museum_", set, sep=""), temp_list)
}


aggregate(musuem_id_no_1B, by=list(musuem_id_no_1B$sets), FUN = coor.gbif)
sets <- round(seq(1,dim(musuem_id_no_1B)[1], length.out = 10))
for (set in round(seq(1,dim(musuem_id_no_1B)[1], length.out = 10))){
        temp_museum <- musuem_id_no_1B[set[],]
}
gbif_columns <- vector()
for(unit in 1:74){
        temp_unit <- get(paste("set_museum_", unit, sep = ""))
        length_unit <- length(temp_unit)
        if(length_unit > 0){
                colnames_unit <- unique(unlist(lapply(temp_unit, colnames)))
                gbif_columns <- unique(c(gbif_columns,colnames_unit))
       }
}






#### Delete under this line 
## records with values "Ploceus nigerrimus Vieillot" in the scientific name seem
### to have an unrecognized character which stops R
musuem_id_no_1B[52312,]
kk <- musuem_id_no_1B[which(musuem_id_no_1B$Species == "Poecile_varius_yakushimensis"),]

x<- musuem_id_no_1B

kkk <- coor.gbif(kk)

empty_gbif <- df_hits[0,]


x$gbif_coor_long <- NA
x$gbif_coor_lat <- NA
x$verbatimLocality <- NA
x$locality <- NA
x$gbif_id <- NA
x$gbif_sp <- NA
x$gbif_sci_name <- NA
                
                if(length(m) > 1){
                        
                if(length(m) == 1){
                        
                        if(is.element("decimalLongitude", colnames(found))){
                                x$gbif_coor_long[r] <- found$decimalLongitude[m]
                                x$gbif_coor_lat[r] <- found$decimalLatitude[m]
                                x$gbif_id[r] <- found$gbifID[m]
                         }
                        if(is.element("species", colnames(found))){
                                x$gbif_sp[r] <- found$species
                        }
                        if(is.element("scientificName", colnames(found))){
                                x$gbif_sci_name[r] <- found$scientificName
                        }
                        if(is.element("verbatimLocality", colnames(found))){
                                x$verbatimLocality[r] <- found$verbatimLocality
                        }
                        if(is.element("locality", colnames(found))){
                                x$locality[r] <- found$locality
                        }
                }
                if(length(m) == 0){
                        x$gbif_coor_long[r] <- "Different species name"
                        x$gbif_coor_lat[r] <- "Different species name"
                }
                print(paste(r, length(m), sep = " hits "))
        }
        return(x)
}

## for the exploratory running I will remove the museum 1B
musuem_id_no_1B <- museum_id[!museum_id$Museum_id == "1B",]
## running the function for the 5 first museum

museum_gbif <- coor.gbif(musuem_id_no_1B[1:100,])
x <- musuem_id_no_1B[1:100,]



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
