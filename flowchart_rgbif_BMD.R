#### files needed ####
## ---- flowchart.rgbif ----
db <- "/Users/afr/Desktop/A/Postdoc/Birds_museum_data/BMD_exploratory/Data/coordinates.temp"
# Read the database
BMD_raw <- read.delim(db, header = F, stringsAsFactors = F)
# Add names to the columns
colnames(BMD_raw) <- c("ID", "Species", "Coordinates", "Location", "Voucher","Isolate", "Haplotype")
# rgbif outfile
rgbif_300 <- read.delim("../rgbif_out3/3600_unq_vou_300_rgbif_out.txt", header = T, sep = "\t")
# node_names.rgbif is a table with one colum indicating the name of the nodes
node_names.rgbif <- read.delim("rgbif_node_names.txt", header=T, sep = "\t")
# fromto_edge indicate the connections among nodes
fromto_edge <- read.delim("rgbif_fchart_fromto.txt", header=F)

#### libraries and internal functions ####
library(diagram)
library(schoolmath)
make.flowtable_rgbif <- function(DB_raw, rgbif_out, node_names_tbl){
        tmp_flow <- node_names_tbl
        tmp_flow$Pos <- c(1,2, 5, 11, 15, 17, 27, 29)
        # Number of mtDNA sequences with unique voucher information
        BMD_all_voucher <- DB_raw[DB_raw$Voucher != "voucher_is_not_available",]
        BMD_unq_voucher <- BMD_all_voucher[-which(duplicated(BMD_all_voucher$Voucher)),]
        tmp_flow$Value[1] <- dim(BMD_unq_voucher)[1]
        tmp_flow$Value[2] <- dim(BMD_unq_voucher)[1]
        # Sequences in the museum with more than 300 sequences
        target_rgbif_300 <- museum.threshold(BMD_unq_voucher, 300)
        target_rgbif_300 <- target_rgbif_300[-which(duplicated(target_rgbif_300$ID)),]
        tmp_flow$Value[3] <- dim(target_rgbif_300)[1]
        # Sequences in the tail of the museum's barplot 
        tmp_flow$Value[4] <- dim(BMD_unq_voucher)[1] - dim(target_rgbif_300)[1]
        # Sequences WITHOUT geographical information in gbif
        tmp_flow$Value[5] <- dim(target_rgbif_300)[1] - dim(rgbif_out)[1]
        # Sequences with geographical information in gbif
        tmp_flow$Value[6] <- dim(rgbif_out)[1]
        # Sequences with coordinates in GBIF
        rgbif_300_coo <- rgbif_out[which(!is.na(rgbif_out$decimalLatitude)),]
        tmp_flow$Value[7] <- dim(rgbif_300_coo)[1]
        # Sequences WITHOUT coordinates in GBIF
        tmp_flow$Value[8] <-  dim(rgbif_out)[1] - dim(rgbif_300_coo)[1]
        tmp_flow$Percent <- round(tmp_flow$Value * 100 / dim(DB_raw)[1], digits = 0)
        tmp_flow
}
BMD.fchart.backbone.rgbif <- function(fromto_edge, boolean_lab){
        par(mar = c(4, 0, 0, 0), oma=c(0,0,0,0))
        openplotmat()
        elpos <- coordinates(c(1,1, rep(11, 3)))
        fromto <- fromto_edge[,c(1,2)]
        nr <- nrow(fromto)
        arrpos <- matrix(ncol = 2, nrow = nr)
        for (i in 1:nr) {
                arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ], from = elpos[fromto[i, 1], ],
                                              lwd = 1, arr.length = 0)
        }
        for(pos in seq_along(arrpos[,1])){
                if (is.even(pos)){
                        text(arrpos[pos,1] - 0.017, arrpos[pos,2] + 0.01, boolean_lab[pos], cex=.7)
                }else{
                        text(arrpos[pos,1] + 0.017, arrpos[pos,2] + 0.01, boolean_lab[pos], cex=.7)
                }
        }
}
BMD.fchart.nodes.rgbif <- function(flow_table){
        elpos <- coordinates(c(1,1, rep(11, 3)))
        for(A in seq_along(flow_table[,1])){
                tmp_pos <- flow_table$Pos[A]
                textround (elpos[tmp_pos,], 0.048, 0.04,lab = "",
                           box.col = ifelse(tmp_pos == 1,"#8FBC8F", ifelse(tmp_pos == 15, "#EE6363", "#FF7F00")), shadow.col = NULL,
                           lcol = ifelse(tmp_pos == 1,"#8FBC8F", ifelse(tmp_pos == 15, "#EE6363", "#FF7F00")))
        }
}
BMD.fchart.text.rgbif <- function(flow_table){
        elpos <- coordinates(c(1,1, rep(11, 3)))
        for(A in seq_along(flow_table[,1])){
                tmp_pos <- flow_table$Pos[A]
                tmp_label <- flow_table$Label[A]
                tmp_value <- flow_table$Value[A]
                tmp_percent <- flow_table$Percent[A]
                tmp_paste <- paste(tmp_value, " (", tmp_percent, "%)", sep = "")
                textplain(elpos[tmp_pos,], adj=c(0.5,1),lab = tmp_label, cex=0.7)
                textplain(elpos[tmp_pos,], adj=c(0.5,-0.7),lab = tmp_paste, cex=0.7)
        }
}

#### the script ####
flow_nodes.rgbif <-make.flowtable_rgbif(BMD_raw,rgbif_300, node_names.rgbif)
BMD.fchart.backbone.rgbif(fromto_edge, fromto_edge[,3])
BMD.fchart.nodes.rgbif(flow_nodes.rgbif)
BMD.fchart.text.rgbif(flow_nodes.rgbif)
