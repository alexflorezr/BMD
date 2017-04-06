#### TO SAVE ####
#### This flowchart is for the GenBank data ####
#### files needed ####
db <- "/Users/afr/Desktop/A/Postdoc/Birds_museum_data/BMD_exploratory/Data/coordinates.temp"
# Read the database
BMD_raw <- read.delim(db, header = F, stringsAsFactors = F)
# Add names to the columns
colnames(BMD_raw) <- c("ID", "Species", "Coordinates", "Location", "Voucher","Isolate", "Haplotype")
# node_names is a table with one colum indicating the name of the nodes
node_names <- read.delim("node_names.txt", header=T, sep = "\t")
# fromto_edge indicate the connections among nodes
fromto_edge <- read.delim("fchart_fromto.txt", header=F)
#### libraries and internal functions ####
library(diagram)
library(schoolmath)
make.flowtable_GenB <- function(DB_raw, node_names_tbl){
        tmp_flow <- node_names_tbl
        tmp_flow$Pos <- c(1,2,5,11,15,17,21,23,27,29,31,33,39,41)
        # Total number of mtDNA sequences in GenBank
        tmp_flow$Value[1] <- dim(DB_raw)[1]
        # Total number of mtDNA sequences in GenBank
        tmp_flow$Value[2] <- dim(DB_raw)[1]
        # Sequences with voucher information
        BMD_all_voucher <- DB_raw[DB_raw$Voucher != "voucher_is_not_available",]
        tmp_flow$Value[3] <- dim(BMD_all_voucher)[1]
        # Sequences WITHOUT voucher information 
        BMD_Wo_voucher <- DB_raw[DB_raw$Voucher == "voucher_is_not_available",]
        tmp_flow$Value[4] <- dim(BMD_Wo_voucher)[1]
        # Sequences with voucher information, how many have unique voucher IDs
        BMD_unq_voucher <- BMD_all_voucher[-which(duplicated(BMD_all_voucher$Voucher)),]
        tmp_flow$Value[6] <- dim(BMD_unq_voucher)[1]
        # Sequences with voucher information, how many have REPEATED voucher IDs
        BMD_rep_voucher <- BMD_all_voucher[which(duplicated(BMD_all_voucher$Voucher)),]
        tmp_flow$Value[5] <- dim(BMD_rep_voucher)[1]
        # Sequences WITHOUT voucher information, neither coordinates 
        BMD_Wo_Vo_NO_coor <- BMD_Wo_voucher[BMD_Wo_voucher$Coordinates == "coordinates_are_not_available",]
        tmp_flow$Value[7] <- dim(BMD_Wo_Vo_NO_coor)[1]
        # Sequences WITHOUT voucher information, but with coordinates 
        BMD_Wo_Vo_coor <- BMD_Wo_voucher[BMD_Wo_voucher$Coordinates != "coordinates_are_not_available",]
        tmp_flow$Value[8] <- dim(BMD_Wo_Vo_coor)[1]
        # Sequences with voucher, and coordinates
        BMD_unq_vo_coo <- BMD_unq_voucher[BMD_unq_voucher$Coordinates != "coordinates_are_not_available",]
        tmp_flow$Value[9] <-  dim(BMD_unq_vo_coo)[1]
        # Sequences with voucher, WITHOUT coordinates, but with locality
        BMD_unq_vo_NO_coo <- BMD_unq_voucher[BMD_unq_voucher$Coordinates == "coordinates_are_not_available",]
        tmp_flow$Value[10] <-  dim(BMD_unq_vo_NO_coo)[1]
        # Sequences WITHOUT voucher, no coordinates, NOR with localities
        BMD_Wo_Vo_NOcoo_NO_loc <- BMD_Wo_Vo_NO_coor[BMD_Wo_Vo_NO_coor$Location == "location is not available",]
        tmp_flow$Value[11] <-  dim(BMD_Wo_Vo_NOcoo_NO_loc)[1]
        # Sequences WITHOUT voucher, no coordinates, BUT with localities
        BMD_Wo_Vo_NOcoo_loc <- BMD_Wo_Vo_NO_coor[BMD_Wo_Vo_NO_coor$Location != "location is not available",]
        tmp_flow$Value[12] <-  dim(BMD_Wo_Vo_NOcoo_loc)[1]
        # Sequences with unique voucher, no coordinates, BUT with localities
        BMD_unq_vo_NO_coo_loc <- BMD_unq_vo_NO_coo[BMD_unq_vo_NO_coo$Location != "location is not available",]
        tmp_flow$Value[13] <-  dim(BMD_unq_vo_NO_coo_loc)[1]
        # Sequences with unique voucher, BUT no coordinates, nor localities
        BMD_unq_vo_NO_coo_NO_loc <- BMD_unq_vo_NO_coo[BMD_unq_vo_NO_coo$Location == "location is not available",]
        tmp_flow$Value[14] <-  dim(BMD_unq_vo_NO_coo_NO_loc)[1]
        tmp_flow$Percent <- round(tmp_flow$Value * 100 / tmp_flow$Value[1], digits = 0)
        tmp_flow
}
BMD.fchart.nodes <- function(flow_table){
        elpos <- coordinates(c(1, 1, rep(11, 4)))
        for(A in seq_along(flow_table[,1])){
                tmp_pos <- flow_table$Pos[A]
                textround (elpos[tmp_pos,], 0.055, 0.03,lab = "",
                           box.col = ifelse(tmp_pos == 17,"#EE7600", "#B2DFEE"), shadow.col = NULL,
                           lcol = ifelse(tmp_pos == 17,"#EE7600", "#B2DFEE"))
        }
}
BMD.fchart.edges <- function(array_pos, boolean_lab){
        for(pos in seq_along(array_pos[,1])){
                if (is.even(pos)){
                        text(arrpos[pos,1] - 0.015, arrpos[pos,2] + 0.01, boolean_lab[pos], cex=.8)
                }else{
                        text(arrpos[pos,1] + 0.015, arrpos[pos,2] + 0.01, boolean_lab[pos], cex=.8)
                }
        }
}
BMD.fchart.text <- function(flow_table){
        elpos <- coordinates(c(1, 1, rep(11, 4)))
        for(A in seq_along(flow_table[,1])){
                tmp_pos <- flow_table$Pos[A]
                tmp_label <- flow_table$Label[A]
                tmp_value <- flow_table$Value[A]
                tmp_percent <- flow_table$Percent[A]
                tmp_paste <- paste(tmp_value, " (", tmp_percent, "%)", sep = "")
                textplain(elpos[tmp_pos,], adj=c(0.5,1.2),lab = tmp_label, cex=0.8)
                textplain(elpos[tmp_pos,], adj=c(0.5,-0.8),lab = tmp_paste, cex=0.8)
        }
}
BMD.fchart.backbone <- function(fromto_edge){
        par(mar = c(1, 4, 3, 1))
        openplotmat()
        elpos <- coordinates(c(1, 1, rep(11, 4)))
        fromto <- fromto_edge[,c(1,2)]
        nr <- nrow(fromto)
        arrpos <- matrix(ncol = 2, nrow = nr)
        for (i in 1:nr) {
                arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ], from = elpos[fromto[i, 1], ],
                                              lwd = 1, arr.length = 0)
        }
}
#### the script ####
flow_nodes <-make.flowtable_GenB(BMD_raw,node_names)
BMD.fchart.backbone(fromto_edge)
BMD.fchart.nodes(flow_nodes)
BMD.fchart.edges(arrpos, fromto_edge[,3])
BMD.fchart.text(flow_nodes)
