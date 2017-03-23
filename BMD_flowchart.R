

library(diagram)
library(schoolmath)
# internal functions
BMD.fchart.nodes <- function(flow_table){
        elpos <- coordinates(c(1, 1, 1, rep(19, 6)))
        for(A in seq_along(flow_table[,1])){
                tmp_pos <- flow_table$Pos[A]
                tmp_label <- flow_table$Label[A]
                tmp_good <- flow_table$Good_data[A]
                if(flow_table$Tip[A]){
                        textellipse(elpos[tmp_pos,], 0.045,
                                    lab = tmp_label, box.col = ifelse(tmp_good,"#7CCD7C","#F08080"),
                                    shadow.col = NULL, lcol = ifelse(tmp_good,"#7CCD7C","#F08080"))    
                }else{
                        textround (elpos[tmp_pos,], 0.08, 0.02,lab = tmp_label, 
                                   box.col = "#B2DFEE", shadow.col = NULL,
                                   lcol = "#B2DFEE")
                }
        }
}
BMD.fchart.edges <- function(array_pos, boolean_lab){
        for(pos in seq_along(array_pos[,1])){
                #text(arrpos[pos,1] + 0.03, arrpos[pos,2] + 0.02, pos, cex=.7)
                if (is.even(pos)){
                        text(arrpos[pos,1] + 0.01, arrpos[pos,2] + 0.025, boolean_lab[pos], cex=.8)
                }else{
                        text(arrpos[pos,1] - 0.01, arrpos[pos,2] + 0.025, boolean_lab[pos], cex=.8)
                }
        }
}
# infiles
A <- read.delim("flowchart_plot.txt", header = T, sep = "\t")
fromto_edge <- read.delim("fchart_fromto.txt", header=F)
# The script
par(mar = c(1, 4, 3, 1))
openplotmat()
elpos <- coordinates(c(1, 1, 1, rep(19, 6)))
fromto <- fromto_edge[,c(1,2)]
nr <- nrow(fromto)
arrpos <- matrix(ncol = 2, nrow = nr)
for (i in 1:nr) {
        arrpos[i, ] <- straightarrow (to = elpos[fromto[i, 2], ], from = elpos[fromto[i, 1], ],
                                      lwd = 1, arr.length = 0)
}
textellipse (elpos[2,] + c(0,0.04), 0.09, lab = "Genbank data", box.col = "grey", shadow.col = NULL, lcol = "grey")
textround (elpos[3,], 0.15, 0.02,lab = "Geographical info", box.col = "grey", shadow.col = NULL, lcol = "grey")
BMD.fchart.nodes(A)
BMD.fchart.edges(arrpos, fromto_edge[,3])








text(arrpos[2,1] -0.05, arrpos[2,2] + 0.02, "No")
textround (elpos[7,], 0.10, 0.02,lab = "Voucher info", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#B2DFEE")
text(arrpos[3,1] + 0.05, arrpos[3,2] + 0.02, "Yes")
textround (elpos[24,], 0.08, 0.02,lab = "Geo rGBIF", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#B2DFEE")
textround (elpos[28,], 0.08, 0.02,lab = "No data", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#F08080",lwd=3)
textround (elpos[41,], 0.08, 0.02,lab = "No info", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#F08080",lwd=3)
textround (elpos[45,], 0.08, 0.02,lab = "Coordinates", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#B2DFEE")
textround (elpos[62,], 0.08, 0.02,lab = "Localities", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#B2DFEE")
textround (elpos[66,], 0.08, 0.02,lab = "Good estimate", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#7CCD7C", lwd=3)
textround (elpos[79,], 0.08, 0.02,lab = "No info", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#F08080", lwd=3)
textround (elpos[83,], 0.08, 0.02,lab = "Geonames", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#B2DFEE", lwd=3)
textround (elpos[100,], 0.08, 0.02,lab = "Bad locality", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#F08080", lwd=3)
textround (elpos[104,], 0.08, 0.02,lab = "Good estimate", box.col = "#B2DFEE", shadow.col = NULL, lcol = "#7CCD7C", lwd=3)
textround (elpos[17,], 0.10, 0.02,lab = "Coordinates", box.col = "#CDBA96", shadow.col = NULL, lcol = "#CDBA96")
textround (elpos[34,], 0.08, 0.02,lab = "Localities", box.col = "#CDBA96", shadow.col = NULL, lcol = "#CDBA96")
textround (elpos[38,], 0.08, 0.02,lab = "Good data", box.col = "#CDBA96", shadow.col = NULL,lcol = "#7CCD7C", lwd=3 )
textround (elpos[51,], 0.08, 0.02,lab = "False positive", box.col = "#CDBA96", shadow.col = NULL,lcol = "#F08080",lwd=3)
textround (elpos[55,], 0.08, 0.02,lab = "Geonames", box.col = "#CDBA96", shadow.col = NULL, lcol = "#CDBA96")
textround (elpos[72,], 0.08, 0.02,lab = "Bad locality", box.col = "#CDBA96", shadow.col = NULL,lcol = "#F08080",lwd=3)
textround (elpos[76,], 0.08, 0.02,lab = "Good estimate", box.col = "#CDBA96", shadow.col = NULL,lcol = "#7CCD7C", lwd=3 )

