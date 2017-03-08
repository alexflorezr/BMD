# create a barplot for the number of records per museum 
# the function uses the output of the function per_museum_list
museum_bplot <- function(museum_id_table){
        museum_bplot <- as.data.frame(sort(table(museum_id_uniq$Museum_id), 
                                           decreasing = T))
        colnames(museum_bplot) <- c("Museum", "Records")
        museum_coor_bplot <- as.data.frame(sort(table(museum_id_uniq$Museum_id[museum_id_uniq$Coordinates != "coordinates_are_not_available"]),
                                  decreasing = T))
        colnames(museum_coor_bplot) <- c("Museum", "Records")
        museum_match <- match(museum_bplot$Museum, museum_coor_bplot$Museum)
        museum_bplot$Coor_records <- museum_coor_bplot$Records[museum_match]
        temp_df <- as.data.frame(matrix(nrow = length(unique)))
        # the plot
        par(mar=c(9,8,8,8))
        pdf("museum_id_records.pdf")
        barplot(museum_bplot$Records,las=2, ylim = c(0,8500), xlim=c(1,40),
                names.arg = museum_bplot$Museum, las=2, col="#C1CDCD",
                border = NA,cex.names = 0.8)
        barplot(museum_bplot$Coor_records,las=2, ylim = c(0,8500),
                add = T, col="#CD853F", border = NA)
        mtext(side=3, line=2, "Museums with more than 400 sequences in GenBank", cex=1.5)
        total <- sum(museum_bplot$Records)
        mtext(side=3, line=1, paste(total, "records in total"))
        mtext(side=1, line=4.5, cex=1.2, "Museum short name")
        mtext(side=2, line=4, cex=1.2, "Number of sequences")
        abline(h=seq(0,9000, 1000), lty=3)
        legend(20, 8000, legend = c("Voucher only", "Voucher and coordinates"),
               fill = c("#C1CDCD", "#CD853F"), bty = "n" , border = "white",
               x.intersp = 0.2, y.intersp = 0.7)
        dev.off()
}
museum_bplot(museum_id_uniq)
