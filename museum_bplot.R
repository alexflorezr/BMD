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
        barplot(museum_bplot$Records, decreasing = T,las=2, ylim = c(0,8500),
                names.arg = museum_bplot$Museum, las=2, main="Museums")
        barplot(museum_bplot$Coor_records, decreasing = T,las=2, ylim = c(0,8500),
                add = T, col="red")
        abline(h=seq(0,9000, 500), lty=3)
}
museum_bplot(museum_id_uniq)
