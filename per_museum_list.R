# remove variables in the environment
rm(list=ls())

## @knitr museum.threshold

# create BMD target for museums with more than x (threshold) sequences
museum.threshold <- function(A, threshold){
        museum_abbrev <- museum.list(A, threshold)
        museum_id_block <- data.frame(matrix(nrow = dim(A)[1], ncol = 5))
        colnames(museum_id_block) <- c("Before_id", "Museum_id", "Pre_number_id",
                                       "Number_id", "After_id")
        tmp_BMD_target <- cbind(A ,museum_id_block) 
        tmp_BMD_museum <- cbind(tmp_BMD_target[0,], museum_id_block[0,])
        for(a in seq_along(museum_abbrev$Museum_ID)){
                abbrev <- as.character(museum_abbrev$Museum_ID[a])
                if (sum(grepl(abbrev, museum_abbrev$Museum_ID)) > 1) {
                        abbrev <- paste("_", abbrev, "_", sep = "")
                        temp_museum <- tmp_BMD_target[grep(abbrev, tmp_BMD_target$Voucher),]
                }
                if (sum(grepl(abbrev, museum_abbrev$Museum_ID)) == 1){
                        temp_museum <- tmp_BMD_target[grep(abbrev, tmp_BMD_target$Voucher),]
                }
                temp_pattern <- paste("(.*)(", abbrev, ")([a-zA-Z_]*)([0-9]*)(.*)",sep = "")
                for (b in 8:12){
                        temp_museum[,b] <- gsub(temp_pattern, temp_museum$Voucher, 
                                                replacement = paste("\\", b-7,sep=""))
                }
                tmp_BMD_museum <- rbind(tmp_BMD_museum, temp_museum)
                print(a)
        }
        return(tmp_BMD_museum)        
}

## @knitr barplot.museum
 
# use BMD_target_all in BMD_explo_summary 
BMD_target_400 <- museum.threshold(BMD_Vo_all, 400)
BMD_Vo_all_bplot <- sort(table(BMD_target_400$Museum_id), decreasing = T)

# Sequences with voucher and GenBank coordinates
BMD_Vo_GB_coor <- BMD_target_400[BMD_target_400$Coordinates != "coordinates_are_not_available",]
BMD_Vo_GB_coor_bplot <- table(BMD_Vo_GB_coor$Museum_id)

# Sequences with voucher and only GenBank coordinates
tmp_BMD_Vo_GB_loc <- BMD_target_400[BMD_target_400$Coordinates == "coordinates_are_not_available",]
BMD_Vo_GB_loc <- tmp_BMD_Vo_GB_loc[tmp_BMD_Vo_GB_loc$Location != "location is not available",]
BMD_Vo_GB_loc_bplot <- table(BMD_Vo_GB_loc$Museum_id)


# Table for the stacked barplot 
tmp_bplot <- data.frame(matrix(nrow=3, ncol=length(names(BMD_Vo_all_bplot))))
colnames(tmp_bplot) <- names(BMD_Vo_all_bplot)
m1 <- match(colnames(tmp_bplot), names(BMD_Vo_GB_coor_bplot))
tmp_bplot[2,] <- BMD_Vo_GB_coor_bplot[m1]
tmp_bplot[2,is.na(tmp_bplot[2,])] <- 0
m2 <- match(colnames(tmp_bplot), names(BMD_Vo_GB_loc_bplot))
tmp_bplot[3,] <- BMD_Vo_GB_loc_bplot[m2]
tmp_bplot[3,is.na(tmp_bplot[3,])] <- 0
tmp_bplot[1,] <- BMD_Vo_all_bplot-(tmp_bplot[2,] +tmp_bplot[3,])
rowSums(tmp_bplot)
ymax <- max(colSums(tmp_bplot))*1.1
# plot the genbank data 
par(mai=c(1.5,1.5,1.5,1.5))
barplot(as.matrix(tmp_bplot), las=2, main = NA, 
        ylim = c(0,ymax), xlim=c(1,40),
        names.arg = colnames(tmp_bplot), las=2, col=c( "#D2B48C","#B0E2FF","#778899"),
        border = NA,cex.names = 0.8)
mtext(side=3, line=2, "Museums with more than 400 records in GenBank", cex=1.5)
mtext(side=3, line=1, paste(sum(BMD_Vo_all_bplot), "records in total"))
mtext(side=1, line=4.5, cex=1.2, "Museum (abbreviation)")
mtext(side=2, line=4, cex=1.2, "Sequences")
abline(h=seq(0,ymax, 1000), lty=3)
lgnd_1 <- paste("No geo data (",rowSums(tmp_bplot)[1], ", ", sep = "")
lgnd_1a <- round(rowSums(tmp_bplot)[1]/sum(rowSums(tmp_bplot))*100)
lgnd_2 <- paste("Coordinates (",rowSums(tmp_bplot)[2], ", ", sep = "")
lgnd_2a <- round(rowSums(tmp_bplot)[2]/sum(rowSums(tmp_bplot))*100)
lgnd_3 <- paste("Only locality (",rowSums(tmp_bplot)[3], ", ", sep = "")
lgnd_3a <- round(rowSums(tmp_bplot)[3]/sum(rowSums(tmp_bplot))*100)
legend(18, ymax*0.95, legend = c(paste(lgnd_1, lgnd_1a, "%)", sep = ""),
                                 paste(lgnd_2, lgnd_2a, "%)", sep = ""),
                                 paste(lgnd_3, lgnd_3a, "%)", sep = "")),
        fill = rev(c("#D2B48C","#B0E2FF","#778899")), bty = "n" , border = "white",
        x.intersp = 0.2, y.intersp = 0.7, cex = 0.9)

## Colors used for the barplot 
# #778899  (lightslategrey)
# #9AC0CD (lightskyblue1)
# #D2B48C (tan ) 




