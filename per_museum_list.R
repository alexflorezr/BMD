# remove variables in the environment
rm(list=ls())
# create BMD target for museums with more than x (threshold) sequences
museum.threshold <- function(A, threshold){
        museum_abbrev <- museum.list(A, threshold)
        museum_id_block <- data.frame(matrix(nrow = dim(A)[1], ncol = 5))
        colnames(museum_id_block) <- c("Before_id", "Museum_id", "Pre_number_id",
                                       "Number_id", "After_id")
        tmp_BMD_target <- cbind(A ,museum_id_block) 
        tmp_BMD_museum <- cbind(tmp_BMD_target[0,], museum_id_block[0,])
        if (sum(c("", "U") %in% museum_abbrev$Museum_ID) > 0) {
                museum_abbrev <- museum_abbrev[-match(c("", "U"), museum_abbrev$Museum_ID),]
        }
        for(a in seq_along(museum_abbrev$Museum_ID)){
                abbrev <- as.character(museum_abbrev$Museum_ID[a])
                if (sum(grepl(abbrev, museum_abbrev$Museum_ID)) > 1) {
                        abbrev <- paste("_", abbrev, "_")
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
        
### TO DO::: include a step to remove repeated sequences 

# use BMD_target_all in BMD_explo_summary 
BMD_target_400 <- museum.threshold(BMD_target_all, 400)
table(BMD_target_400$Museum_id)
# barplot for the number of sequences per museum 
barplot(sort(table(BMD_target_400$Museum_id), decreasing = T),las=2, ylim = c(0,8500))
sum(sort(table(BMD_target_400$Museum_id), decreasing = T)[1:6])



kk <- BMD_target_all$Voucher[grep(paste("(.*)(", abbrev, ")([a-zA-Z_]*)([0-9]*)(.*)",sep = ""), BMD_target_all$Voucher)]
table(gsub(pattern, kk, replacement = paste("\\", 2,sep="")))
