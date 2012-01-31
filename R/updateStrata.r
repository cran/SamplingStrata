updateStrata <- function (strata, writeFile = "YES") 
{
    strat <- split(strata, list(strata$DOM1))
    ndom <- length(levels(as.factor(strata$DOM1)))
    nvarX <- length(grep("X", names(strata)))
    newstrata <- NULL
    solution <- NULL
    matstrord <- NULL
    for (i in 1:ndom) {
        statement <- paste("solution <- read.table(\"solution", 
            i, ".txt\")", sep = "")
        eval(parse(text = statement))
        strat[[i]] <- cbind(strat[[i]], aggr_stratum = solution)
        newstrata <- rbind(newstrata, strat[[i]])
    }
    stmt <- "matstrata <- as.data.frame(cbind(newstrata$DOM1,newstrata$V1,"
    stmt2 <- "colnames(matstrata) <- c('DOM1','aggr_stratum',"
    stmt3 <- NULL
	if (nvarX > 1) {
		for (i in 1:(nvarX - 1)) {
			stmt <- paste(stmt, "newstrata$X", i, ",", sep = "")
			stmt2 <- paste(stmt2, "'X", i, "',", sep = "")
			stmt3 <- paste(stmt3, "matstrata$X", i, ",", sep = "")
			}
		stmt <- paste(stmt, "newstrata$X", nvarX, "))", sep = "")
		eval(parse(text = stmt))
		stmt2 <- paste(stmt2, "'X", nvarX, "')", sep = "")
		eval(parse(text = stmt2))
		stmt3 <- paste(stmt3, "matstrata$X", nvarX, sep = "")
		statement <- paste("matstrord <- matstrata[order(matstrata$DOM1,matstrata$aggr_stratum,", 
        stmt3, "),]", sep = "")
		eval(parse(text = statement))
		}
	if (nvarX == 1) {
		for (i in 1:1) {
			stmt <- paste(stmt, "newstrata$X", i, ",", sep = "")
			stmt2 <- paste(stmt2, "'X", i, "',", sep = "")
			stmt3 <- paste(stmt3, "matstrata$X", i, ",", sep = "")
			}
		stmt <- paste(stmt, "newstrata$X", nvarX, "))", sep = "")
		eval(parse(text = stmt))
		stmt2 <- paste(stmt2, "'X", nvarX, "')", sep = "")
		eval(parse(text = stmt2))
		stmt3 <- paste(stmt3, "matstrata$X", nvarX, sep = "")
		statement <- paste("matstrord <- matstrata[order(matstrata$DOM1,matstrata$aggr_stratum,", 
        stmt3, "),]", sep = "")
		eval(parse(text = statement))
		}
    if (nvarX == 1) 
        newstrata$stratum <- newstrata$X1
    if (nvarX > 1) {
        stmt <- NULL
        stmt <- "newstrata$stratum <- paste("
        for (i in 1:(nvarX - 1)) {
            if (i > 0) 
                stmt <- paste(stmt, "newstrata$X", i, ",", sep = "")
        }
        stmt <- paste(stmt, "newstrata$X", nvarX, ",sep='*')", 
            sep = "")
        eval(parse(text = stmt))
    }
    colnames(newstrata)[ncol(newstrata) - 1] <- c("label")
    colnames(newstrata)[ncol(newstrata)] <- c("stratum")
    if (writeFile == "YES") 
        write.table(newstrata, file = "newstrata.txt", sep = "\t", 
            row.names = FALSE, col.names = TRUE, quote = FALSE)
    if (writeFile == "YES") 
        write.table(matstrord, file = "strata_aggregation.txt", 
            sep = "\t", row.names = FALSE, col.names = TRUE, 
            quote = FALSE)
    return(newstrata)
}
