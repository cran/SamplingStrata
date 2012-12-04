updateFrame <- function(frame, newstrata) {
    colnames(frame) <- toupper(colnames(frame))
    colnames(newstrata) <- toupper(colnames(newstrata))
    nvarX <- length(grep("X", names(newstrata)))
    if (nvarX == 1) 
        frame$STRATUM <- newstrata$X1
    if (nvarX > 1) {
        stmt <- NULL
        stmt <- "frame$STRATUM <- paste("
        for (i in 1:(nvarX - 1)) {
            if (i > 0) 
                stmt <- paste(stmt, "frame$X", i, ",", sep = "")
        }
        stmt <- paste(stmt, "frame$X", nvarX, ",sep='*')", sep = "")
        eval(parse(text = stmt))
    }
    newstrata$STRATUM <- as.character(newstrata$STRATUM)
    labels <- as.data.frame(cbind(newstrata$STRATUM, newstrata$DOM1, 
        newstrata$LABEL))
    colnames(labels) <- c("STRATUM", "DOMAINVALUE", "LABEL")
    framenew <- merge(frame, labels, by = c("DOMAINVALUE", "STRATUM"))
    colnames(framenew) <- toupper(colnames(framenew))
    write.table(framenew, "framenew.txt", row.names = FALSE, 
        sep = "\t", quote = FALSE)
    framenew <- read.delim("framenew.txt")
    return(framenew)
}
