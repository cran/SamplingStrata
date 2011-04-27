updateFrame <- function (frame , newstrata ) 
{
    nvarX <- length(grep("X", names(newstrata)))
    if (nvarX == 1) 
        frame$stratum <- newstrata$X1
    if (nvarX > 1) {
        stmt <- NULL
        stmt <- "frame$stratum <- paste("
        for (i in 1:(nvarX - 1)) {
            if (i > 0) 
                stmt <- paste(stmt, "frame$X", i, ",", sep = "")
        }
        stmt <- paste(stmt, "frame$X", nvarX, ",sep='*')", sep = "")
        eval(parse(text = stmt))
    }
    newstrata$stratum <- as.character(newstrata$stratum)
    labels <- as.data.frame(cbind(newstrata$stratum,newstrata$DOM1,newstrata$label))
    colnames(labels) <- c("stratum", "domainvalue", "label")
    framenew <- merge(frame, labels, by = c("domainvalue", "stratum"))
    write.table(framenew, "framenew.txt", row.names = FALSE, sep = "\t", quote = FALSE)
    framenew <- read.delim("framenew.txt")
    return(framenew)
}
