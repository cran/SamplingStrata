# ----------------------------------------------------
# Main for calling genetic algorithm GA-based optimal
# stratification and optimal allocation 
# cycling on different domains
# Author: Giulio Barcaroli
# Date: 4 January 2012
# ----------------------------------------------------
optimizeStrata <- function(errors, strata, cens = NULL, strcens = FALSE, 
    alldomains = TRUE, dom = NULL, initialStrata = nrow(strata), addStrataFactor = 0.01, 
    minnumstr = 2, iter = 20, pops = 20, mut_chance = 0.05, elitism_rate = 0.2, 
    highvalue = 1e+08, suggestions = NULL, realAllocation = FALSE, 
    writeFiles = FALSE, showPlot = TRUE) {
    # --------------------------------------------------------------------------
    # Parameters
    # --------------------------------------------------------------------------
    colnames(errors) <- toupper(colnames(errors))
    colnames(strata) <- toupper(colnames(strata))
	checkInput(errors,strata)
    erro <- split(errors, list(errors$DOMAINVALUE))
    stcamp <- split(strata, list(strata$DOM1))
    if (strcens == TRUE) 
        colnames(cens) <- toupper(colnames(cens))
    if (strcens == TRUE) 
        stcens <- split(cens, list(cens$DOM1))
    ndom <- length(levels(as.factor(strata$DOM1)))
    # begin alldomains = TRUE
    if (alldomains == TRUE) {
		v <- NULL
		outstrata <- NULL
        for (i in 1:ndom) {
            erro[[i]] <- erro[[i]][, -ncol(errors)]
            if (strcens == TRUE) 
                cens <- stcens[[i]]
            solut <- strataGenalg(errors = erro[[i]], strata = stcamp[[i]], 
                cens = cens, strcens, dominio = i, initialStrata, 
                minnumstr, iter, pops, mut_chance, elitism_rate, 
                addStrataFactor, highvalue, suggestions, realAllocation, 
				writeFiles, showPlot)
			v <- c(v,solut[[1]])
			outstrata <- rbind(outstrata, solut[[2]])
        }
    }
    # end alldomains = TRUE begin alldomains = FALSE
    if (alldomains == FALSE) {
        if (dom < 1 | dom > ndom) 
            stop("\nInvalid value of the indicated domain\n")
        i <- dom
        erro[[i]] <- erro[[i]][, -ncol(errors)]
        if (strcens == TRUE) 
            cens <- stcens[[i]]
        solut <- strataGenalg(errors = erro[[i]], strata = stcamp[[i]], 
            cens = cens, strcens, dominio = i, initialStrata, 
            minnumstr, iter, pops, mut_chance, elitism_rate, 
            addStrataFactor, highvalue, suggestions, realAllocation,
			writeFiles, showPlot)
		v <- solut[[1]]
		outstrata <- solut[[2]]
    }
    
    # end alldomains = FALSE
	colnames(outstrata) <- toupper(colnames(outstrata))
    dimens <- sum(outstrata$SOLUZ)
# Results
    cat("\n *** Sample size : ", dimens)
    cat("\n *** Number of strata : ", nrow(outstrata))
    cat("\n---------------------------")
    if (writeFiles == TRUE) {
        write.table(outstrata, file = "outstrata.txt", sep = "\t", 
            row.names = FALSE, col.names = TRUE, quote = FALSE)
			cat("\n...written output to outstrata.txt")
			}
	solution <- list(indices=v,aggr_strata=outstrata)
    return(solution)
}
# End function

