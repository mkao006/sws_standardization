##' Adjust Commodity Tree
##' 
##' This function takes a set of commodity trees (typically all countries for
##' one year) and adjusts the extraction rates.  The rationale is that previous
##' extraction rates were manually adjusted, sometimes to extreme values.
##' Thus, we need a consistent way to modify those rates, and the logic for
##' adjusting those rates is contained in this function.
##' 
##' @param commodityTree A data.table containing the edge structure of the
##' commodity tree (specified by parent/child columns) and the extraction
##' rates and shares.  Typically, this is produced from getOldCommodityTree.
##' @param parentColname The column name of commodityTree which contains the
##' ID for the parent commodity.
##' @param childColname The column name of commodityTree which contains the
##' ID for the child commodity.
##' @param extractionColname The column name of commodityTree which contains the
##' extraction rates.  Extraction rates represent the quantity of the child that
##' can be produced from 1 unit of the parent.  Typically, though, they are
##' expressed in 10,000's: i.e. if the extraction rate is 8000, 1 parent unit
##' can be processed into 1 * 8000/10000 = 0.8 child units.
##' @param shareColname The column name of commodityTree which contains the
##' shares variable.  This value represents the percent of the parent commodity
##' that is processed into the child.  For example, a share value of 50 means
##' that 300 units of the parent would be processed into 300*50/100 = 150 units
##' of the child.
##' @param byKey A character vector of column names of commodityTree.
##' We group extraction rates based on averages across groups.
##' The byKey argument provides the column names which should be used as
##' grouping variables for estimating this mean.  Note that parentColname and
##' childColname are always used as grouping variables, as extraction rates are
##' specific to edges.  This defaults to NULL, in which case all values (for
##' the same edge) are averaged.
##' @param nSigma The number of standard deviations away from the mean that
##' is deemed acceptable.  Any extraction rates outside of this range will be
##' updated based on the estimated mean and standard error.  The new extraction
##' rate value will be based on the quantile of the original value.  If the 
##' original value was the largest value in a group of N extraction rates, then
##' the new value will be the (N-0.5)/N*100 percentile of a normal distribution
##' with mean and variance estimated from the data.  If all data should be
##' mapped to the corresponding normal quantile, set nSigma = 0.  To just
##' adjust extreme values, set nSigma to something like 2 or 3.  Note: this
##' mapping may not preserve the original ordering, and it is only guaranteed
##' to do so if nSigma = 0 (all adjusted) or nSigma = Inf (none adjusted).
##' 
##' @return This function returns an object that is the same as the input
##' commodityTree except that extreme extraction rates have been adjusted.
##' 

adjustCommodityTree = function(commodityTree,
                               parentColname = "measuredItemParentFS",
                               childColname = "measuredItemChildFS",
                               extractionColname = "extractionRate",
                               shareColname = "share",
                               byKey = NULL,
                               nSigma){
    
    ## Data Quality Checks
    stopifnot(is(commodityTree, "data.table"))
    stopifnot(c(parentColname, childColname, extractionColname, shareColname)
              %in% colnames(commodityTree))
    
    ## Estimate the mean and variance for each unique year/commodity pair
    byKey = c(byKey, parentColname, childColname)
    ## Overwrite the default huber function so it returns NA/NA instead of
    ## errors when the MAD (Median Absolute Deviation) can't be estimated.
    huber = function(...){
        values = try(MASS::huber(...), silent = TRUE)
        if(is(values, "try-error"))
            return(list(mu = NA_real_, s = NA_real_))
        return(values)
    }
    commodityTree[, c("meanRate", "sdRate") :=
                       huber(get(extractionColname), k = 1.5), by = byKey]
    
    ## Now, update extreme values with the corresponding value on a normal
    ## curve (based on their quantiles).
    commodityTree[, normalScore := (extractionRate - meanRate)/sdRate]
    ## Subtract 0.5 so that quantiles are at 0.5/N, ..., (N-0.5)/N instead of
    ## 1/N, 2/N, ..., 1.
    commodityTree[, extractionQuantile := (rank(normalScore) - .5)/.N,
                   by = byKey]
    commodityTree[, normalValue := qnorm(extractionQuantile, mean = meanRate,
                                         sd = sdRate)]
    ## Update the extreme values, where "extreme" is defined based on a
    ## user-provided parameter.
    commodityTree[normalScore > nSigma, extractionRate := normalValue]
}