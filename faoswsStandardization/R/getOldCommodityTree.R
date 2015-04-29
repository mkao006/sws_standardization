##' Get Old Commodity Tree
##' 
##' This function pulls the old commodity trees from the SWS.  These trees are
##' constructed from the extraction rate and shares data from the AUPUS domain.
##' 
##' @param geographicAreaFS A character vector containing the geographic area
##' codes in FS format.
##' @param timePointYears A character vector containing the years for which
##' commodity trees are wanted.
##' 
##' @return
##' 

getOldCommodityTree = function(geographicAreaFS, timePointYears){
    
    ## Define item and element codes
    measuredItemFS = GetCodeList(domain = "faostat_one",
                                 dataset = "FS1_SUA",
                                 dimension = "measuredItemFS")$code
    extractionElementCode = "41"
    
    ## Pull extraction rate data
    key = DatasetKey(domain = "faostat_one", dataset = "FS1_SUA",
                     dimensions = list(
                         Dimension(name = "geographicAreaFS",
                                   keys = geographicAreaFS),
                         Dimension(name = "measuredItemFS",
                                   keys = measuredItemFS),
                         Dimension(name = "timePointYears",
                                   keys = timePointYears),
                         Dimension(name = "measuredElementFS",
                                   keys = extractionElementCode)))
    extractionRateData = GetData(key)

    ## Pull share data (defines the shares but also the parent/child
    ## relationships for the commodity tree)
    key = DatasetKey(domain = "faostat_one", dataset = "aupus_share_fs",
                     dimensions = list(
                         Dimension(name = "geographicAreaFS",
                                   keys = geographicAreaFS),
                         Dimension(name = "measuredItemParentFS",
                                   keys = measuredItemFS),
                         Dimension(name = "measuredItemChildFS",
                                   keys = measuredItemFS),
                         Dimension(name = "timePointYearsSP",
                                   keys = timePointYears)))
    shareData = GetData(key)

#     # I don't think we need this, shareData defines the parent/child
#     #
#     key = DatasetKey(domain = "faostat_one", dataset = "input_from_proc_fs",
#                      dimensions = list(
#                          Dimension(name = "geographicAreaFS",
#                                    keys = geographicAreaFS),
#                          Dimension(name = "measuredItemParentFS",
#                                    keys = measuredItemFS),
#                          Dimension(name = "measuredItemChildFS",
#                                    keys = measuredItemFS),
#                          Dimension(name = "timePointYearsSP",
#                                    keys = timePointYears)))
#     inputFromProcessingData = GetData(key)
    
    setnames(extractionRateData,
             old = c("measuredItemFS", "timePointYears",
                     "Value", "flagFaostat"),
             new = c("measuredItemChildFS", "timePointYearsSP",
                     "extractionRate", "flagExtractionRate"))
    setnames(shareData, old = "Value", new = "share")
    edges = merge(shareData, extractionRateData,
                  by = c("geographicAreaFS", "measuredItemChildFS",
                         "timePointYearsSP"))
    edges
}