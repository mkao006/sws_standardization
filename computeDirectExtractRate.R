########################################################################
## Title: Function to compute the direct extraction rate from the
##        processsed products (terminal nodes) to the primary products
##        (root node).
## Date: 2013-05-22
########################################################################

## NOTE (Michael): Need to use the average path rather than the
##                 shortest path since there can be multiple path.
##
## NOTE (Michael): The function assumes a single primary product
##

computeDirectExtractRate = function(Data, child, parent, ExtractionRate,
  plots = FALSE){
  ## Determine the scaling factor to avoid logging number less than 1
  k = max(Data[, ExtractionRate, with = FALSE], na.rm = TRUE)/10000 + 1
  ## Compute the conversion factor
  Data$cf = unlist((k * 10000)/Data[, ExtractionRate, with = FALSE])
  tmp.graph = graph.data.frame(Data[, c(child, parent), with = FALSE],
    directed = TRUE)
  if(plots)
    plot(tmp.graph)
  ## Find the primary product, assuming it has a circular loop
  ## primary = names(which(degree(tmp.graph, mode = "out") == 0))
  primary = V(tmp.graph)[which(diag(get.adjacency(tmp.graph)) == 1)]$name

  ## Compute the direct extraction rate
  wldist = shortest.paths(graph = tmp.graph,
    to = primary, weights = log(Data[, cf]), algorithm = "johnson")
  dist = shortest.paths(graph = tmp.graph, to = primary, algorithm = "johnson")
  wdist = exp(wldist - dist * log(k))
  finite = which(is.finite(wdist))

  ## put into data.frame
  tmp = data.frame(as.numeric(rownames(wdist)[finite]),
    as.numeric(rep(primary, length(finite))),
    wdist[finite], stringsAsFactors = FALSE)
  colnames(tmp) = c(child, "Primary", "Primary.Extraction.Rate")
  tmp[tmp[, 1] == tmp[, 2], 3] = 1
  tmp
}
