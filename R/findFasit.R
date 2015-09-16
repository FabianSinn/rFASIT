#' Search through a FASIT df
#' 
#' Search through a FASIT df
#' 
#' @param fdf A FASIT data frame
#' @param params A key-value list of column names and search criteria (currently only additive search is implemented)
#' @export

findFasit <- function(fdf, params) {
   # Set the keys of the input data.table to the key names
   setkeyv(fdf, names(params))
   
   # Create a data.table from the params list
   pdt <- as.data.table(params)
   setkeyv(pdt, names(params))
   
   # Merge the 
   merge(fdf, pdt, by=names(params))
}
