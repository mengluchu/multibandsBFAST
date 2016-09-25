#' @title  rearrange array from n-d to m-d, with m < n 
#' @param original_array 
#' @param position place /order/position of the new dimension
#' @param flatten dimensions to be flattened. 
#' @return rearranged array
#' @export 
rearrange_array <- function(original_array, flatten = c(1), position = min(flatten)) {
    flatten1 <- sort(flatten)
    a <- c(1:length(dim(original_array)))
    a <- a[-flatten1]
    apermarray <- aperm(original_array, c(flatten1, a))
    
    arrschema <- c(prod(dim(original_array)[flatten1]), dim(original_array)[a])
    dim(apermarray) <- arrschema
    # now the new dimension is at the front i.e. the first dimension of an
    # array.  the following codes put the new dimension at spicified or
    # default position
    
    if (position == 1) 
        newarr = apermarray else {
        b <- c(1:length(dim(apermarray)))
        if (position + 1 > length(dim(apermarray))) 
            s <- c(2:position, 1)  #if position is at the last dimension
 else s <- c(2:position, 1, (position + 1):length(b))
        # new schema
        newarr <- aperm(apermarray, s)
    }
    return(newarr)
}
# rearrange_array (original_array=X, flatten=c(2,3), position=1)
