#' Get species names vector
#'
#' @param eachrow
#' @param sp.sheet
#'
#' @returns sp.vec
#' @export
#'
#' @examples
get_sp <- function(eachrow, sp.sheet){

  this.row <- sp.sheet[eachrow,]

  sp.vec <- rep(this.row$common_name, this.row$pages)

  return(sp.vec)
}

