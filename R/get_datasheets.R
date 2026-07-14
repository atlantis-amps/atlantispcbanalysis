#' Read database from Google sheet
#'
#' @param sheetnum
#' @param sheet.data
#' @param database.link
#'
#' @returns
#' @export
#'
#' @examples
get_datasheets <- function(sheetnum, sheet.data, database.link){

  print(sheetnum)
  this.sheet <- sheet.data[sheetnum,]$sheet_name
  print(this.sheet)
  this.data <- googlesheets4::read_sheet(database.link, sheet=this.sheet, col_names=TRUE, col_types = c("cccdcddddcdcccccddcccccccc"))
  print(dim(this.data))

  if(!"lon" %in% names(this.data)){
    print(this.sheet)
    stop ("lon is not a column name")
  }
  if(ncol(this.data)!=26)stop("The number of columns should be 26")

  any_list_column <- this.data %>%
    purrr::map_lgl(is.list) %>%
    any()


  if(any_list_column==TRUE) stop("A column is a list")
  return(this.data)
}
