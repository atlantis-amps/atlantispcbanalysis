#' Extract table for West 2001
#'
#' @param eachpdf
#' @param pdf.list
#' @param sp.names
#'
#' @returns
#' @export
#'
#' @examples
get_table_west <- function(eachpdf, pdf.list, sp.sheet.expanded){

  print(eachpdf)
this.pdf <- pdf.list[eachpdf]

print(this.pdf)

tables.file <- tabulapdf::extract_tables(this.pdf)

print(ncol(eachpdf))

if (eachpdf==173){

  table.data.a <- tables.file[[1]] %>%
    .[2:nrow(tables.file[[1]]),] %>%
    dplyr::select(-`...2`) %>%
    dplyr::filter(grepl("Sinclair", `...1`)) %>%
    tidyr::separate(`...1`,into=c("Location","loc2", "Assay"), sep=" ") %>%
    dplyr::mutate(Location = paste(Location, loc2)) %>%
    dplyr::select(-loc2) %>%
   tidyr::separate(`No. Detected:`,into=c("No. Analyzed", "Mean"), sep=" ")

  table.data.b <- tables.file[[1]] %>%
    .[2:nrow(tables.file[[1]]),] %>%
    dplyr::select(-`...2`) %>%
    dplyr::filter(grepl("Waterway", `...1`)) %>%
    tidyr::separate(`...1`,into=c("Location","loc2","loc3","Assay"), sep=" ") %>%
    dplyr::mutate(Location = paste(Location, loc2, loc3)) %>%
    dplyr::select(-loc2, -loc3) %>%
    tidyr::separate(`No. Detected:`,into=c("No. Analyzed", "Mean"), sep=" ")

  table.data <- dplyr::bind_rows(table.data.a, table.data.b)

} else if(eachpdf==172){

  table.data <- tables.file[[1]] %>%
    .[2:nrow(tables.file[[1]]),] %>%
    tidyr::separate(`No. Detected:`,into=c("no_det","mean","min"), sep=" ")


} else if(eachpdf==171){

     table.data <- tables.file[[1]] %>%
      .[2:nrow(tables.file[[1]]),] %>%
      tidyr::separate(`No. Detected:`,into=c("no_det","mean","min"), sep=" ") %>%
       dplyr::select(-`...5`)


} else {

  table.data <- tables.file[[1]] %>%
    .[2:nrow(tables.file[[1]]),]
}

header.names <- c("Location", "Assay", "No. Analyzed", "Mean", "Min", "Max", "SD")


this.species <- sp.sheet.expanded[eachpdf,]
print(this.species)

table.data.sp <- table.data %>%
  setNames(header.names) %>%
  dplyr::bind_cols(this.species)

print(head(table.data.sp))

return(table.data.sp)
}

