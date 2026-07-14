
#' get tables from Sun 2020
#'
#' @param eachpdf
#' @param pdf.name
#' @param pdf.dirname
#' @param sample.list
#'
#' @returns res.table
#' @export
#'
#' @examples
get_table_sun <- function(eachpdf, pdf.name, pdf.dir, sample.list){

  print(eachpdf)

  pdf.filename <- paste0(pdf.name,"_", eachpdf, ".pdf")
  print(pdf.filename)


  pdf.dirname <- paste0(pdf.dir, "_",eachpdf)

  split_pdf(pdf.filename, pdf.dirname)

  this.sample <- sample.list[eachpdf]

  #Extract tables

  pdf.list <- list.files(here::here("inputs", pdf.dirname), pattern = "*.*pdf", full.names = TRUE)

  no.pdf.files <- 1:length(pdf.list)

  tables.file <- lapply(no.pdf.files, get_table, pdf.list, pdf.dirname)

  no.tables <- 1:length(tables.file)

  tables.list <- list()

      for(eachtable in no.tables){

        print(eachtable)
        print(head(tables.file[[eachtable]]))

        colnames(tables.file[[eachtable]]) <- sample.list

        if(eachtable ==1){

          res.table.1 <- tables.file[[eachtable]] %>%
            dplyr::rename(pcb_congener=UNITS) %>%
            dplyr::mutate(pcb_congener = gsub("PCB-", "", pcb_congener)) %>%
            tidyr::pivot_longer(cols=2:ncol(.), names_to="station_no", values_to = "value") %>%
            dplyr::mutate(value=as.numeric(value)) %>%
            tidyr::pivot_wider(names_from = pcb_congener, values_from = value) %>%
            dplyr::mutate(station_no = 1:nrow(.))

        }

        if(eachtable >1){

          res.table.1 <- tables.file[[eachtable]] %>%
            dplyr::rename(pcb_congener=UNITS) %>%
            dplyr::mutate(pcb_congener = gsub("PCB-", "", pcb_congener)) %>%
            tidyr::pivot_longer(cols=2:ncol(.), names_to="station_no", values_to = "value") %>%
            dplyr::mutate(value=as.numeric(value)) %>%
            tidyr::pivot_wider(names_from = pcb_congener, values_from = value) %>%
            dplyr::select(-station_no)

        }

        tables.list[[eachtable]] <- res.table.1
    }


    res.table <- dplyr::bind_cols(tables.list)


  return(res.table)

  }
