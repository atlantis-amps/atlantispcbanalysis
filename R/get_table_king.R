
#' get tables from King 2020
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
get_table_king <- function(eachpdf, pdf.name, pdf.dir, sample.list){

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

  if(eachpdf<6){

    print("Analyzing table 1-5")

    for(eachtable in no.tables){

      print(eachtable)
      print(head(tables.file[[eachtable]]))

      res.table.1 <- tables.file[[eachtable]] %>%
        dplyr::select(`IUPAC`,`CO-ELUTIONS`,`CONC.`) %>%
        dplyr::filter(`CONC.`!="0") %>%
        dplyr::mutate(pcb_congener = dplyr::if_else(`CO-ELUTIONS`=="0",IUPAC,`CO-ELUTIONS`)) %>%
        dplyr::rename(value=`CONC.`) %>%
        dplyr::mutate(value=as.numeric(value)) %>%
        dplyr::select(pcb_congener, value) %>%
        tidyr::pivot_wider(names_from = pcb_congener, values_from = value)

      print(head(res.table.1))

      tables.list[[eachtable]] <- res.table.1
    }

  }

  if(eachpdf>=6){
    print("Analyzing table 6-13")
    # for eachpdf >= 6, the table structure is different

    for(eachtable in no.tables){

      print(eachtable)
      print(head(tables.file[[eachtable]]))

      if("PCB # CAS #" %in% colnames(tables.file[[eachtable]])){

        res.table <- tables.file[[eachtable]] %>%
          tidyr::separate(`PCB # CAS #`, into = c("pcb_congener", "CAS"), sep = " ", extra = "merge") %>%
          dplyr::select(`pcb_congener`, 'Conc.') %>%
          dplyr::filter(`pcb_congener`!="0") %>%
          dplyr::rename(value=`Conc.`) %>%
          dplyr::mutate(value=as.numeric(value)) %>%
          dplyr::mutate(pcb_congener = gsub("PCB-", "", pcb_congener)) %>%
          tidyr::pivot_wider(names_from = pcb_congener, values_from = value)

      } else if ("PCB #" %in% colnames(tables.file[[eachtable]])){

        res.table <- tables.file[[eachtable]] %>%
          dplyr::rename(value=`Conc.`, pcb_congener = `PCB #`) %>%
          dplyr::select(pcb_congener, value) %>%
          dplyr::mutate(value=as.numeric(value)) %>%
          dplyr::mutate(pcb_congener = gsub("PCB-", "", pcb_congener)) %>%
          tidyr::pivot_wider(names_from = pcb_congener, values_from = value)

      } else {
        stop("Unexpected column names in the table")

      }


      tables.list[[eachtable]] <- res.table

     }

   }


  sum.table <- dplyr::bind_cols(tables.list) %>%
    dplyr::mutate(sample_no = this.sample) %>%
    dplyr::select(sample_no, everything())

  return(sum.table)

  }
