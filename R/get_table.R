#' Extract table
#'
#' @param eachpdf
#' @param pdf.list
#' @param pdf.dirname
#'
#' @returns
#' @export
#'
#' @examples
get_table <- function(eachpdf, pdf.list, pdf.dirname){

  print(eachpdf)
  this.pdf <- pdf.list[eachpdf]

  print(this.pdf)

  # img.name <- here::here("inputs","burd2014",paste0(pdf.dirname,"_",eachpdf,".png"))
  #
  # pdftools::pdf_convert(this.pdf, format = "png", dpi = 150, filenames=img.name, verbose = TRUE)
  #
  # team_mgk <- img.name %>%
  #   tesseract::ocr()


   if(pdf.dirname=="wdoe2015" & (eachpdf == 13 | eachpdf == 14 | eachpdf == 15 | eachpdf == 1)) {

     print("extracting table")
    tables.file <- tabulapdf::extract_tables(this.pdf)

   } else if(pdf.dirname=="ross2011_1"){

     if(eachpdf==7 | eachpdf==8){

       #print
       print("Extend selected area to far right otherwise not all columns are captured")
       print("Include the Table name and page number")

       #use extract areas because for some reason extract_tables will skip half of some tables
       tables.file <- tabulapdf::extract_areas(this.pdf)

     } else {

       print("extracting table")
       tables.file <- tabulapdf::extract_tables(this.pdf)

     }
   }  else if (pdf.dirname=="ross2011_2" | pdf.dirname=="calambodikis1999_2" | pdf.dirname=="wdoe2015_2" | pdf.dirname=="grant2011") {

       print("extracting table")
       tables.file <- tabulapdf::extract_tables(this.pdf)


   } else if(pdf.dirname=="calambodikis1999_3"){

     if(eachpdf==7| eachpdf=="B"){

       #print
       print("Extend selected area to far right otherwise not all columns are captured")
       print("Do NOT include the Table name")

       #use extract areas because for some reason extract_tables will skip half of some tables
       tables.file <- tabulapdf::extract_areas(this.pdf)

     } else {

       print("extracting table")
       tables.file <- tabulapdf::extract_tables(this.pdf)

     }


     } else {

    #use extract areas because for some reason extract_tables will skip half of some tables
    tables.file <- tabulapdf::extract_areas(this.pdf)


  }

  tables.unlist <- tables.file[[1]] %>%
    dplyr::mutate_if(is.logical, as.character) %>%
    dplyr::mutate_if(is.numeric, as.character) %>%
    dplyr::mutate(dplyr::across(tidyr::everything(), ~tidyr::replace_na(.x, "0")))

  print(tables.unlist)

if(tables.unlist[2,1]=="PCB Congeners (ng/kg DW)"){

  table.res <- tables.unlist[3:nrow(tables.unlist),]

} else if(pdf.dirname=="ross2011" | pdf.dirname=="calambodikis1999_1" | pdf.dirname=="calambodikis1999_2" | pdf.dirname=="calambodikis1999_3" | pdf.dirname=="sun2020_1" | pdf.dirname=="sun2020_2") {

  table.res <- tables.unlist[1:nrow(tables.unlist),]

} else {

  table.res <- tables.unlist[2:nrow(tables.unlist),]

}

  return(table.res)

}

