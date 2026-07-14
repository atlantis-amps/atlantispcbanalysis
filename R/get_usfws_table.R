#' Get USFWS data
#'
#' @param eachfile
#' @param uwfs.files list of files
#'
#' @returns current.data
#' @export
#'
#' @examples
get_usfws_table <- function(eachfile, uwfs.files){

  print(eachfile)

  this.file <- uwfs.files[eachfile]
  print(this.file)
  num.cols <- readr::count_fields(this.file, readr::tokenizer_csv(), n_max = 1)
  print(num.cols)

  Sys.sleep(1)

  ecmds.data <- data.table::fread(this.file, colClasses = 'character', data.table = FALSE) %>%
    dplyr::mutate(`Genus / Species` = as.character(`Genus / Species`))


  Sys.sleep(1)

    if(any(grepl("Analyte Name", names(ecmds.data))==TRUE)){

    pcb.test <-   ecmds.data %>% dplyr::distinct(`Analyte Name`) %>% dplyr::pull(`Analyte Name`)

    if(any(grepl("PCB", pcb.test)==TRUE)){

    current.data <- ecmds.data %>%
      dplyr::select("Catalog Number","Sample Number","Sample Matrix", "Matrix Category", "Analyte Name", "Common Name","Genus / Species", "Wet Weight (ppm)", "Collection Date", "Location","Decimal Latitude","Decimal Longitude","Sample Age","Sample Sex") %>%
      dplyr::filter(grepl("PCB", `Analyte Name`)) %>%
      dplyr::filter(!grepl("-PCB", `Analyte Name`)) %>%
      dplyr::mutate(pcb_congener=gsub("PCB# ","", `Analyte Name`), file_no = eachfile, file_name = this.file) %>%
      dplyr::filter(pcb_congener!="PCB-TOTAL") %>%
      dplyr::mutate(`Common Name` = dplyr::if_else(`Sample Matrix` == "Sediments", "sediment",
                                                   dplyr::if_else(`Sample Matrix` =="Water","water",`Common Name`)),
                    `Genus / Species` = dplyr::if_else(`Genus / Species` == "Unknown", "NA", `Genus / Species`)) %>%
      dplyr::mutate(year = lubridate::year(lubridate::mdy(`Collection Date`))) %>%
      dplyr::select(-`Analyte Name`, -`Collection Date`)


    return(current.data)


    } else {

      print("No PCB data found in this file")
      print(this.file)

      Sys.sleep(1)
    }


    Sys.sleep(1)

    } else {

      print(this.file)
      print(colnames(ecmds.data))

      stop("Analyte Name not present in the data file. Please check the file format or content.")
    }



 }

