#' Split muli-page PDFs in individual pages
#'
#' @param pdf.filename
#' @param pdf.dirname
#'
#' @returns
#' @export
#'
#' @examples
split_pdf <- function(pdf.filename, pdf.dirname){

  savedir.pdf <- paste0("~/readpcbpdfs/inputs/",pdf.dirname)
  dir.create(savedir.pdf)

  pdf.list <- list.files(savedir.pdf, pattern = "*.*pdf", full.names = TRUE)

  if(rlang::is_empty(pdf.list)==TRUE) {

    tabulapdf::split_pdf(paste0("~/readpcbpdfs/inputs/", pdf.filename), outdir = savedir.pdf)

  }

}

