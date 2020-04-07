# Direct Download in Firefox Docker -------------------------------------------------
lst_ff_direct_download <- list(
  browser.download.dir                      = "/home/seluser/Downloads",
  browser.download.folderList               = 2L,
  browser.download.manager.showWhenStarting = FALSE,
  browser.helperApps.neverAsk.saveToDisk    = "application/pdf",
  pdfjs.disabled                            = TRUE
)
usethis::use_data(lst_ff_direct_download, overwrite = TRUE)
