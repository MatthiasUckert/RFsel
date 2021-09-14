# Direct Download in Firefox Docker -------------------------------------------------
firefox_ecaps_direct_download <- list(
  browser.download.dir = "/home/seluser/Downloads",
  browser.download.folderList = 2,
  browser.download.manager.showWhenStarting = FALSE,
  browser.helperApps.neverAsk.saveToDisk = readLines("data-raw/download_firefox_profile.txt"),
  browser.helperApps.neverAsk.openFile = readLines("data-raw/download_firefox_profile.txt"),
  browser.helperApps.alwaysAsk.force = FALSE,
  pdfjs.disabled = TRUE
)
usethis::use_data(firefox_ecaps_direct_download, overwrite = TRUE)



# lst_ff_direct_download <- list(
#   browser.download.dir                      = "/home/seluser/Downloads",
#   browser.download.folderList               = 2L,
#   browser.download.manager.showWhenStarting = FALSE,
#   browser.helperApps.neverAsk.saveToDisk    = "application/pdf",
#   pdfjs.disabled                            = TRUE
# )
# usethis::use_data(lst_ff_direct_download, overwrite = TRUE)
