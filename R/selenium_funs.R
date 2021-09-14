#' Start Docker Selenium Container
#'
#' @param type Debug/Static
#' @param browser Firefox/Chrome
#' @param port_sel Selenium Port (default: 4445)
#' @param port_vcn VCN Port (default: 5901)
#' @param dir_map Mapping Directory
#' @param version Browser Version
#'
#' @return A List
#' @export
sel_docker_start <- function(
  type = c("debug", "static"), browser  = c("firefox", "chrome"),
  version  = "latest", port_sel = 4445, port_vcn = 5901, dir_map  = NULL
  ) {
  
  
  # Check if Ports are allocated ------------------------------------------------------
  check <- h_docker_check_ports(port_sel, port_vcn)
  if (!is.na(check)) {
    tab <- docker_ps()
    id <- tab[["container_id"]][which(stringi::stri_detect_fixed(tab[["ports"]], port_sel))]
    message(check)
    return(list(id = id, port_sel = port_sel, port_vcn = port_vcn, dir_map = dir_map))
  }
  
  
  # Getting Selenium String -----------------------------------------------------------
  c_pull <- dplyr::case_when(
    type == "debug" & browser == "firefox"  ~ paste0("selenium/standalone-firefox-debug:", version),
    type == "static" & browser == "firefox" ~ paste0("selenium/standalone-firefox:", version),
    type == "debug" & browser == "chrome"   ~ paste0("selenium/standalone-chrome-debug", version),
    type == "static" & browser == "chrome"  ~ paste0("selenium/standalone-chrome", version)
  ) 
  
  
  # Getting Ports ---------------------------------------------------------------------
  p_sel <- ifelse(is.null(port_sel), "", glue::glue("-p {port_sel}:4444"))
  p_vcn <- ifelse(is.null(port_vcn), "", glue::glue("-p {port_vcn}:5900"))
  
  
  # Getting Mapping Directory ---------------------------------------------------------
  d_map <- ifelse(is.null(dir_map), "", normalizePath(dir_map))
  d_map <- ifelse(is.null(dir_map), "", glue::glue("-v {d_map}://home/seluser/Downloads"))
  
  # Commands --------------------------------------------------------------------------
  system(paste('docker pull', c_pull))
  id <- system(
    command = gsub("\\s+", " ", paste('docker run -d', d_map, p_sel, p_vcn, c_pull)), 
    intern = TRUE
  )
  
  return(list(id = id, port_sel = port_sel, port_vcn = port_vcn, dir_map = dir_map))
}


#' Make Selenium Driver
#'
#' @param port_sel Selenium Port (see sel_docker_start())
#' @param ecaps A list of Extra-Capabilites (Default for Firefox Direct DOwnload) 
#'
#' @return An object of class remote driver
#' @export
sel_get_driver <- function(port_sel, ecaps = list()) {
  ecaps_ <- NULL
  
  if (length(ecaps) == 0) {
    ecaps_ <- RSelenium::makeFirefoxProfile(RFsel::firefox_ecaps_direct_download)
  } else {
    ecaps_ <- RSelenium::makeFirefoxProfile(ecaps)
  }
  
  RSelenium::remoteDriver(
    remoteServerAddr = "localhost", 
    port = port_sel, 
    extraCapabilities = ecaps_
  )
}


# HELPER FUCNTIONS ------------------------------------------------------------------
h_docker_check_ports <- function(port_sel, port_vcn) {
  port_sel <- ifelse(is.null(port_sel), "NOT AVAILABLE", port_sel)
  port_vcn <- ifelse(is.null(port_vcn), "NOT AVAILABLE", port_vcn)
  
  check <- stringi::stri_detect(
    str = paste(docker_ps()[["ports"]], collapse = ""), 
    regex = c(as.character(port_sel), as.character(port_vcn))
  )
  
  msg1 <- ifelse(check[1], glue::glue("Selenium Port: {port_sel} is already allocated"), "")
  msg2 <- ifelse(check[2], glue::glue("VCN Port: {port_vcn} is already allocated"), "")
  msg  <- dplyr::case_when(
    all(check) ~ paste(msg1, "AND", msg2),
    check[1] ~ msg1,
    check[2] ~ msg2
  )
  
  return(msg)
  
}
