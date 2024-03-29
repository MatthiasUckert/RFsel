#' PIA get Regions
#'
#' @param piactl default: C:/Program Files/Private Internet Access/piactl.exe
#'
#' @return A character vector
#' @export
pia_get_regions <- function(piactl = NULL) {
  c_piactl <- '"C:/Program Files/Private Internet Access/piactl.exe"'
  i_piactl <- ifelse(is.null(piactl), c_piactl, piactl)
  sort(system(command = paste(i_piactl, 'get regions'), intern = TRUE))
}

#' PIA set Region 
#'
#' @param piactl default: C:/Program Files/Private Internet Access/piactl.exe
#' @param region A Region (see pia_get_regions())
#' @param .wait If TRUE, wait until PIA has connection status "Connected"
#'
#' @return Nothing, changes the PIA region
#' @export
pia_set_region <- function(piactl = NULL, region, .wait = TRUE) {
  c_piactl <- '"C:/Program Files/Private Internet Access/piactl.exe"'
  i_piactl <- ifelse(is.null(piactl), c_piactl, piactl)
  invisible(system(command = paste(i_piactl, 'set region', region), intern = TRUE))
  
  if (.wait) {
    while (!pia_get_connection_state() == "Connected") {
      Sys.sleep(1)
    }
  }
  
}


#' PIA connect
#'
#' @param piactl default: C:/Program Files/Private Internet Access/piactl.exe
#'
#' @return Nothing, connects PIA
#' @export
pia_connect <- function(piactl = NULL) {
  c_piactl <- '"C:/Program Files/Private Internet Access/piactl.exe"'
  i_piactl <- ifelse(is.null(piactl), c_piactl, piactl)
  invisible(system(command = paste(i_piactl, 'connect'), intern = TRUE))
}

#' PIA disconnect
#'
#' @param piactl default: C:/Program Files/Private Internet Access/piactl.exe
#'
#' @return Nothing, disonnects PIA
#' @export
pia_disconnect <- function(piactl = NULL) {
  c_piactl <- '"C:/Program Files/Private Internet Access/piactl.exe"'
  i_piactl <- ifelse(is.null(piactl), c_piactl, piactl)
  invisible(system(command = paste(i_piactl, 'disconnect'), intern = TRUE))
}

#' PIA get Connection State
#'
#' @param piactl default: C:/Program Files/Private Internet Access/piactl.exe
#'
#' @return A character of the PIA connection State
#' @export
pia_get_connection_state <- function(piactl = NULL) {
  c_piactl <- '"C:/Program Files/Private Internet Access/piactl.exe"'
  i_piactl <- ifelse(is.null(piactl), c_piactl, piactl)
  system(command = paste(i_piactl, 'get connectionstate'), intern = TRUE)
}

#' PIA get VPN IP
#'
#' @param piactl default: C:/Program Files/Private Internet Access/piactl.exe
#'
#' @return A caracter, PIA IP Address
#' @export
pia_get_vpn_ip <- function(piactl = NULL) {
  c_piactl <- '"C:/Program Files/Private Internet Access/piactl.exe"'
  i_piactl <- ifelse(is.null(piactl), c_piactl, piactl)
  system(command = paste(i_piactl, 'get vpnip'), intern = TRUE)
}

#' Check if PIA is running
#'
#' @return Logical
#' @export
pia_check_if_running <- function() {
  any(stringi::stri_detect(sort(system(
    "tasklist", intern = TRUE
  )), fixed = "pia-client.exe"))
  
}

