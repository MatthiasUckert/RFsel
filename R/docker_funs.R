#' List Docker Containers
#'
#' @param type running / exited / all
#'
#' @return A Dataframe
#' @export
#'
#' @examples
#' library(RFsel)
#' docker_ps("running")
#' docker_ps("exited")
#' docker_ps("all")
docker_ps <- function(type = c("running", "exited", "all")) {
  type = match.arg(type)
  c_command <- switch(type,
    "running" = 'docker ps',
    "exited"  = 'docker ps -f "status=exited"',
    "all"     = 'docker ps -a'
  )
  
  h_docker_convert_to_tibble(c_command)
}

#' List Docker Images
#'
#' @return A Dataframe
#' @export
#'
#' @examples
#' library(RFsel)
#' docker_images()
docker_images <- function() {
  c_command <- "docker images"
  h_docker_convert_to_tibble(c_command)
}

#' Check if Docker is installed
#'
#' @return Logical
#' @export
#'
#' @examples
#' library(RFsel)
#' docker_check_install()
docker_check_install <- function() {
  system("docker", show.output.on.console = FALSE) == 0L
}


#' Stop Docker Containers
#'
#' @param ids A Character Vector of Conteiner IDs or Names
#'
#' @return Success Message
#' @export
docker_stop_container <- function(ids) {
  for (id in ids) {
    system(paste("docker stop", id), intern = TRUE)
  }
}

#' Stop Restart Containers
#'
#' @param ids A Character Vector of Conteiner IDs or Names
#'
#' @return Success Message
#' @export
docker_restart_container <- function(ids) {
  for (id in ids) {
    system(paste("docker stop", id), intern = TRUE)
  }
}

#' Stop Remove Containers
#'
#' @param ids A Character Vector of Conteiner IDs or Names
#'
#' @return Success Message
#' @export
docker_remove_container <- function(ids) {
  for (id in ids) {
    system(paste("docker rm", id), intern = TRUE)
  }
}





# HELPER FUCNTIONS ------------------------------------------------------------------
h_docker_convert_to_tibble <- function(command) {
  ps <- stringi::stri_split(system(command, intern = TRUE), regex = "\\s{2,}")
  tab <- purrr::map_dfr(ps[-1], ~ dplyr::bind_rows(setNames(.x, ps[[1]])))
  tab <- janitor::clean_names(tab)
  return(tab)
}

