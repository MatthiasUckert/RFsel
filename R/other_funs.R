#' Helper: Extract Doc ID
#'
#' @param .path path to the file
#'
#' @return A character (Doc ID)
extract_doc_id <- function(.path) {
  doc_id_ <- basename(.path)
  stringi::stri_replace_all_fixed(doc_id_, paste0(".", tools::file_ext(doc_id_)), "")
}

#' List files in a dataframe with id column (Updated Function)
#'
#' @description A helper function that is build around list.files (base)
#'
#' @param dirs Paths to the directories
#' @param reg RegEx to find files (defaults to '*' all files)
#' @param id Column name containing the file name (e.g. doc_id)
#' @param rec Should the directories be searched recursively?
#' @param info Additional Infos (base::)
#'
#' @importFrom rlang :=
#' @return A dataframe with file paths
#'
#' @export
#'
lft2 <- function(dirs, reg = "*", id = "doc_id", rec = FALSE, info = FALSE) {
  path <- file_ext <- NULL
  tab_fil <- purrr::map_dfr(
    .x = dirs, 
    .f = ~ tibble::tibble(path = list.files(.x, reg, FALSE, TRUE, rec))
  ) %>%
    dplyr::mutate(
      file_ext = paste0(".", tools::file_ext(path)), 
      `:=`(!!dplyr::sym(id), stringi::stri_replace_last_fixed(basename(path), file_ext, "")), 
      path = purrr::set_names(path, !!dplyr::sym(id))
    ) %>% dplyr::select(!!id, file_ext, path)
  
  if (info) {
    tab_fil <- dplyr::bind_cols(tab_fil, tibble::as_tibble(file.info(tab_fil$path)))
  }
  return(tab_fil)
}

#' List Files as a Named Character
#'
#' @param .dirs Paths to the directories
#' @param reg RegEx to find files (defaults to '*' all files)
#' @param rec Should the directories be searched recursively?
#'
#' @return A named character
#' @export
lfc <- function(.dirs, reg = "*", rec = FALSE) {
  fils <- unlist(purrr::map(.dirs, ~ list.files(.x, reg, FALSE, TRUE, rec)))
  names(fils) <- stringi::stri_replace_all_fixed(
    str = basename(fils),
    pattern = paste0(".", tools::file_ext(fils)), replacement = ""
  )
  return(fils)
}


#' Helper: Download a file via Javascript Injection
#'
#' @param .rsc A Remote Selenium Connection (rsc): Use sel_get_driver()
#' @param .url A Url 
#' @param .js A Javascript to be injected
#' @param .dir_map Mapped directory of the Docker Container (see: sel_docker_start()) 
#' @param .dir_doc Directory to save files
#' @param .wait Wait time before error
#'
#' @return A downloaded file and a character vector (path of the file is returned)
download_js <- function(.rsc, .url, .js, .dir_map, .dir_doc, .wait = 10) {
  doc_id <- file_ext <- NULL
  
  hash_ <- digest::digest(paste0(.url, .js))
  
  check_ <- try(invisible(file.remove(lfc(.dir_map))), TRUE)
  i <- 0
  while (inherits(check_, "try-error")) {
    check_ <- try(invisible(file.remove(lfc(.dir_map))), TRUE)
    Sys.sleep(1)
    i <- i + 1
    if (i == .wait) break
  }
  
  if (length(lfc(.dir_map)) != 0) {
    stop("Mapped Directory is NOT EMPTY")
  }
  
  
  files_out_ <- dplyr::filter(lft2(.dir_doc), doc_id == hash_)
  if (nrow(files_out_) > 0) {
    return(files_out_$path)
  }
  
  check_ <- try(
    expr = {
      .rsc$navigate(.url)
      .rsc$executeScript(.js)
    },
    silent = TRUE
  ) %>%
    suppressMessages() %>%
    suppressWarnings()
  
  if (inherits(check_, "try-error")) {
    return(paste0("Error: ", attr(check_, "condition")$message))
  }
  
  
  files_ <- lfc(.dir_map)
  
  i <- 0
  while (length(files_) == 0) {
    files_ <- lfc(.dir_map)
    Sys.sleep(1)
    i <- i + 1
    if (i == .wait) break
  }
  
  if (length(lfc(.dir_map)) == 0) {
    return("Error: File not downloaded")
  }
  
  if (length(lfc(.dir_map)) != 1) {
    invisible(file.remove(lfc(.dir_map)))
    return("Error: Multiple files downloaded")
  }
  
  files_ <- lft2(.dir_map) %>%
    dplyr::mutate(path_to = file.path(.dir_doc, paste0(hash_, file_ext)))
  
  file.rename(files_$path, files_$path_to)
  return(files_$path_to)
}

# .rsc = rsc; .urls = tab_urls_script_$url; .jss = tab_urls_script_$doc_url; .dir_map = dir_map; .dir_doc = dir_docs_; .wait = 10
#' Download a files via Javascript Injection
#'
#' @param .rsc A Remote Selenium Connection (rsc): Use sel_get_driver()
#' @param .urls A vector of URls 
#' @param .jss A vector of Javascripts to be injected
#' @param .dir_map Mapped directory of the Docker Container (see: sel_docker_start()) 
#' @param .dir_doc Directory to save files
#' @param .wait Wait time before error
#'
#' @return A Dataframe
#' @export
map_download_js <- function(.rsc, .urls, .jss, .dir_map, .dir_doc, .wait = 10) {
  doc_id <- file_ext <- doc_url <- doc_js <- path <- doc_id <- doc_error <- tmp <- doc_path <- NULL
  
  files_ <- lft2(.dir_doc)
  
  hashs_ <- tibble::tibble(
    doc_url = .urls,
    doc_js = .jss,
    doc_id = purrr::map2_chr(.urls, .jss, ~ digest::digest(paste0(.x, .y)))
  ) %>%
    dplyr::left_join(files_, by = "doc_id") %>%
    dplyr::mutate(doc_error = NA_character_) %>%
    dplyr::select(doc_url, doc_js, doc_path = path, doc_id, doc_error)
  
  prc_ <- dplyr::filter(hashs_, !is.na(doc_path))
  use_ <- dplyr::filter(hashs_, is.na(doc_path))
  
  if (nrow(use_) == 0) {
    return(prc_)
  }
  
  add_ <- purrr::map2_dfr(
    .x = purrr::set_names(use_$doc_url, paste0(use_$doc_url, "------------", use_$doc_js)),
    .y = use_$doc_js,
    .f = ~ tibble::tibble(doc_path = download_js(.rsc, .x, .y, .dir_map, .dir_doc, .wait)),
    .id = "tmp"
  ) %>%
    tidyr::separate(tmp, c("doc_url", "doc_js"), sep = "------------") %>%
    dplyr::mutate(doc_id = extract_doc_id(doc_path)) %>%
    dplyr::mutate(
      doc_error = dplyr::if_else(startsWith(doc_path, "Error"), doc_path, NA_character_),
      doc_path = dplyr::if_else(startsWith(doc_path, "Error"), NA_character_, doc_path)
    )
  
  dplyr::bind_rows(prc_, add_)
}

