#' getOpenPaths
#'
#' @description Get the open paths from a dagitty object, optionally adjust for covariates
#'
#' @param dag a dagitty object
#'
#' @return A dataframe listing paths, open(T/F) and directed(T/F)
#' @noRd
#'
getOpenPaths <- function(dag, adj=NULL){

  stopifnot(dagitty::is.dagitty(dag))

  p1 <- dagitty::paths(dag, Z = as.list(adj)) %>%
    as.data.frame()


  p2 <- dagitty::paths(dag, Z = as.list(adj), directed = TRUE) %>%
    as.data.frame() %>%
    dplyr::mutate(directed = TRUE)

  pathList <- dplyr::full_join(p1, p2, by=c('paths','open')) %>%
    dplyr::mutate(directed = ifelse(is.na(.data$directed), FALSE, .data$directed)) %>%
    dplyr::filter(!(.data$directed==FALSE & .data$open==FALSE))

  return(pathList)
}
