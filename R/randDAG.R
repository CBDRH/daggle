#' randDAG
#'
#' @description Generate a random dag with assigned exposure & outcome and standard naming convention
#'
#' @param n The number of nodes
#' @param p The degree of connectivity
#' @param seed A random seed
#'
#' @return A `tidy_dagitty` object
#'
#' @export
#'
#' @examples
#' dag <- randDAG(6, .5)
randDAG <- function(n, p, seed=1234){

  set.seed(seed)

  p1 <- pcalg::randomDAG(n = n, prob = as.numeric(p))
  p2 <- methods::as(p1, "matrix")
  p3 <- pcalg::pcalg2dagitty(p2, labels = paste0('x', 1:n))
  rd0 <- gsub('pdag', 'dag', p3)

  draw <- sample(seq(1,n-1), 1)
  exposure <- paste0('x', max(draw))
  outcome <- paste0('x', draw-1)

  #Assign standard names
  df1 <- data.frame(
    oldName = paste0('x', 1:n)
  ) %>%
    dplyr::mutate(
      exposure = .data$oldName == exposure,
      outcome = .data$oldName == outcome
    ) %>%
    dplyr::arrange(outcome, exposure) %>%
    dplyr::mutate(newName = c(paste0("Z", 1:(n-2)), "X", "Y")) %>%
    dplyr::select(dplyr::all_of(c("oldName", "newName")))

  # Rename the nodes in the random dagitty object
  rd1 <- rd0
  for (i in 1:n) {
    rd1 <- gsub(df1[i, "oldName"], df1[i, "newName"], rd1)
  }

  rd1 <- sub("X", "X [exposure]", rd1) # Designate X as the exposure
  rd1 <- sub("Y", "Y [outcome]", rd1) # Designate Y as the outcome
  rd1 <- sub("X -> Y\n", "", rd1) # Ensure the DAG includes an arrow pointing from X to Y
  rd1 <- sub("}", "X -> Y\n}", rd1) # Ensure the DAG includes an arrow pointing from X to Y

  rd2 <- ggdag::node_status(rd1) # create tidy_dagitty object with status variable

  return(rd2)

}
