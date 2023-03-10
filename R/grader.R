#' grader
#'
#' @param submission The submitted solution, a vector of nodes
#' @param dag A string defining the causal DAG
#' @param effect The desired effect, total (default) or direct
#'
#' @return A message indicating whether the submission was wrong or right

grader <- function(submission, dag, effect = 'total'){

  stopifnot(dagitty::is.dagitty(dag))
  stopifnot(effect %in% c('total', 'direct'))

  minSol <- dagitty::adjustmentSets(dag, type = 'minimal', effect = effect)

  mark <- 'incorrect'
  minSolN <- length(minSol)

  for (i in 1:minSolN) {

    if (setequal(submission, minSol[[i]])) {
      mark <- 'correct'
    }

  }

  if (mark!='correct' & effect=='total') {

    allSol <- dagitty::adjustmentSets(dag, type = 'all', effect = effect)
    allSolN <- length(allSol)

    for (i in 1:allSolN) {

      if (setequal(submission, allSol[[i]])) {
        mark <- 'close'
      }

    }

  }

  return(mark)
}
