#' praiseEm
#'
#' @description Random praise
#'
#' @return A string of random praise
#' @noRd
#'
praiseEm <- function() {

  praiseList <- c(
    "Well done!",
    "Good job!",
    "Fantastic",
    "Nice work!",
    "You did it!",
    "Dazzling!",
    "Bonza, mate!",
    "You're doing great!",
    "There's no stopping you now",
    "Correct! Absolutely fabulous!",
    "Bejasus!",
    "Begorrah!",
    "Hit him again, he's no relation!"
    )


  praise <- sample(x=praiseList, size=1, prob=c(rep(20, length(praiseList)-3), rep(1,3)))

  return(praise)

}



#' encourageEm
#'
#' @description Random encouragement
#'
#' @return A string of random praise
#' @noRd
#'
encourageEm <- function() {

  encouragementList <- c(
          "Have another go",
          "Don't worry, you'll get it next time!",
          "No panic, have another try",
          "Give it another try",
          "Persevere!",
          "Don't worry, you've got this!",
          "Practice makes perfect",
          "Don't worry, this is how we learn",
          "Take another look, you'll get it",
          "Ever tried. Ever failed. No matter - Try again. Fail again. Fail better",
          "Give it a lash Jack"
  )


  encouragement <- sample(x=encouragementList, size=1, prob=c(rep(20, length(encouragementList)-1), rep(1,1)))

  return(encouragement)

}

#' happyFace
#'
#' @description Sample a random negative emoji
#'
#' @return A unicode for an emoji
#' @noRd
#'
happyFace <- function() {

  happyList <- c("\U1F600", "\U1F603", "\U1F604", "\U1F60A",
                 "\U1F929", "\U1F917", "\U1F973", "\U1F60E",
                 "\U1F979", "\U1F63B", "\U2764", "\U1F49A",
                 "\U1F4AF", "\U1F4AB", "\U1F90C", "\U1F918",
                 "\U1F919", "\U1F44D", "\U1F44F", "\U1F64C",
                 "\U1FAF6", "\U1F483", "\U1F57A", "\U1F938",
                 "\U1F436", "\U1F9A9", "\U1F99A", "\U1F99C")

  happyface <- sample(x=happyList, size=1)

  return(happyface)

}

#' thinkingFace
#'
#' @description Sample a random thinking emoji
#'
#' @return A unicode for an emoji
#' @noRd
#'
thinkingFace <- function() {

  thinkingList <- c("\U1F914", "\U1F92F", "\U1F9D0", "\U1F62E", "\U1F640")

  thinkingface <- sample(x=thinkingList, size=1)

  return(thinkingface)

}


#' sadFace
#'
#' @description Sample a random positive emoji
#'
#' @return A unicode for an emoji
#' @noRd
#'
sadFace <- function() {

  sadList <- c("\U1F914", "\U1FAE3", "\U1FAE0", "\U1F610",
               "\U1FAE5", "\U1F62C", "\U1F974", "\U1FAE4",
               "\U1F641", "\U1F633", "\U1F628", "\U1F625",
               "\U1F631	", "\U1F629", "\U1F63F")

  sadface <- sample(x=sadList, size=1)

  return(sadface)

}

