getHint <- function(pathDF, effect){

  # Numbers of different types of paths
  nOpenNonCausal <- pathDF %>% dplyr::filter(.data$open==TRUE & .data$directed==FALSE) %>% nrow()
  nOpenCausal <- pathDF %>% dplyr::filter(.data$open==TRUE & .data$directed==TRUE) %>% nrow()
  nClosedCausal <- pathDF %>% dplyr::filter(.data$open==FALSE & .data$directed==TRUE) %>% nrow()
  #nClosedNonCausal <- pathDF %>% dplyr::filter(open==FALSE & directed==FALSE) %>% nrow()

  text1 <- HTML(paste0("Remember, your aim is to select a minimal adjustment set to identify the ", tags$strong(effect), " effect of",
                       tags$strong(' X '), "on", tags$strong(' Y'), "."))

  text2 <- ifelse(effect=='total',
                  "This means you must close all non-causal paths between X and Y, while leaving all casual paths open.",
                  "This means you must close all causal and non-causal paths between X and Y, apart from the direct path X->Y.")

  text3 <- HTML(paste("Currently you have:", br(),
                      div("Open causal paths:", tags$strong(nOpenCausal), style="color:#419d78;"),
                      div("Open non-causal paths:", tags$strong(nOpenNonCausal), style="color:#75CAA4;"),
                      div("Closed causal paths:", tags$strong(nClosedCausal), style="color:#48284a;"),
                      #div("Closed non-causal paths:", tags$strong(nClosedNonCausal), style="color:#907692;"),
                      br()
  ))

  text4 <- "Explore these paths below!"

  return(
    shiny::tagList(p(text1), helpText(text2), p(text3), p(text4))
  )

}
