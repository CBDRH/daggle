#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # Show loading screen
  observeEvent(rv$start, {

    hints <- c("Break long paths into triplets and examine each triplet in turn",
               "Controlling for a confounder closes the path",
               "Controlling for a mediator closes the path",
               "Controlling for a collider opens the path",
               "Closing one path can sometimes open another path",
               "For a minimal adjustment set, just choose one")

    n <- floor(stats::runif(1, 1, length(hints)+1))
    introGif <- paste0("www/tutorial-gifs-", n, ".gif")

    hint <- hints[n]

    shinyalert::shinyalert(title = 'Loading...', imageUrl = 'www/daggle-logo.png',
                           imageWidth = 300, imageHeight = 120, closeOnEsc = TRUE, closeOnClickOutside = TRUE,
                           showConfirmButton = TRUE, html = TRUE, size = 'm',
                           text = tagList( shiny::h3(paste0("Tip #", n)),
                                           shiny::helpText(hint),
                                           tags$img(src = introGif, width=400))
    )
  }, once = TRUE)

  # Capture dimensions of viewport for DAG plotstimer = 2000,
  dimension <- reactiveValues()
  observe({

    dimension$height1 <- session$clientData[["output_dag1-plot_height"]]
    dimension$width1 <- session$clientData[["output_dag1-plot_width"]]
    dimension$height2 <- session$clientData[["output_dag2-plot_height"]]
    dimension$width2 <- session$clientData[["output_dag2-plot_width"]]
    dimension$height3 <- session$clientData[["output_dag3-plot_height"]]
    dimension$width3 <- session$clientData[["output_dag3-plot_width"]]
    dimension$height4 <- session$clientData[["output_dag4-plot_height"]]
    dimension$width4 <- session$clientData[["output_dag4-plot_width"]]

  })

  # set up reactive values pid = NULL,
  rv <- reactiveValues(n = 5, p = '0.6', effect = 'total', id = NULL, solutionChoice = 1, start = 1)

  observe({
    rv$pid <- ifelse(is.null(rv$pid), s1, rv$pid)  # s1 random starting number set in colors.R
  })

  # Update settings if a unique id is supplied
  observeEvent(rv$id, {
    req(rv$id)

    idA <- as.numeric(substr(rv$id, 1, 1))
    idB <- as.numeric(substr(rv$id, 2, 2))
    idC <- as.numeric(substr(rv$id, 3, 3))
    idD <- as.numeric(substr(rv$id, 4, 6))

    rv$effect <- ifelse(idA == 1, 'total', 'direct')
    rv$n <- floor((idB + 8)/2)
    rv$p <- if (idC < 3) {
      "0.4"
    } else if (idC < 7) {
      "0.6"
    } else "0.8"

    rv$pid <- as.integer(idD)

  })

  # Update unique ID based on current settings
  observe({

    req(rv$n, rv$p, rv$pid, rv$effect)

    aID <- ifelse(rv$effect=="total", 1, 2)
    bID <- (rv$n*2)-8
    cID <- if (rv$p == "0.4") {
      "0"
    } else if (rv$p == "0.6") {
      "5"
    } else "9"

    dID <- as.character(rv$pid)

    rv$id <- as.numeric(paste0(aID, bID, cID, dID))
  })


  # Show settings on click

  # Make the url available on click
  observeEvent(input$settings, {

    updateNumericInput(session, 'n', value = rv$n)
    updateSelectInput(session, 'p', selected = rv$p)
    updateNumericInput(session, 'pid', value = rv$pid)
    updateRadioButtons(session, 'effect', selected = rv$effect)

    showModal(modalDialog(
      title = HTML(paste(icon('gear'), "Settings")),
      helpText("Updating these settings will generate a new DAG"),
      numericInput("n", "Number of nodes", value = 5, min = 3, max = 8, step = 1),
      selectInput("p", "Complexity", choices = c("Easy" = .4, "Moderate" = .6, "Difficult" = .8), selected = .6),
      numericInput("pid", "Puzzle ID", NULL, step = 1, min = 100, max = 999),
      hr(),
      helpText("Updating the effect of interest won't change the current DAG"),
      radioButtons("effect", "Effect of interest", choices = c('Total effect of X on Y' = 'total', 'Direct effect of X on Y' = 'direct'), selected = 'total', inline = FALSE),
      footer = tagList(div(style = "text-align:right;",
                           actionButton("cancelSettings", "Cancel", icon = icon('window-close')),
                           actionButton("saveSettings", "Save and run", icon = icon('save'))
      )),
      easyClose = FALSE,
      fade = TRUE
    ))
  })

  # Make the url available on click
  observeEvent(input$settings2, {

    updateNumericInput(session, 'n', value = rv$n)
    updateSelectInput(session, 'p', selected = rv$p)
    updateNumericInput(session, 'pid', value = rv$pid)
    updateRadioButtons(session, 'effect', selected = rv$effect)

    showModal(modalDialog(
      title = HTML(paste(icon('gear'), "Settings")),
      numericInput("n", "Number of nodes", value = 5, min = 3, max = 8, step = 1),
      selectInput("p", "Complexity", choices = c("Easy" = .4, "Moderate" = .6, "Difficult" = .8), selected = .6),
      numericInput("pid", "Puzzle ID", NULL, step = 1, min = 100, max = 999),
      radioButtons("effect", "Effect of interest", choices = c('Total effect of X on Y' = 'total', 'Direct effect of X on Y' = 'direct'), selected = 'total', inline = FALSE),
      footer = tagList(div(style = "text-align:right;",
                           actionButton("cancelSettings", "Cancel", icon = icon('window-close')),
                           actionButton("saveSettings", "Save and run", icon = icon('save'))
      )),
      easyClose = FALSE,
      fade = TRUE
    ))
  })


  # Close settings modal
  observeEvent(input$cancelSettings, {
    removeModal()
  })

  observeEvent(input$saveSettings, {
    removeModal()
    rv$n <- input$n
    rv$p <- input$p
    rv$pid <- input$pid
    rv$effect <- input$effect
    rv$controls <- NULL
  })


  observe({
    updateNumericInput(session, 'n', value = rv$n)
    updateSelectInput(session, 'p', selected = rv$p)
    updateNumericInput(session, 'pid', value = rv$pid)
    updateRadioButtons(session, 'effect', selected = rv$effect)
  })

  observeEvent(rv$n == rv$n, {
    updateNumericInput(session, 'n', value = rv$n)
  }, ignoreNULL = FALSE)


  # Instructions modal
  observeEvent(input$instructions, {

    showModal(modalDialog(
      title = HTML(paste(icon('question-circle'), "How to play")),
      HTML(paste0(tags$h3('Welcome to daggle!'),
                  tags$p("The aim is to identify the smallest possible adjustment set to obtain an unbiased estimate of the effect of an exposure ",
                  tags$span(class='xNode', "X"),
                  " on an outcome ",
                  tags$span(class='yNode', "Y"), ".")
      )),


      p("Click or tap on a node to add a variable to the adjustment list. Click or tap on an adjusted node to remove it from the list."),

      p("To estimate the total effect, a minimal adjustment set must close any open backdoor paths between X and Y. To estimate the direct effect you must also control for mediating variables between X and Y." ),

      p('To check if a path is closed, split the path up into consecutive triplets and examine each triplet. If any triplet is closed the whole path is closed.'),

      tags$img(class="center", src="www/dag-examples.png"),

      p(HTML(paste0(tags$p("Click on the gear icon", actionButton("settings2", NULL, icon = icon('gear'), class = "download"), "to change the number of nodes, the DAG complexity or the effect of interest
                   (either the ", span(style = "color: black; background-color:white; font-weight: bold;", "total"), " effect or the ",
                    span(style = "color: black; background-color:white; font-weight: bold;", "direct"), " effect).")
      ))),

      footer = tagList(div(style = "text-align:right;",
                           actionButton("closeInstructions", "Got it", icon = icon('thumbs-up'))
      )),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  # Close instructions modal
  observeEvent(input$closeInstructions, {
    removeModal()
  })


  # Directions text

  output$directions <- renderUI({

    req(rv$effect)

    effect <- rv$effect
    text <- HTML(paste("Select a minimal adjustment set to identify the", tags$strong(effect), "effect of",
                       tags$span(class = 'xNode', "X"), "on",
                       tags$span(class = 'yNode', "Y")
    ))

    helpText(text)

  })


  # Reset reactive values on panel change
  observeEvent(input$panel,{
    rv$controls <- NULL
    rv$controls2 <- NULL
    rv$solutionChoice <- 1
  })


  # Control clicked variable
  clickedVar <- reactive({

    req(input$plotClick$x)

    tol <- 0.1
    currentDag <- dag1()
    if (input$panel == "Tutorial") {
      currentDag <- dag2()
    }

    currentDag$data %>%
      dplyr::filter(dplyr::between(.data$x, input$plotClick$x - tol, input$plotClick$x + tol)) %>%
      dplyr::filter(dplyr::between(.data$y, input$plotClick$y - tol, input$plotClick$y + tol)) %>%
      dplyr::select(.data$name) %>%
      dplyr::filter(.data$name != 'X') %>%
      dplyr::filter(.data$name != 'Y') %>%
      unlist() %>%
      unique()
  })

  observeEvent(input$plotClick, {

    req(clickedVar())

    if (input$panel == "Random"){
      if (clickedVar() %in% rv$controls) {
        rv$controls <- rv$controls[! rv$controls %in% clickedVar()]
      }
      else {
        rv$controls <- append(rv$controls, clickedVar()) %>% unique()
      }
    }

    if (input$panel == "Tutorial"){
      if (clickedVar() %in% rv$controls2) {
        rv$controls2 <- rv$controls2[! rv$controls2 %in% clickedVar()]
      }
      else {
        rv$controls2 <- append(rv$controls2, clickedVar()) %>% unique()
      }
    }

  })

  output$printSelected <- renderText({

    if (all.equal(0, length(rv$controls)) == TRUE) {
      "No adjustment needed"
    } else c('Adjust for', knitr::combine_words(sort(rv$controls), oxford_comma = FALSE))
  })

  output$printSelected2 <- renderText({

    if (all.equal(0, length(rv$controls2)) == TRUE) {
      "No adjustment needed"
    } else c('Adjust for', knitr::combine_words(sort(rv$controls2), oxford_comma = FALSE))
  })


  # Control the active DAG

  # Reset reactive values on Run
  observeEvent(input$run,{

    rv$controls <- NULL
    rv$controls2 <- NULL
    rv$reveal <- 0
    rv$reveal2 <- 0
    rv$solutionChoice <- 1
    rv$pid <- as.integer(100 + stats::runif(1)*900)
  })

  # Reset reactive values on Run2
  observeEvent(input$run2,{

    removeModal()
    rv$controls <- NULL
    rv$controls2 <- NULL
    rv$reveal <- 0
    rv$reveal2 <- 0
    rv$solutionChoice <- 1
    rv$pid <- as.integer(100 + stats::runif(1)*900)
  })

  ## Random DAG
  dag1 <- eventReactive(input$run | input$saveSettings | input$run2, {

    req(rv$n, rv$p, rv$pid)

    randDAG(rv$n, rv$p, rv$pid)
  }, ignoreNULL = FALSE)

  ## Tutorial DAG
  dag2 <- reactive({

    req(input$panel, input$tuteID)

    dag <- eval(as.name(paste0('g', input$tuteID)))
    labs <- eval(as.name(paste0('label', input$tuteID)))

    dag %>%
      dagitty::dagitty() %>%
      dag_label(labels = labs) %>%
      adjust_for(NULL) %>%
      node_status()
  })

  # Control the adjusted DAG
  dagAdj1 <- reactive({
    dag1() %>% adjust_for(rv$controls)
  })

  # Control the adjusted DAG
  dagAdj2 <- reactive({
    dag2() %>% adjust_for(rv$controls2)
  })

  dagSolution1 <- reactive({
    dagitty::adjustmentSets(dag1()$dag, type = 'minimal', effect = rv$effect)
  })

  dagSolution2 <- reactive({
    dagitty::adjustmentSets(dag2()$dag, type = 'minimal', effect = eval(as.name(paste0("effect", input$tuteID))))
  })

  observeEvent(input$solutionID, {
    rv$solutionChoice <- input$solutionID
  })

  dagSolved1 <- reactive({
    dag1() %>% adjust_for(dagSolution1()[[rv$solutionChoice]])
  })

  dagSolved2 <- reactive({
    dag2() %>% adjust_for(dagSolution2()[[1]])
  })


  # The drawDAG modules
    mod_drawDag_server("dag1", dag = dagAdj1, did = reactive(rv$id), n = reactive(rv$n), pid = reactive(rv$pid), height = reactive(dimension$height1), width = reactive(dimension$width1)) # Random DAG
    mod_drawDag_server("dag3", dag = dagAdj2, did = reactive(rv$id), n = reactive(5), pid = reactive(rv$pid), label = 1, height = reactive(dimension$height3), width = reactive(dimension$width3)) # Tutorial DAG




  # Make the code available on click
  observeEvent(input$code, {

    codeSnip <- untidy_dagitty(dagAdj1())

    showModal(modalDialog(
      title = HTML(paste(icon('code'), "Code to draw this DAG")),
      footer = modalButton("Done"),
      splitLayout(
        column(width = 6,
               h3("dagitty.net"),
               hr(),
               helpText(HTML(paste("Reproduce on", tags$a(href="http://www.dagitty.net/dags.html", "dagitty.net", target = "_blank")))),
               rclipboard::rclipButton("copy1", "Copy and close", codeSnip$dagitty, modal = TRUE, icon = icon("copy")),
               br(), br(),
               tags$div(class = 'codeBlock', HTML(gsub("\n","<br/>",codeSnip$dagitty[[1]])))

        ),
        column(width = 6,
               h3("R"),
               hr(),
               helpText(HTML(paste("Reproduce  in R using", tags$code("dagitty"), "or", tags$code("ggdag")))),
               rclipboard::rclipButton("copy2", "Copy and close", codeSnip$r, modal = TRUE, icon = icon("copy")),
               br(), br(),
               tags$div(class = 'codeBlock',HTML(gsub("\n","<br/>",codeSnip$r[[1]])))

        )
      ),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  # Close modal when code is copied
  observeEvent(input$copy1, {
    removeModal()
  })

  # Close modal when code is copied
  observeEvent(input$copy2, {
    removeModal()
  })

  url <- reactive({
    paste0('https://cbdrh.shinyapps.io/daggle/?_values_&id=', rv$id)
  })

  twitterLink <- reactive({
    paste0('https://twitter.com/intent/tweet?text=Can%20you%20solve%20this%20%23daggle?%0A%0AFind%20a%20minimal%20adjustment%20set%20to%20identify%20the%20*',rv$effect,'*%20effect%20of%20X%20on%20Y','%0A%0A&url=https://cbdrh.shinyapps.io/daggle/?_values_%26id=',rv$id)
  })

  # Make the url available on click
  observeEvent(input$link, {

    showModal(modalDialog(
      title = HTML(paste(icon('link'), "URL link to this daggle")),
      footer = modalButton("Done"),
      div(style="color:#4FBAE4; background-color:white;", url()),
      br(),
      rclipboard::rclipButton("copy3", "Copy and close", url(), modal = TRUE, icon = icon("copy")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  observeEvent(input$link2, {

    showModal(modalDialog(
      title = "daggle url",
      footer = modalButton("Done"),
      div(style="color:#4FBAE4; background-color:white;", url()),
      br(),
      rclipboard::rclipButton("copy4", "Copy and close", url(), modal = TRUE, icon = icon("copy")),
      easyClose = TRUE,
      fade = TRUE
    ))
  })

  # Close modal when code is copied
  observeEvent(input$copy3, {
    removeModal()
  })

  # Close modal when code is copied
  observeEvent(input$copy4, {
    removeModal()
  })

  # Tweet on click
  eventReactive(input$tweet, {
    twitterLink()
  })

  # Twitter button
  output$tweet <- renderUI({
    tags$a(href=twitterLink(), NULL, class="btn btn-default download2", icon("twitter"), target = "_blank")
  })

  # Check solution in random mode
  observeEvent(input$submit, {

    solution <- dagSolution1()
    if (input$panel=="Tutorial") {
      solution <- dagSolution2()
    }
    nSol <- length(solution)
    grade <- grader(submission = rv$controls, dag=dag1()$dag, effect = rv$effect)

    if(grade == 'correct') {

      shinyalert::shinyalert(title =  paste('Correct', happyFace(), tags$hr()),
                             text = div(style = "text-align: center; display:inline-block",
                                        tagList(
                                          tags$p(praiseEm()),
                                          actionButton("run2", "Generate DAG", icon = icon('arrows-rotate'), width = 140, class="btn btn-default", className='popupButton'),
                                          actionButton("link2", "Get url", icon = icon('link'), width = 140, class="btn btn-default"),
                                          tags$a(href=twitterLink(), "Share", class="btn btn-default twitter-share-button", icon("twitter"), target = "_blank")
                                        )
                             ),
                             animation = FALSE,
                             showConfirmButton = FALSE,
                             closeOnClickOutside = TRUE,
                             closeOnEsc = TRUE,
                             html = TRUE,
                             size = 'm')

    }
    else if (grade == 'incorrect'){

      shinyalert::shinyalert(title =  paste('Incorrect', sadFace(), '\n'),
                             text = tagList(
                               helpText(encourageEm()),
                               shiny::actionButton("hint", "Hint", icon = icon('question'), style="  color: #333;
          background-color: #fff;
          border-color: #ccc !important;
          display: inline-block;
          font-weight: 400;
          text-align: center;
          white-space: nowrap;
          vertical-align: middle;
          cursor: pointer;
          background-image: none;
          border: 1px solid;
          padding: 6px 12px;
          font-size: 14px;
          line-height: 1.42857143;
          border-radius: 4px;
          margin: 10px;") # Had to hardcode this because actionButton css class isn't being recognised
                             ),
                             animation = FALSE,
                             showConfirmButton = FALSE,
                             className = "alert",
                             timer = 10000,
                             closeOnClickOutside = TRUE,
                             closeOnEsc = TRUE,
                             html = TRUE)
    }

    else if (grade == 'close'){

      shinyalert::shinyalert(title =  paste('Close!', thinkingFace(), tags$hr()),
                             text = tagList(tags$p(HTML(paste("That is a valid adjustment set but it's not a", tags$em("minimal"), "adjustment set!")))),
                             animation = FALSE,
                             showConfirmButton = FALSE,
                             className = "alert",
                             timer = 4000,
                             closeOnClickOutside = TRUE,
                             closeOnEsc = TRUE,
                             html = TRUE)

    }

  }, ignoreInit = TRUE)


  output$done <- renderUI({

    if(input$tuteID < nExamples) {
      return(actionButton("advance2", "Next!", icon = icon('arrow-right'), width = 140, class="btn btn-default"))
    }

    else if(input$tuteID == nExamples) {
      return(actionButton("done", "Done!", icon = icon('check'), width = 140, class="btn btn-default"))
    }

  })

  # Check solution in tutorial mode
  observeEvent(input$submit2, {

    submission <- rv$controls2

    solution <- dagSolution2()
    nSol <- length(solution)
    grade <- grader(submission = rv$controls2, dag=dag2()$dag, effect = eval(as.name(paste0("effect", input$tuteID))))

    if(grade == 'correct') {

      showModal(modalDialog(title = div(style = "text-align: center; font-size: 22pt;", paste('Correct!', happyFace(), '\n')),
                            div(style = "text-align: center;", tagList(
                              text = praiseEm(),
                              br(), br(),
                              uiOutput("done")
                            )),
                            easyClose = TRUE,
                            fade = TRUE
      ))

    }

    else if (grade == 'incorrect'){

      shinyalert::shinyalert(title =  paste('Incorrect', sadFace(), '\n'),
                             text = tagList(
                               helpText(encourageEm()),
                               shiny::actionButton("hint2", "Hint", icon = icon('question'), style="  color: #333;
          background-color: #fff;
          border-color: #ccc !important;
          display: inline-block;
          font-weight: 400;
          text-align: center;
          white-space: nowrap;
          vertical-align: middle;
          cursor: pointer;
          background-image: none;
          border: 1px solid;
          padding: 6px 12px;
          font-size: 14px;
          line-height: 1.42857143;
          border-radius: 4px;
          margin: 10px;") # Had to hardcode this because actionButton css class isn't being recognised
                             ),
                             animation = FALSE,
                             showConfirmButton = FALSE,
                             className = "alert",
                             timer = 1500,
                             closeOnClickOutside = TRUE,
                             closeOnEsc = TRUE,
                             html = TRUE)
    }


    else if (grade == 'close'){

      shinyalert::shinyalert(title =  paste('Close!', thinkingFace(), tags$hr()),
                             text = tagList(tags$p(HTML(paste("That is a valid answer set but it's not the", tags$em("minimal"), "adjustment set!")))),
                             animation = FALSE,
                             showConfirmButton = FALSE,
                             className = "alert",
                             timer = 1500,
                             closeOnClickOutside = TRUE,
                             closeOnEsc = TRUE,
                             html = TRUE)

    }


  }, ignoreInit = TRUE)



  # Show solution options if more than 1 solution
  output$solutionOpts <- renderUI({

    req(dagSolution1())

    nSets <- length(dagSolution1())

    # Update the possible solution to view
    choiceSolutions <- c(paste('Solution', seq(1, nSets)))
    tagList(
      h4(HTML((paste("Minimal adjustment sets to estimate the", tags$strong(rv$effect), "effect of X on Y")))),
      br(),
      radioButtons("solutionID",
                   NULL,
                   choiceNames = choiceSolutions,
                   choiceValues = seq(1:nSets),
                   selected = 1
      )
    )

  })


  output$solutionText <- renderUI({

    text <- if (length(dagSolution1()[[1]]) == 0) {
      "No adjustment necessary!"
    } else {
      paste("Adjust for", knitr::combine_words(sort(dagSolution1()[[rv$solutionChoice]])))
    }

    tagList(

      p(text)
    )

  })

  # Update contents of reactive before modal is opened
  outputOptions(output, "solutionOpts", suspendWhenHidden = FALSE)
  outputOptions(output, "solutionText", suspendWhenHidden = FALSE)

  # Reveal solution on click
  observeEvent(input$reveal, {

    req(dagSolution1(), rv$solutionChoice)

    mod_drawDag_server("dag2", dag = dagSolved1, did = reactive(rv$id), n = reactive(rv$n), pid = reactive(rv$pid), height = reactive(dimension$height2), width = reactive(dimension$width2)) # Random DAG solution

    grDevices::quartz.options(width = 8, height = 6,
                              pointsize = 10)

    showModal(modalDialog(
      title = HTML(paste(icon('diagram-project'), 'Solution')),
      tagList(
        htmlOutput("solutionOpts"),
        htmlOutput("solutionText"),
        mod_drawDag_ui("dag2")
      ),
      easyClose = TRUE,
      fade = TRUE,
      footer = tagList(div(style = "text-align:right;", actionButton("closeSolution", "Got it", icon = icon('thumbs-up'))))
    ))

  })

  # Close solution modal on click
  observeEvent(input$closeSolution, {
    removeModal()
  })

  ###########################
  ## Tutorial server logic ##
  ###########################

  observeEvent(input$tuteID, {
    rv$reveal2 <- 0
    rv$controls2 <- NULL
  })

  observeEvent(input$previous, {
    req(input$tuteID)
    prevTute = max(1, as.numeric(input$tuteID) - 1)
    updateRadioButtons(session, "tuteID", selected = prevTute)
    rv$reveal2 <- 0
    rv$controls2 <- NULL
  })

  observeEvent(input$advance, {
    req(input$tuteID)
    nextTute = min(nExamples, as.numeric(input$tuteID) + 1)
    updateRadioButtons(session, "tuteID", selected = nextTute)
    rv$reveal2 <- 0
    rv$controls2 <- NULL
  })

  observeEvent(input$advance2, {
    req(input$tuteID)
    removeModal()
    nextTute = min(nExamples, as.numeric(input$tuteID) + 1)
    updateRadioButtons(session, "tuteID", selected = nextTute)
    rv$reveal2 <- 0
    rv$controls2 <- NULL
  })

  observeEvent(input$done, {
    removeModal()
    rv$reveal2 <- 0
  })

  output$tuteHeader <- renderUI({
    req(input$tuteID)
    tags$h4(tuteHeaders[as.numeric(input$tuteID)])
  })

  output$tuteText <- renderUI({
    # tute <- paste0("R/tute", input$tuteID, ".html")
    # includeHTML(tute)
    if(input$tuteID=='1'){ includeHTML("inst/app/www/tutorials/tute1.html")}
    else if (input$tuteID=='2'){ includeHTML("inst/app/www/tutorials/tute2.html")}
    else if (input$tuteID=='3'){ includeHTML("inst/app/www/tutorials/tute3.html")}
    else if (input$tuteID=='4'){ includeHTML("inst/app/www/tutorials/tute4.html")}
    else if (input$tuteID=='5'){ includeHTML("inst/app/www/tutorials/tute5.html")}
    else if (input$tuteID=='6'){ includeHTML("inst/app/www/tutorials/tute6.html")}
    else if (input$tuteID=='7'){ includeHTML("inst/app/www/tutorials/tute7.html")}
    else if (input$tuteID=='8'){ includeHTML("inst/app/www/tutorials/tute8.html")}
  })

  # Reveal solution on click
  observeEvent(input$reveal2, {

    req(dagSolution2())

    mod_drawDag_server("dag4", dag = dagSolved2, did = reactive(rv$id), n = reactive(5), pid = reactive(rv$pid), label = 1, height = reactive(dimension$height4), width = reactive(dimension$width4)) # Tutorial DAG solution

    text <- if (length(dagSolution2()[[1]]) == 0) {
      "No adjustment necessary!"
    } else {
      paste("Adjust for", knitr::combine_words(sort(dagSolution2()[[1]])))
    }
    #
    showModal(modalDialog(
      title = HTML(paste(icon('diagram-project'), 'Solution')),
      tagList(
        h4(HTML((paste("Minimal adjustment sets to estimate the",
                       tags$strong(eval(as.name(paste0("effect", input$tuteID)))),
                       "effect of X on Y")))),
        p(text),
        mod_drawDag_ui("dag4")
      ),
      easyClose = TRUE,
      fade = TRUE,
      footer = tagList(div(style = "text-align:right;", actionButton("closeSolution2", "Got it", icon = icon('thumbs-up'))))
    ))

  })

  # Close tutorial solution modal on click
  observeEvent(input$closeSolution2, {
    removeModal()
  })

  # Bookmarking
  # Exclude certain parameters from bookmark
  setBookmarkExclude(names = c("run", "reveal2", "instructions", "settings", "link", "previous", "panel", "submit", "tuteID", "submit2", "code", "reveal", "advance", "plotClick", "cancelSettings", "saveSettings", "n", "p", "pid", "pid2", "effect"))

  # Save extra values when we bookmark
  onBookmark(function(state) {
    state$values$id <- rv$id
  })

  # Read values from state$values when we restore
  onRestore(function(state) {
    rv$id <- state$values$id
  })


# Handle hint feature

  currentDagString <- reactive({
    codeSnip <- untidy_dagitty(dagAdj1())
    return(codeSnip$rString)
  })

  currentPaths <- reactive({
    getOpenPaths(dagitty::dagitty(currentDagString()), adj = rv$controls)
  })

  output$pathHint <- renderUI({

    req(rv$effect)

    getHint(currentPaths(), rv$effect)

  })

  output$pathHighlight <- renderPlot({

    req(rv$path)

    drawPath(dag=dagitty::dagitty(currentDagString()),
             adj=rv$controls,
             path=currentPaths()$path[rv$path],
             open = currentPaths()$open[rv$path],
             directed = currentPaths()$directed[rv$path])
  })

  output$pathIndex <- renderUI({

    req(rv$path, currentPaths())

    helpText(paste0(rv$path, "/", nrow(currentPaths())))
  })


  observeEvent(input$hint, {

    rv$path <- 1

    showModal(modalDialog(
      title = NULL,
      tagList(
        tags$div(style = 'text-align: center;',
                 h3('Hint \U1F680'),
                 helpText("You got this!"),
                 hr(),
        ),
        htmlOutput("pathHint"),
        htmlOutput("pathIndex"),
        tags$div(style = 'text-align: center;',
                 actionButton('previousPath', NULL, icon = icon("arrow-left"), width = 68),
                 actionButton('nextPath', NULL, icon = icon("arrow-right"), width = 68)),
        plotOutput("pathHighlight")
      ),
      easyClose = TRUE,
      fade = TRUE,
      footer = tagList(div(style = "text-align:right;", actionButton("closeHint", "Got it", icon = icon('thumbs-up'))))
    ))

  })

  # Close solution modal on click
  observeEvent(input$closeHint, {
    removeModal()
  })

  observeEvent(input$nextPath, {
    rv$path <- ifelse(rv$path < nrow(currentPaths()), rv$path + 1, 1)
  })

  observeEvent(input$previousPath, {
    rv$path <- ifelse(rv$path > 1, rv$path - 1, nrow(currentPaths()))
  })



  # Hint in tutorial section

  # Current tutorial DAG
  currentDagString2 <- reactive({
    req(input$tuteID)
    codeSnip <- eval(as.name(paste0('g', input$tuteID)))
    return(codeSnip)
  })

  # paths from current tutorial DAG
  currentPaths2 <- reactive({
    getOpenPaths(dagitty::dagitty(currentDagString2()), adj = rv$controls2)
  })

  # Generate the hint
  output$pathHint2 <- renderUI({
    req(input$tuteID)
    getHint(currentPaths2(), eval(as.name(paste0("effect", input$tuteID))))

  })

  # Plot highligted paths
  output$pathHighlight2 <- renderPlot({
    req(rv$path2)
    drawPath(dag=dagitty::dagitty(currentDagString2()),
             adj=rv$controls2,
             path=currentPaths2()$path[rv$path2],
             open = currentPaths2()$open[rv$path2],
             directed = currentPaths2()$directed[rv$path2])
  })

  # Keep track of path index
  output$pathIndex2 <- renderUI({
    helpText(paste0(rv$path2, "/", nrow(currentPaths2())))
  })


  observeEvent(input$hint2, {
    showModal(modalDialog(
      title = NULL,
      tagList(
        tags$div(style = 'text-align: center;',
                 h3('Hint \U1F680'),
                 helpText("You got this!"),
                 hr(),
        ),
        htmlOutput("pathHint2"),
        htmlOutput("pathIndex2"),
        tags$div(style = 'text-align: center;',
                 actionButton('previousPath2', NULL, icon = icon("arrow-left"), width = 68),
                 actionButton('nextPath2', NULL, icon = icon("arrow-right"), width = 68)),
        plotOutput("pathHighlight2")
      ),
      easyClose = TRUE,
      fade = TRUE,
      footer = tagList(div(style = "text-align:right;", actionButton("closeHint2", "Got it", icon = icon('thumbs-up'))))
    ))

  })

  # Close solution modal on click
  observeEvent(input$closeHint2, {
    removeModal()
  })

  observeEvent(input$nextPath2, {
    rv$path2 <- ifelse(rv$path2 < nrow(currentPaths2()), rv$path2 + 1, 1)
  })

  observeEvent(input$previousPath2, {
    rv$path2 <- ifelse(rv$path2 > 1, rv$path2 - 1, nrow(currentPaths2()))
  })


} # Close app_server function
