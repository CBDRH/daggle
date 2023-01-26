#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinyjs::useShinyjs(),
    navbarPage("daggle", id = "panel", selected = 'Random',

               # Random mode
               tabPanel("Random", icon = icon('diagram-project'),

                        # Left hand column
                        column(width = 3,
                               tippy::tippy_class("tool-tip") # Need this for tippy libraries to be loaded
                        ),

                        # Central column
                        column(width = 6,
                               tags$div(style="text-align:center;",
                                        actionButton("instructions", NULL, icon = icon('circle-question'), class = "download"),
                                        actionButton("settings", NULL, icon = icon('gear'), class = "download"),
                                        hr(class = 'myHr'),
                                        uiOutput("directions"),
                                        hr(class = 'myHr')
                               ),

                               mod_drawDag_ui("dag1"),

                               tags$div(style="text-align:right;",
                                        tags$div(style="display:inline-block",title="Get the code", actionButton("code", NULL, icon = icon('code'), class = "download")),
                                        tags$div(style="display:inline-block",title="Get a link", actionButton("link", NULL, icon = icon('link'), class = "download")),
                                        tags$div(style="display:inline-block",title="Share on twitter", uiOutput("tweet"))
                               ),
                               br(),
                               hr(class = 'myHr'),
                               tags$div(style="text-align:center;",
                                        strong("Your solution:"),
                                        textOutput("printSelected", inline = TRUE)
                               ),
                               hr(class = 'myHr'),
                               br(),
                               tags$div(style="text-align:center;",
                                        actionButton("run", "Generate DAG", icon = icon('rotate'), width = 140),
                                        actionButton("submit", "Submit answer", icon = icon('share-from-square'), width = 140),
                                        actionButton("reveal", "Reveal solution", icon = icon('diagram-project'), width = 140)
                               )
                        ),

                        # Output controls
                        column(width = 3,

                        )
               ),

               # Tutorial mode
               tabPanel("Tutorial", icon = icon('person-chalkboard'),

                        tags$div(style="text-align:center;",
                                 uiOutput("tuteHeader")
                        ),
                        hr(),
                        br(),

                        column(width = 2,
                               radioButtons("tuteID", "Tutorial",
                                            choiceValues = 1:nExamples,
                                            choiceNames = tuteNames)
                        ),

                        # Central column
                        column(width = 5,
                               mod_drawDag_ui("dag3"),
                               tags$div(style="text-align:center;",
                                        strong("Your solution:"),
                                        textOutput("printSelected2", inline = TRUE)
                               ),
                               br(),
                               tags$div(style = 'text-align: center;',
                                        actionButton('previous', NULL, icon = icon("arrow-left"), width = 68),
                                        actionButton('advance', NULL, icon = icon("arrow-right"), width = 68),
                                        actionButton("submit2", "Submit answer", icon = icon('share-from-square'), width = 140),
                                        actionButton("reveal2", "Reveal solution", icon = icon('diagram-project'), width = 140),
                                        br(), br()
                               )
                        ),

                        # Output controls
                        column(width = 5,
                               htmlOutput("tuteText")
                        )

               ),


               # About section
               tabPanel(NULL, icon = icon('circle-info'),

                        sidebarPanel(style = "color: #333;
                      background-color: #f8f8f8;
                      border-color: #e7e7e7;",
                                     width=3,
                                     helpText(style="font-weight:bold;", "On this page"),
                                     HTML("&emsp;"), tags$a(style="color: #333", href="#about", HTML(paste("About daggle", icon(style="color:#6A6A6A;", 'arrow-up-right-from-square')))),
                                     br(),
                                     HTML("&emsp;"), tags$a(style="color: #333", href="#using", HTML(paste("Using and citing daggle", icon(style="color:#6A6A6A;", 'arrow-up-right-from-square')))),
                                     br(),
                                     HTML("&emsp;"), tags$a(style="color: #333", href="#resources", HTML(paste("Resources", icon(style="color:#6A6A6A;", 'arrow-up-right-from-square')))),
                                     br(),
                                     HTML("&emsp;&emsp;"), tags$a(style="color: #333", href="#introductory", HTML(paste("Introductory resources", icon(style="color:#6A6A6A;", 'arrow-up-right-from-square')))),
                                     br(),
                                     HTML("&emsp;&emsp;"), tags$a(style="color: #333", href="#advanced", HTML(paste("Advanced applications", icon(style="color:#6A6A6A;", 'arrow-up-right-from-square')))),
                                     br(),
                                     HTML("&emsp;&emsp;"), tags$a(style="color: #333", href="#software", HTML(paste("Software", icon(style="color:#6A6A6A;", 'arrow-up-right-from-square')))),
                        ),
                        mainPanel(style = "color: #333;
                   overflow-y: scroll;
                   height: 100vh;",
                                  width=9,
                                  tags$img(src="www/daggle-logo.png", width='80%'),
                                  h2(tags$span(id="about", "About daggle")),
                                  tags$em("coming soon"),
                                  h2(tags$span(id="using", "Using and citing daggle")),
                                  tags$em("coming soon"),
                                  h2(tags$span(id="resources", "Resources")),
                                  helpText("This section provides a list of resources for people wising to learn more about DAGs. This list is biased towards applied health service research and R software. To add or amend this list please contact me or submit a pull request on GitHub."),
                                  h3(tags$span(id="introductory", "Introductory tutorials")),
                                  tags$ul(
                                    tags$li(tags$strong("Directed acyclic graphs: An under-utilized tool for child maltreatment research."), "Austin AE, Desrosiers TA, Shanahan ME. Child Abuse & Neglect. 2019;91:78-87.", tags$a(href="https://doi.org/10.1016/j.chiabu.2019.02.011", "https://doi.org/10.1016/j.chiabu.2019.02.011")),
                                    tags$li(tags$strong("Use of directed acyclic graphs (DAGs) to identify confounders in applied health research: review and recommendations."), "Tennant PW, Murray EJ, Arnold KF, Berrie L, Fox MP, Gadd SC, Harrison WJ, Keeble C, Ranker LR, Textor J, Tomova GD. International Journal of Epidemiology. 2021;50(2):620-32.", tags$a(href="https://doi.org/10.1093/ije/dyaa213", "https://doi.org/10.1093/ije/dyaa213")),
                                    tags$li(tags$strong("Using causal diagrams to improve the design and interpretation of medical research."), "Etminan M, Collins GS, Mansournia MA. Chest. 2020 1;158(1):S21-8.", tags$a(href="https://doi.org/10.1016/j.chest.2020.03.011", "https://doi.org/10.1016/j.chest.2020.03.011")),
                                    tags$li(tags$strong("Tutorial on directed acyclic graphs."), "Digitale JC, Martin JN, Glymour MM. Journal of Clinical Epidemiology. 2022;142:264-7.", tags$a(href="https://doi.org/10.1016/j.jclinepi.2021.08.001", "https://doi.org/10.1016/j.jclinepi.2021.08.001")),
                                    tags$li(tags$strong("A crash course in good and bad controls."), "Cinelli C, Forney A, Pearl J. Sociological Methods & Research. 2021.", tags$a(href="https://doi.org/10.1177/00491241221099552", "https://doi.org/10.1177/00491241221099552")),
                                    tags$li(tags$strong("Causal Diagrams: Draw Your Assumptions Before Your Conclusions"), tags$a(href="https://www.edx.org/course/causal-diagrams-draw-your-assumptions-before-your", "https://www.edx.org/course/causal-diagrams-draw-your-assumptions-before-your"))
                                  ),
                                  h3(tags$span(id="advanced", "Advanced applications")),
                                  tags$ul(
                                    tags$li(tags$strong("Reflection on modern methods: understanding bias and data analytical strategies through DAG-based data simulations. "), "Duan C, Dragomir AD, Luta G, Breitling LP. International Journal of Epidemiology. 2021;50(6):2091-7.", tags$a(href="https://doi.org/10.1093/ije/dyab096", "https://doi.org/10.1093/ije/dyab096")),
                                    tags$li(tags$strong("Illustrating How to Simulate Data From Directed Acyclic Graphs to Understand Epidemiologic Concepts."), "Fox MP, Nianogo R, Rudolph JE, Howe CJ. American Journal of Epidemiology. 2022;191(7)1300-6.", tags$a(href="https://doi.org/10.1093/aje/kwac041", "https://doi.org/10.1093/aje/kwac041")),
                                    tags$li(tags$strong("A proposal for capturing interaction and effect modification using DAGs."), "Attia J, Holliday E, Oldmeadow C. International Journal of Epidemiology. 2022;51(4):1047-53.", tags$a(href="https://doi.org/10.1093/ije/dyac126", "https://doi.org/10.1093/ije/dyac126")),
                                    tags$li(tags$strong("Directed acyclic graphs, effect measure modification, and generalizability."), "Webster-Clark M, Breskin A. American Journal of Epidemiology. 2021;190(2):322-7.", tags$a(href="https://doi.org/10.1093/aje/kwaa185", "https://doi.org/10.1093/aje/kwaa185")),
                                    tags$li(tags$strong("Recommendations for using causal diagrams to study racial health disparities."), "Howe CJ, Bailey ZD, Raifman JR, Jackson JW. American Journal of Epidemiology. 2022.", tags$a(href="https://doi.org/10.1093/aje/kwac140", "https://doi.org/10.1093/aje/kwac140"))
                                  ),
                                  h3(tags$span(id="software", "Software")),
                                  tags$ul(
                                    tags$li(tags$strong("DAGitty"), "A browser-based environment for creating, editing, and analyzing causal diagrams", tags$a(href="http://dagitty.net", "dagitty.net")),
                                    tags$li(tags$strong("The R package 'dagitty'"), "An R package providing all the capabilities of the DAGitty web application (and more).", tags$a(href="https://doi.org/10.1093/ije/dyw341", "https://doi.org/10.1093/ije/dyw341")),
                                    tags$li(tags$strong("ggdag"), "An R package to plot and analyse DAGs.", tags$a(href="https://ggdag.malco.io/", "https://ggdag.malco.io/")),
                                    tags$li(tags$strong("pcalg"), "An R package for causal structure learning and estimation of causal effects from observational data.", tags$a(href="https://doi.org/10.18637/jss.v047.i11", "https://doi.org/10.18637/jss.v047.i11")),
                                    tags$li(tags$strong("dagR"), "An R package to draw, manipulate, and evaluate DAGs and simulate corresponding data", tags$a(href="https://doi.org/10.1093/ije/dyab167", "https://doi.org/10.1093/ije/dyab167")),
                                    tags$li(tags$strong("shinyDAG"), "A web application that uses R and LaTeX to create publication-quality images of DAGs", tags$a(href="https://www.gerkelab.com/project/shinydag/", "https://www.gerkelab.com/project/shinydag/")),
                                    tags$li(tags$strong("causaldiagrams.org"), "A searchable database of published health research articles that include a causal diagram", tags$a(href="https://causaldiagrams.org", "https://causaldiagrams.org"))
                                  )
                        ) # Closes main panel

                        ) # Closes tab panel
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'daggle'
    ),
    shinyjs::useShinyjs(),
    rclipboard::rclipboardSetup()
    # Add here other external resources
  )
}
