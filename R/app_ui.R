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
                               wellPanel(
                               radioButtons("tuteID", "Tutorial",
                                            choiceValues = 1:nExamples,
                                            choiceNames = tuteNames)
                        )),

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
               tabPanel("About", icon = icon('circle-info'),

                        column(width=2,
                                     wellPanel(
                                     p(style="font-weight:bold;", "On this page"),
                                     p(tags$a(style="color: #333", href="#about", "About daggle")),
                                     p(HTML("&emsp;"), tags$a(style="color: #606060", href="#motivation", "Motivation")),
                                     p(HTML("&emsp;"), tags$a(style="color: #606060", href="#features", "General features")),
                                     p(HTML("&emsp;"), tags$a(style="color: #606060", href="#implementation", "Implementation")),
                                     p(HTML("&emsp;"), tags$a(style="color: #606060", href="#availability", "Availability")),
                                     p(tags$a(style="color: #333", href="#sap", "Software Application Profile")),
                                     p(tags$a(style="color: #333", href="#using", "Using and citing daggle")),
                                     p(tags$a(style="color: #333", href="#resources", "Resources")),
                                     p(HTML("&emsp;"), tags$a(style="color: #606060", href="#introductory", "Introductory tutorials")),
                                     p(HTML("&emsp;"), tags$a(style="color: #606060", href="#advanced", "Advanced applications")),
                                     p(HTML("&emsp;"), tags$a(style="color: #606060", href="#software", "Software")),
                                     p(tags$a(style="color: #333", href="#contact", "Contact")),
                        )),
                        mainPanel(style = "color: #333;
                   overflow-y: scroll;
                   height: 100vh;",
                                  width=9,
                                  tags$img(src="www/daggle-logo.png", width='80%'),
                                  h2(tags$span(id="about", "About the daggle app")),
                                  h3(tags$span(id="motivation", "Motivation")),
                                  p("Directed acyclic graphs (DAGs) are used in epidemiological research to communicate causal assumptions and guide the selection of covariate adjustment sets when estimating causal effects. For any given DAG, a set of graphical rules can be applied to identify minimally sufficient adjustment sets that can be used to adjust for bias due to confounding when estimating the causal effect of an exposure on an outcome. The daggle app is a web-based application that aims to assist in the learning and teaching of adjustment set identification using DAGs."),
                                  h3(tags$span(id="features", "General features")),
                                  p("The application offers two modes: tutorial and random. The tutorial mode presents a guided introduction to how common causal structures can be presented using DAGs and how graphical rules can be used to identify minimally sufficient adjustment sets for causal estimation. The random mode tests this understanding by presenting the user with a randomly generated DAG\u2014a daggle. To solve the daggle, users must correctly identify a valid minimally sufficient adjustment set."),
                                  h3(tags$span(id="implementation", "Implementation")),
                                  p("The daggle app is implemented as an R shiny application using the golem framework. The application builds upon existing R libraries including pcalg to generate reproducible random DAGs, dagitty to identify all valid minimal adjustment sets and ggdag to visualize DAGs."),
                                  h3(tags$span(id="availability", "Availability")),
                                  p("The daggle app can be accessed online at", tags$a(href='http://cbdrh.shinyapps.io/daggle', 'cbdrh.shinyapps.io/daggle.', target = "_blank"), "The source code is available on GitHub at", tags$a(href='https://github.com/CBDRH/daggle', 'github.com/CBDRH/daggle.')),
                                  hr(),
                                  h2(tags$span(id="sap", "Software Application Profile")),
                                  p("Read more about the daggle app in the", tags$a(href="https://doi.org/10.1093/ije/dyad038", "Software Application Profile", target = "_blank"), "published in the International Journal of Epidemiology."),
                                  p("Mark Hanly, Bronwyn K Brew, Anna Austin, Louisa Jorm, Software Application Profile:
                                    The daggle app\u2014a tool to support learning and teaching the graphical rules of selecting
                                    adjustment variables using directed acyclic graphs, International Journal of Epidemiology,
                                    2023;, dyad038", tags$a(href="https://doi.org/10.1093/ije/dyad038", "https://doi.org/10.1093/ije/dyad038", target = "_blank")),
                                  hr(),
                                  h2(tags$span(id="using", "Using and citing daggle")),
                                  p("The daggle app is released under a Creative Commons CC BY-NC-SA 4.0 licence. This means you are free to copy and share this app and/or source code (non-commercially), provided you tell people where it is from. If you use daggle in your teaching and learning", tags$strong("please let us know!"), "using the", tags$a(href="#contact", "contact details below.")),
                                  p("The best way to cite the daggle app is by citing the IJE Software Application Profile paper:", br(), "Mark Hanly, Bronwyn K Brew, Anna Austin, Louisa Jorm, Software Application Profile: The daggle app\u2014a tool to support learning and teaching the graphical rules of selecting adjustment variables using directed acyclic graphs, International Journal of Epidemiology, 2023;, dyad038", tags$a(href="https://doi.org/10.1093/ije/dyad038", "https://doi.org/10.1093/ije/dyad038", target = "_blank")),
                                  hr(),
                                  h2(tags$span(id="resources", "Resources")),
                                  helpText("This section provides a list of resources for people wising to learn more about DAGs. This list is biased towards applied health service research and R software. To add or amend this list please contact me or submit a pull request on GitHub."),
                                  h3(tags$span(id="introductory", "Introductory tutorials")),
                                  tags$ul(
                                    tags$li(tags$strong("Directed acyclic graphs: An under-utilized tool for child maltreatment research."), "Austin AE, Desrosiers TA, Shanahan ME. Child Abuse & Neglect. 2019;91:78-87.", tags$a(href="https://doi.org/10.1016/j.chiabu.2019.02.011", "https://doi.org/10.1016/j.chiabu.2019.02.011", target = "_blank")),
                                    tags$li(tags$strong("Use of directed acyclic graphs (DAGs) to identify confounders in applied health research: review and recommendations."), "Tennant PW, Murray EJ, Arnold KF, Berrie L, Fox MP, Gadd SC, Harrison WJ, Keeble C, Ranker LR, Textor J, Tomova GD. International Journal of Epidemiology. 2021;50(2):620-32.", tags$a(href="https://doi.org/10.1093/ije/dyaa213", "https://doi.org/10.1093/ije/dyaa213", target = "_blank")),
                                    tags$li(tags$strong("Using causal diagrams to improve the design and interpretation of medical research."), "Etminan M, Collins GS, Mansournia MA. Chest. 2020 1;158(1):S21-8.", tags$a(href="https://doi.org/10.1016/j.chest.2020.03.011", "https://doi.org/10.1016/j.chest.2020.03.011", target = "_blank")),
                                    tags$li(tags$strong("Tutorial on directed acyclic graphs."), "Digitale JC, Martin JN, Glymour MM. Journal of Clinical Epidemiology. 2022;142:264-7.", tags$a(href="https://doi.org/10.1016/j.jclinepi.2021.08.001", "https://doi.org/10.1016/j.jclinepi.2021.08.001", target = "_blank")),
                                    tags$li(tags$strong("A crash course in good and bad controls."), "Cinelli C, Forney A, Pearl J. Sociological Methods & Research. 2021.", tags$a(href="https://doi.org/10.1177/00491241221099552", "https://doi.org/10.1177/00491241221099552", target = "_blank")),
                                    tags$li(tags$strong("Causal Diagrams: Draw Your Assumptions Before Your Conclusions"), tags$a(href="https://www.edx.org/course/causal-diagrams-draw-your-assumptions-before-your", "https://www.edx.org/course/causal-diagrams-draw-your-assumptions-before-your", target = "_blank"))
                                  ),
                                  h3(tags$span(id="advanced", "Advanced applications")),
                                  tags$ul(
                                    tags$li(tags$strong("Reflection on modern methods: understanding bias and data analytical strategies through DAG-based data simulations. "), "Duan C, Dragomir AD, Luta G, Breitling LP. International Journal of Epidemiology. 2021;50(6):2091-7.", tags$a(href="https://doi.org/10.1093/ije/dyab096", "https://doi.org/10.1093/ije/dyab096", target = "_blank")),
                                    tags$li(tags$strong("Illustrating How to Simulate Data From Directed Acyclic Graphs to Understand Epidemiologic Concepts."), "Fox MP, Nianogo R, Rudolph JE, Howe CJ. American Journal of Epidemiology. 2022;191(7)1300-6.", tags$a(href="https://doi.org/10.1093/aje/kwac041", "https://doi.org/10.1093/aje/kwac041", target = "_blank")),
                                    tags$li(tags$strong("A proposal for capturing interaction and effect modification using DAGs."), "Attia J, Holliday E, Oldmeadow C. International Journal of Epidemiology. 2022;51(4):1047-53.", tags$a(href="https://doi.org/10.1093/ije/dyac126", "https://doi.org/10.1093/ije/dyac126", target = "_blank")),
                                    tags$li(tags$strong("Directed acyclic graphs, effect measure modification, and generalizability."), "Webster-Clark M, Breskin A. American Journal of Epidemiology. 2021;190(2):322-7.", tags$a(href="https://doi.org/10.1093/aje/kwaa185", "https://doi.org/10.1093/aje/kwaa185", target = "_blank")),
                                    tags$li(tags$strong("Recommendations for using causal diagrams to study racial health disparities."), "Howe CJ, Bailey ZD, Raifman JR, Jackson JW. American Journal of Epidemiology. 2022.", tags$a(href="https://doi.org/10.1093/aje/kwac140", "https://doi.org/10.1093/aje/kwac140", target = "_blank"))
                                  ),
                                  h3(tags$span(id="software", "Software")),
                                  tags$ul(
                                    tags$li(tags$strong("DAGitty"), "A browser-based environment for creating, editing, and analyzing causal diagrams", tags$a(href="http://dagitty.net", "dagitty.net")),
                                    tags$li(tags$strong("The R package 'dagitty'"), "An R package providing all the capabilities of the DAGitty web application (and more).", tags$a(href="https://doi.org/10.1093/ije/dyw341", "https://doi.org/10.1093/ije/dyw341", target = "_blank")),
                                    tags$li(tags$strong("ggdag"), "An R package to plot and analyse DAGs.", tags$a(href="https://ggdag.malco.io/", "https://ggdag.malco.io/"), target = "_blank"),
                                    tags$li(tags$strong("pcalg"), "An R package for causal structure learning and estimation of causal effects from observational data.", tags$a(href="https://doi.org/10.18637/jss.v047.i11", "https://doi.org/10.18637/jss.v047.i11"), target = "_blank"),
                                    tags$li(tags$strong("dagR"), "An R package to draw, manipulate, and evaluate DAGs and simulate corresponding data", tags$a(href="https://doi.org/10.1093/ije/dyab167", "https://doi.org/10.1093/ije/dyab167"), target = "_blank"),
                                    tags$li(tags$strong("shinyDAG"), "A web application that uses R and LaTeX to create publication-quality images of DAGs", tags$a(href="https://www.gerkelab.com/project/shinydag/", "https://www.gerkelab.com/project/shinydag/"), target = "_blank"),
                                    tags$li(tags$strong("causaldiagrams.org"), "A searchable database of published health research articles that include a causal diagram", tags$a(href="https://causaldiagrams.org", "https://causaldiagrams.org"), target = "_blank")
                                  ),
                                  hr(),
                                  h2(tags$span(id="contact", "Contact")),
                                  p("Feedback, bug reports, and feature requests can be submitted through the GitHub issues page here:",
                                    tags$a(href="https://github.com/CBDRH/daggle/issues", "https://github.com/CBDRH/daggle/issues", target="_blank"),
                                  "or by contacting Mark at", tags$a(href="mailto:m.hanly@unsw.edu.au", "m.hanly@unsw.edu.au", target="_blank")),
                                  hr()
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
