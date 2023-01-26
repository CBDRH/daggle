#' drawDag UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr mutate case_when
#' @importFrom ggplot2 ggplot aes ggproto guides guide_legend scale_color_manual scale_shape_manual scale_fill_manual ggsave theme
#' @importFrom ggdag ggdag adjust_for geom_dag_point geom_dag_edges geom_dag_collider_edges geom_dag_text theme_dag geom_dag_label_repel scale_adjusted dag_label node_status
mod_drawDag_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(style="text-align:right;",
      tags$div(style="display:inline-block;",title="Download as .png", downloadButton(ns("download"), NULL, class = "download"))
    ),
    plotOutput(ns("plot"), click = "plotClick")
    )
}

#' drawDag Server Functions
#'
#' @noRd
mod_drawDag_server <- function(id, did, dag, n, pid, label = 0, colliderlines = 0, height, width){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

        dagPlot <- reactive({

          validate(
            need(n() >2 & n() <9, "The number of nodes should be between 3 and 8"),
            need(is.integer(pid())==TRUE & pid()>=100 & pid()<=999, "The daggle id should be an integer between 100 and 999")
          )

        p <- dag() %>%
          node_status() %>% # Need to refresh node status here in case adjustment has added new records to the tidy_dagitty data object
          mutate(col = factor(case_when(status=='exposure' ~ 1,
                                        status=='outcome' ~ 2,
                                        adjusted=='adjusted' ~ 3,
                                        adjusted=='unadjusted' ~ 4),
                              levels = 1:4,
                              labels = c('exposure', 'outcome', 'adjusted', 'unadjusted')
          )) %>%
            ggplot(aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend, fill = .data$col, shape = .data$col)) +
            geom_dag_point(aes(color = col)) +
            geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed")) +
            geom_dag_text() +
            theme_dag(legend.position = 'bottom', plot.background = ggplot2::element_rect(fill='white', color='white')) +
            guides(color = guide_legend(override.aes = list(size = 9))) +
            scale_fill_manual(NULL,
                              values = c('exposure' = exposureCol, 'outcome' = outcomeCol, 'adjusted' = adjustedCol, 'unadjusted' = unadjustedCol),
                              labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome', 'adjusted' = "Adjusted", 'unadjusted' = "Unadjusted"),
                              na.value = naCol, drop = FALSE) +
            scale_color_manual(NULL,
                               values = c('exposure' = exposureCol, 'outcome' = outcomeCol, 'adjusted' = adjustedCol, 'unadjusted' = unadjustedCol),
                               labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome', 'adjusted' = "Adjusted", 'unadjusted' = "Unadjusted"),
                               na.value = naCol, drop = FALSE) +
            scale_shape_manual(NULL,
                               values = c('exposure' = 21, 'outcome' = 21, 'adjusted' = 22, 'unadjusted' = 21),
                               labels = c('exposure' = 'Exposure', 'outcome' = 'Outcome', 'adjusted' = "Adjusted", 'unadjusted' = "Unadjusted"),
                               na.value = naCol, drop = FALSE)


          # Add label if one is defined
          if (label == 1) {
            p <- p + geom_dag_label_repel(aes(label = label, fill = col), show.legend = FALSE, box.padding = 4, segment.color = 'grey80')
          }

          # Add collider lines if requested
          if (colliderlines == 1) {
            p <- p + geom_dag_collider_edges(color = 'pink', show.legend = FALSE)
          }
          return(p)

        })

        # Render the plot


        output$plot <- renderPlot({
          req(dagPlot())

          dagPlot()
          }, width = function(){ # ensures that the height isn't unavailable when a modal containing a plot is first launched. Avoids the invalid quartz device size error.
            ifelse(is.null(width()), 400, 'auto')
          },
          height = function(){
            ifelse(is.null(height()), 400, 'auto')
          })

        # Download the current
        output$download <- downloadHandler(
          filename = function() {
            paste0("daggle-",did(), ".png")
          },
          content = function(file) {
            ggsave(file, plot = dagPlot(), device = "png")
          }
        )

        # Update contents of reactive before modal is opened
        outputOptions(output, "plot", suspendWhenHidden = FALSE)

  })
}

## To be copied in the UI
# mod_drawDag_ui("drawDag_ui_1")

## To be copied in the server
# mod_drawDag_server("drawDag_ui_1")


#' mod_drawDag test Function
#'
#' @noRd
test_drawDag <- function() {
  ui <- fluidPage(
    mod_drawDag_ui('dag1')
  )
  server <- function(input, output, session) {

    d <- randDAG(4, .6)

    mod_drawDag_server('dag1', dag = d, n = 4)

  }

  shinyApp(ui, server)
}
