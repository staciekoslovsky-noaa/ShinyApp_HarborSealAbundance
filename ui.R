## Define UI -----------------------------------------
ui <- shiny::fluidPage(
  theme = shinytheme("superhero"),

  # Hide warnings and errors within the app
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }",
  ),

  titlePanel(fluidRow(
    column(6, "Alaska Harbor Seal Abundance"),
    column(
      6,
      img(
        src = "https://www.fisheries.noaa.gov/themes/custom/noaa_components/images/fisheries_header_logo_jul2019.png",
        height = 90.8,
        class = "pull-right"
      )
    )
  )),

  # First row: informational box and map
  fluidRow(
    column(
      3,
      tabsetPanel(
        tabPanel(
          "Introduction",
          div(style = 'overflow-y:scroll;height:300px', HTML(introduction))
        ),
        tabPanel(
          "Disclaimers",
          div(style = 'overflow-y:scroll;height:300px', HTML(disclaimer))
        ),
        tabPanel(
          "Contact",
          div(style = 'overflow-y:scroll;height:300px', HTML(contact_info))
        ),
        tabPanel(
          "Data Access",
          div(style = 'overflow-y:scroll;height:300px', HTML(data_access))
        ),
        type = "pills" #gives the highlighted tab a background color
      )
    ),
    column(9, leafletOutput(outputId = "map1"))
  ),
  br(),

  # Second row: inputs and plots
  fluidRow(
    # Column with the inputs
    column(
      3,
      tabsetPanel(
        tabPanel(
          "Data Controls",
          verticalLayout(
            # selectInput(inputId = "num.years",
            #             label = div(style = "font-size:16px", "Number of Years for Trend"),
            #             choices = c(3, 5, 8),
            #             selected= 8),
            (radioGroupButtons(
              inputId = "trend.type",
              label = div(style = "font-size:16px", "Trend Type:"),
              choices = c("Linear", "Proportional"),
              selected = "Linear",
              direction = "vertical",
              status = "danger",
              individual = TRUE,
              size = "sm",
              checkIcon = list(
                yes = (icon("ok", lib = "glyphicon")),
                no = icon("remove", lib = "glyphicon")
              )
            )),
            br(),
            (radioGroupButtons(
              inputId = "filter",
              label = div(style = "font-size:16px", "Filter Data By:"),
              choices = c(
                "Stock",
                "Survey Unit",
                "Custom Polygon",
                "Custom Circle",
                "Glacial Sites"
              ),
              selected = "Stock",
              direction = "vertical",
              status = "danger",
              individual = TRUE,
              size = "sm",
              checkIcon = list(
                yes = (icon("ok", lib = "glyphicon")),
                no = icon("remove", lib = "glyphicon")
              )
            )),
            br(),
            (selectInput(
              inputId = "stock.select",
              label = div(
                style = "font-size:16px",
                "Select Stock (if applicable)"
              ),
              choices = c("All", sort(survey_polygons$stockname)),
              multiple = FALSE,
              selected = "All"
            )),
            br(),
            (actionBttn(
              inputId = "update",
              label = "Update Plot",
              style = "jelly",
              size = "md",
              color = "danger"
            )),
            br(),
            (actionBttn(
              inputId = "default",
              label = "Reset",
              style = "jelly",
              size = "sm",
              color = "warning"
            ))
          )
        ),
        tabPanel(
          title = "Instructions",
          div(style = 'overflow-y:scroll;height:400px', HTML(instructions))
        ),
        type = "pills" # Selected tabs use the background fill color (which for some reason is orange)
      )
    ),

    # Column with the plots (under different tabs)
    column(
      9,
      tabsetPanel(
        tabPanel("Abundance", highchartOutput(outputId = "plot1")),
        tabPanel("Trend", highchartOutput(outputId = "plot2")),
        type = "pills" # Selected tabs use the background fill color (which for some reason is orange)
      )
    )
  ),
  hr(),
  print('Alaska Fisheries Science Center | Marine Mammal Laboratory')
)
