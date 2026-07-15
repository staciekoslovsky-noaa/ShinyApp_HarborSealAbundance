## Define UI -----------------------------------------
ui <- shinydashboard::dashboardPage(
  skin = "blue",

  # Dashboard based Shiny set up (collapsible sidebar)
  dashboardHeader(
    title = "Alaska Harbor Seal Abundance",
    titleWidth = 400,
    tags$li(
      class = "dropdown",
      tags$a(
        tags$img(
          src = "https://www.fisheries.noaa.gov/themes/custom/noaa_components/images/fisheries_header_logo_jul2019.png",
          height = "50px",
          width = "125px"
        ),
        style = "padding: 0;"
      )
    )
  ),

  dashboardSidebar(
    tags$head(
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css"
      )
    ),
  
    # Various tabs inclued in sidebar menu
    sidebarMenu(
      shinydashboard::menuItem(
        text = "Welcome",
        tabName = "welcome",
        icon = icon("info-circle")
      ),
      shinydashboard::menuItem(
        text = "Explore Data",
        tabName = "explore",
        icon = icon("map")
      ),
      shinydashboard::menuItem(
        text = "Download Data",
        tabName = "download",
        icon = icon("th")
      ),
      shinydashboard::menuItem(
        text = "Contact",
        tabName = "contact",
        icon = icon("book")
      )
    )
  ),

  dashboardBody(
    useShinyjs(),
    shinydashboard::tabItems(
      
      # Welcome tab
      tabItem(
        tabName = 'welcome',
        wellPanel(
          h2(strong("Welcome"), style = "color: #011f4b"),
          p(introduction),
          p(instructions),
          p(disclaimer)
        )
      ),

      # Explore Data tab
      tabItem(
        tabName = "explore",
        wellPanel(
          # Top Row: Map on the Left, Controls on the Right
          fluidRow(
            column(
              8,
              leafletOutput(outputId = "map1")
            ),
            column(
              width = 4,
              selectizeInput(
                "trend.type",
                "Trend Type",
                choices = c("Linear", "Proportional"),
                selected = "Linear"
              ),
              selectizeInput(
                "filter",
                "Filter Data By",
                choices = c("Stock", "Survey Unit", "Custom Polygon", "Custom Circle", "Glacial Sites"),
                selected = "Stock"
              ),
              selectizeInput(
                "stock.select",
                "Select Stock (if applicable)",
                choices = NULL,
                selected = "All"
              ),
              br(),
              actionButton("update", "Update Plot", class = "btn-primary"), # Optional: adds bootstrap color
              actionButton("default", "Reset")
            )
          ),
          fluidRow(
            column(
              width = 12,
              tabsetPanel(
                type = "pills",
                tabPanel("Abundance", highchartOutput(outputId = "plot1")),
                tabPanel("Trend", highchartOutput(outputId = "plot2"))
              )
            )
          )
        )
      ),

      # Download Data tab
      tabItem(
        tabName = 'download',
        wellPanel(
          p(data_access)
        )
      ),

      # Contact tab
      tabItem(
        tabName = 'contact',
        wellPanel(
          p(contact_info)
        )
      )
    )
  )
)
