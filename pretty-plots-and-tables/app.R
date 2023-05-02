# CRAN Libraries
if (!require(shiny)) {         install.packages("shiny");          library(shiny) }
if (!require(shinyWidgets)) {  install.packages("shinyWidgets");   library(shinyWidgets) }
if (!require(shinydashboard)) {install.packages("shinydashboard"); library(shinydashboard) }
if (!require(shinyjs)) {       install.packages("shinyjs");        library(shinyjs) }
if (!require(magrittr)) {      install.packages("magrittr");       library(magrittr) }
if (!require(dplyr)) {         install.packages("dplyr");          library(dplyr) }
if (!require(ggplot2)) {       install.packages("ggplot2");        library(ggplot2) }
if (!require(tidyr)) {         install.packages("tidyr");          library(tidyr) }
if (!require(scales)) {        install.packages("scales");         library(scales) }
if (!require(gridExtra)) {     install.packages("gridExtra");      library(gridExtra) }
if (!require(DT)) {            install.packages("DT");             library(DT) }
if (!require(fresh)) {         install.packages('fresh', repos = "http://cran.us.r-project.org"); library(fresh) }
if (!require(devtools)) {      install.packages("devtools");       library(devtools) }

# Non-CRAN Libraries
if (!require(styles)) {        install_github("https://github.com/Daniel-Carpenter/styles.git"); library(styles) }



options(encoding = 'UTF-8')



# Default Inputs ===============================================================

# Titles in body
header1Title = "Header 1"
header2Title = "Header 2"

# Titles outside of body
mainTitle         = "Performance Tracker"
inputsHeaderTitle = "Select Inputs"


# Market data
markets = list(
  'Texas'            = c('DFW', 'San Antonio', 'Houston'),
  'OKC Metro'        = c('OKC', 'Norman', 'Moore'),
  'Central Oklahoma' = c('Ada', "Ardmore", 'Sulfer')
)

marketsUnlist = unlist(markets)
descriptionCols = c('Realized', 'Forecast')

# For resets
default_yearRange      = c(2010, as.numeric(format(Sys.Date(), "%Y")))
default_markets        = c('OKC', 'DFW')
default_realizedGrowth = 2.5
default_forecastGrowth = 0



# Theme  =======================================================================

# Reusable colors
color_blueMain   = '#1f4e79'
color_blueLight  = '#BECDE0'
color_grayLight  = '#f1f1f1'
color_text       = '#363636'
color_white      = '#FAFAFA'

# Set colors
ribbonColor     = color_blueMain
sideBarColor    = color_grayLight
backgroundColor = color_white

# HMTL tag colors
color_textTag = paste0("color:", color_text, ';')
headerFont   = paste0('font-weight: bold;', color_textTag)

# GGplot theme to increase text size
baseTextSize  = 16
titleTextSize = 22
themeIncreaseTextSize = theme(text          = element_text(size = baseTextSize),
                              axis.text     = element_text(size = baseTextSize),
                              plot.title    = element_text(size = titleTextSize),
                              plot.subtitle = element_text(size = baseTextSize),
                              axis.title    = element_text(size = baseTextSize),
                              legend.text   = element_text(size = baseTextSize  + 1),
                              strip.text    = element_text(size = titleTextSize - 4)
                              )


## Create a theme --------------------------------------------------------------

themeUI <- create_theme(
  
  # Top Ribbon
  adminlte_color(
    light_blue = ribbonColor,
  ),
  
  # Left Side bar
  adminlte_sidebar(
    width = "300px",
    dark_bg = sideBarColor
  ),
  
  # Page Background
  adminlte_global(
    content_bg = backgroundColor, # Page Background
  )
)


# Generate fake data  ==========================================================
makeFakeData <- function(startYear, endYear, 
                         realizedGrowth, 
                         forecastGrowth,
                         markets,
                         seed = 1024,
                         realizedMean = 100, realizedSigma = 5, 
                         forecastMean = 100, forecastSigma = 5,
                         ROUND_OUTPUT_TO = 0
                         ) {
  
  
  set.seed(seed)
  
  # Convert growth to pct pts
  realizedGrowth = realizedGrowth / 100
  forecastGrowth = forecastGrowth / 100
  
  # set of years
  years = seq(startYear, endYear, 1)
  
  # Determine number of rows to generate
  numRows = length(years)*length(markets)
  
  # generate fake data
  data <- data.frame(Fiscal_Year = factor(seq(startYear, endYear)),
                     Forecast   = round(rnorm(numRows, realizedMean, realizedSigma) * (1 + realizedGrowth)^(years - startYear), ROUND_OUTPUT_TO),
                     Realized   = round(rnorm(numRows, forecastMean, forecastSigma) * (1 + forecastGrowth)^(years - startYear), ROUND_OUTPUT_TO),
                     Market     = factor(rep(markets, each = length(years)))
  )
  
  dataPivoted <- data |>
    
    pivot_longer(cols = descriptionCols,
                 names_to  = 'Description',
                 values_to = 'Value') |>
    
    mutate(Description = factor(Description, 
                                levels = descriptionCols, 
                                labels = descriptionCols)) |> 
    
    arrange(Market, Description, Fiscal_Year)
  
  return(dataPivoted)
}



# UI ===========================================================================

ui <- dashboardPage(
  
  
  ## Header Text ---------------------------------------------------------------
  dashboardHeader(
    title = mainTitle,
    titleWidth = 350
  ),
  
  
  ## Sidebar Inputs ------------------------------------------------------------
  dashboardSidebar(
    
    # Titles and non-input buttons
    width = 350,
    
    # Button to go back to CMAC Website
    tags$a(actionButton("externalLinkBtn", "  Go back to Enterprise Reporting", icon = icon("arrow-left")), 
           href = NULL, target = "_blank"),
    br(),
    h4(inputsHeaderTitle, style = "padding-left:20px"),
    
    
    # Inputs for dashboard
    sidebarMenu(
      
      # Styles
      tags$style(
        HTML(
          paste(".sidebar {color:", color_text, ";}",
                # Selected Tab
                ".tabbable > .nav > li > a                  {background-color:", color_white,    "; color:",  color_text, "}",
                ".tabbable > .nav > li[class=active]    > a {background-color:", color_blueLight, "; color:", color_text, "}"
          )
        )
      ),
      
      
      setSliderColor(rep(color_blueMain, 2), sliderId = 1:2),
      
      # Year range
      numericRangeInput("year_range", "Select Fiscal Year Range", 
                        min = 2000, 
                        max = as.numeric(format(Sys.Date(), "%Y")), 
                        value = default_yearRange, 
                        step = 1),
      
      # Market Area
      virtualSelectInput("market", "Select Market", 
                         choices = markets,
                         selected         = default_markets,
                         showValueAsTags  = TRUE,
                         search           = TRUE,
                         multiple         = TRUE
                         ),
      
      # Forecast
      sliderInput("realizedGrowth", "Select Forecast % Growth", 
                  min = -3.5, max = 3.5, value = default_forecastGrowth, step = 0.5,
                  ticks = F, post = '%'),
      
      # Realized
      sliderInput("forecastGrowth", "Select Realized % Growth", 
                  min = -3.5, max = 3.5, value = default_realizedGrowth, step = 0.5,
                  ticks = F, post = '%'),
      
      # Reset button
      actionButton("resetBtn", "Reset Inputs", icon = icon("undo"))
      
      # # Print button with print icon and text
      # tags$button("Print Page", id = "printBtn", class = "btn btn-primary", 
      #             icon("print"), onclick = "window.print();")
    )
  ),
  
  
  ## Dashboard Placement--------------------------------------------------------
  dashboardBody(
    use_theme(themeUI),
    tabsetPanel(
      
      # Pills for tabs
      type = 'pills',
      
      # Tab 1 - Plots
      tabPanel(title = "Graphical Summary",
               plotOutput("linePlot"),
               plotOutput("boxplot")
               ), 
      
      # Tab 2 - Tables
      tabPanel(title = "Detailed Data", br(), 
               DT::dataTableOutput("table"))
    )
  )
)



# Server  ======================================================================

server <- function(input, output, session) {
  
  ## Generate Dataset ----------------------------------------------------------
  dataset <- reactive({
    
    makeFakeData(startYear = input$year_range[1], 
                 endYear   = input$year_range[2], 
                 realizedGrowth = input$realizedGrowth, 
                 forecastGrowth = as.numeric(input$forecastGrowth),
                 markets = input$market)
  })
  
  
  ## Output Line Plot  ---------------------------------------------------------
  output$linePlot <- renderPlot({
    
    dataset() |>
      
      # Map data
      ggplot(aes(x = Fiscal_Year, 
                 y = Value,
                 color = Market,
                 linetype = Description,
                 group = interaction(Market, Description)
      )) +
      
      # Lines
      geom_line(alpha = 0.75,
                size  = 0.75) +
      
      # Line colors
      styles::scale_color_dbc() + 
      
      # Labels
      labs(title    = "Forecast vs. Realized Performance",
           subtitle = 'Evaluating Market Performance through Time', 
           x        = "Fiscal Year", 
           y        = "Performance Value") +
      
      # Theme
      styles::theme_dbc() + themeIncreaseTextSize
  })
  
  
  ## Output Boxplot  -----------------------------------------------------------
  
  # Create the boxplot
  output$boxplot <- renderPlot({
    
    dataset() |>
      
      # Map data
      ggplot(aes(x     = Description, 
                 y     = Value,
                 fill  = Market,
                 color = Market)) +
      
      # Boxplot
      geom_boxplot() +
      
      # COlors
      styles::scale_fill_dbc() + 
      styles::scale_color_dbc() + 
      
      facet_grid(~Market) + 
      
      # Labels
      labs(title = "Boxplot of Forecast and Realized Values",
           x = NULL,
           y = "Value") +
      
      # Theme
      styles::theme_dbc() + themeIncreaseTextSize
      
  })
  
  
  ## Output Table --------------------------------------------------------------
  output$table <- DT::renderDataTable({
    
    datatable(dataset(),
              colnames = c('Fiscal Year', 'Market Group', 'Data Description', 'Value'),
              rownames = FALSE,
              selection = 'none',
              extensions = c('Buttons', 'RowGroup'),
              options = list(
                pageLength      = (input$year_range[2] - input$year_range[1] + 1)*length(unique(dataset()$Description)),
                searchHighlight = TRUE,
                dom             = 'Bfrtip',
                buttons         = c('copy', 'excel', 'pdf', 'print'),
                rowGroup        = list(dataSrc = c(1, 2)),
                selection = 'none'
              ) )
  })
  
  
  ## Output Reset button -------------------------------------------------------
  observeEvent(input$resetBtn, {
    updateSliderInput(session,        "year_range",     value    = default_yearRange)
    updateCheckboxGroupInput(session, "market",         selected = default_markets)
    updateSliderInput(session,        "realizedGrowth", value    = default_realizedGrowth)
    updateSelectInput(session,        "forecastGrowth", selected = default_forecastGrowth)
  })
}



# Run App ======================================================================

shinyApp(ui, server) # run the shiny server
