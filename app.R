# Load necessary libraries
rm(list = ls())
library(shiny)
library(tidyr)
library(tidyverse)
library(dplyr)
library(shinyWidgets)
library(stringr)
library(colourpicker)
library(seasonal)
library(lubridate)
library(plotly)
load("data_index.RData")

# Get current month and year
current_month <- as.integer(format(Sys.Date(), "%m"))
current_year <- as.integer(format(Sys.Date(), "%Y"))


css <- HTML("
/* Style for the smaller control boxes within panels */
.control-box {
  background-color: #f5f5f5 !important;  /* Light grey background */
  border: 1px solid #ddd !important;     /* Light grey border */
  margin: 5px;
  padding: 10px;
}

/* Make the large well panels blue */
.well {
  background-color: #f0f8ff !important;  /* Light blue background */
  border: 1px solid #add8e6 !important;  /* Blue border */
}

/* Override nested wellPanel backgrounds */
.well .well.control-box {
  background-color: #f5f5f5 !important;  /* Ensure control boxes stay light grey */
  border: 1px solid #ddd !important;     /* Light grey border */
}

/* Title panel styling */
.title-panel {
  padding: 15px 0;
  margin-bottom: 20px;
}

.title-panel h2 {
  font-weight: 500;
  margin: 0;
  color: #333;
}

/* Container for sub-navigation */
#sub-navigation-container {
  margin-top: 0;
  background-color: #ffffff;
}

.container-fluid {
  padding-left: 30px;
  padding-right: 30px;
}

/* Main navigation bar styling */
.main-navbar {
  background-color: #ffffff;
  border-bottom: 1px solid #e0e0e0;
  margin-bottom: 0;
  padding: 0;
}

.main-navbar .nav-tabs {
  border-bottom: none;
  display: flex;
  width: 100%;
}

.main-navbar .nav-tabs > li {
  flex: 1;
  text-align: center;
  margin-bottom: 0;
}

.main-navbar .nav-tabs > li > a {
  color: #505050;
  background-color: #ffffff;
  border: none;
  border-radius: 0;
  font-size: 16px;
  font-weight: 500;
  margin-right: 0;
  padding: 15px 0;
  transition: all 0.2s ease;
}

.main-navbar .nav-tabs > li > a:hover {
  background-color: #f8f9fa;
  border: none;
}

.main-navbar .nav-tabs > li.active > a,
.main-navbar .nav-tabs > li.active > a:hover,
.main-navbar .nav-tabs > li.active > a:focus {
  color: #2470dc;
  background-color: #ffffff;
  border: none;
  border-bottom: 3px solid #2470dc;
  font-weight: 600;
}

/* Sub navigation bar styling */
.sub-navbar {
  background-color: #ffffff;
  border-bottom: 1px solid #e6e6e6;
  padding: 0;
  margin-bottom: 20px;
}

.sub-navbar .nav-tabs {
  border-bottom: none;
  display: flex;
  padding: 0 15px;
}

.sub-navbar .nav-tabs > li {
  margin-bottom: 0;
  margin-right: 30px;
}

.sub-navbar .nav-tabs > li > a {
  color: #707070;
  background-color: transparent;
  border: none;
  border-radius: 0;
  font-size: 14px;
  padding: 12px 0;
  margin-right: 0;
  transition: all 0.2s ease;
}

.sub-navbar .nav-tabs > li > a:hover {
  color: #2470dc;
  background-color: transparent;
  border: none;
}

.sub-navbar .nav-tabs > li.active > a,
.sub-navbar .nav-tabs > li.active > a:hover,
.sub-navbar .nav-tabs > li.active > a:focus {
  color: #2470dc;
  background-color: transparent;
  border: none;
  border-bottom: 2px solid #2470dc;
  font-weight: 500;
}

/* Style for chart and data box in visualization manager */
.panel-content .chart-container {
  margin-bottom: 15px;
}

/* Ensure proper spacing for the data box */
.panel-content .data-box {
  margin-top: 15px;
}

/* Ensure full width for the chart */
.panel-content .chart-container, 
.panel-content .data-box {
  width: 100%;
}

/* Style for stacked date range inputs */
.input-daterange input {
  display: block !important;
  width: 100% !important;
  margin-bottom: 5px;
}

/* Style for action buttons container */
.control-box .btn-group-vertical {
  display: flex;
  flex-direction: column;
  width: 100%;
  gap: 15px;  /* Increased gap between buttons */
}

/* Style for individual buttons */
.control-box .btn {
  margin-bottom: 10px;  /* Add margin to bottom of each button */
  width: 100%;         /* Make buttons full width */
  white-space: normal; /* Allow text to wrap */
  min-height: 38px;    /* Minimum height for buttons */
}

/* Remove margin from last button */
.control-box .btn:last-child {
  margin-bottom: 0;
}

/* Custom button styling */
.custom-button {
  background-color: #f8f9fa;
  border: 1px solid #ddd;
  transition: background-color 0.3s;
}

.custom-button:hover {
  background-color: #e9ecef;
}

/* Sidebar panel styling */
.sidebar-panel {
  background-color: #f0f8ff;
  border: 1px solid #add8e6;
  border-radius: 4px;
  margin-bottom: 15px;
  padding: 15px;
}

/* Style for sidebar inputs */
.sidebar-panel .form-group {
  margin-bottom: 12px;
}

/* Two-column layout for sidebar */
.sidebar-row {
  display: flex;
  flex-wrap: wrap;
  margin-left: -5px;
  margin-right: -5px;
}

.sidebar-col-6 {
  flex: 0 0 50%;
  max-width: 50%;
  padding-left: 5px;
  padding-right: 5px;
  box-sizing: border-box;
}

.sidebar-col-12 {
  flex: 0 0 100%;
  max-width: 100%;
  padding-left: 5px;
  padding-right: 5px;
  box-sizing: border-box;
}

/* Bottom buttons in sidebar */
.sidebar-buttons {
  margin-top: 15px;
  padding-top: 15px;
  border-top: 1px solid #e6e6e6;
}

.sidebar-btn {
  width: 100%;
  margin-bottom: 10px;
  white-space: normal;
}

/* Chart container styling */
.chart-container {
  background-color: white;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 15px;
  margin-bottom: 20px;
}

.chart-title {
  font-size: 18px;
  font-weight: bold;
  margin-bottom: 15px;
  color: #2c3e50;
}

/* Manager panel styling */
.manager-panel {
  background-color: #f5f5f5;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 15px;
}

.manager-title {
  font-size: 16px;
  font-weight: bold;
  margin-bottom: 15px;
  color: #2c3e50;
  border-bottom: 1px solid #ddd;
  padding-bottom: 10px;
}

/* Date range input styling for sidebar */
.date-range-full {
  width: 100%;
}

/* Combined panel styling */
.combined-panel {
  background-color: #f0f8ff;
  border: 1px solid #add8e6;
  border-radius: 4px;
  padding: 15px;
  margin-top: 20px;
  margin-bottom: 20px;
}

/* Make smaller controls more compact */
.compact-control .control-label {
  font-size: 12px;
  margin-bottom: 3px;
}

.compact-control .form-control {
  font-size: 12px;
  height: 30px;
  padding: 4px 8px;
}

/* Make picker input more compact in sidebar */
.sidebar-panel .selectize-input {
  min-height: 30px;
  padding: 4px 8px;
  font-size: 12px;
}

.sidebar-panel .selectize-dropdown {
  font-size: 12px;
}

/* Action box styling */
.action-box {
  background-color: #f5f5f5;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 15px;
  height: 100%;
}

.action-box h4 {
  font-size: 16px;
  margin-top: 0;
  margin-bottom: 15px;
  padding-bottom: 8px;
  border-bottom: 1px solid #ddd;
  color: #2c3e50;
}

.action-button {
  width: 100%;
  margin-bottom: 10px;
  text-align: left;
  white-space: normal;
}

/* Section divider in the action box */
.action-section {
  margin-bottom: 15px;
  padding-bottom: 15px;
}

/* Data management box styling */
.data-box {
  background-color: #f5f5f5;
  border: 1px solid #ddd;
  border-radius: 4px;
  padding: 15px;
  height: 100%;
}

.data-box h4 {
  font-size: 16px;
  margin-top: 0;
  margin-bottom: 15px;
  padding-bottom: 8px;
  border-bottom: 1px solid #ddd;
  color: #2c3e50;
}

/* New styles for the collapsible panel */
.panel-collapsible {
  border: 1px solid #add8e6;
  border-radius: 4px;
  margin-bottom: 15px;
  background-color: #f0f8ff;
  overflow: hidden;
}

.panel-header {
  padding: 10px 15px;
  background-color: #4a90e2;
  color: white;
  cursor: pointer;
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.panel-header:hover {
  background-color: #357abd;
}

.panel-header h3 {
  margin: 0;
  font-size: 18px;
}

.panel-icon {
  transition: transform 0.3s;
}

.panel-icon.collapsed {
  transform: rotate(-90deg);
}

.panel-content {
  padding: 15px;
  max-height: 2000px;
  transition: max-height 0.5s ease-in-out, padding 0.5s ease-in-out;
  overflow: hidden;
}

.panel-content.collapsed {
  max-height: 0;
  padding-top: 0;
  padding-bottom: 0;
  overflow: hidden;
}
")

# UI Section
ui <- fluidPage(
  tags$head(
    tags$style(css),
    tags$script(HTML("
      $(document).ready(function() {
        // Make both the header and icon clickable for toggle
        $('#panel-header').click(function(e) {
          // This prevents double-firing when clicking the icon
          e.stopPropagation();
          // Toggle the panel state
          Shiny.setInputValue('panel_header_click', Math.random(), {priority: 'event'});
        });
        
        Shiny.addCustomMessageHandler('updatePanelState', function(message) {
          if (message.collapsed) {
            $('#panel-icon').addClass('collapsed');
            $('#panel-content').addClass('collapsed');
          } else {
            $('#panel-icon').removeClass('collapsed');
            $('#panel-content').removeClass('collapsed');
          }
        });
        
        // Add smooth transitions when switching tabs
        $('.nav-tabs a').on('shown.bs.tab', function(event){
          $(event.target).parent().addClass('active').siblings().removeClass('active');
        });
      });
    "))
  ),
  
  div(class = "title-panel",
      div(class = "container-fluid",
          h2("Sectoral Economic Data Tool")
      )
  ),
  
  # Main Navigation Bar (Dataset Categories)
  div(class = "main-navbar",
      div(class = "container-fluid",
          tabsetPanel(id = "datasetNav", type = "tabs",
                      tabPanel("International Trade", value = "International Trade"),
                      tabPanel("Production and Capacity", value = "Production and Capacity"),
                      tabPanel("Price Indices", value = "Price Indices"),
                      tabPanel("Current Employment Statistics", value = "Current Employment Statistics"),
                      tabPanel("M3 Manufacturers Shipments, Inventories & Orders", 
                               value = "M3 Manufacturers Shipments, Inventories & Orders"),
                      tabPanel("Investment", value = "Investment")
          )
      )
  ),
  
  # Container for the sub-navigation
  div(id = "sub-navigation-container",
      # Sub Navigation Bar for International Trade
      conditionalPanel(
        condition = "input.datasetNav == 'International Trade'",
        div(class = "sub-navbar",
            div(class = "container-fluid",
                tabsetPanel(id = "internationalTradeNav", type = "tabs",
                            tabPanel("Imports", value = "Imports"),
                            tabPanel("Exports", value = "Exports"),
                            tabPanel("PPI-Deflated Imports", value = "PPI-Deflated Imports"),
                            tabPanel("IPI-Deflated Imports", value = "IPI-Deflated Imports"),
                            tabPanel("PPI-Adjusted Exports", value = "PPI-Adjusted Exports"),
                            tabPanel("EPI-Adjusted Exports", value = "EPI-Adjusted Exports")
                )
            )
        )
      ),
      
      # Sub Navigation for Production and Capacity
      conditionalPanel(
        condition = "input.datasetNav == 'Production and Capacity'",
        div(class = "sub-navbar",
            div(class = "container-fluid",
                tabsetPanel(id = "productionCapacityNav", type = "tabs",
                            tabPanel("Industrial Production", value = "Industrial Production"),
                            tabPanel("Capacity Utilization", value = "Capacity Utilization"),
                            tabPanel("Capacity", value = "Capacity"),
                            tabPanel("Relative Importance Weights", value = "Relative Importance Weights")
                )
            )
        )
      ),
      
      # Sub Navigation for Price Indices
      conditionalPanel(
        condition = "input.datasetNav == 'Price Indices'",
        div(class = "sub-navbar",
            div(class = "container-fluid",
                tabsetPanel(id = "priceIndicesNav", type = "tabs",
                            tabPanel("Import Price Index", value = "Import Price Index"),
                            tabPanel("Export Price Index", value = "Export Price Index"),
                            tabPanel("Producer Price Index", value = "Producer Price Index")
                )
            )
        )
      ),
      
      # Sub Navigation for Current Employment Statistics
      conditionalPanel(
        condition = "input.datasetNav == 'Current Employment Statistics'",
        div(class = "sub-navbar",
            div(class = "container-fluid",
                tabsetPanel(id = "employmentStatsNav", type = "tabs",
                            tabPanel("All Employees", value = "All Employees"),
                            tabPanel("Women Employees", value = "Women Employees"),
                            tabPanel("Average Hourly Earnings of All Employees", 
                                     value = "Average Hourly Earnings of All Employees"),
                            tabPanel("Average Weekly Hours of All Employees", 
                                     value = "Average Weekly Hours of All Employees")
                )
            )
        )
      ),
      
      # Sub Navigation for M3 Manufacturers
      conditionalPanel(
        condition = "input.datasetNav == 'M3 Manufacturers Shipments, Inventories & Orders'",
        div(class = "sub-navbar",
            div(class = "container-fluid",
                tabsetPanel(id = "manufacturersNav", type = "tabs",
                            tabPanel("Finished Goods Inventories", value = "Finished Goods Inventories"),
                            tabPanel("Inventories to Shipments Ratio", value = "Inventories to Shipments Ratio"),
                            tabPanel("Material and Supply Inventories", value = "Material and Supply Inventories"),
                            tabPanel("Total Inventories", value = "Total Inventories"),
                            tabPanel("Unfilled Orders", value = "Unfilled Orders"),
                            tabPanel("Unfilled Orders to Shipments Ratios", 
                                     value = "Unfilled Orders to Shipments Ratios"),
                            tabPanel("Value of Shipments", value = "Value of Shipments"),
                            tabPanel("Work in Process Inventories", value = "Work in Process Inventories"),
                            tabPanel("New Orders", value = "New Orders")
                )
            )
        )
      ),
      
      # Sub Navigation for Investment
      conditionalPanel(
        condition = "input.datasetNav == 'Investment'",
        div(class = "sub-navbar",
            div(class = "container-fluid",
                tabsetPanel(id = "investmentNav", type = "tabs",
                            tabPanel("Structures Investment", value = "Structures Investment"),
                            tabPanel("Real Structures Investment", value = "Real Structures Investment"),
                            tabPanel("Equipment Investment", value = "Equipment Investment"),
                            tabPanel("Real Equipment Investment", value = "Real Equipment Investment")
                )
            )
        )
      )
  ),
  
  # New split-view layout (25% - 75%)
  fluidRow(
    # Left sidebar (25% width) for Select and Transform
    column(width = 3,
           # Combined panel for Select and Transform
           div(class = "sidebar-panel",
               # Date Range - Full Width
               div(class = "sidebar-row",
                   div(class = "sidebar-col-12",
                       dateRangeInput("dateRange", 
                                      label = "Date Range",
                                      start = as.Date("1900-01-01"),
                                      end = Sys.Date(),
                                      min = "1900-01-01",
                                      max = Sys.Date(),
                                      format = "mm/dd/yyyy",
                                      separator = " → ")
                   )
               ),
               
               # Industry and NAICS constraints - Two columns
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       pickerInput("thematicGroupings", "Industry Constraint",
                                   choices = c("No Constraint", "INFRASTRUCTURE", "Roads and Bridges", "Public Transit",  
                                               "EV Infrastructure", "Airports", "Port Infrastructure",
                                               "Water Infrastructure", "Internet and Communications Infrastructure",
                                               "Electrical Grid and Transmission Infrastructure", "CLEAN ENERGY", 
                                               "Wind Turbines", "Solar Panels", "Batteries", "Electric Vehicles", 
                                               "Nuclear Power", "Carbon Capture and Storage", "Clean Hydrogen", 
                                               "Critical Minerals", "Geothermal Energy", "Heat Pumps", 
                                               "SEMICONDUCTORS", "HOUSING", "AUTOMOBILES"),
                                   options = list(`live-search` = TRUE))
                   ),
                   div(class = "sidebar-col-6",
                       pickerInput("naicsConstraint", "NAICS Code Constraint",
                                   choices = c("No Constraint", "", unique(data$index_col[grepl("^\\d{3}\\b", data$NAICS_Code)])),
                                   options = list(`live-search` = TRUE))
                   )
               ),
               
               # NAICS selection - Full width
               div(class = "sidebar-row",
                   div(class = "sidebar-col-12",
                       pickerInput("naicsIndex", "Select NAICS Code:",
                                   choices = NULL,
                                   selected = NULL,
                                   options = list(`live-search` = TRUE))
                   )
               ),
               
               # Checkboxes - Two columns
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       checkboxInput("showSubIndustries", "Show Sub-Industries?", value = FALSE)
                   ),
                   div(class = "sidebar-col-6",
                       checkboxInput("useSeasonalAdjustment", "Show Seasonally Adjusted Data", value = FALSE)
                   )
               ),
               
               # Index to date - Two columns
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       checkboxInput("useIndexDate", "Index to Date", value = FALSE)
                   ),
                   div(class = "sidebar-col-6",
                       dateInput("indexDate", "Index Date:",
                                 value = as.Date("2020-01-01"),
                                 min = "2000-01-01",
                                 max = Sys.Date(),
                                 format = "yyyy-mm",
                                 startview = "year")
                   )
               ),
               
               # Transformations - Two columns
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       selectInput("movingAverageTransform", "Moving Average:", 
                                   choices = c("No Transform", "3 Months", "6 Months", "12 Months", "18 Months", "36 Months"),
                                   selected = "No Transform")
                   ),
                   div(class = "sidebar-col-6",
                       selectInput("changeTransform", "Change:", 
                                   choices = c("No Transform", "1 Month", "3 Months", "6 Months", "12 Months", "18 Months", "36 Months"),
                                   selected = "No Transform")
                   )
               ),
               
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       selectInput("percentChangeTransform", "Percent Change:", 
                                   choices = c("No Transform", "1 Month", "3 Months", "6 Months", "12 Months", "18 Months", "36 Months"),
                                   selected = "No Transform")
                   ),
                   div(class = "sidebar-col-6",
                       selectInput("cagrTransform", "CAGR:", 
                                   choices = c("No Transform", "1 Month", "3 Months", "6 Months", "12 Months", "18 Months", "36 Months"),
                                   selected = "No Transform")
                   )
               ),
               
               # Bottom buttons 
               div(class = "sidebar-buttons",
                   div(class = "sidebar-row",
                       div(class = "sidebar-col-12",
                           actionButton("addToStoredData", "Add Data to Final Visualization",
                                        class = "sidebar-btn custom-button")
                       )
                   ),
                   div(class = "sidebar-row",
                       div(class = "sidebar-col-12",
                           actionButton("resetInputs", "Reset Inputs",
                                        class = "sidebar-btn custom-button")
                       )
                   )
               )
           )
    ),
    
    # Right content area (75% width) for visualization
    column(width = 9,
           # First chart - Data Finder
           div(class = "chart-container",
               div(class = "chart-title", "Data Finder"),
               plotlyOutput("lineChart", height = "600px")
           )
    )
  ),
  
  # Full-width combined panel for visualization controls and saved data
  # Replace the existing fluidRow for the combined panel with this:
  
  fluidRow(
    column(width = 12,
           tags$div(id = "visualization-manager", class = "panel-collapsible",
                    # Header with toggle functionality - KEEP THIS PART
                    tags$div(id = "panel-header", class = "panel-header",
                             tags$h3(textOutput("panelHeaderText")),
                             tags$span(id = "panel-icon", class = "panel-icon",
                                       icon("chevron-down"))
                    ),
                    # Content area (collapsible) - UPDATED LAYOUT
                    tags$div(id = "panel-content", class = "panel-content",
                             fluidRow(
                               # Left side: Actions with all visualization controls
                               column(width = 3,
                                      div(class = "action-box",
                                          h4("Adjust Visualization"),
                                          
                                          # Title input with reduced margin
                                          div(style = "margin-bottom: 10px;",
                                              textInput("finishedVisualizationTitle", "Title:", value = "Finished Visualization")
                                          ),
                                          
                                          # Add Graphics section - more compact layout
                                          div(class = "sidebar-row", style = "margin-bottom: 10px;",
                                              div(class = "sidebar-col-12",
                                                  h5("Add Graphics", style = "margin-top: 0; margin-bottom: 8px;")
                                              ),
                                              # Horizontal line controls on same row
                                              div(class = "sidebar-col-6",
                                                  div(class = "compact-control",
                                                      checkboxInput("addHorizontalLine", "Add Horizontal Line", value = FALSE)
                                                  )
                                              ),
                                              div(class = "sidebar-col-6",
                                                  div(class = "compact-control",
                                                      numericInput("horizontalLineValue", "Line Value", value = 0, step = 0.1)
                                                  )
                                              ),
                                              
                                              # Vertical line controls on same row
                                              div(class = "sidebar-col-6",
                                                  div(class = "compact-control",
                                                      checkboxInput("addVerticalLine", "Add Vertical Line", value = FALSE)
                                                  )
                                              ),
                                              div(class = "sidebar-col-6",
                                                  div(class = "compact-control",
                                                      dateInput("verticalLineDate", "Line Date:",
                                                                value = Sys.Date(),
                                                                min = "2000-01-01",
                                                                max = Sys.Date(),
                                                                format = "yyyy-mm",
                                                                startview = "year")
                                                  )
                                              ),
                                              
                                              # Recession shading on its own row
                                              div(class = "sidebar-col-12",
                                                  div(class = "compact-control",
                                                      checkboxInput("addRecessionShading", "Add NBER Recession Shading", value = FALSE)
                                                  )
                                              )
                                          ),
                                          
                                          # Layout section with more compact style
                                          div(class = "sidebar-row", style = "margin-bottom: 10px;",
                                              div(class = "sidebar-col-12",
                                                  h5("Layout", style = "margin-top: 0; margin-bottom: 8px;")
                                              ),
                                              div(class = "sidebar-col-6",
                                                  div(class = "compact-control",
                                                      numericInput("xLabelFreq", "Data Finder X-Labels (yrs):", value = 3)
                                                  )
                                              ),
                                              div(class = "sidebar-col-6",
                                                  div(class = "compact-control",
                                                      numericInput("storedXLabelFreq", "Finished X-Labels (yrs):", value = 3)
                                                  )
                                              ),
                                              div(class = "sidebar-col-12",
                                                  div(class = "compact-control",
                                                      checkboxInput("alignZeros", "Align zero lines on dual axes", value = TRUE)
                                                  )
                                              )
                                          ),
                                          
                                          # Main action buttons with compact spacing
                                          div(class = "sidebar-row",
                                              div(class = "sidebar-col-6",
                                                  downloadButton("downloadStoredData", "Download Data",
                                                                 class = "sidebar-btn")
                                              ),
                                              div(class = "sidebar-col-6",
                                                  actionButton("clearStoredData", "Clear Data",
                                                               class = "sidebar-btn custom-button")
                                              )
                                          )
                                      )
                               ),
                               
                               # Right side: Visualization and Saved Data (stacked vertically)
                               column(width = 9,
                                      # Add Visualization Chart
                                      div(class = "chart-container",
                                          uiOutput("visualizationTitle"),
                                          plotlyOutput("storedLineChart", height = "600px")
                                      ),
                                      # Saved Data section below chart
                                      div(class = "data-box",
                                          h4("Saved Data"),
                                          uiOutput("seriesManager")
                                      )
                               )
                             )
                    )
           )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Add these code blocks to your server function:
  # Reactive value to track panel state
  panelCollapsed <- reactiveVal(TRUE)
  
  # Toggle panel collapse state when header is clicked
  observeEvent(input$panel_header_click, {
    # This will toggle the state regardless of data presence
    panelCollapsed(!panelCollapsed())
  })
  
  
  
  # Update panel header text based on data presence
  output$panelHeaderText <- renderText({
    if (nrow(storedData()) == 0) {
      "Finished Visualization Manager (No Data)"
    } else {
      paste0("Finished Visualization Manager (", 
             nrow(unique(storedData()[, "Combined", drop = FALSE])), 
             " Series)")
    }
  })
  
  # Use JavaScript to handle the panel collapse/expand
  observe({
    # Update panel state based on panelCollapsed reactive value
    session$sendCustomMessage("updatePanelState", 
                              list(collapsed = panelCollapsed()))
  })
  
  recessions_df <- data.frame(
    start = as.Date(c(
      "1973-11-01", "1980-01-01", "1981-07-01", 
      "1990-07-01", "2001-03-01", "2007-12-01", 
      "2020-02-01"
    )),
    end = as.Date(c(
      "1975-03-01", "1980-07-01", "1982-11-01", 
      "1991-03-01", "2001-11-01", "2009-06-01", 
      "2020-04-01"
    ))
  )
  
  # Create a reactive value to store the stored data
  # Update this block
  storedData <- reactiveVal(data.frame(Date = as.Date(character()), 
                                       NAICS_Code = character(),
                                       NAICS_Name = character(),
                                       Indicator = character(),
                                       Value = numeric(),
                                       index_col = character(),
                                       Transform_Name = character(),
                                       Combined = character(),
                                       Axis_Type = character(),
                                       Units = character()))  # Add Units here
  
  
  
  # Reactive expression to extract the first three characters of the selected NAICS Constraint
  naicsConstraintCode <- reactive({
    if (input$naicsConstraint != "" && input$naicsConstraint != "No Constraint") {
      substr(input$naicsConstraint, 1, 3)
    } else {
      ""
    }
  })
  
  # Thematic NAICS codes
  thematicNAICS <- reactive({
    if (input$thematicGroupings == "No Constraint") {
      unique(data$NAICS_Code)
    } else {
      switch(input$thematicGroupings,
             "Roads and Bridges" = c("32412", "32732", "3311", "3323", "33312", "3273", "3273", "3323", "333"),
             "Public Transit" = c("3261", "3272", "3323", "3323", "332", "3325", "332", "33313", "3334", "3336", "333", "3343", "3344", "3345", "3353", "3359", "3365"),
             "EV Infrastructure" = c("3273", "3315", "332", "3342", "3345", "3353", "33591", "33593", "3359"),
             "Airports" = c("32412", "3261", "32732", "3273", "3273", "3279", "3323", "3323", "3323", "3323"),
             "Port Infrastructure" = c("32732", "3311", "3323", "3323", "33312", "333", "333", "333"),
             "Water Infrastructure" = c("3261", "3273", "331", "3329", "3333", "333", "3345", "3345", "3359"),
             "Internet and Communications Infrastructure" = c("3323", "3342", "3342", "3342", "3344", "3359", "3359"),
             "Electrical Grid and Transmission Infrastructure" = c("3314", "3323", "3345", "3353", "3353", "33592", "33593", "3359"),
             "SEMICONDUCTORS" = c("3329", "3329", "3332", "3332", "3333", "3333", "333", "3344", "3345", "3353", "3391"),
             "Wind Turbines" = c("3315", "3323", "332991", "3336", "3336", "3336", "3344", "3359", "3359"),
             "Solar Panels" = c("32518", "3261", "3272", "3314", "3323", "332", "3332", "3332", "3344", "3344", "33593", "3359"),
             "Batteries" = c("32518", "3261", "33141", "3314", "3344", "3344", "33591"),
             "Electric Vehicles" = c("3342", "3344", "3344", "3345", "3353", "33591", "3363"),
             "Nuclear Power" = c("3323", "3323", "332", "332", "3329", "3336", "3336", "3336", "3345", "3345", "3353", "3359"),
             "Carbon Capture and Storage" = c("32518", "3329", "33313", "3334", "3345", "3345"),
             "Clean Hydrogen" = c("32518", "3329", "33313", "333", "3344", "3345"),
             "Critical Minerals" = c("21221", "21222", "2123", "2122", "32518", "33141", "3314", "33313"),
             "Geothermal Energy" = c("3327", "3329", "33313", "3334", "3345", "3353"),
             "Heat Pumps" = c("3314", "3334", "3345", "3353"),
             "HOUSING" = c("32191", "32191", "3261", "32712", "32731", "32732", "3274", "3279", "3311", "331", "3325", "3334", "3351", "33522", "3371"),
             "AUTOMOBILES" = c("3315", "3315", "3321", "3335", "3336", "3344", "3362", "3363", "3363", "3363", "3363"),
             "INFRASTRUCTURE" = c("32412", "32732", "3311", "3323", "33312", "3273", "3273", "3323", "333",
                                  "3261", "3272", "3323", "3323", "332", "3325", "332", "33313", "3334", "3336", "333", "3343", "3344", "3345", "3353", "3359", "3365",
                                  "3273", "3315", "332", "3342", "3345", "3353", "33591", "33593", "3359",
                                  "32412", "3261", "32732", "3273", "3273", "3279", "3323", "3323", "3323", "3323",
                                  "32732", "3311", "3323", "3323", "33312", "333", "333", "333",
                                  "3261", "3273", "331", "3329", "3333", "333", "3345", "3345", "3359",
                                  "3323", "3342", "3342", "3342", "3344", "3359", "3359",
                                  "3314", "3323", "3345", "3353", "3353", "33592", "33593", "3359"),
             "CLEAN ENERGY" = c("3314", "3323", "3345", "3353", "3353", "33592", "33593", "3359",
                                "3315", "3323", "332991", "3336", "3336", "3336", "3344", "3359", "3359",
                                "32518", "3261", "3272", "3314", "3323", "332", "3332", "3332", "3344", "3344", "33593", "3359",
                                "32518", "3261", "33141", "3314", "3344", "3344", "33591",
                                "3342", "3344", "3344", "3345", "3353", "33591", "3363",
                                "3323", "3323", "332", "332", "3329", "3336", "3336", "3336", "3345", "3345", "3353", "3359",
                                "32518", "3329", "33313", "3334", "3345", "3345",
                                "32518", "3329", "33313", "333", "3344", "3345",
                                "21221", "21222", "2123", "2122", "32518", "33141", "3314", "33313",
                                "3327", "3329", "33313", "3334", "3345", "3353",
                                "3314", "3334", "3345", "3353")
      )
    }
  })
  
  #Observers to make sure NAICS Constraint and Thematic Groupings are mutually exclusive
  observeEvent(input$naicsConstraint, {
    if (input$naicsConstraint != "No Constraint") {
      updateSelectInput(session, "thematicGroupings", selected = "No Constraint")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$thematicGroupings, {
    if (input$thematicGroupings != "No Constraint") {
      updateSelectInput(session, "naicsConstraint", selected = "No Constraint")
    }
  }, ignoreInit = TRUE)
  
  
  # Filtered NAICS choices based on Thematic Constraints, NAICS Constraint, and selected Indicator
  filteredNAICS <- reactive({
    if (input$thematicGroupings != "No Constraint") {
      data %>%
        filter(NAICS_Code %in% thematicNAICS(),
               Indicator == getCurrentIndicator())
    } else if (naicsConstraintCode() != "") {
      data %>%
        filter(str_starts(NAICS_Code, naicsConstraintCode()),
               Indicator == getCurrentIndicator())
    } else {
      data %>%
        filter(Indicator == getCurrentIndicator())
    }
  })
  
  #Observer to run the dataset/indicator dropdown
  observeEvent(getCurrentDataset(), {
    # Filter indicators based on selected dataset
    filtered_indicators <- data %>%
      filter(Dataset == getCurrentDataset()) %>%
      pull(Indicator) %>%
      unique()
    
    # Update the indicator dropdown using picker input
    updatePickerInput(session, "indicatorDropdown",
                      choices = filtered_indicators,
                      selected = filtered_indicators[1])
  })
  
  observeEvent(c(naicsConstraintCode(), getCurrentIndicator(), input$thematicGroupings), {
    updatePickerInput(session, "naicsIndex",
                      choices = unique(filteredNAICS()$index_col),
                      selected = unique(filteredNAICS()$index_col)[1])
  })
  
  # Define reactive variable for transform name
  transformName <- reactive({
    transforms <- c()
    
    # Moving Average transformation
    if (input$movingAverageTransform != "No Transform") {
      # Extract the number of months from the selection
      period <- as.numeric(gsub(" Months", "", input$movingAverageTransform))
      transforms <- c(transforms, paste0(period, "MMA"))
    }
    
    # Change transformation
    if (input$changeTransform != "No Transform") {
      # Extract the number of months from the selection
      period <- as.numeric(gsub(" Months?", "", input$changeTransform))
      transforms <- c(transforms, paste0(period, "m Change"))
    }
    
    # Percent Change transformation
    if (input$percentChangeTransform != "No Transform") {
      # Extract the number of months from the selection
      period <- as.numeric(gsub(" Months?", "", input$percentChangeTransform))
      transforms <- c(transforms, paste0(period, "m % Change"))
    }
    
    # CAGR transformation
    if (input$cagrTransform != "No Transform") {
      # Extract the number of months from the selection
      period <- as.numeric(gsub(" Months?", "", input$cagrTransform))
      transforms <- c(transforms, paste0(period, "m CAGR"))
    }
    
    if (length(transforms) == 0) {
      transforms <- ""
    }
    transforms
  })
  
  #  Create a list of NAICS codes based on the selected index column, Show Sub-Industries option, and Show All Included Series option
  #  Create a list of NAICS codes based on the selected index column and Show Sub-Industries option
  selectedNAICS <- reactive({
    # Now we're only using the showSubIndustries checkbox
    if (input$showSubIndustries) {
      selected_index <- input$naicsIndex
      selected_code <- strsplit(selected_index, " - ")[[1]][1]
      naics_codes <- c(selected_code, paste0(selected_code, 1:9))
      data %>%
        filter(NAICS_Code %in% naics_codes) %>%
        pull(index_col)
    } else {
      input$naicsIndex
    }
  })
  
  #  Convert selected months and years to dates
  startDate <- reactive({
    # Force day to first of month
    as.Date(format(input$dateRange[1], "%Y-%m-01"))
  })
  
  endDate <- reactive({
    # Force day to first of month
    as.Date(format(input$dateRange[2], "%Y-%m-01"))
  })
  
  #  Convert selected index month and year to date
  indexDate <- reactive({
    # Force day to first of month
    as.Date(format(input$indexDate, "%Y-%m-01"))
  })
  
  #fiilter data based on user selections
  #  Filter data based on user selections
  #  Filter data based on user selections
  filteredData <- reactive({
    filtered <- data %>%
      filter(Date >= startDate() & Date <= endDate() &
               index_col %in% selectedNAICS() &
               Indicator == getCurrentIndicator() &
               Dataset == getCurrentDataset()) %>%
      arrange(Date) %>%
      group_by(Indicator, index_col) %>%
      arrange(Date, .by_group = TRUE)
    
    # Store original Units for each series before any transformations
    original_units <- filtered %>%
      select(Indicator, index_col, Units) %>%
      distinct()
    
    # Ensure Transform_Name column exists (initialize with empty string)
    filtered <- filtered %>%
      mutate(Transform_Name = "")
    
    # Check if seasonally adjusted data is requested
    if (input$useSeasonalAdjustment) {
      # Check if the selected data has pre-seasonally adjusted version available
      has_sa_data <- data %>%
        filter(Date >= startDate() & Date <= endDate() &
                 index_col %in% selectedNAICS() &
                 Indicator == getCurrentIndicator() &
                 Dataset == getCurrentDataset() &
                 seasonal_adjustment == TRUE) %>%
        nrow() > 0
      
      if (has_sa_data) {
        # Use the pre-seasonally adjusted data
        filtered <- filtered %>%
          filter(seasonal_adjustment == TRUE)
        
        # Add "Seasonally Adjusted" to the transform name
        filtered <- filtered %>%
          mutate(Transform_Name = if_else(Transform_Name == "", 
                                          "Seasonally Adjusted", 
                                          paste(Transform_Name, "Seasonally Adjusted", sep = ", ")))
      } else {
        # We need to apply X-13 seasonal adjustment
        # Keep only non-seasonally adjusted data for processing
        filtered <- filtered %>%
          filter(seasonal_adjustment == FALSE)
        
        # Store original data before applying seasonal adjustment
        original_filtered <- filtered
        
        # Create a flag to track if adjustment succeeds
        sa_success <- TRUE
        
        # Apply X-13-ARIMA-SEATS
        filtered <- filtered %>%
          group_modify(~{
            # Create a copy of the input data frame
            result_df <- .x %>% arrange(Date)
            
            # Convert to time series object
            ts_data <- ts(result_df$Value, 
                          start = c(year(min(result_df$Date)), month(min(result_df$Date))),
                          frequency = 12)
            
            # Apply X-13-ARIMA-SEATS
            tryCatch({
              seas_adj <- seas(ts_data, 
                               transform.function = "none",
                               regression.aictest = NULL,
                               outlier = NULL,
                               slidingspans = NULL,
                               history = NULL)
              
              # Extract seasonally adjusted series and update values
              result_df$Value <- as.numeric(final(seas_adj))
              result_df$Transform_Name <- if(result_df$Transform_Name[1] == "") {
                "X-13 Seasonally Adjusted"  # Use this label to indicate it's X-13 adjusted
              } else {
                paste(result_df$Transform_Name, "X-13 Seasonally Adjusted", sep = ", ")
              }
              
            }, error = function(e) {
              # Set the flag to indicate failure
              sa_success <<- FALSE
              warning(paste("Seasonal adjustment failed for", unique(.x$index_col), 
                            ". Returning original data."))
            })
            
            result_df
          })
        
        # If adjustment failed, revert to original data
        if (!sa_success) {
          filtered <- original_filtered
          # Auto-uncheck the seasonal adjustment checkbox
          updateCheckboxInput(session, "useSeasonalAdjustment", value = FALSE)
        }
      }
    } else {
      # If seasonal adjustment not requested, use non-seasonally adjusted data
      filtered <- filtered %>%
        filter(seasonal_adjustment == FALSE)
    }
    
    # Apply indexing if useIndexDate is TRUE
    if (input$useIndexDate) {
      index_date <- paste(month.name[as.integer(format(indexDate(), "%m"))], 
                          format(indexDate(), "%Y"))
      index_format <- format(indexDate(), "%m-%Y")
      filtered <- filtered %>%
        group_by(Indicator, index_col) %>%
        mutate(Value = Value / Value[Date == indexDate()][1] * 100,
               Transform_Name = paste0("Index, [", index_format, "] = 100"),
               Units = paste0("Index, ", index_format, " = 100")) %>%  # Better format for indexed values
        ungroup()
    }
    
    # Add Axis Type column using dropdown values
    filtered <- filtered %>%
      mutate(Axis_Type = ifelse(input$percentChangeTransform != "No Transform" || 
                                  input$cagrTransform != "No Transform", 
                                "Percentage", "Index"))
    
    # Apply other transforms in the specified order
    transforms <- transformName()
    for (transform in transforms) {
      if (grepl("MMA$", transform)) {
        # Extract the period from the transform name (e.g., "12MMA" -> 12)
        period <- as.numeric(gsub("([0-9]+)MMA$", "\\1", transform))
        filtered <- filtered %>%
          group_by(Indicator, index_col) %>%
          arrange(Date, .by_group = TRUE) %>%
          mutate(Value = zoo::rollmean(Value, k = period, 
                                       fill = NA, align = "right"),
                 Transform_Name = ifelse(Transform_Name == "",
                                         transform,
                                         paste(Transform_Name, transform, sep = ", ")))
      } else if (grepl("% Change$", transform)) {
        # Extract the period from the transform name (e.g., "12m % Change" -> 12)
        period <- as.numeric(gsub("([0-9]+)m % Change$", "\\1", transform))
        filtered <- filtered %>%
          group_by(Indicator, index_col) %>%
          arrange(Date, .by_group = TRUE) %>%
          mutate(Value = (Value - lag(Value, n = period)) / 
                   lag(Value, n = period),
                 Transform_Name = ifelse(Transform_Name == "",
                                         transform,
                                         paste(Transform_Name, transform, sep = ", ")),
                 Units = "Percent",  # Override units for percent change
                 Axis_Type = "Percentage") %>%
          ungroup()
      } else if (grepl("CAGR$", transform)) {
        # Extract the period from the transform name (e.g., "12m CAGR" -> 12)
        period <- as.numeric(gsub("([0-9]+)m CAGR$", "\\1", transform))
        filtered <- filtered %>%
          group_by(Indicator, index_col) %>%
          arrange(Date, .by_group = TRUE) %>%
          mutate(Value = ((Value / lag(Value, n = period)) ^ 
                            (1 / (period / 12)) - 1),
                 Transform_Name = ifelse(Transform_Name == "",
                                         transform,
                                         paste(Transform_Name, transform, sep = ", ")),
                 Units = "Percent",  # Override units for CAGR
                 Axis_Type = "Percentage") %>%
          ungroup()
      } else if (grepl("m Change$", transform) && !grepl("% Change$", transform)) {
        # Extract the period from the transform name (e.g., "12m Change" -> 12)
        period <- as.numeric(gsub("([0-9]+)m Change$", "\\1", transform))
        filtered <- filtered %>%
          group_by(Indicator, index_col) %>%
          arrange(Date, .by_group = TRUE) %>%
          mutate(Value = Value - lag(Value, n = period),
                 Transform_Name = ifelse(Transform_Name == "",
                                         transform,
                                         paste(Transform_Name, transform, sep = ", "))) %>%
          # Keep original units for Change (absolute difference maintains units)
          ungroup()
      }
    }
    
    # Final cleanup and ordering
    filtered <- filtered %>%
      tidyr::drop_na() %>%
      ungroup() %>%
      arrange(Date, Indicator, index_col)
    
    filtered
  })
  
  #  Render line chart for filtered data
  output$lineChart <- renderPlotly({
    # Initialize the plot with hover formatting
    p <- plot_ly(filteredData(), x = ~Date,
                 hovertemplate = paste(
                   "<b>%{text}</b><br>",
                   "<span style='color:%{line.color}'></span> Value: %{y:.2f} ", filteredData()$Units,  # Add units to hover
                   "<extra></extra>"
                 ),
                 text = ~index_col) %>%
      layout(
        hoverlabel = list(
          align = "left",
          bgcolor = "white",
          bordercolor = "darkgray"
        ),
        hovermode = "x unified"
      )
    
    # Get unique Units for y-axis title
    y_axis_title <- if (nrow(filteredData()) > 0) {
      unique_units <- unique(filteredData()$Units)
      if (length(unique_units) == 1) {
        unique_units[1]  # If only one unit type, use it directly
      } else if (length(unique_units) > 1) {
        paste(unique_units, collapse = ", ")  # Multiple units, join with commas
      } else {
        "Value"  # Fallback if no units found
      }
    } else {
      "Value"  # Default if no data
    }
    
    # Handle recession shading
    recession_shapes <- list()
    if (input$addRecessionShading) {
      # Get the date range of our displayed data
      data_start <- min(filteredData()$Date)
      data_end <- max(filteredData()$Date)
      
      # Filter recessions to only show those overlapping with our data
      visible_recessions <- recessions_df %>%
        filter(
          (start >= data_start & start <= data_end) |
            (end >= data_start & end <= data_end) |
            (start <= data_start & end >= data_end)
        ) %>%
        mutate(
          # Clip recession dates to our data range
          start = pmax(start, data_start),
          end = pmin(end, data_end)
        )
      
      # Create shapes for each visible recession period
      recession_shapes <- lapply(seq_len(nrow(visible_recessions)), function(i) {
        list(
          type = "rect",
          fillcolor = "rgb(200, 200, 200)",
          opacity = 0.5,
          line = list(width = 0),
          x0 = visible_recessions$start[i],
          x1 = visible_recessions$end[i],
          xref = "x",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          layer = "below"
        )
      })
    }
    
    # Add each series as a line trace
    for (series in unique(paste(filteredData()$Indicator, filteredData()$Transform_Name, filteredData()$index_col, sep = " - "))) {
      series_data <- filteredData() %>% 
        filter(paste(Indicator, Transform_Name, index_col, sep = " - ") == series)
      
      # Create a sentence format for the legend
      legend_text <- paste0(
        series_data$Indicator[1], " for NAICS ", series_data$NAICS_Code[1], ": ", 
        series_data$NAICS_Name[1], 
        ifelse(series_data$Transform_Name[1] != "", paste0(", ", series_data$Transform_Name[1]), "")
      )
      
      # Get units for this series
      series_units <- series_data$Units[1]
      
      p <- p %>% add_lines(
        y = ~Value,
        data = series_data,
        name = legend_text,  # Use new formatted legend text
        line = list(width = 1),
        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          "<span style='color:%{line.color}'></span> Value: %{y:.2f} ", series_units,
          "<extra></extra>"
        ),
        text = ~index_col,
        showlegend = TRUE
      )
    }
    
    # Apply appropriate tick formatting
    if (any(filteredData()$Axis_Type == "Percentage")) {
      p <- p %>% layout(yaxis = list(tickformat = ".1%"))
    } else {
      p <- p %>% layout(yaxis = list(tickformat = ","))
    }
    
    # Add final layout properties
    p %>% layout(
      shapes = recession_shapes,  # Place shapes first in layout
      title = list(text = "Data Finder", font = list(size = 24)),
      xaxis = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = "Date",
        tickfont = list(size = 12),
        dtick = paste0("M", input$xLabelFreq * 12),
        rangeslider = list(
          visible = TRUE,
          thickness = 0.1,
          bgcolor = "#F3F3F3",
          yaxis = list(
            rangemode = "auto",
            fixedrange = FALSE
          )
        )
      ),
      yaxis = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = y_axis_title,  # Using the dynamic units-based title
        tickfont = list(size = 12),
        fixedrange = FALSE,
        autorange = TRUE,
        rangemode = "auto"
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        yanchor = "top",
        y = -0.4,
        xanchor = "center",
        x = 0.5,
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = "rgba(0, 0, 0, 0.2)",
        borderwidth = 1,
        itemsizing = "constant"
      ),
      margin = list(
        t = 50,
        r = 50,
        b = 120,
        l = 50
      ),
      hoverlabel = list(
        align = "left",
        bgcolor = "white",
        bordercolor = "darkgray"
      ),
      hovermode = "x unified",
      plot_bgcolor = "rgba(255, 255, 255, 0.9)",
      paper_bgcolor = "rgba(255, 255, 255, 0.9)"
    )
  })  
  # Reactive expression for the chart title
  chartTitle <- reactive({
    if(input$finishedVisualizationTitle == "") {
      "Finished Visualization"
    } else {
      input$finishedVisualizationTitle
    }
  })
  
  # Modify the existing storedLineChart output
  # Render the plotly visualization for stored data with custom colors
  output$storedLineChart <- renderPlotly({
    # Initialize the base plot with proper hover label formatting
    p <- plot_ly(storedData(), x = ~Date) %>%
      layout(
        # Set up hover label formatting
        hoverlabel = list(
          align = "left",
          bgcolor = "white",
          bordercolor = "darkgray",
          font = list(family = "Arial", size = 12),
          namelength = -1
        ),
        hovermode = "x unified"
      )
    
    # Determine which series go on left vs right axis
    left_axis_series <- character(0)
    right_axis_series <- character(0)
    
    for (series in unique(storedData()$Combined)) {
      use_right_axis <- isTRUE(input[[paste0("rightaxis_", gsub("[^[:alnum:]]", "", series))]])
      if (use_right_axis) {
        right_axis_series <- c(right_axis_series, series)
      } else {
        left_axis_series <- c(left_axis_series, series)
      }
    }
    
    # Get unique units for each axis
    left_axis_units <- character(0)
    right_axis_units <- character(0)
    
    if (length(left_axis_series) > 0) {
      left_axis_units <- storedData() %>%
        filter(Combined %in% left_axis_series) %>%
        pull(Units) %>%
        unique()
    }
    
    if (length(right_axis_series) > 0) {
      right_axis_units <- storedData() %>%
        filter(Combined %in% right_axis_series) %>%
        pull(Units) %>%
        unique()
    }
    
    # Create axis titles from units
    left_axis_title <- if (length(left_axis_units) > 0) {
      paste(left_axis_units, collapse = ", ")
    } else {
      "Axis Title"
    }
    
    right_axis_title <- if (length(right_axis_units) > 0) {
      paste(right_axis_units, collapse = ", ")
    } else {
      "Axis Title"
    }
    
    # Initialize range variables
    left_range <- NULL
    right_range <- NULL
    
    # Calculate aligned zero ranges if both axes have data and align zeros is enabled
    if (input$alignZeros && length(right_axis_series) > 0 && length(left_axis_series) > 0) {
      # Get the value ranges for both axes
      left_data <- storedData() %>% filter(Combined %in% left_axis_series)
      right_data <- storedData() %>% filter(Combined %in% right_axis_series)
      
      # Calculate the ranges for each axis
      left_range <- if(nrow(left_data) > 0) {
        c(min(left_data$Value, na.rm = TRUE), max(left_data$Value, na.rm = TRUE))
      } else {
        c(0, 1)  # Default range if no data
      }
      
      right_range <- if(nrow(right_data) > 0) {
        c(min(right_data$Value, na.rm = TRUE), max(right_data$Value, na.rm = TRUE))
      } else {
        c(0, 1)  # Default range if no data
      }
      
      # Ensure ranges include zero for better alignment
      left_range[1] <- min(left_range[1], 0)
      right_range[1] <- min(right_range[1], 0)
      
      # Calculate how to align the zeros
      # We'll derive scale factors to ensure the zero points align
      left_span <- left_range[2] - left_range[1]
      right_span <- right_range[2] - right_range[1]
      
      # Calculate the zero position as a fraction of the range
      left_zero_pos <- if(left_range[1] < 0) abs(left_range[1]) / left_span else 0
      right_zero_pos <- if(right_range[1] < 0) abs(right_range[1]) / right_span else 0
      
      # Adjust ranges to align zeros
      if(left_zero_pos > 0 || right_zero_pos > 0) {
        # Use the larger zero position to adjust both scales
        target_zero_pos <- max(left_zero_pos, right_zero_pos)
        
        # Recalculate ranges to position zero at the same relative position
        if(left_zero_pos != target_zero_pos) {
          if(left_range[1] < 0) {
            # Negative values exist
            new_min <- -target_zero_pos * left_span / (1 - target_zero_pos)
            left_range <- c(new_min, left_range[2])
          } else {
            # Only positive values
            new_min <- -target_zero_pos * left_range[2] / (1 - target_zero_pos)
            left_range[1] <- new_min
          }
        }
        
        if(right_zero_pos != target_zero_pos) {
          if(right_range[1] < 0) {
            # Negative values exist
            new_min <- -target_zero_pos * right_span / (1 - target_zero_pos)
            right_range <- c(new_min, right_range[2])
          } else {
            # Only positive values
            new_min <- -target_zero_pos * right_range[2] / (1 - target_zero_pos)
            right_range[1] <- new_min
          }
        }
      }
    }
    
    # Configure right-side y-axis
    if (!is.null(right_range)) {
      p <- p %>% layout(
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = right_axis_title,
          showgrid = FALSE,
          range = right_range,  # Set explicit range if calculated
          fixedrange = FALSE,
          rangemode = "normal"
        )
      )
    } else {
      p <- p %>% layout(
        yaxis2 = list(
          overlaying = "y",
          side = "right",
          title = right_axis_title,
          showgrid = FALSE,
          autorange = TRUE,
          fixedrange = FALSE,
          rangemode = "auto"
        )
      )
    }
    
    # Handle recession shading if enabled
    recession_shapes <- list()
    if (input$addRecessionShading) {
      # Get the date range of displayed data
      data_start <- min(storedData()$Date)
      data_end <- max(storedData()$Date)
      
      # Filter recessions to only show those overlapping with our data
      visible_recessions <- recessions_df %>%
        filter(
          (start >= data_start & start <= data_end) |
            (end >= data_start & end <= data_end) |
            (start <= data_start & end >= data_end)
        ) %>%
        mutate(
          # Clip recession dates to our data range
          start = pmax(start, data_start),
          end = pmin(end, data_end)
        )
      
      # Create shapes for each visible recession period
      recession_shapes <- lapply(seq_len(nrow(visible_recessions)), function(i) {
        list(
          type = "rect",
          fillcolor = "rgb(200, 200, 200)",
          opacity = 0.5,
          line = list(width = 0),
          x0 = visible_recessions$start[i],
          x1 = visible_recessions$end[i],
          xref = "x",
          y0 = 0,
          y1 = 1,
          yref = "paper",
          layer = "below"
        )
      })
    }
    
    # Add each stored series with custom colors
    for (series in unique(storedData()$Combined)) {
      # Filter data for current series
      series_data <- storedData() %>% filter(Combined == series)
      
      # Get line width based on highlight status
      line_width <- if (isTRUE(input[[paste0("highlight_", gsub("[^[:alnum:]]", "", series))]])) 2 else 1
      
      # Check if series should use right axis
      use_right_axis <- isTRUE(input[[paste0("rightaxis_", gsub("[^[:alnum:]]", "", series))]])
      
      # Get the color from the color picker and ensure it exists
      color_input_id <- paste0("color_", gsub("[^[:alnum:]]", "", series))
      series_color <- if (!is.null(input[[color_input_id]])) {
        input[[color_input_id]]
      } else {
        # Fallback to a default color if the input doesn't exist yet
        "#000000"
      }
      
      # Get units for this series
      series_units <- series_data$Units[1]
      
      # Add the line trace for this series
      p <- p %>% add_trace(
        data = series_data,
        x = ~Date,
        y = ~Value,
        name = series,
        type = 'scatter',
        mode = 'lines',
        yaxis = if(use_right_axis) "y2" else "y",
        line = list(
          width = line_width,
          color = series_color
        ),
        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          "<span style='color:", series_color, "'></span> Value: %{y:.2f} ", series_units,  # Add units to hover
          "<extra></extra>"
        ),
        text = ~Combined,
        showlegend = TRUE
      )
    }
    
    # Determine tick formatting for each axis based on the series assigned to them
    left_percent <- FALSE
    right_percent <- FALSE
    
    if (length(left_axis_series) > 0) {
      left_percent <- any(storedData() %>% 
                            filter(Combined %in% left_axis_series) %>% 
                            pull(Axis_Type) == "Percentage")
    }
    
    if (length(right_axis_series) > 0) {
      right_percent <- any(storedData() %>% 
                             filter(Combined %in% right_axis_series) %>% 
                             pull(Axis_Type) == "Percentage")
    }
    
    # Apply appropriate tick formatting based on axis type
    p <- p %>% layout(
      yaxis = list(
        tickformat = if(left_percent) ".1%" else ",", 
        autorange = is.null(left_range),
        range = left_range,
        fixedrange = FALSE, 
        rangemode = if(is.null(left_range)) "auto" else "normal"
      ),
      yaxis2 = list(
        tickformat = if(right_percent) ".1%" else ",", 
        autorange = is.null(right_range),
        range = right_range,
        fixedrange = FALSE, 
        rangemode = if(is.null(right_range)) "auto" else "normal"
      )
    )
    
    # Add reference lines if enabled
    if (input$addHorizontalLine) {
      p <- p %>% add_segments(
        x = min(storedData()$Date),
        xend = max(storedData()$Date),
        y = input$horizontalLineValue,
        yend = input$horizontalLineValue,
        line = list(color = "black", width = 1),
        showlegend = FALSE,
        name = "Reference Line",
        hoverinfo = "none"
      )
    }
    
    if (input$addVerticalLine) {
      vertical_line_date <- as.Date(format(input$verticalLineDate, "%Y-%m-01"))
      p <- p %>% add_segments(
        x = vertical_line_date,
        xend = vertical_line_date,
        y = min(storedData()$Value),
        yend = max(storedData()$Value),
        line = list(color = "black", width = 1),
        showlegend = FALSE,
        name = "Reference Date",
        hoverinfo = "none"
      )
    }
    
    # Add final layout configuration
    p %>% layout(
      shapes = recession_shapes,  # Add recession shading if enabled
      title = list(text = chartTitle(), font = list(size = 24)),
      xaxis = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = "Date",
        tickfont = list(size = 12),
        dtick = paste0("M", input$storedXLabelFreq * 12),
        rangeslider = list(
          visible = TRUE,
          thickness = 0.1,
          bgcolor = "#F3F3F3",
          yaxis = list(
            rangemode = "auto",
            fixedrange = FALSE
          )
        )
      ),
      yaxis = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = left_axis_title,  # Dynamic left axis title based on units
        tickfont = list(size = 12),
        autorange = is.null(left_range),
        range = left_range,
        fixedrange = FALSE,
        rangemode = if(is.null(left_range)) "auto" else "normal"
      ),
      yaxis2 = list(
        showline = TRUE,
        linewidth = 1,
        linecolor = 'black',
        mirror = FALSE,
        title = right_axis_title,  # Dynamic right axis title based on units
        tickfont = list(size = 12),
        autorange = is.null(right_range),
        range = right_range,
        fixedrange = FALSE,
        rangemode = if(is.null(right_range)) "auto" else "normal"
      ),
      showlegend = TRUE,
      legend = list(
        orientation = "h",
        yanchor = "top",
        y = -0.45,
        xanchor = "center",
        x = 0.5,
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = "rgba(0, 0, 0, 0.2)",
        borderwidth = 1,
        itemsizing = "constant"
      ),
      margin = list(
        t = 50,
        r = 50,
        b = 120,
        l = 50
      )
    )
  })
  
  #  Add currently visualized data to stored data
  # Replace your existing addToStoredData observer with this:
  
  observeEvent(input$addToStoredData, {
    new_data <- filteredData() %>%
      # Create the new Combined field in sentence format
      mutate(Combined = paste0(
        Indicator, " for NAICS ", NAICS_Code, ": ", NAICS_Name, 
        ifelse(Transform_Name != "", paste0(", ", Transform_Name), "")
      ))
    
    # Check if any of the new series already exist in storedData
    existing_series <- unique(storedData()$Combined)
    new_series <- unique(new_data$Combined)
    
    # Only add series that don't already exist
    series_to_add <- new_data %>%
      filter(!(Combined %in% existing_series))
    
    if (nrow(series_to_add) > 0) {
      storedData(rbind(storedData(), series_to_add))
      
      # Expand the panel when data is added
      #      panelCollapsed(FALSE)
    }
  })  
  
  observe({
    # Get current series names
    current_series <- unique(storedData()$Combined)
    
    # Clean up any orphaned UI elements
    all_inputs <- names(input)
    highlight_inputs <- all_inputs[grep("^highlight_", all_inputs)]
    
    for (highlight_input in highlight_inputs) {
      series_name <- gsub("^highlight_", "", highlight_input)
      series_name <- gsub("[^[:alnum:]]", "", series_name)
      
      if (!any(grepl(series_name, gsub("[^[:alnum:]]", "", current_series)))) {
        removeUI(
          selector = paste0("#", highlight_input, "-label"),
          immediate = TRUE
        )
      }
    }
  })
  
  #  Clear stored data
  observeEvent(input$clearStoredData, {
    storedData(data.frame(Date = as.Date(character()),
                          NAICS_Code = character(),
                          NAICS_Name = character(),
                          Indicator = character(),
                          Value = numeric(),
                          index_col = character(),
                          Transform_Name = character(),
                          Combined = character(),
                          Axis_Type = character(),
                          Units = character()))
    
    # Collapse the panel when data is cleared
    panelCollapsed(TRUE)
  })
  
  #  Download handler for stored data
  # Update this block
  output$downloadStoredData <- downloadHandler(
    filename = function() {
      paste("final_visualization_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Ungroup the data first
      data_to_export <- storedData() %>%
        ungroup() %>%
        # Create a unique identifier for each series
        mutate(Series = paste(Indicator, Transform_Name, index_col, sep = " - ")) %>%
        # Select only the columns we need
        select(Date, Series, Value, Units)  # Add Units to the export
      
      # Pivot the data from long to wide format
      wide_data <- data_to_export %>%
        pivot_wider(names_from = Series, values_from = Value)
      
      # Add the horizontal line value if it's enabled
      if(input$addHorizontalLine == TRUE) {
        wide_data$`User Horizontal Line` <- input$horizontalLineValue
      }
      
      # Add the vertical line date if it's enabled
      if(input$addVerticalLine == TRUE) {
        vertical_line_date <- as.Date(format(input$verticalLineDate, "%Y-%m-01"))
        wide_data$`User Vertical Line` <- format(vertical_line_date, "%Y-%m-%d")
      }
      
      # Add a Units metadata row
      units_row <- data_to_export %>%
        distinct(Series, Units) %>%
        pivot_wider(names_from = Series, values_from = Units)
      
      # Combine data with units metadata
      metadata <- data.frame(Date = "Units", stringsAsFactors = FALSE)
      for (col in names(wide_data)[-1]) {  # Skip the Date column
        if (col %in% names(units_row)) {
          metadata[[col]] <- units_row[[col]][1]
        } else {
          metadata[[col]] <- NA
        }
      }
      
      combined_data <- rbind(metadata, wide_data)
      
      # Write the wide-format data to a CSV file
      write.csv(combined_data, file, row.names = FALSE)
    }
  )
  
  #  Render series manager UI
  # Render the UI for managing stored series, including color pickers
  # Update this block in the seriesManager renderUI function
  output$seriesManager <- renderUI({
    series_names <- unique(storedData()$Combined)
    
    # Define a colorblind-friendly default color palette
    default_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", 
                        "#FFFF33", "#A65628", "#F781BF", "#999999")
    
    tagList(
      tags$div(
        lapply(seq_along(series_names), function(i) {
          name <- series_names[i]
          # Create a sanitized ID for the series name
          sanitized_id <- gsub("[^[:alnum:]]", "", name)
          
          # Get the units for this series
          series_units <- storedData() %>%
            filter(Combined == name) %>%
            pull(Units) %>%
            unique() %>%
            .[1]
          
          # Calculate default color index, cycling through the palette if needed
          color_index <- ((i-1) %% length(default_colors)) + 1
          
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
            tags$div(style = "width: 25%; word-wrap: break-word;", 
                     tags$span(name),
                     tags$br(),
                     tags$small(style = "color: #666;", series_units)),  # Add units as smaller text
            tags$div(style = "width: 15%; text-align: center;",
                     checkboxInput(paste0("highlight_", sanitized_id), 
                                   "Bold", value = FALSE)
            ),
            tags$div(style = "width: 15%; text-align: center;",
                     checkboxInput(paste0("rightaxis_", sanitized_id), 
                                   "Right Axis", value = FALSE)
            ),
            tags$div(style = "width: 15%; text-align: center;",
                     colourInput(paste0("color_", sanitized_id),
                                 NULL,  # No label
                                 default_colors[color_index])
            ),
            actionButton(paste0("remove_", sanitized_id), "Remove",  
                         style = "width: 20%; font-size: 0.8em;")
          )
        })
      )
    )
  })
  
  #  Remove series from stored data
  observe({
    series_names <- unique(storedData()$Combined)
    lapply(series_names, function(name) {
      # Create the same sanitized ID
      sanitized_id <- gsub("[^[:alnum:]]", "", name)
      
      observeEvent(input[[paste0("remove_", sanitized_id)]], {
        # Create a new data frame excluding the removed series
        new_stored_data <- storedData() %>% 
          filter(Combined != name)
        
        # Update storedData with the new data frame
        storedData(new_stored_data)
        
        # Remove the associated input elements
        removeUI(
          selector = paste0("#highlight_", sanitized_id, "-label"),
          immediate = TRUE
        )
      }, ignoreInit = TRUE)
    })
  })
  
  # Function to get the current selected indicator based on active navigation tabs
  getCurrentIndicator <- reactive({
    # Get the current dataset
    dataset <- input$datasetNav
    
    # Return the appropriate indicator based on the selected dataset
    if (dataset == "International Trade") {
      return(input$internationalTradeNav)
    } else if (dataset == "Production and Capacity") {
      return(input$productionCapacityNav)
    } else if (dataset == "Price Indices") {
      return(input$priceIndicesNav)
    } else if (dataset == "Current Employment Statistics") {
      return(input$employmentStatsNav)
    } else if (dataset == "M3 Manufacturers Shipments, Inventories & Orders") {
      return(input$manufacturersNav)
    } else if (dataset == "Investment") {
      return(input$investmentNav)
    }
    
    # Default return (should not reach this point)
    return(NULL)
  })
  
  # Function to get the current selected dataset
  getCurrentDataset <- reactive({
    return(input$datasetNav)
  })
  
  # Initialize the selected tabs when the app starts
  observe({
    # This runs once when the app initializes
    # Set default selections for each navigation tab
    updateTabsetPanel(session, "datasetNav", selected = "International Trade")
    updateTabsetPanel(session, "internationalTradeNav", selected = "Imports")
    updateTabsetPanel(session, "productionCapacityNav", selected = "Industrial Production")
    updateTabsetPanel(session, "priceIndicesNav", selected = "Import Price Index")
    updateTabsetPanel(session, "employmentStatsNav", selected = "All Employees")
    updateTabsetPanel(session, "manufacturersNav", selected = "Finished Goods Inventories")
    updateTabsetPanel(session, "investmentNav", selected = "Structures Investment")
  })
  
  # Update your existing code to use getCurrentDataset() and getCurrentIndicator()
  # instead of getCurrentDataset() and getCurrentIndicator()
  
  # Example: Update your observers
  observeEvent(c(naicsConstraintCode(), getCurrentIndicator(), input$thematicGroupings), {
    updatePickerInput(session, "naicsIndex",
                      choices = unique(filteredNAICS()$index_col),
                      selected = unique(filteredNAICS()$index_col)[1])
  })
  
  # Update your filteredNAICS reactive
  filteredNAICS <- reactive({
    if (input$thematicGroupings != "No Constraint") {
      data %>%
        filter(NAICS_Code %in% thematicNAICS(),
               Indicator == getCurrentIndicator())
    } else if (naicsConstraintCode() != "") {
      data %>%
        filter(str_starts(NAICS_Code, naicsConstraintCode()),
               Indicator == getCurrentIndicator())
    } else {
      data %>%
        filter(Indicator == getCurrentIndicator())
    }
  })
  
  #  Text output
  output$indicatorDescription <- renderText({
    if (getCurrentIndicator() == "Industrial Production") {
      "The G.17 Industrial Production and Capacity Utilization report is a monthly publication by the Federal Reserve Board that produces an index designed to measure the real output of the manufacturing, mining, and electric and gas utilities industries in the United States. It is released monthly at https://www.federalreserve.gov/releases/g17/default.htm . This data is useful for understanding the level and composition of domestic industrial production in the United States. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (getCurrentIndicator() == "Producer Price Index") {
      "The Producer Price Index statistics are designed to measure changes in the prices paid for upstream inputs by domestic firms. They are produced monthly by the Bureau of Labor Statistics and can be found at: https://www.bls.gov/mxp/ . This data is useful for understanding changes in the price of input costs for domestic producers. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (getCurrentIndicator() == "Import Price Index") {
      "The Import Price Index statistics are designed to measure changes in the prices paid for imports by category of good. They are produced monthly by the Bureau of Labor Statistics and can be found at: https://www.bls.gov/mxp/ . This data is useful for understanding changes in the price of foreign-produced goods. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (getCurrentIndicator() == "Nominal Imports") {
      "This data is an index of the level of nominal imports, where measures of dollar values are produced by the Census Bureau. New releases can be found monthly at https://usatrade.census.gov/ . This data is useful for understanding changes in the total amount paid from month to month for imports of specific goods. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (getCurrentIndicator() == "IPI-Adjusted Imports") {
      "This data is compiled specifically for this viewer, using the Census Bureau's data on nominal imports and the Bureau of Labor Statistics data on Import Prices. The index of Nominal Imports is deflated by the index for Import Prices to create an inflation-adjusted index of imports. This index is more accurate than the index of PPI-Adjusted Imports, but at the cost of more limited data coverage. All data are reported as indices initially benchmarked to January 1 2020."
    } else if (getCurrentIndicator() == "PPI-Adjusted Imports") {
      "This data is compiled specifically for this viewer, using the Census Bureau's data on nominal imports and the Bureau of Labor Statistics data on US Domestic Producer Prices. The index of Nominal Imports is deflated by the index for domestic Producer Prices to create an inflation-adjusted index of imports. This index is less accurate than the index of IPI-Adjusted Imports, but has the benefit of broader data coverage. All data are reported as indices initially benchmarked to January 1 2020."
    } else {
      "No description available for the selected indicator."
    }
  })
  
  # Reset inputs to default values
  observeEvent(input$resetInputs, {
    # Update the date range input
    updateDateRangeInput(session, "dateRange", 
                         start = as.Date("1900-01-01"),
                         end = Sys.Date())
    
    # Reset filters
    updatePickerInput(session, "thematicGroupings", selected = "No Constraint")
    updatePickerInput(session, "naicsConstraint", selected = "")
    updatePickerInput(session, "naicsIndex", choices = unique(filteredNAICS()$index_col), 
                      selected = unique(filteredNAICS()$index_col)[1])
    updatePickerInput(session, "datasetDropdown", selected = unique(data$Dataset)[1])
    updatePickerInput(session, "indicatorDropdown", selected = unique(data$Indicator)[1])
    updateCheckboxInput(session, "showSubIndustries", value = FALSE)
    updateCheckboxInput(session, "showAllSeries", value = FALSE)
    
    # Reset transforms
    updateCheckboxInput(session, "useIndexDate", value = FALSE)
    updateSelectInput(session, "movingAverageTransform", selected = "No Transform")
    updateSelectInput(session, "changeTransform", selected = "No Transform")
    updateSelectInput(session, "percentChangeTransform", selected = "No Transform")
    updateSelectInput(session, "cagrTransform", selected = "No Transform")
    
    # Reset visualization settings
    updateNumericInput(session, "xLabelFreq", value = 3)
    updateNumericInput(session, "storedXLabelFreq", value = 3)
  })
}

#Run the application
shinyApp(ui = ui, server = server)