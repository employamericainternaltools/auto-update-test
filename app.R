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
/* Set the background color for the entire app */
body {
  background-color: #edeada;
}
/* Add black borders around main navbar tabs */
.main-navbar .nav-tabs > li > a {
  border: 1px solid #000000;
  margin-right: -1px; /* Prevent double borders between tabs */
  border-radius: 4px 4px 0 0; /* Round top corners only */
  margin-bottom: -1px; /* Connect with bottom border */
}

/* Ensure active tab in main navbar properly connects with content below */
.main-navbar .nav-tabs > li.active > a,
.main-navbar .nav-tabs > li.active > a:hover,
.main-navbar .nav-tabs > li.active > a:focus {
  border: 1px solid #000000;
  border-bottom-color: transparent; /* Hide bottom border for active tab */
}

/* Add black borders around sub navbar tabs */
.sub-navbar .nav-tabs > li > a {
  border: 1px solid #000000;
  border-radius: 4px;
  margin: 0 5px; /* Add spacing between tabs */
}

/* Style active tab in sub navbar */
.sub-navbar .nav-tabs > li.active > a,
.sub-navbar .nav-tabs > li.active > a:hover,
.sub-navbar .nav-tabs > li.active > a:focus {
  border: 1px solid #000000;
  border-bottom-width: 3px; /* Thicker bottom border for active tab */
}

/* Center the sub-navigation tabs */
.sub-navbar .nav-tabs {
  display: flex;
  justify-content: center; /* Center the tabs */
  width: 100%;
  padding: 0;
  border-bottom: none;
}

/* Update container backgrounds to match or complement the new background */
.container-fluid {
  background-color: #edeada;
  padding-left: 30px;
  padding-right: 30px;
}

/* Style for the smaller control boxes within panels */
.control-box {
  background-color: #f5f5f5 !important;  /* Light grey background */
  border: 1px solid #000000 !important;   /* Black border */
  margin: 5px;
  padding: 10px;
}

/* Make the large well panels blue with black borders */
.well {
  background-color: #f0f8ff !important;  /* Keeping the original light blue background */
  border: 1px solid #000000 !important;  /* Black border */
}

/* Override nested wellPanel backgrounds */
.well .well.control-box {
  background-color: #f5f5f5 !important;  /* Ensure control boxes stay light grey */
  border: 1px solid #000000 !important;  /* Black border */
}

/* NAICS description box styling */
.naics-description-box {
  background-color: #f5f5f5;
  border: 1px solid #000000; /* Black outline */
  border-radius: 4px;
  padding: 8px;
  margin-top: 5px;
  margin-bottom: 10px;
  font-size: 12px;
  max-height: 100px;
  overflow-y: auto;
}

/* Title panel styling */
.title-panel {
  padding: 15px 0;
  margin-bottom: 20px;
  background-color: #edeada;
}

.title-panel h2 {
  font-weight: 500;
  margin: 0;
  color: #333;
}

/* Container for sub-navigation */
#sub-navigation-container {
  margin-top: 0;
  background-color: #edeada;
}

/* Main navigation bar styling */
.main-navbar {
  background-color: #edeada;
  border-bottom: 1px solid #000000;
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
  background-color: #edeada;  /* Match the background color */
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
  background-color: #edeada;
  border-bottom: 1px solid #000000;
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
  border: 1px solid #000000;
  transition: background-color 0.3s;
}

.custom-button:hover {
  background-color: #e9ecef;
}

/* Sidebar panel styling */
.sidebar-panel {
  background-color: #f0f8ff;  /* Keeping the original light blue background */
  border: 1px solid #000000;
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
  border-top: 1px solid #000000;
}

.sidebar-btn {
  width: 100%;
  margin-bottom: 10px;
  white-space: normal;
}

/* Chart container styling */
.chart-container {
  background-color: white;
  border: 1px solid #000000;
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
  border: 1px solid #000000;
  border-radius: 4px;
  padding: 15px;
}

.manager-title {
  font-size: 16px;
  font-weight: bold;
  margin-bottom: 15px;
  color: #2c3e50;
  border-bottom: 1px solid #000000;
  padding-bottom: 10px;
}

/* Date range input styling for sidebar */
.date-range-full {
  width: 100%;
}

/* Combined panel styling */
.combined-panel {
  background-color: #f0f8ff;  /* Keeping the original light blue background */
  border: 1px solid #000000;
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
  border: 1px solid #000000;
  border-radius: 4px;
  padding: 15px;
  height: 100%;
}

.action-box h4 {
  font-size: 16px;
  margin-top: 0;
  margin-bottom: 15px;
  padding-bottom: 8px;
  border-bottom: 1px solid #000000;
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
  border: 1px solid #000000;
  border-radius: 4px;
  padding: 15px;
  height: 100%;
}

.data-box h4 {
  font-size: 16px;
  margin-top: 0;
  margin-bottom: 15px;
  padding-bottom: 8px;
  border-bottom: 1px solid #000000;
  color: #2c3e50;
}

/* New styles for the collapsible panel */
.panel-collapsible {
  border: 1px solid #000000;
  border-radius: 4px;
  margin-bottom: 15px;
  background-color: #f0f8ff;  /* Keeping the original light blue background */
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

/* Copy button styling */
.copy-btn {
  border: none;
  background: transparent;
  cursor: pointer;
  padding: 0;
  margin-left: 5px;
  color: #555;
  transition: color 0.3s;
}
.copy-btn:hover {
  color: #2470dc;
}
.chart-code-container {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;
}
.chart-code-text {
  flex-grow: 1;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
/* Flash animation for copy confirmation */
@keyframes flash {
  0% { background-color: transparent; }
  30% { background-color: rgba(36, 112, 220, 0.2); }
  100% { background-color: transparent; }
}
.flash {
  animation: flash 1s;
}
")

# UI Section
ui <- fluidPage(
  tags$head(
    tags$style(css),
    tags$style(HTML("
    .copy-btn {
      border: none;
      background: transparent;
      cursor: pointer;
      padding: 0;
      margin-left: 5px;
      color: #555;
      transition: color 0.3s;
    }
    .copy-btn:hover {
      color: #2470dc;
    }
    .chart-code-container {
      display: flex;
      align-items: center;
      justify-content: space-between;
      width: 100%;
    }
    .chart-code-text {
      flex-grow: 1;
      overflow: hidden;
      text-overflow: ellipsis;
      white-space: nowrap;
    }
    /* Flash animation for copy confirmation */
    @keyframes flash {
      0% { background-color: transparent; }
      30% { background-color: rgba(36, 112, 220, 0.2); }
      100% { background-color: transparent; }
    }
    .flash {
      animation: flash 1s;
    }
  ")),
    tags$script(HTML("
    $(document).ready(function() {
    $('#naicsIndex').on('show.bs.select', function () {
      Shiny.setInputValue('naicsIndex_open', Math.random(), {priority: 'event'});
    });
  });
  $(document).ready(function() {
      // Function to copy text to clipboard
      function copyToClipboard(text, buttonId) {
        // Create temporary element
        var tempInput = document.createElement('textarea');
        tempInput.value = text;
        document.body.appendChild(tempInput);
        
        // Select the text
        tempInput.select();
        tempInput.setSelectionRange(0, 99999); // For mobile devices
        
        // Copy the text
        document.execCommand('copy');
        
        // Remove temporary element
        document.body.removeChild(tempInput);
        
        // Flash animation on the container
        $('#' + buttonId).closest('.chart-code-container').addClass('flash');
        setTimeout(function() {
          $('#' + buttonId).closest('.chart-code-container').removeClass('flash');
        }, 1000);
      }
      
      // Add click event to copy button
      $(document).on('click', '#copyChartCodeBtn', function() {
        var chartCode = $('#globalParameterID').text();
        copyToClipboard(chartCode, 'copyChartCodeBtn');
      });
    });
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
  
  # Main Navigation Bar (Dataset Categories)
  div(class = "main-navbar",
      div(class = "container-fluid",
          tabsetPanel(id = "datasetNav", type = "tabs",
                      tabPanel("Trade", value = "International Trade"),
                      tabPanel("Production", value = "Production and Capacity"),
                      tabPanel("Prices", value = "Price Indices"),
                      tabPanel("Employment", value = "Current Employment Statistics"),
                      tabPanel("Manufacturing", 
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
                            tabPanel("Trade Balance", value = "Trade Balance"),  # Add this line
                            tabPanel("PPI-Adjusted Imports", value = "PPI-Deflated Imports"),
                            tabPanel("IPI-Adjusted Imports", value = "IPI-Deflated Imports"),
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
      # Sub Navigation for Current Employment Statistics
      conditionalPanel(
        condition = "input.datasetNav == 'Current Employment Statistics'",
        div(class = "sub-navbar",
            div(class = "container-fluid",
                tabsetPanel(id = "employmentStatsNav", type = "tabs",
                            tabPanel("All Employees", value = "All Employees"),
                            tabPanel("Average Weekly Hours of All Employees", 
                                     value = "Average Weekly Hours of All Employees"),
                            tabPanel("Average Hourly Earnings of All Employees", 
                                     value = "Average Hourly Earnings of All Employees"),
                            tabPanel("Production and Nonsupervisory Employees", 
                                     value = "Production and Nonsupervisory Employees"),
                            tabPanel("Average Weekly Hours of Production and Nonsupervisory Employees", 
                                     value = "Average Weekly Hours of Production and Nonsupervisory Employees"),
                            tabPanel("Average Hourly Earnings of Production and Nonsupervisory Employees", 
                                     value = "Average Hourly Earnings of Production and Nonsupervisory Employees"),
                            tabPanel("Women Employees", value = "Women Employees")
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
           # "Select Data" blue box
           div(class = "sidebar-panel",
               h4("Select Data", style = "margin-top: 0; margin-bottom: 10px; border-bottom: 1px solid #000000; padding-bottom: 5px;"),
               
               # Date Range - Full Width without label
               div(class = "sidebar-row",
                   div(class = "sidebar-col-12",
                       dateRangeInput("dateRange", 
                                      label = NULL, # Removed the label
                                      start = as.Date("1900-01-01"),
                                      end = Sys.Date(),
                                      min = "1900-01-01",
                                      max = Sys.Date(),
                                      format = "mm/dd/yyyy",
                                      separator = " → ")
                   )
               ),
               
               # NAICS constraint (sector) - Full width
               div(class = "sidebar-row",
                   div(class = "sidebar-col-12",
                       pickerInput("naicsConstraint", "Filter by Sector",
                                   choices = c("No Constraint", unique(data$index_col[grepl("^\\d{3}\\b", data$NAICS_Code)])),
                                   options = list(`live-search` = TRUE))
                   )
               ),
               
               # Industry selection
               div(class = "sidebar-row",
                   div(class = "sidebar-col-12",
                       pickerInput("naicsIndex", "Industry",
                                   choices = NULL,
                                   selected = NULL,
                                   options = list(`live-search` = TRUE))
                   )
               ),
               
               # Checkboxes on the same row
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       checkboxInput("showSubIndustries", "Show Components", value = FALSE)
                   ),
                   div(class = "sidebar-col-6",
                       checkboxInput("useSeasonalAdjustment", "Seasonally Adjusted", value = FALSE)
                   )
               ),
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       checkboxInput("showAllSeries", "Show All Series", value = FALSE)
                   ),
                   div(class = "sidebar-col-6",
                       # Empty div to balance the layout
                   )
               )
           ),
           
           # "Transform Data" blue box
           div(class = "sidebar-panel", style = "margin-top: 15px;",
               h4("Transform Data", style = "margin-top: 0; margin-bottom: 10px; border-bottom: 1px solid #000000; padding-bottom: 5px;"),
               
               # Index to date row
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       checkboxInput("useIndexDate", "Index to Date", value = FALSE)
                   ),
                   div(class = "sidebar-col-6",
                       dateInput("indexDate", NULL, # Removed the label
                                 value = as.Date("2020-01-01"),
                                 min = "1900-01-01",
                                 max = Sys.Date(),
                                 format = "yyyy-mm",
                                 startview = "year")
                   )
               ),
               
               # Transformations - First row
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       selectInput("movingAverageTransform", "Moving Average", 
                                   choices = c("No Transform", "3 Months", "6 Months", "12 Months", "18 Months", "36 Months"),
                                   selected = "No Transform")
                   ),
                   div(class = "sidebar-col-6",
                       selectInput("changeTransform", "Change", 
                                   choices = c("No Transform", "1 Month", "3 Months", "6 Months", "12 Months", "18 Months", "36 Months"),
                                   selected = "No Transform")
                   )
               ),
               
               # Transformations - Second row
               div(class = "sidebar-row",
                   div(class = "sidebar-col-6",
                       selectInput("percentChangeTransform", "Percent Change", 
                                   choices = c("No Transform", "1 Month", "3 Months", "6 Months", "12 Months", "18 Months", "36 Months"),
                                   selected = "No Transform")
                   ),
                   div(class = "sidebar-col-6",
                       selectInput("cagrTransform", "CAGR", 
                                   choices = c("No Transform", "1 Month", "3 Months", "6 Months", "12 Months", "18 Months", "36 Months"),
                                   selected = "No Transform")
                   )
               )
           ),
           
           # "Manage Data" blue box
           div(class = "sidebar-panel", style = "margin-top: 15px;",
               h4("Manage Data", style = "margin-top: 0; margin-bottom: 10px; border-bottom: 1px solid #000000; padding-bottom: 5px;"),
               
               # Chart Code section
               div(class = "sidebar-row",
                   div(class = "sidebar-col-12",
                       div(id = "globalParameterIDDisplay",
                           h5("Chart Code", style = "margin-top: 0; margin-bottom: 10px;"),
                           # Container for text output and copy button
                           div(class = "chart-code-container",
                               div(class = "chart-code-text", textOutput("globalParameterID")),
                               # Copy button with icon
                               tags$button(
                                 id = "copyChartCodeBtn",
                                 class = "copy-btn",
                                 title = "Copy to clipboard",
                                 icon("copy")
                               )
                           ),
                           textInput("globalParameterIdInput", "", ""),
                           actionButton("applyGlobalParameterId", "Apply Chart Code", 
                                        class = "sidebar-btn custom-button")
                       )
                   )
               ),
               
               # Divider
               hr(style = "border-top: 1px solid #000000; margin: 10px 0;"),
               
               # Action buttons
               div(class = "sidebar-row",
                   div(class = "sidebar-col-12",
                       actionButton("addToStoredData", "Add to Multi-Series Viewer",
                                    class = "sidebar-btn custom-button"),
                       actionButton("resetInputs", "Reset Inputs",
                                    class = "sidebar-btn custom-button"),
                       downloadButton("downloadCurrentData", "Download Current Data", class = "sidebar-btn")
                   )
               )
           )
    ),
    
    
    # Right content area (75% width) for visualization
    column(width = 9,
           # First chart - Data Finder
           div(class = "chart-container",
               plotlyOutput("lineChart", height = "670px")
           ),
           
           # Add the new information box here
           div(class = "naics-description-box", style = "max-height: 315px; margin-top: 15px; margin-bottom: 20px;",
               htmlOutput("dataInformationBox")
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
                                              textInput("finishedVisualizationTitle", "Title:", value = "Multi-Series Viewer")
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

  # Replace the simple dataNavigatorTitle reactive with this more comprehensive one
dataNavigatorTitle <- reactive({
  # Get the currently selected industry
  selectedIndustry <- input$naicsIndex
  
  # Get the current indicator
  currentIndicator <- getCurrentIndicator()
  
  # Extract NAICS code and name
  naicsCode <- gsub("^(\\d+).*", "\\1", selectedIndustry)
  naicsName <- sub("^\\d+ - ", "", selectedIndustry)
  
  # Determine if seasonally adjusted
  seasonalText <- if (!input$useSeasonalAdjustment) ", Not Seasonally Adjusted" else ""
  
  # Get transform names
  transformText <- ""
  transforms <- transformName()
  if (length(transforms) > 0 && transforms != "") {
    transformText <- paste0(", ", paste(transforms, collapse = ", "))
  }
  
  # Create title in the same format as the legend
  unwrapped_title <- paste0(
    currentIndicator, " for NAICS ", naicsCode, ": ", naicsName,
    transformText, seasonalText
  )
  
  # Wrap the title with a maximum width of 80 characters
  # Function to wrap text at word boundaries
  wrap_title <- function(text, width = 80) {
    # Split the text into words
    words <- strsplit(text, " ")[[1]]
    lines <- ""
    current_line <- ""
    
    for (word in words) {
      # If adding this word would exceed the width, start a new line
      if (nchar(current_line) + nchar(word) + 1 > width && nchar(current_line) > 0) {
        lines <- paste0(lines, current_line, "\n")
        current_line <- word
      } else {
        # Add the word to the current line
        if (current_line == "") {
          current_line <- word
        } else {
          current_line <- paste(current_line, word)
        }
      }
    }
    
    # Add the last line
    if (current_line != "") {
      lines <- paste0(lines, current_line)
    }
    
    return(lines)
  }
  
  # Apply the wrapping
  wrapped_title <- wrap_title(unwrapped_title, width = 80)
  
  return(wrapped_title)
})
  
  # First, add these lines near the top of your server function where you load data
  # Load additional description files
  dataset_descriptions <- reactive({
    read.csv("dataset.csv", stringsAsFactors = FALSE)
  })
  
  indicator_descriptions <- reactive({
    read.csv("indicators.csv", stringsAsFactors = FALSE)
  })
  
  citation_info <- reactive({
    read.csv("citations.csv", stringsAsFactors = FALSE)
  })
  
  # Add this to your server function
  output$dataInformationBox <- renderUI({
    # Get current selections
    current_dataset <- getCurrentDataset()
    current_indicator <- getCurrentIndicator()
    current_naics <- input$naicsIndex
    
    # Get the most recent data for the selected series
    recent_data <- filteredData() %>%
      filter(index_col == current_naics) %>%
      arrange(desc(Date)) %>%
      head(13)  # Get top 13 rows to ensure we have current, prior, and year-ago data
    
    # Function to format values based on units
    format_value <- function(value, units) {
      if (is.na(value) || value == "N/A") return("N/A")
      
      # Format based on units
      if (grepl("Percent", units, ignore.case = TRUE)) {
        return(paste0(format(round(value * 100, 1), nsmall = 1), "%"))
      } else if (grepl("Dollar", units, ignore.case = TRUE)) {
        return(paste0("$", format(round(value, 2), big.mark = ",", nsmall = 2)))
      } else if (grepl("Index", units, ignore.case = TRUE)) {
        # Extract index date if present (format: "Index, MM-YYYY = 100")
        index_date <- gsub(".*Index, ([0-9]{2}-[0-9]{4}) = 100.*", "\\1", units)
        if (index_date != units) {  # If we successfully extracted a date
          return(paste0(format(round(value, 1), nsmall = 1), " (", index_date, " = 100)"))
        } else {
          return(format(round(value, 1), nsmall = 1))
        }
      } else {
        # For other units, just round to 2 decimal places
        return(format(round(value, 2), big.mark = ",", nsmall = 2))
      }
    }
    
    # Extract values for the three time periods
    if (nrow(recent_data) >= 1) {
      most_recent_date <- format(recent_data$Date[1], "%b %Y")
      most_recent_value <- format_value(recent_data$Value[1], recent_data$Units[1])
    } else {
      most_recent_date <- "N/A"
      most_recent_value <- "N/A"
    }
    
    if (nrow(recent_data) >= 2) {
      prior_month_date <- format(recent_data$Date[2], "%b %Y")
      prior_month_value <- format_value(recent_data$Value[2], recent_data$Units[2])
    } else {
      prior_month_date <- "N/A"
      prior_month_value <- "N/A"
    }
    
    # Find the data from approximately one year ago (12 months back, or the closest we have)
    year_ago_index <- which(abs(as.numeric(difftime(recent_data$Date, recent_data$Date[1], units = "days"))) 
                            >= 360 & abs(as.numeric(difftime(recent_data$Date, recent_data$Date[1], units = "days"))) 
                            <= 370)
    
    if (length(year_ago_index) > 0) {
      year_ago_date <- format(recent_data$Date[year_ago_index[1]], "%b %Y")
      year_ago_value <- format_value(recent_data$Value[year_ago_index[1]], recent_data$Units[year_ago_index[1]])
    } else if (nrow(recent_data) >= 12) {
      year_ago_date <- format(recent_data$Date[12], "%b %Y")
      year_ago_value <- format_value(recent_data$Value[12], recent_data$Units[12])
    } else {
      year_ago_date <- "N/A"
      year_ago_value <- "N/A"
    }
    
    # Look up descriptions
    dataset_desc <- dataset_descriptions() %>%
      filter(Name == current_dataset) %>%
      pull(Description)
    
    indicator_desc <- indicator_descriptions() %>%
      filter(Name == current_indicator) %>%
      pull(Description)
    
    citation <- citation_info() %>%
      filter(Name == current_dataset) %>%
      pull(Description)
    
    # Get the existing NAICS description
    naics_code <- gsub("^(\\d+).*", "\\1", current_naics)
    naics_desc <- naics_descriptions() %>%
      filter(`NAICS.Code` == naics_code) %>%
      pull(Description)
    
    # Handle cases where descriptions are not found
    if(length(dataset_desc) == 0) dataset_desc <- "No description available."
    if(length(indicator_desc) == 0) indicator_desc <- "No description available."
    if(length(citation) == 0) citation <- "No citation information available."
    if(length(naics_desc) == 0) naics_desc <- "No description available for this NAICS code."
    
    # Create HTML output
    tagList(
      tags$p(
        tags$strong("Recent Readings:")
      ),
      # Combined Recent Readings section (all on one line)
      tags$p(
        tags$strong(most_recent_date, ": "), most_recent_value, ", ",
        tags$strong(prior_month_date, ": "), prior_month_value, ", ",
        tags$strong(year_ago_date, ": "), year_ago_value
      ),
      
      # Original content below
      tags$p(
        tags$strong("Dataset: "), current_dataset
      ),
      tags$p(dataset_desc),
      tags$p(
        tags$strong("Indicator: "), current_indicator
      ),
      tags$p(indicator_desc),
      tags$p(
        tags$strong("Industry: "), current_naics
      ),
      tags$p(paste0("NAICS ", naics_code, ": ", naics_desc)),
      tags$p(
        tags$strong("Suggested Citation:")
      ),
      tags$p(citation)
    )
  })
  
  # Add a click observer for the NAICS dropdown
  observeEvent(input$naicsIndex_open, {
    # Skip if we're currently applying a chart code
    if(applyingChartCode()) {
      return()
    }
    
    # Get current filtered NAICS choices
    new_choices <- unique(filteredNAICS()$index_col)
    
    # Keep current selection if it exists in new choices
    current_selection <- input$naicsIndex
    selected_value <- if(!is.null(current_selection) && current_selection %in% new_choices) {
      current_selection
    } else {
      new_choices[1]
    }
    
    # Update dropdown with new options
    updatePickerInput(session, "naicsIndex",
                      choices = new_choices,
                      selected = selected_value)
  }, ignoreInit = TRUE)
  
  datasetCode <- reactiveVal(NA)
  indicatorCode <- reactiveVal(NA)
  movingAverageCode <- reactiveVal(NA)
  changeCode <- reactiveVal(NA)
  percentChangeCode <- reactiveVal(NA)
  cagrCode <- reactiveVal(NA)
  subIndustriesCode <- reactiveVal(NA)
  seasonalAdjustmentCode <- reactiveVal(NA)
  indexToDateCode <- reactiveVal(NA)
  indexDateCode <- reactiveVal(NA)
  naicsCode <- reactiveVal(NA)
  
  # First, let's add numeric IDs for datasets and indicators
  
  # In the server function, define numeric IDs for datasets
  datasetIDs <- reactiveVal(list(
    "International Trade" = 1,
    "Production and Capacity" = 2,
    "Price Indices" = 3,
    "Current Employment Statistics" = 4,
    "M3 Manufacturers Shipments, Inventories & Orders" = 5,
    "Investment" = 6
  ))
  
  # Define a reactive function to map dataset IDs to indicator IDs
  indicatorIDsByDataset <- reactive({
    list(
      # Dataset 1: International Trade
      "1" = list(
        "Imports" = 1,
        "Exports" = 2,
        "Trade Balance" = 3,  # Add this line
        "PPI-Deflated Imports" = 4,  # Updated from 3
        "IPI-Deflated Imports" = 5,  # Updated from 4
        "PPI-Adjusted Exports" = 6,  # Updated from 5
        "EPI-Adjusted Exports" = 7   # Updated from 6
      ),
      # Dataset 2: Production and Capacity
      "2" = list(
        "Industrial Production" = 1,
        "Capacity Utilization" = 2,
        "Capacity" = 3,
        "Relative Importance Weights" = 4
      ),
      # Dataset 3: Price Indices
      "3" = list(
        "Import Price Index" = 1,
        "Export Price Index" = 2,
        "Producer Price Index" = 3
      ),
      # Dataset 4: Current Employment Statistics
      "4" = list(
        "All Employees" = 1,
        "Average Weekly Hours of All Employees" = 2,
        "Average Hourly Earnings of All Employees" = 3,
        "Production and Nonsupervisory Employees" = 4,
        "Average Weekly Hours of Production and Nonsupervisory Employees" = 5,
        "Average Hourly Earnings of Production and Nonsupervisory Employees" = 6,
        "Women Employees" = 7
      ),
      # Dataset 5: M3 Manufacturers
      "5" = list(
        "Finished Goods Inventories" = 1,
        "Inventories to Shipments Ratio" = 2,
        "Material and Supply Inventories" = 3,
        "Total Inventories" = 4,
        "Unfilled Orders" = 5,
        "Unfilled Orders to Shipments Ratios" = 6,
        "Value of Shipments" = 7,
        "Work in Process Inventories" = 8,
        "New Orders" = 9
      ),
      # Dataset 6: Investment
      "6" = list(
        "Structures Investment" = 1,
        "Real Structures Investment" = 2,
        "Equipment Investment" = 3,
        "Real Equipment Investment" = 4
      )
    )
  })
  
  # Add these to your server function, with the other ID outputs
  # Output the current Dataset ID
  output$datasetID <- renderText({
    # Get the current selected dataset
    selected <- input$datasetNav
    # Look up its numeric ID
    id <- datasetIDs()[[selected]]
    # Return as string
    as.character(id)
  })
  
  # Add these to your server, with the other input observers
  # Update the dataset dropdown when the numeric input changes
  observeEvent(datasetCode(), {
    # Skip if the value is NA or NULL
    req(datasetCode())
    
    # Get the numeric ID input by the user
    user_id <- datasetCode()
    
    # Get the list of IDs
    ids <- datasetIDs()
    
    # Find the dataset name corresponding to this ID
    dataset_name <- names(ids)[which(unlist(ids) == user_id)]
    
    # If a valid dataset was found, update the dropdown
    if (length(dataset_name) > 0) {
      updateTabsetPanel(session, "datasetNav", selected = dataset_name)
      
      # Clear the input box after making the selection (with a slight delay)
      invalidateLater(300)
      isolate({
        datasetCode(NA)
      })
    }
  })
  
  # Update the indicator dropdown when the numeric input changes
  observeEvent(indicatorCode(), {
    # Skip if the value is NA or NULL
    req(indicatorCode())
    
    # Get the numeric ID input by the user
    user_id <- indicatorCode()
    
    # Get the current dataset and its ID
    selected_dataset <- input$datasetNav
    dataset_id <- datasetIDs()[[selected_dataset]]
    
    # Get the indicator IDs for this dataset
    indicator_ids_list <- indicatorIDsByDataset()[[as.character(dataset_id)]]
    
    # Find the indicator name corresponding to this ID
    indicator_name <- names(indicator_ids_list)[which(unlist(indicator_ids_list) == user_id)]
    
    # If a valid indicator was found, update the appropriate tab panel
    if (length(indicator_name) > 0) {
      # Determine which tabset panel to update based on the dataset
      if (selected_dataset == "International Trade") {
        updateTabsetPanel(session, "internationalTradeNav", selected = indicator_name)
      } else if (selected_dataset == "Production and Capacity") {
        updateTabsetPanel(session, "productionCapacityNav", selected = indicator_name)
      } else if (selected_dataset == "Price Indices") {
        updateTabsetPanel(session, "priceIndicesNav", selected = indicator_name)
      } else if (selected_dataset == "Current Employment Statistics") {
        updateTabsetPanel(session, "employmentStatsNav", selected = indicator_name)
      } else if (selected_dataset == "M3 Manufacturers Shipments, Inventories & Orders") {
        updateTabsetPanel(session, "manufacturersNav", selected = indicator_name)
      } else if (selected_dataset == "Investment") {
        updateTabsetPanel(session, "investmentNav", selected = indicator_name)
      }
      
      # Clear the input box after making the selection
      invalidateLater(300)
      isolate({
        indicatorCode(NA)
      })
    }
  })
  
  # Output the current Indicator ID
  output$indicatorID <- renderText({
    # Get the current selected dataset and indicator
    selected_dataset <- input$datasetNav
    selected_indicator <- getCurrentIndicator()
    
    # Get the dataset ID
    dataset_id <- datasetIDs()[[selected_dataset]]
    
    # Get the indicator IDs for this dataset
    indicator_ids_list <- indicatorIDsByDataset()[[as.character(dataset_id)]]
    
    # Look up the indicator's numeric ID
    id <- indicator_ids_list[[selected_indicator]]
    
    # Return as string
    as.character(id)
  })
  
  # Add these reactive values at the top of your server function
  chartCodeStep <- reactiveVal(0)  # Controls which step we're on
  chartCodeParams <- reactiveVal(NULL)  # Stores the parsed parameters
  
  # Add a debug logger
  logStep <- function(step_name) {
    message(paste0("CHART CODE STEP: ", step_name))
  }
  
  # Replace your Apply Global ID observer with this step-based approach
  observeEvent(input$applyGlobalParameterId, {
    # Reset step counter to start the process
    chartCodeStep(1)
    logStep("1 - Starting Chart Code application process")
    
    # Get the input string
    global_id <- input$globalParameterIdInput
    
    # Check if the string is of reasonable length
    if (nchar(global_id) < 10) {
      showNotification("Chart Code appears too short", type = "error")
      chartCodeStep(0)  # Reset step counter on error
      return()
    }
    
    # Set flag to prevent NAICS dropdown updates during code application
    applyingChartCode(TRUE)
    
    # Parse the string into components
    params <- list(
      dataset_id = as.numeric(substr(global_id, 1, 1)),
      indicator_id = as.numeric(substr(global_id, 2, 2)),
      ma_id = as.numeric(substr(global_id, 3, 3)),
      change_id = as.numeric(substr(global_id, 4, 4)),
      percent_change_id = as.numeric(substr(global_id, 5, 5)),
      cagr_id = as.numeric(substr(global_id, 6, 6)),
      sub_industries = as.logical(as.numeric(substr(global_id, 7, 7))),
      seasonal_adjustment = as.logical(as.numeric(substr(global_id, 8, 8))),
      index_to_date = as.logical(as.numeric(substr(global_id, 9, 9))),
      index_date = substr(global_id, 10, 15),
      naics_code = substr(global_id, 16, 21)
    )
    
    # Store parsed parameters for later steps
    chartCodeParams(params)
    
    # Step 1: Reset constraints
    updatePickerInput(session, "naicsConstraint", selected = "No Constraint")
    
    # Move to Step 2
    chartCodeStep(2)
  })
  
  # At the top of your server function, add these new reactive values
  chartCodeSubStep <- reactiveVal(0)  # For tracking sub-steps within a step
  naicsOptionsReady <- reactiveVal(FALSE)  # Flag to indicate when NAICS options are ready
  
  # Replace your step 2 observer with this
  observeEvent(chartCodeStep(), {
    if (chartCodeStep() == 2) {
      logStep("2 - Applying dataset, indicator and transformation parameters")
      
      # Reset sub-step counter and ready flag
      chartCodeSubStep(1)
      naicsOptionsReady(FALSE)
      
      # Get stored parameters
      params <- chartCodeParams()
      if (is.null(params)) {
        chartCodeStep(0)  # Reset on error
        applyingChartCode(FALSE)
        return()
      }
      
      # 2.1: Apply dataset parameter
      ids_list <- datasetIDs()
      dataset_name <- names(ids_list)[which(unlist(ids_list) == params$dataset_id)]
      
      if (length(dataset_name) > 0) {
        updateTabsetPanel(session, "datasetNav", selected = dataset_name)
      } else {
        message("Invalid dataset_id: ", params$dataset_id)
      }
      
      # 2.2: Apply indicator parameter (based on dataset)
      indicator_ids_list <- indicatorIDsByDataset()[[as.character(params$dataset_id)]]
      if (!is.null(indicator_ids_list)) {
        indicator_name <- names(indicator_ids_list)[which(unlist(indicator_ids_list) == params$indicator_id)]
        
        if (length(indicator_name) > 0) {
          # Select appropriate tab panel based on dataset
          if (dataset_name == "International Trade") {
            updateTabsetPanel(session, "internationalTradeNav", selected = indicator_name)
          } else if (dataset_name == "Production and Capacity") {
            updateTabsetPanel(session, "productionCapacityNav", selected = indicator_name)
          } else if (dataset_name == "Price Indices") {
            updateTabsetPanel(session, "priceIndicesNav", selected = indicator_name)
          } else if (dataset_name == "Current Employment Statistics") {
            updateTabsetPanel(session, "employmentStatsNav", selected = indicator_name)
          } else if (dataset_name == "M3 Manufacturers Shipments, Inventories & Orders") {
            updateTabsetPanel(session, "manufacturersNav", selected = indicator_name)
          } else if (dataset_name == "Investment") {
            updateTabsetPanel(session, "investmentNav", selected = indicator_name)
          }
        } else {
          message("Invalid indicator_id: ", params$indicator_id)
        }
      }
      
      # 2.3: Apply transformation parameters
      # Moving Average
      ma_choices <- names(movingAverageIDs())
      if (params$ma_id >= 0 && params$ma_id < length(ma_choices)) {
        updateSelectInput(session, "movingAverageTransform", selected = ma_choices[params$ma_id + 1])
      }
      
      # Change
      change_choices <- names(changeIDs())
      if (params$change_id >= 0 && params$change_id < length(change_choices)) {
        updateSelectInput(session, "changeTransform", selected = change_choices[params$change_id + 1])
      }
      
      # Percent Change
      pct_change_choices <- names(percentChangeIDs())
      if (params$percent_change_id >= 0 && params$percent_change_id < length(pct_change_choices)) {
        updateSelectInput(session, "percentChangeTransform", selected = pct_change_choices[params$percent_change_id + 1])
      }
      
      # CAGR
      cagr_choices <- names(cagrIDs())
      if (params$cagr_id >= 0 && params$cagr_id < length(cagr_choices)) {
        updateSelectInput(session, "cagrTransform", selected = cagr_choices[params$cagr_id + 1])
      }
      
      # Checkboxes
      updateCheckboxInput(session, "showSubIndustries", value = params$sub_industries)
      updateCheckboxInput(session, "useSeasonalAdjustment", value = params$seasonal_adjustment)
      updateCheckboxInput(session, "useIndexDate", value = params$index_to_date)
      
      # Index Date
      if (nchar(params$index_date) == 6) {
        year <- as.numeric(substr(params$index_date, 1, 4))
        month <- as.numeric(substr(params$index_date, 5, 6))
        
        if (!is.na(year) && !is.na(month) && year >= 1900 && year <= 2100 && month >= 1 && month <= 12) {
          date_str <- sprintf("%04d-%02d-01", year, month)
          updateDateInput(session, "indexDate", value = as.Date(date_str))
        }
      }
      
      # Move to the next sub-step to check if inputs have changed
      message("Step 2 complete, moving to sub-step monitoring")
      chartCodeSubStep(2)
    }
  })
  
  # Add a NEW observer that monitors input changes after step 2
  observeEvent(chartCodeSubStep(), {
    if (chartCodeSubStep() == 2) {
      message("Sub-step 2: Monitoring input changes...")
      
      # Get current dataset and indicator to watch for changes
      params <- chartCodeParams()
      if (is.null(params)) return()
      
      # Create reactive expressions that will be triggered when dataset/indicator changes
      dataset_expr <- reactive({
        input$datasetNav
      })
      
      indicator_expr <- reactive({
        getCurrentIndicator()
      })
      
      # Add observer to monitor when these inputs match what we expect
      observe({
        current_dataset <- dataset_expr()
        current_indicator <- indicator_expr()
        
        # Get the expected values from our params
        expected_dataset <- names(datasetIDs())[which(unlist(datasetIDs()) == params$dataset_id)]
        indicator_ids_list <- indicatorIDsByDataset()[[as.character(params$dataset_id)]]
        expected_indicator <- names(indicator_ids_list)[which(unlist(indicator_ids_list) == params$indicator_id)]
        
        # Check if we've reached the expected state
        if (length(expected_dataset) > 0 && length(expected_indicator) > 0) {
          if (current_dataset == expected_dataset && current_indicator == expected_indicator) {
            message("Dataset and indicator match expected values:")
            message("  Dataset: ", current_dataset)
            message("  Indicator: ", current_indicator)
            
            # IMPORTANT: Let's check the NAICS options now
            options_count <- length(unique(filteredNAICS()$index_col))
            message("  NAICS options count: ", options_count)
            
            # If we've got a reasonable number of options, we're ready to move on
            if (options_count > 0) {
              message("NAICS options are available, marking as ready")
              naicsOptionsReady(TRUE)
              chartCodeSubStep(3)  # Move to next sub-step
            } else {
              message("No NAICS options available yet, waiting...")
            }
          } else {
            message("Still waiting for dataset and indicator to update:")
            message("  Current dataset: ", current_dataset, " (expecting ", expected_dataset, ")")
            message("  Current indicator: ", current_indicator, " (expecting ", expected_indicator, ")")
          }
        }
      })
    }
    
    # Once we're ready, update the NAICS dropdown
    if (chartCodeSubStep() == 3 && naicsOptionsReady()) {
      message("Sub-step 3: NAICS options are ready, updating dropdown")
      
      # Get the current filtered NAICS options
      current_filtered_naics <- unique(filteredNAICS()$index_col)
      message("Found ", length(current_filtered_naics), " NAICS options")
      
      # Update the dropdown with these options
      updatePickerInput(session, "naicsIndex", choices = current_filtered_naics)
      
      # Now move to step 2.5
      chartCodeStep(2.5)
    }
  })
  
  # Step 2.5: Update NAICS dropdown options
  observeEvent(chartCodeStep(), {
    if (chartCodeStep() == 2.5) {
      logStep("2.5 - Updating NAICS dropdown options")
      
      # Get the current filtered NAICS options
      current_filtered_naics <- unique(filteredNAICS()$index_col)
      message("Available NAICS options after filtering: ", 
              paste(head(current_filtered_naics, 5), collapse=", "), 
              "... (", length(current_filtered_naics), " total)")
      
      # Update the dropdown with these options
      updatePickerInput(session, "naicsIndex", choices = current_filtered_naics)
      
      # Wait for the UI update to complete before moving to Step 3
      session$onFlushed(function() {
        message("NAICS dropdown updated, proceeding to matching step")
        # Now move to Step 3 after the UI has been updated
        chartCodeStep(3)
      })
    }
  })
  
  # Step 3: Apply NAICS code and finalize
  observeEvent(chartCodeStep(), {
    if (chartCodeStep() == 3) {
      logStep("3 - Applying NAICS code and finalizing")
      
      # Get stored parameters
      params <- chartCodeParams()
      if (is.null(params)) {
        chartCodeStep(0)  # Reset on error
        applyingChartCode(FALSE)
        return()
      }
      
      # 3.1: Apply NAICS code
      # Now that all other parameters have been applied and processed,
      # we can set the NAICS code based on the filtered options
      target_naics_code <- params$naics_code
      message("Trying to apply NAICS code: ", target_naics_code)
      
      # Get current available options after all filters have been applied
      naics_options <- unique(filteredNAICS()$index_col)
      
      # Try to find a match
      found_match <- FALSE
      for (option in naics_options) {
        option_code <- regmatches(option, regexpr("^\\d{1,6}", option))
        if (length(option_code) > 0 && option_code != "") {
          padded_option_code <- sprintf("%06d", as.numeric(option_code))
          if (padded_option_code == target_naics_code) {
            message("Found matching NAICS option: ", option)
            updatePickerInput(session, "naicsIndex", selected = option)
            found_match <- TRUE
            break
          }
        }
      }
      
      if (!found_match) {
        message("No matching NAICS code found for: ", target_naics_code)
        # Consider showing a notification to the user that the NAICS code couldn't be found
        showNotification(paste("NAICS code", target_naics_code, "not found in available options"), 
                         type = "warning")
      }
      
      # 3.2: Show success notification
      showNotification("Chart Code applied successfully", type = "message")
      
      # 3.3: Clear the input field
      updateTextInput(session, "globalParameterIdInput", value = "")
      
      # Reset step counter and allow normal UI updates again
      chartCodeStep(0)
      applyingChartCode(FALSE)
    }
  })
  
  
  
  # Function to standardize NAICS code to 6 digits with leading zeros
  standardizeNaicsCode <- function(naics_digits) {
    # Convert to character and pad with leading zeros to 6 digits
    if (is.na(naics_digits)) return(NA)
    return(sprintf("%06d", as.numeric(naics_digits)))
  }
  
  # Extract and standardize NAICS code digits from the display string
  extractNaicsDigits <- function(naics_string) {
    # Extract just the digits at the beginning of the string
    naics_digits <- regmatches(naics_string, regexpr("^\\d{1,6}", naics_string))
    
    # If we found digits, standardize to 6 digits, otherwise return NA
    if (length(naics_digits) > 0 && naics_digits != "") {
      return(standardizeNaicsCode(as.numeric(naics_digits)))
    } else {
      return(NA)
    }
  }
  
  # Define numeric IDs for Moving Average options
  movingAverageIDs <- reactiveVal(list(
    "No Transform" = 0,
    "3 Months" = 1,
    "6 Months" = 2,
    "12 Months" = 3,
    "18 Months" = 4,
    "36 Months" = 5
  ))
  
  # Define numeric IDs for Change options
  changeIDs <- reactiveVal(list(
    "No Transform" = 0,
    "1 Month" = 1,
    "3 Months" = 2,
    "6 Months" = 3,
    "12 Months" = 4,
    "18 Months" = 5,
    "36 Months" = 6
  ))
  
  # Define numeric IDs for Percent Change options
  percentChangeIDs <- reactiveVal(list(
    "No Transform" = 0,
    "1 Month" = 1,
    "3 Months" = 2,
    "6 Months" = 3,
    "12 Months" = 4,
    "18 Months" = 5,
    "36 Months" = 6
  ))
  
  # Define numeric IDs for CAGR options
  cagrIDs <- reactiveVal(list(
    "No Transform" = 0,
    "1 Month" = 1,
    "3 Months" = 2,
    "6 Months" = 3,
    "12 Months" = 4,
    "18 Months" = 5,
    "36 Months" = 6
  ))
  
  # Output the current Moving Average ID
  output$movingAverageID <- renderText({
    # Get the current selected value
    selected <- input$movingAverageTransform
    # Look up its numeric ID
    id <- movingAverageIDs()[[selected]]
    # Return as string
    as.character(id)
  })
  
  # Output the current Change ID
  output$changeID <- renderText({
    # Get the current selected value
    selected <- input$changeTransform
    # Look up its numeric ID
    id <- changeIDs()[[selected]]
    # Return as string
    as.character(id)
  })
  
  # Output the current Percent Change ID
  output$percentChangeID <- renderText({
    # Get the current selected value
    selected <- input$percentChangeTransform
    # Look up its numeric ID
    id <- percentChangeIDs()[[selected]]
    # Return as string
    as.character(id)
  })
  
  # Output the current CAGR ID
  output$cagrID <- renderText({
    # Get the current selected value
    selected <- input$cagrTransform
    # Look up its numeric ID
    id <- cagrIDs()[[selected]]
    # Return as string
    as.character(id)
  })
  
  # Output the current Show Sub-Industries ID (binary 0/1)
  output$subIndustriesID <- renderText({
    as.character(as.integer(input$showSubIndustries))
  })
  
  # Output the current Seasonal Adjustment ID (binary 0/1)
  output$seasonalAdjustmentID <- renderText({
    as.character(as.integer(input$useSeasonalAdjustment))
  })
  
  # Output the current Index to Date ID (binary 0/1)
  output$indexToDateID <- renderText({
    as.character(as.integer(input$useIndexDate))
  })
  
  # Output the current Index Date ID (as YYYYMM)
  output$indexDateID <- renderText({
    # Get the current selected date
    selected_date <- input$indexDate
    
    # Format as YYYYMM
    date_id <- format(selected_date, "%Y%m")
    
    # Return as string
    return(date_id)
  })
  
  # Output the standardized NAICS Code ID
  output$naicsCodeID <- renderText({
    # Get the current selected value
    selected <- input$naicsIndex
    
    # Extract and standardize the NAICS code digits
    naics_digits <- extractNaicsDigits(selected)
    
    # Return as string
    if (!is.na(naics_digits)) {
      return(naics_digits)  # Already standardized with leading zeros
    } else {
      return("000000")  # Default value if no code found
    }
  })
  
  # Update the global parameter ID output
  output$globalParameterID <- renderText({
    # Get the dataset ID
    dataset_id <- as.character(datasetIDs()[[input$datasetNav]])
    
    # Get the indicator ID
    selected_dataset <- input$datasetNav
    selected_indicator <- getCurrentIndicator()
    dataset_id_num <- datasetIDs()[[selected_dataset]]
    indicator_ids_list <- indicatorIDsByDataset()[[as.character(dataset_id_num)]]
    indicator_id <- as.character(indicator_ids_list[[selected_indicator]])
    
    # Get values from all individual parameters
    ma_id <- as.character(movingAverageIDs()[[input$movingAverageTransform]])
    change_id <- as.character(changeIDs()[[input$changeTransform]])
    percent_change_id <- as.character(percentChangeIDs()[[input$percentChangeTransform]])
    cagr_id <- as.character(cagrIDs()[[input$cagrTransform]])
    show_sub_industries <- as.character(as.integer(input$showSubIndustries))
    seasonal_adjustment <- as.character(as.integer(input$useSeasonalAdjustment))
    index_to_date <- as.character(as.integer(input$useIndexDate))
    
    # Format date as YYYYMM
    index_date <- format(input$indexDate, "%Y%m")
    
    # Get NAICS code
    naics_code <- extractNaicsDigits(input$naicsIndex)
    if (is.na(naics_code)) naics_code <- "000000"
    
    # Concatenate all values into a single string, now starting with dataset and indicator IDs
    global_id <- paste0(
      dataset_id,
      indicator_id,
      ma_id,
      change_id,
      percent_change_id,
      cagr_id,
      show_sub_industries,
      seasonal_adjustment,
      index_to_date,
      index_date,
      naics_code
    )
    
    return(global_id)
  })
  
  # Update the dropdown when the Moving Average numeric input changes
  observeEvent(movingAverageCode(), {
    # Skip if the value is NA or NULL
    req(movingAverageCode())
    
    # Get the numeric ID input by the user
    user_id <- movingAverageCode()
    
    # Get the list of IDs
    ids <- movingAverageIDs()
    
    # Find the option name corresponding to this ID
    option_name <- names(ids)[which(unlist(ids) == user_id)]
    
    # If a valid option was found, update the dropdown
    if (length(option_name) > 0) {
      updateSelectInput(session, "movingAverageTransform", selected = option_name)
      
      # Clear the input box after making the selection (with a slight delay)
      invalidateLater(300)
      isolate({
        movingAverageCode(NA)
      })
    }
  })
  
  # Update the dropdown when the Change numeric input changes
  observeEvent(changeCode(), {
    # Skip if the value is NA or NULL
    req(changeCode())
    
    # Get the numeric ID input by the user
    user_id <- changeCode()
    
    # Get the list of IDs
    ids <- changeIDs()
    
    # Find the option name corresponding to this ID
    option_name <- names(ids)[which(unlist(ids) == user_id)]
    
    # If a valid option was found, update the dropdown
    if (length(option_name) > 0) {
      updateSelectInput(session, "changeTransform", selected = option_name)
      
      # Clear the input box after making the selection (with a slight delay)
      invalidateLater(300)
      isolate({
        changeCode(NA)
      })
    }
  })
  
  # Update the dropdown when the Percent Change numeric input changes
  observeEvent(percentChangeCode(), {
    # Skip if the value is NA or NULL
    req(percentChangeCode())
    
    # Get the numeric ID input by the user
    user_id <- percentChangeCode()
    
    # Get the list of IDs
    ids <- percentChangeIDs()
    
    # Find the option name corresponding to this ID
    option_name <- names(ids)[which(unlist(ids) == user_id)]
    
    # If a valid option was found, update the dropdown
    if (length(option_name) > 0) {
      updateSelectInput(session, "percentChangeTransform", selected = option_name)
      
      # Clear the input box after making the selection (with a slight delay)
      invalidateLater(300)
      isolate({
        percentChangeCode(NA)
      })
    }
  })
  
  # Update the dropdown when the CAGR numeric input changes
  observeEvent(cagrCode(), {
    # Skip if the value is NA or NULL
    req(cagrCode())
    
    # Get the numeric ID input by the user
    user_id <- cagrCode()
    
    # Get the list of IDs
    ids <- cagrIDs()
    
    # Find the option name corresponding to this ID
    option_name <- names(ids)[which(unlist(ids) == user_id)]
    
    # If a valid option was found, update the dropdown
    if (length(option_name) > 0) {
      updateSelectInput(session, "cagrTransform", selected = option_name)
      
      # Clear the input box after making the selection (with a slight delay)
      invalidateLater(300)
      isolate({
        cagrCode(NA)
      })
    }
  })
  
  # Update the Show Sub-Industries checkbox when the numeric input changes
  observeEvent(subIndustriesCode(), {
    # Skip if the value is NA or NULL
    req(subIndustriesCode())
    
    # Convert to logical (0 = FALSE, 1 = TRUE)
    value <- as.logical(subIndustriesCode())
    
    # Update the checkbox
    updateCheckboxInput(session, "showSubIndustries", value = value)
    
    # Clear the input box after making the selection
    invalidateLater(300)
    isolate({
      subIndustriesCode(NA)
    })
  })
  
  # Update the Seasonal Adjustment checkbox when the numeric input changes
  observeEvent(seasonalAdjustmentCode(), {
    # Skip if the value is NA or NULL
    req(seasonalAdjustmentCode())
    
    # Convert to logical (0 = FALSE, 1 = TRUE)
    value <- as.logical(seasonalAdjustmentCode())
    
    # Update the checkbox
    updateCheckboxInput(session, "useSeasonalAdjustment", value = value)
    
    # Clear the input box after making the selection
    invalidateLater(300)
    isolate({
      seasonalAdjustmentCode(NA)
    })
  })
  
  # Update the Index to Date checkbox when the numeric input changes
  observeEvent(indexToDateCode(), {
    # Skip if the value is NA or NULL
    req(indexToDateCode())
    
    # Convert to logical (0 = FALSE, 1 = TRUE)
    value <- as.logical(indexToDateCode())
    
    # Update the checkbox
    updateCheckboxInput(session, "useIndexDate", value = value)
    
    # Clear the input box after making the selection
    invalidateLater(300)
    isolate({
      indexToDateCode(NA)
    })
  })
  
  # Update the Index Date when the numeric input changes
  observeEvent(indexDateCode(), {
    # Skip if the value is NA or NULL
    req(indexDateCode())
    
    # Get the numeric ID input by the user
    user_id <- indexDateCode()
    
    # Convert to a date string
    # Extract year and month from the numeric input
    year <- floor(user_id / 100)
    month <- user_id %% 100
    
    # Validate month (1-12)
    if (month >= 1 && month <= 12) {
      # Create a date string
      date_str <- sprintf("%04d-%02d-01", year, month)
      date_obj <- as.Date(date_str)
      
      # Update the date input
      updateDateInput(session, "indexDate", value = date_obj)
    }
    
    # Clear the input box after making the selection
    invalidateLater(300)
    isolate({
      indexDateCode(NA)
    })
  })
  
  # Update the NAICS dropdown when the numeric input changes
  observeEvent(naicsCode(), {
    # Skip if the value is NA or NULL
    req(naicsCode())
    
    # Standardize the user input to 6 digits
    user_id <- standardizeNaicsCode(naicsCode())
    
    # Get all available NAICS options
    naics_options <- isolate(unique(filteredNAICS()$index_col))
    
    # Extract and standardize digits from all options
    naics_digits <- sapply(naics_options, extractNaicsDigits)
    
    # Try to find an exact match first
    exact_match_indices <- which(naics_digits == user_id)
    
    if (length(exact_match_indices) > 0) {
      # We found an exact match
      updatePickerInput(session, "naicsIndex", selected = naics_options[exact_match_indices[1]])
    } else {
      # No exact match, try removing trailing zeros for partial matches
      # This allows entering "336" to match "336000"
      user_id_numeric <- as.numeric(user_id)
      naics_digits_numeric <- as.numeric(naics_digits)
      
      # Find options where the user input is a prefix
      potential_matches <- which(floor(naics_digits_numeric / 10^(6 - nchar(naicsCode()))) == 
                                   as.numeric(naicsCode()))
      
      if (length(potential_matches) > 0) {
        updatePickerInput(session, "naicsIndex", selected = naics_options[potential_matches[1]])
      }
    }
    
    # Clear the input box after making the selection
    #  invalidateLater(300)
    isolate({
      naicsCode(NA)
    })
  })
  
  # Clear numeric inputs when respective UI elements change
  observeEvent(input$movingAverageTransform, {
    movingAverageCode(NA)
  })
  
  observeEvent(input$changeTransform, {
    changeCode(NA)
  })
  
  observeEvent(input$percentChangeTransform, {
    percentChangeCode(NA)
  })
  
  observeEvent(input$cagrTransform, {
    cagrCode(NA)
  })
  
  observeEvent(input$showSubIndustries, {
    subIndustriesCode(NA)
  })
  
  observeEvent(input$useSeasonalAdjustment, {
    seasonalAdjustmentCode(NA)
  })
  
  observeEvent(input$useIndexDate, {
    indexToDateCode(NA)
  })
  
  observeEvent(input$indexDate, {
    indexDateCode(NA)
  })
  
  observeEvent(input$naicsIndex, {
    naicsCode(NA)
  })
  
  # Load NAICS descriptions
  naics_descriptions <- reactive({
    read.csv("naics desc.csv", stringsAsFactors = FALSE)
  })
  
  # Add these code blocks to your server function:
  # Reactive value to track panel state
  panelCollapsed <- reactiveVal(TRUE)
  
  # Toggle panel collapse state when header is clicked
  observeEvent(input$panel_header_click, {
    # This will toggle the state regardless of data presence
    panelCollapsed(!panelCollapsed())
  })
  
  # NAICS description output
  output$naicsDescription <- renderText({
    req(input$naicsIndex)  # Ensure NAICS selection exists
    
    # Extract NAICS code from the selected string (everything before first space or dash)
    naics_code <- gsub("^(\\d+).*", "\\1", input$naicsIndex)
    
    # Look up the description in the NAICS descriptions dataframe
    description <- naics_descriptions() %>%
      filter(`NAICS.Code` == naics_code) %>%
      pull(Description)
    
    # If no description found, provide a default message
    if (length(description) == 0 || is.na(description)) {
      return("No description available for this NAICS code.")
    }
    
    # Return the description with a header
    paste0("NAICS ", naics_code, ": ", description)
  })
  
  # Update panel header text based on data presence
  output$panelHeaderText <- renderText({
    if (nrow(storedData()) == 0) {
      "Multi-Series Viewer (No Data)"
    } else {
      paste0("Multi-Series Viewer (", 
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
  
  
  
  
  
  # Filtered NAICS choices based on Thematic Constraints, NAICS Constraint, and selected Indicator
  filteredNAICS <- reactive({
    if (naicsConstraintCode() != "") {
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
  
  # Add this near the top of your server function with other reactive values
  applyingChartCode <- reactiveVal(FALSE)
  # Add these at the beginning of your server function
  lastValidNAICSSelection <- reactiveVal(NULL)
  applyingChartCode <- reactiveVal(FALSE)
  
  # Modify your existing observer that was causing issues - completely remove the indicator as a trigger
  observeEvent(naicsConstraintCode(), {
    # Skip if we're currently applying a chart code
    if(applyingChartCode()) {
      return()
    }
    
    message("NAICS filter update from constraint change")
    new_choices <- unique(filteredNAICS()$index_col)
    
    # Try to use the last valid selection if it exists in new choices
    current_selection <- lastValidNAICSSelection()
    selected_value <- if(!is.null(current_selection) && current_selection %in% new_choices) {
      current_selection
    } else {
      new_choices[1]
    }
    
    updatePickerInput(session, "naicsIndex",
                      choices = new_choices,
                      selected = selected_value)
  })
  
  # Add this at the top of your server function
  currentNaicsSelection <- reactiveVal(NULL)
  
  # Add an observer to track the current NAICS selection
  observeEvent(input$naicsIndex, {
    currentNaicsSelection(input$naicsIndex)
  })
  
  # Add this to track changes to naicsIndex
  observeEvent(input$naicsIndex, {
    lastValidNAICSSelection(input$naicsIndex)
  }, ignoreInit = TRUE)
  
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
  # Modify the selectedNAICS() reactive function to handle the new checkbox
  selectedNAICS <- reactive({
    # Check which checkboxes are selected
    show_components <- input$showSubIndustries
    show_all <- input$showAllSeries
    
    selected_index <- input$naicsIndex
    selected_code <- strsplit(selected_index, " - ")[[1]][1]
    
    if (show_all) {
      # Show all series that begin with the selected code
      data %>%
        filter(grepl(paste0("^", selected_code), NAICS_Code)) %>%
        pull(index_col) %>%
        unique()
    } else if (show_components) {
      # Existing functionality - show components (one digit longer)
      naics_codes <- c(selected_code, paste0(selected_code, 1:9))
      data %>%
        filter(NAICS_Code %in% naics_codes) %>%
        pull(index_col) %>%
        unique()
    } else {
      # Just show the selected index
      selected_index
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
  
  # Add "Not Seasonally Adjusted" to the transform name
  filtered <- filtered %>%
    mutate(Transform_Name = if_else(Transform_Name == "", 
                                   "Not Seasonally Adjusted", 
                                   paste(Transform_Name, "Not Seasonally Adjusted", sep = ", ")))
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
  # Add this function near the top of your server section
  format_tick_labels <- function(value) {
    if (is.na(value)) return("")
    
    if (abs(value) >= 1e12) {
      return(paste0(format(value/1e12, digits=1, nsmall=1, trim=TRUE), "T"))
    } else if (abs(value) >= 1e9) {
      return(paste0(format(value/1e9, digits=1, nsmall=1, trim=TRUE), "B"))
    } else if (abs(value) >= 1e6) {
      return(paste0(format(value/1e6, digits=1, nsmall=1, trim=TRUE), "M"))
    } else if (abs(value) >= 1e3) {
      return(paste0(format(value/1e3, digits=1, nsmall=1, trim=TRUE), "K"))
    } else {
      return(format(value, digits=1, nsmall=1, trim=TRUE))
    }
  }
  
  # Render line chart for filtered data
  output$lineChart <- renderPlotly({
    # Initialize the plot with hover formatting
    p <- plot_ly(filteredData(), x = ~Date) %>%
      layout(
        hoverlabel = list(
          align = "left",
          bgcolor = "rgb(237, 234, 218)",  # Fully opaque
          bordercolor = "rgb(0, 0, 0)",  # Fully opaque darkgray
          font = list(size = 12)
        ),
        hovermode = "closest" # Changed from "x unified" to "closest"
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
    
    # Get the originally selected series before expansion
    selected_base_series <- input$naicsIndex
    
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
      
      # Create a unit-specific hover template with date included
      if (grepl("Percent", series_units, ignore.case = TRUE)) {
        hover_template <- paste0(
          "<b>%{text}</b><br>",
          "%{x|%b %Y}: %{y:.1%}<br>",  # Format as percentage with date
          "<extra></extra>"
        )
      } else if (grepl("Dollar|dollars|nominal", series_units, ignore.case = TRUE)) {
        # Match any of: "Dollar", "dollars", or "nominal"
        hover_template <- paste0(
          "<b>%{text}</b><br>",
          "%{x|%b %Y}: $%{y:,.2f}<br>",  # Format as dollars with date
          "<extra></extra>"
        )
      } else if (grepl("Index", series_units, ignore.case = TRUE)) {
        hover_template <- paste0(
          "<b>%{text}</b><br>",
          "%{x|%b %Y}: %{y:.1f}<br>",  # Format with date
          "<extra></extra>"
        )
      } else {
        hover_template <- paste0(
          "<b>%{text}</b><br>",
          "%{x|%b %Y}: %{y:,.2f}<br>",  # Format with date
          "<extra></extra>"
        )
      }
      
      # Determine if this is the selected base series
      is_selected_base <- series_data$index_col[1] == selected_base_series
      
      # Use a thicker line for the selected base series when Show Components or Show All Series is checked
      line_width <- if ((input$showSubIndustries || input$showAllSeries) && is_selected_base) 3 else 1
      
      p <- p %>% add_lines(
        y = ~Value,
        data = series_data,
        name = legend_text,  # Use new formatted legend text
        line = list(width = line_width),
        hovertemplate = hover_template,
        text = ~index_col,
        showlegend = TRUE
      )
    }
    
    # Custom tick formatting for y-axis
    if (nrow(filteredData()) > 0) {
      # Get value range
      y_range <- range(filteredData()$Value, na.rm = TRUE)
      
      # Generate appropriate tick values
      tick_count <- 6 # number of ticks desired
      y_ticks <- pretty(y_range, n = tick_count)
      
      # Format tick labels with our custom function
      y_ticktext <- sapply(y_ticks, format_tick_labels)
      
      # Apply appropriate tick formatting based on axis type
      if (any(filteredData()$Axis_Type == "Percentage")) {
        p <- p %>% layout(yaxis = list(tickformat = ".1%"))
      } else {
        p <- p %>% layout(
          yaxis = list(
            tickmode = "array",
            tickvals = y_ticks,
            ticktext = y_ticktext
          )
        )
      }
    }
    
    
    # Add final layout properties
    p %>% layout(
      shapes = recession_shapes,  # Place shapes first in layout
      title = list(text = dataNavigatorTitle(), font = list(size = 24)),
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
        bgcolor = "rgb(237, 234, 218)",  # Fully opaque
        bordercolor = "rgb(0, 0, 0)",  # Fully opaque darkgray
        font = list(size = 12)
      ),
      hovermode = "closest", # Changed from "x unified" to "closest"
      plot_bgcolor = "rgba(255, 255, 255, 0.9)",
      paper_bgcolor = "rgba(255, 255, 255, 0.9)"
    )
  })
  
  
  # Reactive expression for the chart title
  chartTitle <- reactive({
    if(input$finishedVisualizationTitle == "") {
      "Multi-Series Viewer"
    } else {
      input$finishedVisualizationTitle
    }
  })
  
  # Modify the existing storedLineChart output
  # Render the plotly visualization for stored data with custom colors
  # Render the plotly visualization for stored data with custom colors
  output$storedLineChart <- renderPlotly({
    # Initialize the base plot with proper hover label formatting
    p <- plot_ly(storedData(), x = ~Date) %>%
      layout(
        # Set up hover label formatting
        hoverlabel = list(
          align = "left",
          bgcolor = "rgb(237, 234, 218)",  # Fully opaque background
          bordercolor = "rgb(0, 0, 0)",    # Black border
          font = list(family = "Arial", color = "rgb(0, 0, 0)", size = 12),  # Black text
          namelength = -1
        ),
        hovermode = "closest" # Changed from "x unified" to "closest"
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
    
    # Generate custom ticks for left axis
    left_tick_labels <- NULL
    if (!is.null(left_range) && length(left_axis_series) > 0) {
      left_ticks <- pretty(left_range, n = 6)
      left_tick_labels <- sapply(left_ticks, format_tick_labels)
    }
    
    # Generate custom ticks for right axis
    right_tick_labels <- NULL
    if (!is.null(right_range) && length(right_axis_series) > 0) {
      right_ticks <- pretty(right_range, n = 6)
      right_tick_labels <- sapply(right_ticks, format_tick_labels)
    }
    
    # Configure right-side y-axis
    if (!is.null(right_range)) {
      # Check if right axis shows percentages
      right_percent <- FALSE
      if (length(right_axis_series) > 0) {
        right_percent <- any(storedData() %>% 
                               filter(Combined %in% right_axis_series) %>% 
                               pull(Axis_Type) == "Percentage")
      }
      
      if (right_percent) {
        p <- p %>% layout(
          yaxis2 = list(
            overlaying = "y",
            side = "right",
            title = right_axis_title,
            showgrid = FALSE,
            range = right_range,
            tickformat = ".1%",
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
            range = right_range,
            tickmode = if (!is.null(right_tick_labels)) "array" else "auto",
            tickvals = if (!is.null(right_tick_labels)) right_ticks else NULL,
            ticktext = right_tick_labels,
            fixedrange = FALSE,
            rangemode = "normal"
          )
        )
      }
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
      
      # Create a unit-specific hover template with date included
      if (grepl("Percent", series_units, ignore.case = TRUE)) {
        hover_template <- paste0(
          "<b>%{text}</b><br>",
          "%{x|%b %Y}: %{y:.1%}<br>",  # Format as percentage with date
          "<extra></extra>"
        )
      } else if (grepl("Dollar|dollars|nominal", series_units, ignore.case = TRUE)) {
        hover_template <- paste0(
          "<b>%{text}</b><br>",
          "%{x|%b %Y}: $%{y:,.2f}<br>",  # Format as dollars with date
          "<extra></extra>"
        )
      } else if (grepl("Index", series_units, ignore.case = TRUE)) {
        hover_template <- paste0(
          "<b>%{text}</b><br>",
          "%{x|%b %Y}: %{y:.1f}<br>",  # Format with date
          "<extra></extra>"
        )
      } else {
        hover_template <- paste0(
          "<b>%{text}</b><br>",
          "%{x|%b %Y}: %{y:,.2f}<br>",  # Format with date
          "<extra></extra>"
        )
      }
      
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
        hovertemplate = hover_template,
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
    
    # Apply appropriate tick formatting for left axis
    if (left_percent) {
      p <- p %>% layout(
        yaxis = list(
          tickformat = ".1%", 
          autorange = is.null(left_range),
          range = left_range,
          fixedrange = FALSE, 
          rangemode = if(is.null(left_range)) "auto" else "normal"
        )
      )
    } else {
      p <- p %>% layout(
        yaxis = list(
          tickmode = if (!is.null(left_tick_labels)) "array" else "auto",
          tickvals = if (!is.null(left_tick_labels)) left_ticks else NULL,
          ticktext = left_tick_labels,
          autorange = is.null(left_range),
          range = left_range,
          fixedrange = FALSE, 
          rangemode = if(is.null(left_range)) "auto" else "normal"
        )
      )
    }
    
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
      ),
      hoverlabel = list(
        align = "left",
        bgcolor = "rgb(237, 234, 218)",  # Fully opaque background
        bordercolor = "rgb(0, 0, 0)",    # Black border
        font = list(family = "Arial", color = "rgb(0, 0, 0)", size = 12),  # Black text
        namelength = -1
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
  
# Update just the downloadCurrentData handler
output$downloadCurrentData <- downloadHandler(
  filename = function() {
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
    return(paste0("data_viewer_data_", timestamp, ".csv"))
  },
  content = function(file) {
    # Safely capture the filtered data 
    current_data <- filteredData()
    
    # Get the current chart code from the globalParameterID
    chart_code <- isolate(output$globalParameterID())
    
    # Create export version with formatted columns
    data_to_export <- current_data %>%
      ungroup() %>%
      # Create a user-friendly Series identifier
      mutate(Series = paste0(
        Indicator, " for NAICS ", NAICS_Code, ": ", NAICS_Name,
        ifelse(Transform_Name != "", paste0(", ", Transform_Name), ""),
        " (", Units, ")"
      )) %>%
      # Format date as Excel-friendly
      mutate(Date = format(Date, "%Y-%m-%d")) %>%
      # Select only the needed columns
      select(Date, Series, Value)
    
    # Pivot to wide format
    wide_data <- tidyr::pivot_wider(
      data = data_to_export,
      names_from = Series,
      values_from = Value
    )
    
    # Create a file connection to write to
    con <- file(file, "w")
    
    # Write header rows
    writeLines("Data downloaded from the Industry Data Navigator", con)
    writeLines(paste0("Chart Code: ", chart_code), con)
    
    # Write the wide-format data to the CSV file
    write.csv(wide_data, con, row.names = FALSE)
    
    # Close the connection
    close(con)
  }
)

# Update downloadStoredData handler
output$downloadStoredData <- downloadHandler(
  filename = function() {
    # Create a timestamp in format YYYY-MM-DD_HHMMSS
    timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
    paste0("final_visualization_data_", timestamp, ".csv")
  },
  content = function(file) {
    data_to_export <- storedData() %>%
      ungroup() %>%
      # Create a user-friendly Series identifier like in the legend
      mutate(Series = paste0(
        Indicator, " for NAICS ", NAICS_Code, ": ", NAICS_Name,
        ifelse(Transform_Name != "", paste0(", ", Transform_Name), ""),
        " (", Units, ")"  # Append units in parentheses
      )) %>%
      # Format date as Excel-friendly (YYYY-MM-DD)
      mutate(Date = format(Date, "%Y-%m-%d")) %>%
      # Select only the needed columns
      select(Date, Series, Value)
    
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
    
    # Create a file connection to write to
    con <- file(file, "w")
    
    # Write header row
    writeLines("Data downloaded from the Industry Data Navigator", con)
    
    # Write the wide-format data to the CSV file
    write.csv(wide_data, con, row.names = FALSE)
    
    # Close the connection
    close(con)
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
  
  
  
# Reset inputs to default values
observeEvent(input$resetInputs, {
  # Update the date range input
  updateDateRangeInput(session, "dateRange", 
                       start = as.Date("1900-01-01"),
                       end = Sys.Date())
  
  # Reset filters
  updatePickerInput(session, "naicsConstraint", selected = "No Constraint")
  updatePickerInput(session, "naicsIndex", choices = unique(filteredNAICS()$index_col), 
                    selected = unique(filteredNAICS()$index_col)[1])
  updatePickerInput(session, "datasetDropdown", selected = unique(data$Dataset)[1])
  updatePickerInput(session, "indicatorDropdown", selected = unique(data$Indicator)[1])
  updateCheckboxInput(session, "showSubIndustries", value = FALSE)
  updateCheckboxInput(session, "showAllSeries", value = FALSE)
  updateCheckboxInput(session, "useSeasonalAdjustment", value = FALSE)  # Added this line
  
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
