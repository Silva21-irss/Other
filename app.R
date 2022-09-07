## R Shiny app for visualization of CMIP6 global climate model simulations for North America and subregions
## author: Colin Mahony colin.mahony@gov.bc.ca

# Copyright 2021 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Adapted for use for Silva21 by Amy Wotherspoon (amy.wotherspoon@ubc.ca) and Michael Burnett (michael.burnett@ubcc.ca)
# with permission from Colin Mahony under the same copyright. 

library(rgdal)
library(shiny)
library(RColorBrewer)
library(scales)
library(DT)
library(shinydashboard)
library(markdown)
library(plotly)
library(stinepack) # for interpolation splines
library(rmarkdown)
library("shinyLP")
library("shinyBS")
library("shinythemes")
library("shinyWidgets")
# ----------------------------------------------
# Load the input data
# ----------------------------------------------
#options(shiny.autoload.r=FALSE)
#setwd('Y:/ByUser/Mike_B/CMIP6 App')

modelMetadata <- read.csv("data/ModelList.csv")
kkzRank.includeUKESM <- read.csv("kkzrank.includeukesm.csv")
kkzRank.excludeUKESM <- read.csv("kkzrank.excludeukesm.csv")

# Define the subregions
silvaregions <- readOGR("silva21regions1.shp")
regions <- c("Acadia", silvaregions$Hub_Site[match(names(kkzRank.includeUKESM)[-1], silvaregions$Hub_Site)])
region.names <- c("Acadia", silvaregions$Hub_Site[match(names(kkzRank.includeUKESM)[-1], silvaregions$Hub_Site)])

# Define climate elements
elements <- c("Tmax", "Tmin", "PPT")
element.names <- c("Mean daily maximum temperature (Tmax)", "Mean daily minimum temperature (Tmin)", "Precipitation")
element.names.units <- c(bquote(Mean~daily~bold(maximum)~temperature~"("*degree*C*")"),bquote(Mean~daily~bold(minimum)~temperature~"("*degree*C*")"), "Precipitation (mm)")
variable.names <- read.csv("data/variables_climatebc.csv")

# extract the global climate models and scenarios from an arbitrary file. 
template <- read.csv("change.Haliburton.csv", stringsAsFactors = F)
gcms <- unique(template$gcm)[-c(1:2)]
scenarios <- unique(template$scenario)[-c(1,5)]
scenario.names <- c("SSP1-2.6", "SSP2-4.5", "SSP3-7.0")
proj.years <- unique(template$proj.year)[-c(1:2)]
proj.year.names <- c("2021-2040", "2041-2060", "2061-2080", "2081-2100")

mods <- substr(gcms, 1, 2)

colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
set.seed(2)
ColScheme <- c(brewer.pal(n=12, "Paired"),sample(colors,length(gcms)-12))
ColScheme[11] <- "blue"

# Other definitions
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)

seasons <- c("wt", "sp", "sm", "at")
season.names <- c("Winter", "Spring", "Summer", "Autumn")

yeartimes <- c(seasons, monthcodes)
yeartime.names <- c(season.names, month.name)

tab_block <- function(text, cor, icon, id){
  HTML(paste0('<a id="', id,'" href="#" class="action-button">
                  <div class = "voronoys-block" style = "background-color:', cor, ';"> 
                  <span class = "name">', text, '</span>
                  <div class="img_block">
                    <div class="img_block_conteiner">
                      <img src="img/',icon,'">
                    </div>
                  </div>
              </div></a>'))
}

# Define UI ----
ui <- fluidPage(
  navbarPage(
    id = "CMIP6-Silva21",
    title = "CMIP6 ensemble for Silva21 Hub Sites",
#    theme = "bcgov.css",
    
    
    ## -----------------------------------------------------
    ## LANDING PAGE
    
    tabPanel(
      title = "Intro",
      value = "Intro",
      column(width = 12,
             wellPanel(
               HTML(
                 "<h2><b>SilvR21</b> - The climate model ensemble for Silva21</h2>"
               ),
               HTML(
                 "<h4>This tool provides visualizations and documentation of the global climate model ensemble featured in Version 7
                                    of ClimateNA. The ensemble is from the new generation of global climate model simulations, the sixth Coupled Model
                                    Intercomparison Project (CMIP6). Use this tool to learn about the model simulations within each Silva21 hub site and choose a small
                                    ensemble suited for your research. </h4>"
               )
             )),
      # column(width = 2, align = "left",
      #        wellPanel(
      #          actionButton("link_to_timeSeries", HTML("<h4><b>Time series</b></h4>")),
      #          HTML("<h5> Compare historical and future model projections against observations,
      #                                    for individual models and customizable ensembles,
      #                                    with and without bias correction.</h5 >")
      #        )
      # ),
      column(
        width = 4,
        align = "left",
        wellPanel(
          actionButton("link_to_Change", HTML("<h4><b>View models</b></h4>")),
          HTML(
            "<h5>Compare model projections in a two-variable climate space.
                                    Create smaller ensembles based on predefined or custom criteria.</h5 >"
          )
        )
      ),
      # column(width = 2, align = "left",
      #        wellPanel(
      #          actionButton("link_to_Bias", HTML("<h4><b>Assess Bias</b></h4>")),
      #          HTML("<h5>Assess model biases relative to historical observations.</h5 >")
      #        )
      # ),
      column(
        width = 4,
        align = "left",
        wellPanel(
          actionButton("link_to_Maps", HTML("<h4><b>Maps</b></h4>")),
          HTML(
            "<h5>Compare spatial variation in climate change among models. </h5 >"
          )
        )
      ),
      # column(width = 2, align = "left",
      #        wellPanel(
      #          actionButton("link_to_Guidance", HTML("<h4><b>Guidance</b></h4>")),
      #          HTML("<h5>Guidance for selecting models, emissions scenarios, and time periods. </h5 >")
      #        )
      # ),
      column(
        width = 12,
        br(),
        HTML("<h4><b>Contributors</b></h4>"),
        HTML(
          "<h5 >
                               App adapted for Silva21 by Amy Wotherspoon using open-source code of the CMIP6-NA app created by Colin Mahony<br>
                                 Amy Wotherspoon, PhD<br>
                                 Postdoctoral Fellow<br>
                                 Silva 21<br>
                                 amy.wotherspoon@ubc.ca<br>
                               <br>
                               <b>Code: </b>The code and data for this tool are available at <a href='https://github.com/Silva21-irss/Climate_Data_Processing' target='_blank'>https://github.com/Silva21-irss/Climate_Data_Processing"
      )
        
      ),
      column(
        width = 12,
        style = "background-color:#173d09; border-top:2px solid #f8faf7;",
        
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www.silva21.com/", "About Silva21", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(a(href = "https://www.silva21.com/projects/future-climate-envelopes", "Contact", style =
                          "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style=
                          "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")           )
            )
          )
        )
      )
    ),
    
    ## -----------------------------------------------------
    ## ABOUT
    
    tabPanel(
      "About",
      
      includeMarkdown("about.Rmd"),
      
      column(
        width = 12,
        style = "background-color:#173d09; border-top:2px solid #f8faf7;",
        
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www.silva21.com/", "About Silva21", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(a(href = "https://www.silva21.com/projects/future-climate-envelopes", "Contact", style =
                     "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style=
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    ),
    
    
    
    ## -----------------------------------------------------
    ## CHANGE
    
    tabPanel(
      "View Models",
      sidebarLayout(
        sidebarPanel(
          helpText(
            "This tab shows the amount of change projected by each model, relative to the 1981-2010 period.
                                   You can use this tab to reduce the ensemble size base on predefined or custom model selection methods.
                                   Click on a legend item to hide it; double-click to isolate it.
                                   Drag a box on the plot to zoom in; double-click the plot to zoom back out.
                                   Click 'Download data' for a full data table that includes projected changes of each variable for all models, time horizones and scenarios."
          ),
          
          tags$head(
            tags$script(
              '$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            '
            )
          ),
          
          tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
          
          
          radioButtons(
            "modeChange",
            "Ensemble selection mode",
            choiceNames = c("Predefined", "Custom"),
            choiceValues = c("Predefined", "Custom"),
            selected = "Predefined",
            inline = T
          ),
          
          conditionalPanel(
            condition = "input.modeChange == 'Predefined'",
            
            checkboxInput("includeUKESM", label = "Include UKESM1 in small ensembles (<9 models)", value = T),
            
            sliderInput(
              "kkzN",
              label = "Reduce ensemble size in predefined order",
              min = 1,
              max = 13,
              value = 13,
              step = 1
            ),
          ),
          
          
          conditionalPanel(
            condition = "input.modeChange == 'Custom'",
            
            checkboxGroupInput(
              "gcms.change",
              "Choose global climate models:",
              choiceNames = gcms,
              choiceValues = gcms,
              selected = gcms,
              inline = T
            ),
          ),
          
          radioButtons(
            "proj.year.change",
            inline = TRUE,
            label = "Choose a time slice",
            choiceNames = proj.year.names,
            choiceValues = proj.years,
            selected = proj.years[3]
          ),
          
          radioButtons(
            "scenario.change",
            "Choose emissions scenario",
            choiceNames = scenario.names,
            choiceValues = scenarios,
            selected = scenarios[2],
            inline = T
          ),
          
          checkboxInput("trajectories", label = "Include model trajectories", value = T),
          
          selectInput(
            "element1.change",
            label = "x-axis: choose the climate element",
            choices = as.list(element.names),
            selected = element.names[1]
          ),
          
          selectInput(
            "yeartime1.change",
            label = "x-axis: Choose the month/season",
            choices = as.list(yeartime.names),
            selected = yeartime.names[3]
          ),
          
          selectInput(
            "element2.change",
            label = "y-axis: choose the climate element",
            choices = as.list(element.names),
            selected = element.names[3]
          ),
          
          selectInput(
            "yeartime2.change",
            label = "y-axis: Choose the month/season",
            choices = as.list(yeartime.names),
            selected = yeartime.names[3]
          ),
          
          selectInput(
            "region.name.change",
            label = "Choose a region",
            choices = as.list(region.names),
            selected = region.names[1]
          ),
          
          img(
            src = "https://static.wixstatic.com/media/066ab2_8451e3b06b03438094181f3ce2fe42ed~mv2.png/v1/fill/w_600,h_500,al_c,q_85,usm_0.66_1.00_0.01/carte_hub.webp",
            height = 1861 * 1 / 5,
            width = 1600 * 1 / 5
          )
        ),
        
        mainPanel(
          plotlyOutput(outputId = "ChangePlot", height = "600px"),
          downloadButton(outputId = "downloadData_change", label = "Download data")
          
        )
      ),
      column(
        width = 12,
        style = "background-color:#173d09; border-top:2px solid #f8faf7;",
        
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www.silva21.com/", "About Silva21", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(a(href = "https://www.silva21.com/projects/future-climate-envelopes", "Contact", style =
                          "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style=
                          "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    ),
    
    
    
    ## -----------------------------------------------------
    ## Maps TAB

###### MABS TAB IS CURRENTLY HIDDEN SINCE THERE IS NO SPATIAL VARIATION ######    
#    tabPanel(
#      "Maps",
#      sidebarLayout(
#        sidebarPanel(
#          helpText(
#            "These maps show the spatial pattern of simulated climate change for each model.
#                                   The change is the mean climate within each projected time period of the various SSP simulations
#                                   relative to the 1981-2010 period of the model's historical simulations.
#                                   All maps are derived from raw GCM files.
#                                   Temperature units (K) are Kelvins, which are equivalent to degrees Celsius.
#                                   "
#          ),
#          
#          tags$head(
#            tags$script(
#              '$(document).on("shiny:connected", function(e) {
#                            Shiny.onInputChange("innerWidth", window.innerWidth);
#                            });
#                            $(window).resize(function(e) {
#                            Shiny.onInputChange("innerWidth", window.innerWidth);
#                            });
#                            '
#            )
#          ),
#          selectInput(
#            "areaMap",
#            label = "Choose a hub site",
#            choices = as.list(region.names),
#            selected = region.names[1]
#          ),
#          
#          radioButtons(
#            "varType",
#            inline = F,
#            label = "Choose the scenario",
#            choices = c("Minimum Temperature", "Average Temperature","Maximum Temperature","Precipitation"),
#            selected = "Maximum Temperature"
#          ),
#          
#          selectInput(
#            "sspType",
#            #inline = F,
#           label = "Choose the scenario",
#            choices = c("ssp126", "ssp245","ssp370","ssp585"),
#            selected = "ssp126"
#          ),
#          
#         selectInput(
#            "timeFrameType",
#            #inline = F,
#            label = "Choose the time frame",
#            choices = c("2001-2020", "2021-2040","2041-2060","2061-2080","2081-2100"),
#            selected = "2081-2100"
#          ),
#          
#          selectInput(
#            inputId = "seasonbuttons",
#            label = "Choose a season",
#            choices = c('annual','winter','spring','summer','autumn'),
#            selected = season.names[1]
#          )
#        ),
#        
#        mainPanel(imageOutput(
#          "changeMap", width = "100%", height = "100%"
#        ))
#      ),
#      column(
#        width = 12,
#        style = "background-color:#173d09; border-top:2px solid #f8faf7;",
#        
#        tags$footer(
#          class = "footer",
#          tags$div(
#            class = "container",
#            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
#            tags$ul(
#              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
#              tags$li(
#                a(href = "https://www.silva21.com/", "About Silva21", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
#              ),
#             tags$li(a(href = "https://www.silva21.com/projects/future-climate-envelopes", "Contact", style =
#                          "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
#              ),
#              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style=
#                          "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
#              )
#            )
#          )
#       )
#      )
#    ),
#
##
##

    ## -----------------------------------------------------
    ## MODEL INFO
    
    tabPanel(
      "Model Info",
      DT::dataTableOutput("table"),
      column(
        width = 12,
        style = "background-color:#173d09; border-top:2px solid #f8faf7;",
        
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www.silva21.com/", "About Silva21", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(a(href = "https://www.silva21.com/projects/future-climate-envelopes", "Contact", style =
                          "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style=
                          "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ))
            )
          )
        )
      )
    )
  )


# Define server logic ----
server <- function(input, output, session) {
  observeEvent(input$link_to_Change, {
    updateNavbarPage(session, "CMIP6-Silva21", selected = "View Models")
  })
  
  observeEvent(input$link_to_Maps, {
    updateNavbarPage(session, "CMIP6-Silva21", selected = "Maps")
  })
  
  # This is the gcm selection for the time series plot. done as a renderUI to allow the reset button
  output$reset_gcms <- renderUI({
    times <- input$reset_input
    div(
      id = letters[(times %% length(letters)) + 1],
      checkboxGroupInput(
        "gcms.ts2",
        "Choose global climate models:",
        choiceNames = gcms,
        choiceValues = gcms,
        selected = gcms[select],
        inline = T
      )
    )
  })
  
  
  
  output$ChangePlot <- renderPlotly({
    # region <- regions[1]
    # yeartime1 <- yeartimes[1]
    # yeartime2 <- yeartimes[1]
    # element1 <- elements[1]
    # element2 <- elements[4]
    # proj.year <- proj.years[3]
    # scenario <- scenarios[2]
    # gcms.change <- gcms
    
    # observe(updateCheckboxGroupInput(session, "gcms.change", selected = gcms[which(gcms%in%kkzRank[1:input$kkzN,which(region.names==input$region.name.change)])]))
    
    region <- regions[which(region.names == input$region.name.change)]
    yeartime1 <-
      yeartimes[which(yeartime.names == input$yeartime1.change)]
    yeartime2 <-
      yeartimes[which(yeartime.names == input$yeartime2.change)]
    element1 <-
      elements[which(element.names == input$element1.change)]
    element2 <-
      elements[which(element.names == input$element2.change)]
    proj.year <- input$proj.year.change
    scenario <- input$scenario.change
    if (input$modeChange == "Predefined") {
      if (input$includeUKESM == T) {
        gcms.change <-
          gcms[which(gcms %in% kkzRank.includeUKESM[1:input$kkzN, which(region.names ==
                                                                          input$region.name.change)])]
      } else {
        gcms.change <-
          gcms[which(gcms %in% kkzRank.excludeUKESM[1:input$kkzN, which(region.names ==
                                                                          input$region.name.change)])]
      }
    } else {
      gcms.change <- input$gcms.change
    }
    
    variable1 <-
      paste(element1, yeartime1, sep = if (yeartime1 %in% seasons)
        "_"
        else
          "")
    variable2 <-
      paste(element2, yeartime2, sep = if (yeartime2 %in% seasons)
        "_"
        else
          "")
    
    data <- read.csv(paste("change", region, "csv", sep = "."))
    
    x <- data[, which(names(data) == variable1)]
    y <- data[, which(names(data) == variable2)]
    x0 <-
      data[which(data$proj.year == 2010 &
                   data$gcm == "obs"), which(names(data) == variable1)]
    y0 <-
      data[which(data$proj.year == 2010 &
                   data$gcm == "obs"), which(names(data) == variable2)]
    x.mean <-
      mean(data[which(data$scenario == scenario &
                        data$proj.year == proj.year &
                        data$gcm %in% gcms.change), which(names(data) == variable1)])
    y.mean <-
      mean(data[which(data$scenario == scenario &
                        data$proj.year == proj.year &
                        data$gcm %in% gcms.change), which(names(data) == variable2)])
    x.mean.ClimateBC <-
      mean(data[which(data$scenario == scenario &
                        data$proj.year == proj.year &
                        data$gcm %in% gcms), which(names(data) == variable1)])
    y.mean.ClimateBC <-
      mean(data[which(data$scenario == scenario &
                        data$proj.year == proj.year &
                        data$gcm %in% gcms), which(names(data) == variable2)])
    
    xlim = range(x) * c(if (min(x) < 0)
      1.1
      else
        0.9, if (max(x) > 0)
          1.1
      else
        0.9)
    ylim = range(y) * c(if (min(y) < 0)
      1.1
      else
        0.9, if (max(y) > 0)
          1.1
      else
        0.9)
    
    #initiate the plot
    fig <-
      plot_ly(
        x = x,
        y = y,
        type = 'scatter',
        mode = 'markers',
        marker = list(color = "lightgrey", size = 5),
        hoverinfo = "none",
        color = "All models/scenarios/times"
      )
    
    fig <-
      fig %>% layout(
        xaxis = list(
          title = paste("Change in", variable.names$Variable[which(variable.names$Code ==
                                                                     variable1)]),
          range = xlim
        ),
        yaxis = list(
          title = paste("Change in", variable.names$Variable[which(variable.names$Code ==
                                                                     variable2)]),
          range = ylim
        )
      )
    
    fig <-
      fig %>% add_markers(
        x = x0,
        y = y0,
        color = "Observed (2001-2020)",
        text = "Observed\n(2001-2020)",
        hoverinfo = "text",
        marker = list(size = 25,
                      color = "grey")
      )
    
    fig <-
      fig %>% add_markers(
        x = x.mean,
        y = y.mean,
        color = "Custom ensemble mean",
        text = "Custom ensemble mean",
        hoverinfo = "text",
        marker = list(
          size = 20,
          color = "grey",
          symbol = 3
        )
      )
    
    fig <-
      fig %>% add_markers(
        x = x.mean.ClimateBC,
        y = y.mean.ClimateBC,
        color = "ClimateNA 13-model mean",
        text = "ClimateNA 13-model mean",
        hoverinfo = "text",
        marker = list(
          size = 20,
          color = "black",
          symbol = 103
        )
      )
    
    gcm = gcms.change[3]
    for (gcm in gcms.change) {
      i = which(gcms == gcm)
      x1 <-
        data[which(data$scenario == scenario &
                     data$proj.year == proj.year &
                     data$gcm == gcm), which(names(data) == variable1)]
      y1 <-
        data[which(data$scenario == scenario &
                     data$proj.year == proj.year &
                     data$gcm == gcm), which(names(data) == variable2)]
      x2 <-
        data[c(1, which(data$scenario == scenario &
                          data$gcm == gcm)), which(names(data) == variable1)]
      y2 <-
        data[c(1, which(data$scenario == scenario &
                          data$gcm == gcm)), which(names(data) == variable2)]
      
      if (input$trajectories == T) {
        if (length(unique(sign(diff(x2)))) == 1) {
          x3 <- if (unique(sign(diff(x2))) == -1)
            rev(x2)
          else
            x2
          y3 <- if (unique(sign(diff(x2))) == -1)
            rev(y2)
          else
            y2
          s <-
            stinterp(x3, y3, seq(min(x3), max(x3), diff(xlim) / 1500)) # way better than interpSpline, not prone to oscillations
          fig <-
            fig %>% add_trace(
              x = s$x,
              y = s$y,
              type = 'scatter',
              mode = 'lines',
              line = list(
                color = ColScheme[i],
                width = 2,
                dash = 'dash'
              ),
              marker = NULL,
              legendgroup = paste("group", i, sep = ""),
              showlegend = FALSE
            )
          limit <-
            if (unique(sign(diff(x2))) == -1)
              which(s$x > x1)
          else
            which(s$x < x1)
          fig <-
            fig %>% add_trace(
              x = s$x[limit],
              y = s$y[limit],
              type = 'scatter',
              mode = 'lines',
              line = list(color = ColScheme[i]),
              marker = NULL,
              legendgroup = paste("group", i, sep = ""),
              showlegend = FALSE
            )
        } else {
          fig <-
            fig %>% add_trace(
              x = x2,
              y = y2,
              type = 'scatter',
              mode = 'lines',
              line = list(
                color = ColScheme[i],
                width = 2,
                dash = 'dash'
              ),
              marker = NULL,
              legendgroup = paste("group", i, sep = ""),
              showlegend = FALSE
            )
          limit <- c(1, (which(proj.years <= proj.year) + 1))
          fig <-
            fig %>% add_trace(
              x = x2[limit],
              y = y2[limit],
              type = 'scatter',
              mode = 'lines',
              line = list(color = ColScheme[i]),
              marker = NULL,
              legendgroup = paste("group", i, sep = ""),
              showlegend = FALSE
            )
        }
        fig <-
          fig %>% add_markers(
            x = x2,
            y = y2,
            color = gcms[i],
            text = gcms[i],
            hoverinfo = "text",
            marker = list(size = 8,
                          color = ColScheme[i]),
            legendgroup = paste("group", i, sep =
                                  ""),
            showlegend = FALSE
          )
      }
      
      fig <- fig %>% add_markers(
        x = x1,
        y = y1,
        color = gcms[i],
        marker = list(
          size = 20,
          color = ColScheme[i],
          line = list(color = "black",
                      width = 1)
        ),
        legendgroup = paste("group", i, sep = "")
      )
      
      fig <-
        fig %>% add_annotations(
          x = x1,
          y = y1,
          text = sprintf("<b>%s</b>", mods[i]),
          xanchor = 'center',
          yanchor = 'center',
          showarrow = F,
          legendgroup = paste("group", i, sep =
                                "")
        )
      
    }
    
    if (element1 == "PPT")
      fig <- fig %>% layout(xaxis = list(tickformat = "%"))
    if (element2 == "PPT")
      fig <- fig %>% layout(yaxis = list(tickformat = "%"))
    
    fig
    
  })
  
  # Downloadable csv of selected dataset ----
  data_change <-
    reactive(read.csv(paste("change", regions[which(region.names == input$region.name.change)], "csv", sep =
                              ".")))
  
  output$downloadData_change <- downloadHandler(
    filename = function() {
      paste("change", regions[which(region.names == input$region.name.change)], "csv", sep =
              ".")
    },
    content = function(file) {
      write.csv(data_change(), file, row.names = FALSE)
    }
  )
  
  output$changeMap <- renderImage({
    if(input$varType == 'Maximum Temperature'){
      filename <- normalizePath(file.path(
        './maps',paste(
          'changemap',
          tolower(input$areaMap),
          input$timeFrameType,
          input$sspType,
          input$seasonbuttons,
          'png',
          sep='.'
        )
      ))
    }
    if(input$varType == 'Minimum Temperature'){
      filename <- normalizePath(file.path(
        './maps',paste(
          'changeminmap',
          tolower(input$areaMap),
          input$timeFrameType,
          input$sspType,
          input$seasonbuttons,
          'png',
          sep='.'
        )
      ))
    }
    if(input$varType == 'Average Temperature'){
      filename <- normalizePath(file.path(
        './maps',paste(
          'changeavgmap',
          tolower(input$areaMap),
          input$timeFrameType,
          input$sspType,
          input$seasonbuttons,
          'png',
          sep='.'
        )
      ))
    }
    if(input$varType == 'Precipitation'){
      filename <- normalizePath(file.path(
        './maps',paste(
          'changepptmap',
          tolower(input$areaMap),
          input$timeFrameType,
          input$sspType,
          input$seasonbuttons,
          'png',
          sep='.'
        )
      ))
    }
    # if (input$mapType == "Topography") {
    #   if (input$areaMap == "Pacific Northwest") {
    #     filename <-
    #       normalizePath(file.path('./www', paste("Orography.PNW.png", sep = ".")))
    #   } else {
    #     filename <-
    #       normalizePath(file.path(
    #         './www',
    #         paste("Orography.NorthAmerica.png", sep = ".")
    #       ))
    #   }
    # }
    # 
    # if (input$mapType == "Climate change") {
    #   yeartimeMap <-
    #     if (input$seasonsOrMonths == "Seasons")
    #       seasons[which(season.names == input$seasonbuttons)]
    #   else
    #     monthcodes[which(month.abb == input$monthslider)]
    #   
    #   if (input$areaMap == "Pacific Northwest") {
    #     filename <-
    #       normalizePath(file.path(
    #         './www',
    #         paste(
    #           "changeMap",
    #           input$elementMap,
    #           yeartimeMap,
    #           "png",
    #           sep = "."
    #         )
    #       ))
    #   } else {
    #     filename <-
    #       normalizePath(file.path(
    #         './www',
    #         paste(
    #           "changeMap.NorAm",
    #           input$elementMap,
    #           yeartimeMap,
    #           "png",
    #           sep = "."
    #         )
    #       ))
    #   }
    
    #}
    
    list(src = filename,
         width = "100%",
         height = "100%")
    
  }, deleteFile = FALSE)
  
  output$table <- DT::renderDataTable({
    DT::datatable(
      modelMetadata,
      options = list(pageLength = dim(modelMetadata)[1]),
      rownames = FALSE,
      caption = HTML(
        "<p><h4><b>Information about CMIP6 models featured in this app.</b>
                                 ECS is equilibrium climate sensitivity (long-term temperature change in response to an instant doubling of CO2), and values are quoted from <a href='https://advances.sciencemag.org/content/6/26/eaba1981.abstract' target='_blank'>Meehl et al. (2020)</a>.
                                 Grid resolutions are in degrees of latitude and longitude. The last five columns are the number of model runs for each scenario that are included in ClimateNA and this app</p></h4>"
      )
    )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
#shiny::runAPP()

# Silva21 Account Information
# Account email : michael.burnett@ubc.ca
# Account password : tpV9UAVnmBBu5SN