# install.packages(c("rsconnect", "shiny", "ggplot2", "scales", "shinythemes", "tidyverse",
# "shinyWidgets", "ggrepel", "itertools", "ggiraph", "maps", "ggsci", "mapcan", "rapport"))

require(rsconnect)
require(shiny)
require(ggplot2)
require(scales)
require(shinythemes)
require(shinyWidgets)
require(ggrepel)
require(itertools)
require(ggiraph)
require(maps)
require(mapcan)
require(ggsci)
require(rapport)

unempData <- read.csv("data/unempFinalData.csv", head=T, sep=",")
unempData$GEO <- factor(unempData$GEO, levels=c("Canada", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC"))
unempData$Age.group <- factor(unempData$Age.group, levels=c("15 years and over","15 to 24 years","25 to 54 years","55 years and over"))
unempData$Sex <- factor(unempData$Sex, levels = c("Both sexes", "Males", "Females"))
unempData$refPeriod <- as.Date(unempData$refPeriod)


ui <- fluidPage(
  theme = shinytheme("paper"),
  tags$style(
    HTML(
      ".navbar-default .navbar-brand {color: #212121;}
      .navbar-default .navbar-brand:hover {color: #212121;}"
    )
  ),
  navbarPage(
    "Jobs and Unemployment",
    tabPanel("Unemployment",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("unempStatistic",
                   label = "What are you interested in?",
                   choices = list(
                     "Number unemployed (x1,000)",
                     "Official unemployment rate, seasonally adjusted",
                     "Official unemployment rate, not seasonally adjusted",
                     "Comprehensive unemployment rate, not seasonally adjusted",
                     "Number unemployed one month or less (x1,000)",
                     "Number unemployed by reason" = c("Job leavers",
                                                        "Own illness or disability",
                                                        "Personal or family reasons",
                                                        "Going to school",
                                                        "Dissatisfied",
                                                        "Retired",
                                                        "Other reasons",
                                                       "Job losers",
                                                        "Permanent layoff",
                                                        "Temporary layoff"
                                                       )),
                   selected = "Number unemployed"
                 ),
                 sliderInput("unempRefPeriod",
                   label = "Select reference period:",
                   min = min(unempData$refPeriod),
                   max = max(unempData$refPeriod),
                   value = c(min(unempData$refPeriod), max(unempData$refPeriod)),
                   timeFormat = "%b %Y"
                 ),
                 selectInput("unempGeo",
                   label = "Select geography:",
                   choices = c("Canada" = "Canada","Newfoundland and Labrador" = "NL","Prince Edward Island" = "PE","Nova Scotia" = "NS",
                     "New Brunswick" = "NB","Quebec" = "QC","Ontario" = "ON","Manitoba" = "MB","Saskatchewan" = "SK","Alberta" = "AB",
                     "British Columbia" = "BC"),
                   selected = "Canada"
                 ),
                 selectInput("unempAge",
                   label = "Select age group:",
                   choices = c("15 years and over","15 to 24 years","25 to 54 years","55 years and over"),
                   selected = "15 years and over"
                 ),
                 selectInput("unempSex",
                   label = "Select sex:",
                   choices = c("Both sexes", "Males", "Females"),
                   selected = "Both sexes"
                 )
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   style = "display: flex; align-items: center;",
                   column(5, girafeOutput("unempMap")),
                   column(
                     7,
                     div(
                       style = "display: inline-block;vertical-align:top;",
                       dropdownButton(
                         label = "Geography",
                         circle = F,
                         size = "sm",
                         status = "custom",
                         checkboxGroupInput(
                           "unempByGeo",
                           label = NULL,
                           choices = c("Canada" = "Canada","Newfoundland and Labrador" = "NL","Prince Edward Island" = "PE","Nova Scotia" = "NS",
                                       "New Brunswick" = "NB","Quebec" = "QC","Ontario" = "ON","Manitoba" = "MB","Saskatchewan" = "SK","Alberta" = "AB",
                                       "British Columbia" = "BC")
                         )
                       )
                     ),
                     div(
                       style = "display: inline-block;vertical-align:top;",
                       dropdownButton(
                         label = "Sex",
                         circle = F,
                         size = "sm",
                         status = "custom",
                         checkboxGroupInput(
                           "unempBySex",
                           label = NULL,
                           choices = c("Both sexes", "Males", "Females")
                         )
                       )
                     ),
                     div(
                       style = "display: inline-block;vertical-align:top;",
                       dropdownButton(
                         label = "Age group",
                         circle = F,
                         size = "sm",
                         status = "custom",
                         checkboxGroupInput(
                           "unempByAge",
                           label = NULL,
                           choices = c("15 years and over","15 to 24 years","25 to 44 years","45 years and over")
                         )
                       )
                     ),
                     tags$style(
                       ".btn-custom {-webkit-box-shadow: none; box-shadow: none; position: relative;}"
                     ),
                     girafeOutput("unempYears")
                   )
                 ),
                 br(),
                 fluidRow(
                   style = "display: flex; align-items: center;",
                   column(4, girafeOutput("unempAgePlot")),
                   column(4, girafeOutput("unempSexPlot")),
                   column(4, wellPanel(p("text to come")))
                 )
               )
             )),
    tabPanel("Employment",
             p("this is coming"))
  )
)


server <- function(input, output) {
  
  # unemployment plots
  output$unempMap <- renderGirafe({
    unempMapData <- subset(unempData, Statistics == input$unempStatistic  &
                                substr(refPeriod,0,7) == substr(input$unempRefPeriod[2],0,7) &
                                Sex == input$unempSex &
                                Age.group == input$unempAge &
                                GEO != "Canada")
    
    unempMapData$GEO <- as.character(unempMapData$GEO)
    
    unempMapData <- dplyr::left_join(mapcan(boundaries = province, type = standard),
                                        unempMapData,
                                        by = c("pr_alpha" = "GEO"))
    
    unempMapPlot <- ggplot() +
      geom_polygon_interactive(data=unempMapData, 
                               aes(x = long, y = lat, group = group, 
                                   fill=VALUE, tooltip=paste(pr_alpha, VALUE))) +
      coord_fixed() +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), legend.position = "bottom", axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(), panel.spacing = element_blank(),
            legend.box.spacing = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
            text=element_text(family="Roboto")) +
      scale_fill_material("blue", name=paste(input$unempStatistic)) +
      labs(title=paste0(input$unempStatistic,", ",format(input$unempRefPeriod[2], "%b %Y")))
    
    girafe(ggobj = unempMapPlot, width_svg = 7)
    
  })
  
  # Plot of statistic over reference period
  output$unempYears <- renderGirafe({
    if (is.null(input$unempByGeo) & is.null(input$unempByAge) & is.null(input$unempBySex)) 
      unempYearsData <- subset(unempData, Statistics == input$unempStatistic &
                                  substr(refPeriod,0,7) >= substr(input$unempRefPeriod[1],0,7) &
                                  substr(refPeriod,0,7) <= substr(input$unempRefPeriod[2],0,7) &
                                  Age.group == input$unempAge &
                                  Sex == input$unempSex &
                                  GEO == input$unempGeo)
    else unempYearsData <- subset(unempData, Statistics == input$unempStatistic &
                                    substr(refPeriod,0,7) >= substr(input$unempRefPeriod[1],0,7) &
                                    substr(refPeriod,0,7) <= substr(input$unempRefPeriod[2],0,7) &
                                     {if (is.null(input$unempByAge)) Age.group == input$unempAge
                                       else Age.group %in% input$unempByAge} &
                                       {if (is.null(input$unempBySex)) Sex == input$unempSex
                                         else Sex %in% input$unempBySex} &
                                         {if (is.null(input$unempByGeo)) GEO == input$unempGeo
                                           else GEO %in% input$unempByGeo})
          
    # only show selected variables in the interaction legend
    byVarsBool <- c(!is.null(input$unempByGeo),
                    !is.null(input$unempByAge),
                    !is.null(input$unempBySex))
    
    byVars <- c("GEO", "Age.group", "Sex")
    
    byVars <- byVars[unlist(byVarsBool)]
    
    unempYearsPlot <- ggplot(unempYearsData) + 
                        {if (!rapportools::is.empty(byVars[1])) geom_line(aes(x=refPeriod, y=VALUE, colour=interaction(unempYearsData[,byVars], drop=T, sep=", ")), size=1) 
                          else geom_line(aes(x=refPeriod, y=VALUE, colour=Statistics), size=1)} +
                        {if (!rapportools::is.empty(byVars[1])) geom_point_interactive(aes(x=refPeriod, y=VALUE, tooltip=paste0(format(refPeriod, "%b %Y"), ": ", VALUE),
                                                                                        colour=interaction(unempYearsData[,byVars] ,drop=T, sep=", ")), size = 1.5)
                          else geom_point_interactive(aes(x=refPeriod, y=VALUE, tooltip=paste0(format(refPeriod, "%b %Y"), ": ", VALUE), colour=Statistics), size = 1.5)} +
                        theme_classic() + scale_y_continuous(labels = comma) +
                        {if (!rapportools::is.empty(byVars)) guides(colour = "legend") else guides(colour=F)} +
                        scale_colour_jco() +
                        theme_classic() + theme(text=element_text(family="Roboto"), plot.margin=grid::unit(c(0,0,0,0), "mm"),legend.position = "bottom") +
                        labs(y=paste(input$unempStatistic), 
                             x="Reference period", 
                             title=paste0(input$unempStatistic, ", by year"),
                             colour=NULL) 
    
    girafe(ggobj = unempYearsPlot, width_svg = 9)
    
  })

  unempSexData <- reactive ({unempData$highlight <- ifelse((unempData$Sex == input$unempSex), 1, ifelse((input$unempSex == "Both sexes"),1,0))
                              
                              thismonth <- input$unempRefPeriod[2]
                              lastmonth <- seq(input$unempRefPeriod[2], length=2, by=("-1 month"))[2]
                              lastyear <- seq(input$unempRefPeriod[2], length=2, by=("-1 year"))[2]
  
                              unempData$month <- ifelse(substr(unempData$refPeriod,0,7) == substr(thismonth,0,7), "This month", 
                                                      (ifelse(substr(unempData$refPeriod,0,7) == substr(lastmonth,0,7), "Last month", "Last year")))
                            
                              unempData$month <- factor(unempData$month,levels=c("This month", "Last month", "Last year"))
                            
                            return(subset(unempData,
                                          GEO == input$unempGeo &
                                            Sex != "Both sexes" &
                                            Age.group == input$unempAge &
                                            substr(refPeriod,0,7) %in% c(substr(c(thismonth, lastmonth, lastyear), 0, 7)) &
                                            Statistics == input$unempStatistic))})
  
  output$unempSexPlot <- renderGirafe({
    
    unempSexPlot <- ggplot(unempSexData()) + 
                      geom_col_interactive(aes(x=Sex, y=VALUE, alpha = highlight, fill=month, tooltip=VALUE), position="dodge") + 
                      theme_classic() + scale_fill_jco() + theme(text=element_text(family="Roboto")) +
                      scale_alpha(range = c(max(0.45, min(unempSexData()$highlight)),1)) +
                      guides(alpha = FALSE) +
                      labs(y=input$unempStatistic, 
                           x="Sex",
                           fill = NULL,
                           title=paste0(input$unempStatistic," by sex"))
    
    girafe(ggobj = unempSexPlot, height_svg = 7)
  })
  
  unempAgeData <- reactive ({unempData$highlight <- ifelse((unempData$Age.group == input$unempAge), 1, ifelse((input$unempAge == "15 years and over"),1,0))
                             
                            thismonth <- input$unempRefPeriod[2]
                            lastmonth <- seq(input$unempRefPeriod[2], length=2, by=("-1 month"))[2]
                            lastyear <- seq(input$unempRefPeriod[2], length=2, by=("-1 year"))[2]
                            
                            unempData$month <- ifelse(substr(unempData$refPeriod,0,7) == substr(thismonth,0,7), "This month", 
                                                      (ifelse(substr(unempData$refPeriod,0,7) == substr(lastmonth,0,7), "Last month", "Last year")))
                            
                            unempData$month <- factor(unempData$month,levels=c("This month", "Last month", "Last year"))

                              return(subset(unempData,
                                            GEO == input$unempGeo &
                                              Sex == input$unempSex &
                                              Age.group != "15 years and over" &
                                              substr(refPeriod,0,7) %in% c(substr(c(thismonth, lastmonth, lastyear), 0, 7)) &
                                              Statistics == input$unempStatistic))})
  
  output$unempAgePlot <- renderGirafe({
    
    unempAgePlot <- ggplot(unempAgeData()) + 
      geom_col_interactive(aes(x=Age.group, y=VALUE, alpha = highlight, fill=month, tooltip=VALUE), position = "dodge") + 
      theme_classic() + scale_fill_jco() + theme(text=element_text(family="Roboto")) +
      scale_alpha(range = c(max(0.45, min(unempAgeData()$highlight)),1)) +
      guides(alpha = FALSE) +
      labs(y=input$unempStatistic, 
           x="Age group", 
           fill = NULL,
           title=paste0(input$unempStatistic," by age group"))
    
    girafe(ggobj = unempAgePlot, height_svg = 7)
    
  })
  
}

shinyApp(ui, server)


