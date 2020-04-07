# install.packages(c("rsconnect", "shiny", "ggplot2", "scales", "shinythemes", "tidyverse",
# "shinyWidgets", "ggrepel", "itertools", "ggiraph", "maps", "ggsci", "mapcan", "rapport"))

require(rsconnect)
require(shiny)
require(ggplot2)
require(scales)
require(shinythemes)
require(tidyverse)
require(shinyWidgets)
require(ggrepel)
require(itertools)
require(ggiraph)
require(maps)
require(ggsci)
require(mapcan)
require(rapport)

# read in unemployment data
unempData <- read.csv("data/unempFinalData.csv", head=T, sep=",")
unempData$GEO <- factor(unempData$GEO, levels=c("Canada", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC"))
unempData$Age.group <- factor(unempData$Age.group, levels=c("15 years and over","15 to 24 years","25 to 54 years","55 years and over"))
unempData$Sex <- factor(unempData$Sex, levels = c("Both sexes", "Males", "Females"))
unempData$refPeriod <- as.Date(unempData$refPeriod)

# read in employment data
empData <- read.csv("data/empFinalData.csv", head=T, sep=",")
empData$GEO <- factor(empData$GEO, levels=c("Canada", "NL", "PE", "NS", "NB", "QC", "ON", "MB", "SK", "AB", "BC"))
empData$Age.group <- factor(empData$Age.group, levels=c("15 years and over","15 to 24 years","25 to 54 years","55 years and over"))
empData$Sex <- factor(empData$Sex, levels = c("Both sexes", "Males", "Females"))
empData$refPeriod <- as.Date(empData$refPeriod)


ui <- fluidPage(
  theme = shinytheme("paper"),
  tags$style(
    HTML(
      ".navbar-default .navbar-brand {color: #212121;}
      .navbar-default .navbar-brand:hover {color: #212121;}"
    )
  ),
  navbarPage(
    "Canada’s Labour Market",
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
                               "Number unemployed by reason (x1,000)" = c("Job leavers",
                                                                          "Own illness or disability",
                                                                          "Personal or family reasons",
                                                                          "Going to school",
                                                                          "Dissatisfied",
                                                                          "Retired",
                                                                          "Other reasons",
                                                                          "Jobs losers",
                                                                          "Permanent layoff",
                                                                          "Temporary layoff")),
                             selected = "Number unemployed (x1,000)"
                 ),
                 sliderInput("unempRefPeriod",
                             label = "Select reference period:",
                             min = min(unempData$refPeriod),
                             max = max(unempData$refPeriod),
                             value = c(as.Date("2000-01-01"), max(unempData$refPeriod)),
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
                 ),
                 actionButton("updateUnemp",
                              label = "Apply changes",
                              width = "100%",
                              class = "btn btn-primary btn-custom")
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
                     div(
                       style = "display: inline-block;vertical-align:top;",
                       actionButton("updateYearsUnemp",
                                    label = "Update plot",
                                    class = "btn btn-primary btn-sm btn-custom"
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
                   column(4, wellPanel(p("This dashboard lets you visualize information from Statistics Canada’s 
                                         Labour Force Survey."),
                                       p("Use the tabs across the top to focus on either Employment or on 
                                           Unemployment, and then use the left panel to choose a statistic and 
                                           time period, as well as geographic and demographic breakdowns. Hover over 
                                           the images to see specific data values."),
                                       p("See the April 3rd, 2020 post on ",tags$a(href="https://wp.me/p1Ydsu-2fS", "MilesCorak.com")," for definitions and 
                                           interpretations of the data. Feedback is welcome ",tags$a(href="https://twitter.com/MilesCorak", "@MilesCorak"),".")))
                 )
               )
             )),
    tabPanel("Employment",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("empStatistic",
                             label = "What are you interested in?",
                             choices = list(
                               "Number employed (x1,000)",
                               "Employment rate, seasonally adjusted",
                               "Number employed three months or less (x1,000)",
                               "Number employed by industry (x1,000)" = c("Goods-producing sector",
                                                                          "Agriculture [111-112, 1100, 1151-1152]",
                                                                          "Forestry, fishing, mining, quarrying, oil and gas [21, 113-114, 1153, 2100]",
                                                                          "Utilities [22]",
                                                                          "Construction [23]",
                                                                          "Manufacturing [31-33]",
                                                                          "Services-producing sector",
                                                                          "Wholesale and retail trade [41, 44-45]",
                                                                          "Transportation and warehousing [48-49]",
                                                                          "Finance, insurance, real estate, rental and leasing [52-53]",
                                                                          "Professional, scientific and technical services [54]",
                                                                          "Business, building and other support services [55-56]",
                                                                          "Educational services [61]",
                                                                          "Health care and social assistance [62]",
                                                                          "Information, culture and recreation [51, 71]",
                                                                          "Accommodation and food services [72]",
                                                                          "Other services (except public administration) [81]",
                                                                          "Public administration [91]")
                               ),
                             selected = "Number employed (x1,000)"
                 ),
                 sliderInput("empRefPeriod",
                             label = "Select reference period:",
                             min = min(empData$refPeriod),
                             max = max(empData$refPeriod),
                             value = c(as.Date("2000-01-01"), max(empData$refPeriod)),
                             timeFormat = "%b %Y"
                 ),
                 selectInput("empGeo",
                             label = "Select geography:",
                             choices = c("Canada" = "Canada","Newfoundland and Labrador" = "NL","Prince Edward Island" = "PE","Nova Scotia" = "NS",
                                         "New Brunswick" = "NB","Quebec" = "QC","Ontario" = "ON","Manitoba" = "MB","Saskatchewan" = "SK","Alberta" = "AB",
                                         "British Columbia" = "BC"),
                             selected = "Canada"
                 ),
                 selectInput("empAge",
                             label = "Select age group:",
                             choices = c("15 years and over","15 to 24 years","25 to 54 years","55 years and over"),
                             selected = "15 years and over"
                 ),
                 selectInput("empSex",
                             label = "Select sex:",
                             choices = c("Both sexes", "Males", "Females"),
                             selected = "Both sexes"
                 ),
                 actionButton("updateEmp",
                              label = "Apply changes",
                              width = "100%",
                              class = "btn btn-primary btn-custom")
               ),
               mainPanel(
                 width = 9,
                 fluidRow(
                   style = "display: flex; align-items: center;",
                   column(5, girafeOutput("empMap")),
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
                           "empByGeo",
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
                           "empBySex",
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
                           "empByAge",
                           label = NULL,
                           choices = c("15 years and over","15 to 24 years","25 to 44 years","45 years and over")
                         )
                       )
                     ),
                     div(
                       style = "display: inline-block;vertical-align:top;",
                       actionButton("updateYearsEmp",
                                    label = "Update plot",
                                    class = "btn btn-primary btn-sm btn-custom"
                       )
                     ),
                     tags$style(
                       ".btn-custom {-webkit-box-shadow: none; box-shadow: none; position: relative;}"
                     ),
                     girafeOutput("empYears")
                   )
                 ),
                 br(),
                 fluidRow(
                   style = "display: flex; align-items: center;",
                   column(4, girafeOutput("empAgePlot")),
                   column(4, girafeOutput("empSexPlot")),
                   column(4, wellPanel(p("This dashboard lets you visualize information from Statistics Canada’s 
                                         Labour Force Survey."),
                                       p("Use the tabs across the top to focus on either Employment or on 
                                         Unemployment, and then use the left panel to choose a statistic and 
                                         time period, as well as geographic and demographic breakdowns. Hover over 
                                         the images to see specific data values."),
                                       p("See the April 3rd, 2020 post on ",tags$a(href="https://wp.me/p1Ydsu-2fS", "MilesCorak.com")," for definitions and 
                                         interpretations of the data. Feedback is welcome ",tags$a(href="https://twitter.com/MilesCorak", "@MilesCorak"),".")))
                                       )
               )
             )
    )
  )
)
  
             

server <- function(input, output, session) {
  
  ### unemployment plots
  output$unempMap <- renderGirafe({
    input$updateUnemp
    isolate({unempMapData <- subset(unempData, Statistics == input$unempStatistic  &
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
      theme(legend.position = "bottom", axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(), panel.spacing = element_blank(),
            legend.box.spacing = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
            text=element_text(family="Roboto")) +
      scale_fill_material("blue", name=paste(strwrap(paste(input$unempStatistic),width=25), collapse="\n")) +
      labs(title= paste(strwrap(paste0(input$unempStatistic,", ",format(input$unempRefPeriod[2], "%b %Y")), 
                                       width = 45), collapse = "\n"))
    
    girafe(ggobj = unempMapPlot, width_svg = 5)})
    
  })
  
  # Plot of statistic over reference period
  output$unempYears <- renderGirafe({
    (input$updateUnemp | input$updateYearsUnemp)
    isolate({if (is.null(input$unempByGeo) & is.null(input$unempByAge) & is.null(input$unempBySex)) 
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
                                                                                        colour=interaction(unempYearsData[,byVars] ,drop=T, sep=", ")), size = 1.25)
                          else geom_point_interactive(aes(x=refPeriod, y=VALUE, tooltip=paste0(format(refPeriod, "%b %Y"), ": ", VALUE), colour=Statistics), size = 1.25)} +
                        theme_classic() + scale_y_continuous(labels = comma) +
                        {if (!rapportools::is.empty(byVars)) guides(colour = "legend") else guides(colour=F)} +
                        scale_colour_jco() +
                        theme_classic() + theme(text=element_text(family="Roboto"),legend.position = "bottom") +
                        labs(y=paste(strwrap(paste(input$unempStatistic),width=35), collapse="\n"), 
                             x="Reference period", 
                             title = paste(strwrap(paste0(input$unempStatistic," by year"), width = 75), collapse = "\n"),
                             colour=NULL) 
    
    girafe(ggobj = unempYearsPlot, width_svg = 8, height_svg = 4)})
    
  })

  unempSexData <- reactive ({
    input$updateUnemp
    
    isolate({unempData$highlight <- ifelse((unempData$Sex == input$unempSex), 1, ifelse((input$unempSex == "Both sexes"),1,0))
    
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
    })
  
  output$unempSexPlot <- renderGirafe({
    input$updateUnemp
    isolate({unempSexPlot <- ggplot(unempSexData()) + 
                      geom_col_interactive(aes(x=Sex, y=VALUE, alpha = highlight, fill=month, tooltip=VALUE), position="dodge") + 
                      theme_classic() + scale_fill_jco() + theme(text=element_text(family="Roboto"),legend.position = "bottom") +
                      scale_alpha(range = c(max(0.45, min(unempSexData()$highlight)),1)) +
                      guides(alpha = FALSE) +
                      labs(y=paste(strwrap(paste(input$unempStatistic),width=35), collapse="\n"), 
                           x="Sex",
                           fill = NULL,
                           title = paste(strwrap(paste0(input$unempStatistic," by sex, ",
                                                        format(input$unempRefPeriod[2], "%b %Y")), width = 40), collapse = "\n"))
    
    girafe(ggobj = unempSexPlot, height_svg = 5, width_svg = 4)})
  })
  
  unempAgeData <- reactive ({
    input$updateUnemp
    isolate({unempData$highlight <- ifelse((unempData$Age.group == input$unempAge), 1, ifelse((input$unempAge == "15 years and over"),1,0))
    
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
    })
  
  output$unempAgePlot <- renderGirafe({
    input$updateUnemp
    isolate({unempAgePlot <- ggplot(unempAgeData()) + 
                geom_col_interactive(aes(x=Age.group, y=VALUE, alpha = highlight, fill=month, tooltip=VALUE), position = "dodge") + 
                theme_classic() + scale_fill_jco() + theme(text=element_text(family="Roboto"),legend.position = "bottom") +
                scale_alpha(range = c(max(0.45, min(unempAgeData()$highlight)),1)) +
                guides(alpha = FALSE) +
                labs(y=paste(strwrap(paste(input$unempStatistic),width=35), collapse="\n"), 
                     x="Age group", 
                     fill = NULL,
                     title = paste(strwrap(paste0(input$unempStatistic," by age group, ",
                                                  format(input$unempRefPeriod[2], "%b %Y")), width = 40), collapse = "\n"))
              
              girafe(ggobj = unempAgePlot, height_svg = 5, width_svg = 4)})

  })
  
  
  ### employment drop down menus
  observe({
    stat <- input$empStatistic
    
    # Can use character(0) to remove all choices
    if (!(stat %in% c("Number employed (x1,000)", 
                      "Employment rate, seasonally adjusted",
                      "Number employed three months or less (x1,000)"))) {
    
    # update select menus in side bar
    updateSelectInput(session, "empSex",
                      label = "Select sex:",
                      choices = "Both sexes",
                      selected = "Both sexes")
    
    updateSelectInput(session, "empAge",
                      label = "Select age group:",
                      choices = "15 years and over",
                      selected = "15 years and over")
    
    # update select menus in years plot
    updateCheckboxGroupInput(session, "empBySex",
                      label = "Select sex:",
                      choices = "Both sexes",
                      selected = NULL)
    
    updateCheckboxGroupInput(session, "empByAge",
                      label = "Select age group:",
                      choices = "15 years and over",
                      selected = NULL)
    }
    
    else {
      updateSelectInput(session, "empSex",
                        label = "Select sex:",
                        choices = c("Both sexes", "Males", "Females"),
                        selected = "Both sexes")
      
      updateSelectInput(session, "empAge",
                        label = "Select age group:",
                        choices = c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over"),
                        selected = "15 years and over")
      
      updateCheckboxGroupInput(session, "empBySex",
                               label = "Select sex:",
                               choices = c("Both sexes", "Males", "Females"),
                               selected = NULL)
      
      updateCheckboxGroupInput(session, "empByAge",
                               label = "Select age group:",
                               choices = c("15 years and over", "15 to 24 years", "25 to 54 years", "55 years and over"),
                               selected = NULL)
      
    }
  })
  
  ### employment plots
  output$empMap <- renderGirafe({
    input$updateEmp
    isolate({empMapData <- subset(empData, Statistics == input$empStatistic  &
                                      substr(refPeriod,0,7) == substr(input$empRefPeriod[2],0,7) &
                                      Sex == input$empSex &
                                      Age.group == input$empAge &
                                      GEO != "Canada")
    
    empMapData$GEO <- as.character(empMapData$GEO)
    
    empMapData <- dplyr::left_join(mapcan(boundaries = province, type = standard),
                                     empMapData,
                                     by = c("pr_alpha" = "GEO"))
    
    empMapPlot <- ggplot() +
      geom_polygon_interactive(data=empMapData, 
                               aes(x = long, y = lat, group = group, 
                                   fill=VALUE, tooltip=paste(pr_alpha, VALUE))) +
      coord_fixed() +
      theme(legend.position = "bottom", axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(), panel.spacing = element_blank(),
            legend.box.spacing = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
            text=element_text(family="Roboto")) +
      scale_fill_material("blue", name=paste(strwrap(paste(input$empStatistic),width=25), collapse="\n")) +
      labs(title= paste(strwrap(paste0(input$empStatistic,", ",format(input$empRefPeriod[2], "%b %Y")), 
                                width = 45), collapse = "\n"))
    
    girafe(ggobj = empMapPlot, width_svg = 5)})
    
  })
  
  # Plot of statistic over reference period
  output$empYears <- renderGirafe({
    (input$updateEmp | input$updateYearsEmp)
    isolate({if (is.null(input$empByGeo) & is.null(input$empByAge) & is.null(input$empBySex)) 
      empYearsData <- subset(empData, Statistics == input$empStatistic &
                                 substr(refPeriod,0,7) >= substr(input$empRefPeriod[1],0,7) &
                                 substr(refPeriod,0,7) <= substr(input$empRefPeriod[2],0,7) &
                                 Age.group == input$empAge &
                                 Sex == input$empSex &
                                 GEO == input$empGeo)
    else empYearsData <- subset(empData, Statistics == input$empStatistic &
                                    substr(refPeriod,0,7) >= substr(input$empRefPeriod[1],0,7) &
                                    substr(refPeriod,0,7) <= substr(input$empRefPeriod[2],0,7) &
                                    {if (is.null(input$empByAge)) Age.group == input$empAge
                                      else Age.group %in% input$empByAge} &
                                      {if (is.null(input$empBySex)) Sex == input$empSex
                                        else Sex %in% input$empBySex} &
                                        {if (is.null(input$empByGeo)) GEO == input$empGeo
                                          else GEO %in% input$empByGeo})
    
    # only show selected variables in the interaction legend
    byVarsBool <- c(!is.null(input$empByGeo),
                    !is.null(input$empByAge),
                    !is.null(input$empBySex))
    
    byVars <- c("GEO", "Age.group", "Sex")
    
    byVars <- byVars[unlist(byVarsBool)]
    
    empYearsPlot <- ggplot(empYearsData) + 
    {if (!rapportools::is.empty(byVars[1])) geom_line(aes(x=refPeriod, y=VALUE, colour=interaction(empYearsData[,byVars], drop=T, sep=", ")), size=1) 
      else geom_line(aes(x=refPeriod, y=VALUE, colour=Statistics), size=1)} +
      {if (!rapportools::is.empty(byVars[1])) geom_point_interactive(aes(x=refPeriod, y=VALUE, tooltip=paste0(format(refPeriod, "%b %Y"), ": ", VALUE),
                                                                         colour=interaction(empYearsData[,byVars] ,drop=T, sep=", ")), size = 1.25)
        else geom_point_interactive(aes(x=refPeriod, y=VALUE, tooltip=paste0(format(refPeriod, "%b %Y"), ": ", VALUE), colour=Statistics), size = 1.25)} +
      theme_classic() + scale_y_continuous(labels = comma) +
      {if (!rapportools::is.empty(byVars)) guides(colour = "legend") else guides(colour=F)} +
      scale_colour_jco() +
      theme_classic() + theme(text=element_text(family="Roboto"),legend.position = "bottom") +
      labs(y=paste(strwrap(paste(input$empStatistic),width=35), collapse="\n"), 
           x="Reference period", 
           title = paste(strwrap(paste0(input$empStatistic," by year"), width = 75), collapse = "\n"),
           colour=NULL) 
    
    girafe(ggobj = empYearsPlot, width_svg = 8, height_svg = 4)})
    
  })
  
  empSexData <- reactive ({
    input$updateEmp
    
    isolate({empData$highlight <- ifelse((empData$Sex == input$empSex), 1, ifelse((input$empSex == "Both sexes"),1,0))
    
    thismonth <- input$empRefPeriod[2]
    lastmonth <- seq(input$empRefPeriod[2], length=2, by=("-1 month"))[2]
    lastyear <- seq(input$empRefPeriod[2], length=2, by=("-1 year"))[2]
    
    empData$month <- ifelse(substr(empData$refPeriod,0,7) == substr(thismonth,0,7), "This month", 
                              (ifelse(substr(empData$refPeriod,0,7) == substr(lastmonth,0,7), "Last month", "Last year")))
    
    empData$month <- factor(empData$month,levels=c("This month", "Last month", "Last year"))
    
    return(subset(empData,
                  GEO == input$empGeo &
                    Sex != "Both sexes" &
                    Age.group == input$empAge &
                    substr(refPeriod,0,7) %in% c(substr(c(thismonth, lastmonth, lastyear), 0, 7)) &
                    Statistics == input$empStatistic))})
  })
  
  output$empSexPlot <- renderGirafe({
    input$updateEmp
    isolate({empSexPlot <- ggplot(empSexData()) + 
      geom_col_interactive(aes(x=Sex, y=VALUE, alpha = highlight, fill=month, tooltip=VALUE), position="dodge") + 
      theme_classic() + scale_fill_jco() + theme(text=element_text(family="Roboto"),legend.position = "bottom") +
      scale_alpha(range = c(max(0.45, min(empSexData()$highlight)),1)) +
      guides(alpha = FALSE) +
      labs(y=paste(strwrap(paste(input$empStatistic),width=35), collapse="\n"), 
           x="Sex",
           fill = NULL,
           title = paste(strwrap(paste0(input$empStatistic," by sex, ",
                                        format(input$empRefPeriod[2], "%b %Y")), width = 40), collapse = "\n"))
    
    girafe(ggobj = empSexPlot, height_svg = 5, width_svg = 4)})
  })
  
  empAgeData <- reactive ({
    input$updateEmp
    isolate({empData$highlight <- ifelse((empData$Age.group == input$empAge), 1, ifelse((input$empAge == "15 years and over"),1,0))
    
    thismonth <- input$empRefPeriod[2]
    lastmonth <- seq(input$empRefPeriod[2], length=2, by=("-1 month"))[2]
    lastyear <- seq(input$empRefPeriod[2], length=2, by=("-1 year"))[2]
    
    empData$month <- ifelse(substr(empData$refPeriod,0,7) == substr(thismonth,0,7), "This month", 
                              (ifelse(substr(empData$refPeriod,0,7) == substr(lastmonth,0,7), "Last month", "Last year")))
    
    empData$month <- factor(empData$month,levels=c("This month", "Last month", "Last year"))
    
    return(subset(empData,
                  GEO == input$empGeo &
                    Sex == input$empSex &
                    Age.group != "15 years and over" &
                    substr(refPeriod,0,7) %in% c(substr(c(thismonth, lastmonth, lastyear), 0, 7)) &
                    Statistics == input$empStatistic))})
  })
  
  output$empAgePlot <- renderGirafe({
    input$updateEmp
    isolate({empAgePlot <- ggplot(empAgeData()) + 
      geom_col_interactive(aes(x=Age.group, y=VALUE, alpha = highlight, fill=month, tooltip=VALUE), position = "dodge") + 
      theme_classic() + scale_fill_jco() + theme(text=element_text(family="Roboto"),legend.position = "bottom") +
      scale_alpha(range = c(max(0.45, min(empAgeData()$highlight)),1)) +
      guides(alpha = FALSE) +
      labs(y=paste(strwrap(paste(input$empStatistic),width=35), collapse="\n"), 
           x="Age group", 
           fill = NULL,
           title = paste(strwrap(paste0(input$empStatistic," by age group, ",
                                        format(input$empRefPeriod[2], "%b %Y")), width = 40), collapse = "\n"))
    
    girafe(ggobj = empAgePlot, height_svg = 5, width_svg = 4)})
    
  })
  
}

shinyApp(ui, server)


