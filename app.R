library(tidyverse)
library(readxl)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinyWidgets)
library(shinyjqui)
library(DT)
library(survival)
library(survminer)
library(arsenal)

##### GLOBALS #####
CATEGORIC <- c("GRAUDIF", "T", "N", "M", "SUBTYPE")
CONTINUOS <- c("ERBB2", "RE_PER", "RP_PER", "KI67", "EDAT")

####################### UI INTERFACE #######################################
ui <- dashboardPage(
  ########## Header  ########## 
  dashboardHeader(title= "ANALYSING CANCER STAGES", titleWidth = 320),
  
  ########## Sidebar #########
  dashboardSidebar(
    useShinyjs(),
    width = 320,
    sidebarMenu(
      # if we want to put a pseudo-responsive height
      # tags$head(tags$style("#subtype{height:70vh !important;}")),
      id="sidebar",
      menuItem("Upload & Settings", tabName = "upload", icon = icon("upload")),
      menuItem("Plots & Data", tabName = "plots_data", icon = icon("chart-bar")),
      dateRangeInput("diag_date", "Please select diagnosis date: ", max = Sys.Date()),
      uiOutput("age"),
      checkboxInput("if_x", "X axis", FALSE),
      uiOutput("X"),
      # selectInput("x_axis", "Select value for x axis: ", list(
      #   Categoric=Categoric(),
      #   Continuos=Continuos())
      #   ),
      checkboxInput("if_y", "Y axis", FALSE),
      uiOutput("Y"),
      # selectInput("y_axis", "Select value for y axis: ", c("KI67", "EDAT", "RE_PER", "RP_PER")),
      checkboxInput("if_col", "Grouping Variable", FALSE)#,
      # selectInput("colours", "Select group by: ", c( "GRAUDIF", "ESTADI2", "T", "N", "M"))
      )
    
  ),
  ######## Body #############
  dashboardBody(
    
    conditionalPanel(
      condition = "input.sidebar == 'upload'",
      fileInput("inputExcel", HTML("Choose your file"), width = "50%"),
      uiOutput("input_columns")
    ),
    
    conditionalPanel(
      condition = "input.sidebar == 'plots_data'",
      box(id="checks", width = 12, uiOutput("stage_select")),
      tabsetPanel(type="tabs",
                  tabPanel("Plots",
                           fluidRow(
                             column(6, 
                                    tableOutput("tableby"),
                                    uiOutput("downTable_by"),
                                    br(),
                                    
                                    DT::dataTableOutput("dt"),
                                    downloadButton('downloadDT', 'Download DataTable',
                                                   class="btn btn-success")),
                             column(6, 
                                    plotOutput("plot_area", height=600),
                                    downloadButton('downloadPlot', 'Download Plot',
                                                   class="btn btn-success")))),
                  tabPanel("Surv",
                           fluidRow(
                             box(plotOutput("curvsurv", height=600),
                                 downloadButton('downloadCurve', 'Download Curve Plot'))))
      )
      
    )
  )
)

#################### SERVER ########################

server <- function(input, output, session) {

  Continuos <- reactive({
    req(my_data())
    lis <- input$box_continuos
    lis
  })

  Categoric <- reactive({
    req(my_data())
    lis <- input$box_categoric
    lis
  })

  output$X <- renderUI({
    req(my_data(), input$if_x)
    Categoric <- input$box_categoric
    Continuos <- input$box_continuos
    selectInput("x_axis", "Select value for x axis: ", 
                choices=list("Categoric"=Categoric, "Continuos"=Continuos))
  })
  
  output$Y <- renderUI({
    req(my_data(), input$if_y)
    Categoric <- input$box_categoric
    Continuos <- input$box_continuos
    selectInput("y_axis", "Select value for y axis: ", 
                choices=list("Categoric"=Categoric, "Continuos"=Continuos))
  })
  
  output$input_columns <- renderUI({
    req(my_data())
    fluidRow(
      column(width = 4,
             pickerInput(inputId = "box_categoric", label = "Categoric Parameters ",
                         choices= colnames(my_data()), multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
             ),
      column(width = 4,
             pickerInput(inputId = "box_continuos", label = "Continuos Parameters ",
                         choices= colnames(my_data()), multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
             ),
      column(width = 4,
             pickerInput(inputId = "box_date", label = "Date Parameters ",
                         choices= colnames(my_data()), multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
             )
      )
    })
  
  #jqui_resizable('#plot_area,#curvsurv', options = list(aspectRatio = TRUE))
  
  
  ####### Hide checkboxs and download button ###########
  
  observe({
    if(input$sidebar== 'plots_data' && !is.null(input$inputExcel)){
      shinyjs::show("if_x")
      shinyjs::show("if_y")
      shinyjs::show("checks")
      shinyjs::show("age")
      shinyjs::show("downloadDT")
      shinyjs::show("diag_date")
      shinyjs::show("if_col")
    } else {
      shinyjs::hide("if_x")
      shinyjs::hide("if_y")
      shinyjs::hide("checks")
      shinyjs::hide("age")
      shinyjs::hide("downloadDT")
      shinyjs::hide("diag_date")
      shinyjs::hide("if_col")
    }
  })
  
  ###### Hide x_Axis ############
  observe({
    if(input$if_x==TRUE){
      shinyjs::show("x_axis")
      shinyjs::show("downloadPlot")
    } else {
      shinyjs::hide("x_axis")
      shinyjs::hide("downloadPlot")
    }
  })
  
  ###### Hide y_Axis ############
  observe({
    if(input$if_y==TRUE){
      shinyjs::show("y_axis")
    } else {
      shinyjs::hide("y_axis")
    }
  })
  
  ###### Hide col ############
  observe({
    if(input$if_col){
      shinyjs::show("colours")
    } else {
      shinyjs::hide("colours")
    }
  })
  
  ###### Picker Options #########
  output$stage_select <- renderUI({
    req(my_data())
    tagList(
      column(3,
             pickerInput(inputId = "stage", label = "Cancer Stage: ", choices = levels(factor(my_data()$ESTADI2)),
                         multiple = TRUE, options = pickerOptions(actionsBox = TRUE),
                         selected = levels(factor(my_data()$ESTADI2)))
             ),
      column(3,
             pickerInput(inputId = "tumor", label = "Primary Tumor: ", choices = levels(factor(my_data()$T)),
                         multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
             ),
      column(3,
             pickerInput(inputId = "nodes", label = "Nodes: ", choices = levels(factor(my_data()$N)),
                         multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
      ),
      column(3,
             pickerInput(inputId = "metastasis", label = "Metastasis: ", choices = levels(factor(my_data()$M)),
                         multiple = TRUE, options = pickerOptions(actionsBox = TRUE))
      )
    )
  })
  
  ##### Hide download Table_by button #####
  output$downTable_by <- renderUI({
    req(my_data())
    downloadButton('downloadTable', 'Download Table', class="btn btn-success")
  })
  
  ##### Update Checkbox ##########
  observe({
    updateDateRangeInput(session, "diag_date", start=as.Date(min(my_data()$DATA_DIAG, na.rm = TRUE)),
                         end = as.Date(max(my_data()$DATA_DIAG, na.rm = TRUE)), 
                         min=as.Date(min(my_data()$DATA_DIAG, na.rm = TRUE)),
                         max = as.Date(max(my_data()$DATA_DIAG, na.rm = TRUE)))
  })
  
  ##### Age slider ####
  output$age <- renderUI({
    req(my_data())
    min1 <- min(my_data()$EDAT, na.rm = TRUE)
    max1 <- max(my_data()$EDAT, na.rm = TRUE)
    sliderInput("age", "Age", min = min1, max = max1,
                value = c(min1,max1))
  })
  
  # function to transform columns to numeric
  to.numeric <- function(df, x){
    df %>% 
      mutate_at(x, funs(as.numeric))
  }
  
  # function to transform columns to factor
  to.factor <- function(df,x){
    df %>% 
      mutate_at(x, funs(as.factor))
  }
  
  ######## Importing and formatting data   ######## 
  my_data <<- reactive({
    req(input$inputExcel)
    
    my_data <- read_excel(input$inputExcel$datapath)
    
    my_data <- to.numeric(my_data, CONTINUOS)
    
    my_data <- my_data %>%
      mutate_at(c("GRAUDIF"), funs(replace(., . %in% c(0,4,8,9), NA)))
    
    my_data <- my_data %>% 
      mutate("diffdays" = difftime(DATA_UC, DATA_DIAG, units=c("days")))
    
    my_data <- my_data %>% mutate(ESTAT_UC = case_when(ESTAT_UC=="E"~1,
                                                       TRUE ~0))
    
    my_data <- mutate(my_data, "SUBTYPE" = case_when(ERBB2 == 3 ~ "HER2",
                                                     RE_PER >= 1 | RP_PER >= 1 ~ "Luminal",
                                                     ERBB2 %in% c(0,1,4) ~ "TNBC",
                                                     TRUE ~ toString(NA)))
    
    my_data <- my_data %>% 
      mutate(T = case_when(str_detect(T, "1") ~ "1",
                           str_detect(T, "2") ~ "2",
                           str_detect(T, "3") ~ "3",
                           str_detect(T, "4") ~ "4",
                           str_detect(T, "0") ~ "0")
      ) %>% 
      mutate(N = case_when(str_detect(N, "1") ~ "1",
                           str_detect(N, "2") ~ "2",
                           str_detect(N, "3") ~ "3",
                           str_detect(N, "0") ~ "0")
      ) %>% 
      mutate(M = case_when(str_detect(M, "1") ~ "1",
                           str_detect(M, "0") ~ "0")
      )
    
    my_data <- to.factor(my_data, CATEGORIC)
    
    my_data
    
  })
  
  #### Surv fit object ####
  fit1 <- reactive({
    req(input$if_col)
    i <- input$colours
    formula <- as.formula(paste("Surv(diffdays, ESTAT_UC)", i, sep=" ~ "))
    fit <- survfit(formula, data = my_data())
    fit$call$formula <- formula
    fit
  })
  
  
  ##### filtering the dataframe by input in checkbox (stage)   ##### 
  
  # input stage is mandatory 
  # note: in case of needing to remove rows with na, see "complete.cases"
  filtered_data <- reactive ({
    req(input$stage)
    req(my_data())
    df <- my_data() %>% # filters, default = all
      filter(DATA_DIAG>=input$diag_date[1] & DATA_DIAG<=input$diag_date[2]) %>% # by date
      filter(EDAT>=input$age[1] & EDAT<=input$age[2]) %>% # by age
      filter(ESTADI2 %in% input$stage) # by cancer stage
    
    if(!is.null(input$tumor)){
      df <- df %>% filter(T %in% input$tumor)
    }
    if(!is.null(input$nodes)){
      df <- df %>% filter(N %in% input$nodes)
    }
    if(!is.null(input$metastasis)){
      df <- df %>% filter(M %in% input$metastasis)
    }
    df
  })
  
  #####  Filtering for data table   ##### 
  filtered_table <- reactive ({
    req(filtered_data(), input$stage)
    d<- filtered_data()
    d <- d %>% 
      group_by(EDAT, T, N, M, GRAUDIF, SUBTYPE, ESTADI2) %>% 
      summarize(Total =n())
  })
  
  
  ########### PLOT FUNCTIONS  ########### 
  
  ##### Generic plot function ######
  # Kind of plot "process request"
  # depending of user actions, it will decide
  # what kind of graph to plot
  
  plotGraphs <- reactive({
    req(filtered_data())
    data <- filtered_data()
    x1 <- input$x_axis
    y1 <- input$y_axis
    col <- input$colours
    
    if(input$if_y==FALSE){                 # only X axis
      if(input$if_col==FALSE){             # no de-aggregation
        df <- data %>%
          filter(!!ensym(x1) != "NA") %>% 
          group_by(!!ensym(x1)) %>% 
          summarise(counts=n())
        plot_bar(df, x1)
      } 
      else {                               # with aggregation
        df <- data %>% 
          filter(!!ensym(x1) != "NA") %>% 
          drop_na(!!ensym(col)) %>%
          group_by(!!ensym(x1), !!ensym(col)) %>% 
          summarise(counts=n())
        plot_bar(df,x1,col)
      }
    } else {                               # x and y axis
      if(input$if_col==FALSE){             # no de-aggregation
        df <- data %>% 
          filter(!!ensym(x1) != "NA") %>% 
          drop_na(!!ensym(y1)) %>%
          group_by(!!ensym(x1), !!ensym(y1))
        plot_xy(df, x1, y1)
      } else {                             # with aggregation
        df <- data %>% 
          filter(!!ensym(x1) != "NA") %>% 
          drop_na(!!ensym(y1), !!ensym(col)) %>%
          group_by(!!ensym(x1), !!ensym(y1), !!ensym(col))
        plot_xy(df, x1, y1, col)
      }
    }
  })
  
  #### Bar plot function #####
  
  plot_bar <- function(data, x1, col){
    if (missing(col)){
      g <- ggbarplot(data = data, x = x1, y="counts", fill=x1)
    } else {
      g <- ggbarplot(data = data, x = x1, y="counts", fill=col)
    }
    return(g)
  }
  
  #### Box and scatter plot function #####
  
  plot_xy <- function(data,x1,y1, col){
    if(missing(col)){
      if(x1 %in% Categoric()){
        g <- ggboxplot(data, x=x1, y=y1, fill=x1) + stat_compare_means()
      }
      else {
        g <- ggscatter(data, x=x1, y=y1, color=x1, add="reg.line", conf.int = TRUE,cor.coef = TRUE)
      }
    } else {
      if(x1 %in% Categoric()){
        g <- ggboxplot(data, x=x1, y=y1, fill=col) + stat_compare_means()
      }
      else {
        g <- ggscatter(data, x=x1, y=y1, color = col, add="reg.line", conf.int = TRUE,cor.coef = TRUE)
      }
    }
    return(g)
  }
  
  #### Tableby function #####
  # depending of user inputs, tableby will be generated
  # one wayr or another: 
  # 1. if x and y axis are continuos, doesn't do anything
  # 2. if no axis, uses both Continuos and Categoric as variables and no group
  # 3. If only x axis, variable = x
  # 4. if x in Categoric and y in Continuos, group = y, variable = x
  
  plotTableBy <- function(){
    f <- filtered_data()
    Continuos <- Continuos()
    Categoric <- Categoric()
    y <- input$y_axis
    x <- input$x_axis
    
    if(x %in% Continuos && y %in% Continuos)
      return()
    
    if(!input$if_x && !input$if_y){
      tab1 <- f %>%
        select(!!Continuos, !!Categoric) %>%
        tableby(~., .)
      return(tab1)
    }
    
    if(input$if_x && !input$if_y){
      tab1 <- tableby(update(~., reformulate(x)), data=f)
      return(tab1)
    }
    
    if(x %in% Categoric && y %in% Continuos){
      tab1 <-  tableby(update(.~., reformulate(y,x)), data=f)
      return(tab1)
    }
  }
  
  #### plotCurve() ####
  # function for plotting surfvfit object with min and max values
  # Since there is only one plot, I didn't find
  # useful to make a "process request" separation-like layer
  
  plotCurve <- reactive({
    req(fit1())
    min_y <- min(fit1()$surv)
    max_y <- max(fit1()$surv)
    g <- ggsurvplot(fit1(), pval=TRUE, risk.table = TRUE)
    g
  })
  
  ########### RENDERS ###########
  
  ##### Plot ##### 
  output$plot_area <- renderPlot({
    req(filtered_data(), input$if_x)
    plotGraphs()
  })
  
  #####  Tableby ######
  output$tableby <- renderTable({
    req(filtered_data())
    result <- plotTableBy()
    as.data.frame(summary(result, text="html"))
  }, striped = TRUE, bordered = TRUE, sanitize.text.function = function(x) x)
  
  #####  Datatable   ##### 
  output$dt <- DT::renderDataTable(
    filtered_table()
  )
  
  #### Survfit curve ######
  output$curvsurv <- renderPlot({
    plotCurve()
  })

  
  ########################  DOWNLOAD BUTTONS ##############################
  
  ##### Download plot #####
  output$downloadPlot <- downloadHandler(
    filename = "plot.png",
    content = function(file) {
      ggsave(file, plot = plotGraphs(), device = "png")
    }
  )
  
  ##### Download Curve plot ######
  # Warining!
  # ggsurvplot() returns a list of ggplots containing survival curves and 
  # optionally the risk table. You can't directly save the list using ggsave(),
  # but you can save the output of print(ggsurvplot)
  
  output$downloadCurve <- downloadHandler(
    filename = "curve-plot.png",
    content = function(file) {
      ggsave(file, plot = print(plotCurve()), device = "png")
    }
  )
  
  ##### Download Table By ##### 
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      result <- as.data.frame(plotTableBy())
      result <- print(result, sanitize.text.function = NULL)
      write.csv(result, file, row.names=FALSE)
    },
    contentType = "csv"
  )
  
  ##### Download Datatable ##### 
  output$downloadDT <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), ".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(filtered_table(), file, row.names=FALSE)
    }
  )
}

shinyApp(ui, server)