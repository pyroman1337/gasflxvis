###########################################################################
### Soil GreenHouseGas Flux Visualisation and calculation tool          ###
###########################################################################
## Author: Roman Hüppi
## Date : August 2018
## Version: 0.2


# libraries ---------------------------------------------------------------

library(shiny)
library(ggplot2)
# library(ggthemes)
library(plotly)
library(data.table)
library(DT)
library(gasfluxes)
source("wrap-gasfluxes.R")



# Define UI for application
ui <- fluidPage(
  img(src = "ETH_logo.jpg", height = 70, width = 200, align = "right"),
  
   # Application title
   titlePanel("Soil GHG Flux Visualisation"),
   
   # Sidebar layout with a input and output definitions ----
   sidebarLayout(
     
     # Sidebar panel for inputs ----
     sidebarPanel(
       
       radioButtons("ghgtask", "Select a task:",
                    choices = c("Visualise flux dataset"     = "fluxvisual",    # eval(c(expression("N"[2]*"O"))
                                "Calculate gasfluxes (beta)" = "gasfluxes"),
                    selected = 'fluxvisual'),
       
       radioButtons("gas.species", "GHG of your interest",
                    choices = c("Nitrous oxide" = "N2O",    # eval(c(expression("N"[2]*"O"))
                                "Carbon dioxide" = "CO2",
                                "Methane" = "CH4"),
                    selected = 'N2O'),
      
        # Input: Select a file ----
       fileInput("read.input", "Upload input file:",
                 multiple = FALSE,
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
       conditionalPanel(
         condition = "input.ghgtask == 'gasfluxes'",
         numericInput("f.detect", "Enter the minimal detectable flux:", 0.0072, min = 0.001, max = 0.01),
         helpText("When you click the button below,",
                  "the gasfluxes script by Roland Fuss will go crazy",
                  "to calculate whatever you have always dreamed of! :-O"),
         actionButton("gasfluxes.go", "Estimate gasfluxes")
         ),
  
       # Horizontal line ----
       tags$hr(),
       
       # dateRangeInput("dates", label = h4("Select date range")),
       # # Input: Select quotes ----
       # observe({
       h4("Diverfarming's Dataset"),
       
       checkboxGroupInput("site","select Case Study sites:", c("CS09 - Trier DE" = "CS9",
                                                               "CS10 - Jakobsalash HU" = "CS10",
                                                               "CS11 - Villany HU" = "CS11"),selected = "CS10"),

       # })
       tags$hr(),
       
       selectInput("group.var",
                   label = "Grouping variable",
                   choices = c("Treatment factor 1",
                               "Treatment factor 2",
                               "Treatment factor 3",
                               "Chamber number",
                               "Block",
                               "Sub factor"),
                   selected = "Treatment factor 1"),

       
       # # Input: Select quotes ----
       radioButtons("reg.method", "choose a flux calculation method",
                    choices = c("dynamic kappa.max HMR" = "dynamic.kappa.f0",
                                "robust linear" = "robust.linear.f0",
                                "non-linear - HMR" = "HMR.f0"),
                    selected = 'dynamic.kappa.f0')
       
     ),  # end sidebarPanel
     
     # Main panel for displaying outputs ----
     mainPanel(
       
       tabsetPanel(
         tabPanel(p(icon("bar-chart"), "Graph"), br(), plotlyOutput("timelinePlot")), 
         
         # Data 
         tabPanel(p(icon("table"), "Dataset"),
          # dataTableOutput(outputId="dTable")
          # verbatimTextOutput("value"),
          downloadButton("downloadData", "Download"),
          
          DT::dataTableOutput("plot.table")
                   
         ), #, end of "Dataset" tab panel
         
         # conditionalPanel(
           # condition = "input.ghgtask == 'gasfluxes'",
           
         tabPanel(p(icon("line-chart"), "Gasfluxes"),
                  selectInput("gasflux.pic",
                              label = "Select an individual chamber flux:",
                              choices = c("Treatment",
                                          "Chamber",
                                          "Block",
                                          "Position"),
                              selected = "Treatment"),
                  plotOutput("pics"),
                  # br(),
                  DT::dataTableOutput("plot.table2")
                  ),
         # ),
         
         tabPanel(p(icon("question-circle"), "Help"),
            br(),
            h4(strong("File upload for flux visualisation")),
            # br(),
            h5("The input table should contain the following columns:"),
            h5(strong("chamber.nr:"), "This is a factor that contains a lable for each individual chamber"),
            h5("linear.f0: linear least square regression estimate of the flux"),
            h5("robust.linear.f0: robust linear flux estimate (by gasfluxes"),
            h5("HMR.f0: HMR estmate	dynamic.kappa.f0	date	treatment_fact1	treatment_fact2	treatment_fact3	block	sub_factor
            ")
         
         )  ## maybe not "hahahah"
         
         # fluidRow(column(3, verbatimTextOutput("value")))
         ) # end of "Visualize the Data" tab panel
       
       
     ) # end mainPanel
   ),  # end sidebarLayout
  img(src = "DiverFarming_logo.jpg", height = 150, align = "right")
  
)    # end fluidPage



# Define server logic to read selected file ----
server <- function(input, output) {
  
  # ghg.file <- reactive({
  #   data <- fread(paste0("data/gasflux-",input$gas.species,"_DiverFarming.csv"),sep = ";")
  #   return(data)
  # })

  getdata <- reactive({
    
      #   file.read <- NULL
      #   return(NULL)}

      if(is.null(input$read.input$datapath) == FALSE){
        tryCatch(
          {
            ghg.file <- fread(input$read.input$datapath,
                           header = T,
                           sep = ";"
                           # quote = input$quote
            )
          },
          error = function(e) {
            # return a safeError if a parsing error occurs
            stop(safeError(e))
          }
        )
        if (input$ghgtask == "gasfluxes"){
          observeEvent(input$gasfluxes.go, {
            
            gasfluxes.get()
            # session$sendCustomMessage(type = 'testmessage',
            #                           message = 'eimalzwätschgezweimalzwätschgedrümalzwätschge... mach ma hüüüü')
          }, once = T)
          
        }
      }
      if (input$ghgtask == "fluxvisual"){
          if(input$gas.species == "N2O") ghg.file <- fread(paste0("data/gasflux-",input$gas.species,"_DiverFarming.csv"), sep = ";")
          if(input$gas.species == "CO2") ghg.file <- fread(paste0("data/gasflux-",input$gas.species,"_DiverFarming.csv"), sep = ";")
          if(input$gas.species == "CH4") ghg.file <- fread(paste0("data/gasflux-",input$gas.species,"_DiverFarming.csv"), sep = ";")
      }
      # # file2 = input$file2
      # if (is.null(file.read)) {
      #   return(NULL)
      # } else{
        # ghg.file  <- fread("/media/shiny-sae/gasflxvis/data/gasflux-CH4_DiverFarming.csv", sep = ";")
        # data2 = read.csv(file2$datapath)
        # output$plot <- renderPlot({
        # plot(data1[,1],data2[,2])
        ghg.file[ ,date.strp := as.POSIXct(date,format = "%Y-%m-%d")]
        # ghg.file <- ghg.file[is.finite(linear.f0),]
        ghg.file[is.na(robust.linear.f0), robust.linear.f0 := linear.f0]  # use linear for if only 3 datapoints are available
        ghg.file[is.na(HMR.f0),              HMR.f0 := robust.linear.f0]  # use robust linear if HMR could not be fitted
        
        ghg.file[, block := as.factor(block)] # blocks as factors
        ghg.file[, chamber.nr := as.factor(chamber.nr)] # chamber number as factor
        
        # ghg.file[ ,treatment1L := substr(treatment,1,1)]
        # ghg.file[ ,block := substr(treatment,2,2)]
      # 
      # }
        ghg.file.melt  <- melt(ghg.file, id.vars = c("date.strp","site","chamber.nr","treatment_fact1","block","treatment_fact2","treatment_fact3","sub_factor"), 
                               measure.vars = input$reg.method, value.name = "N2O.flux", variable.name = "method")
        
        group.var2 <- switch(input$group.var,
                             "Treatment factor 1" = "treatment_fact1",
                             "Treatment factor 2" = "treatment_fact2",
                             "Treatment factor 3" = "treatment_fact3",
                             "Chamber number"     = "chamber.nr" ,
                             "Block"              = "block",
                             "Sub factor"         = "sub_factor")
                      
        # 
        ghg.file.melted <- ghg.file.melt[site %in% input$site]
        ghg.file.plot <- ghg.file.melted[, .(mean = mean(N2O.flux),  # site == select.site
                                             sd = sd(N2O.flux,na.rm = T),
                                             n = length(N2O.flux)),
                                         # , se = sd/sqrt(n)),
                                         by = c("date.strp","site","method", group.var2)]
        ghg.file.plot[, se := sd/sqrt(n)]
        
      return(ghg.file.plot)
      
      # })
  })

  gasfluxes.get <- reactive({

    withProgress(message = ' Calculating gas fluxes. Please wait ...', {
        # n <- length(ghg.file$ID)
        # updateProgress <- function(detail = NULL) {
        #   progress$inc(amount = 1/n, detail = detail)
        # }
      ghg.file <- wrap_gasfluxes(gc.samples = ghg.file, gas.species = input$gas.species)
    }) 
    return(ghg.file)
  })

  
  m = c('toDateString', 'toLocaleDateString', 'toLocaleString', 'toUTCString')
  
  output$plot.table <- DT::renderDataTable(datatable(getdata(), options = list(pageLength = 15,lengthChange=T)) 
                                           %>% formatSignif(c(5:8),3)
                                           %>% formatDate(1) )
  
  output$plot.table2 <- DT::renderDataTable(ghg.file)
  
  output$value <- renderPrint({input$plot.table2_rows_selected })
  
  output$timelinePlot <- renderPlotly({
    
    # ghg.file <- as.data.table(ghg.file)
    # ghg.file <- isolate(getdata())
    # ghg.file <- getdata()
    # ghg.file <- fread(paste0("data/gasflux-",input$gas.species,"_DiverFarming.csv"),sep = ";")
    group.var2 <- switch(input$group.var,
                         "Treatment factor 1" = "treatment_fact1",
                         "Treatment factor 2" = "treatment_fact2",
                         "Treatment factor 3" = "treatment_fact3",
                         "Chamber number"     = "chamber.nr" ,
                         "Block"              = "block")
    
    gas.unit <- switch(input$gas.species,
                       "N2O" = "mg N<sub>2</sub>O",
                       "CO2" = "g CO<sub>2</sub>" ,
                       "CH4" = "mg CH<sub>4</sub>")
    
    # print(
      ggplotly(
        ggplot(data = getdata(), aes_string(x ="date.strp", y = "mean", colour = group.var2)) + 
          geom_point(size = 0.95, show.legend = T) +
        # ggplot(data = ghg.file.plot, aes(x = date.strp, y = mean, colour = group.var)) + 
          geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2) +
          geom_line() +  #ylim(0,1) +
          # geom_smooth(method = "loess") +
          # theme_gdocs() +  theme_dark() +
          # theme(legend.position="top") +  # this does not work in ggplotly
          ylab(paste0("flux [",gas.unit," m<sup>-2</sub> h<sup>-1</sub>]")) + xlab("date") + labs(colour = paste0("grouped by:\n ",input$group.var,sep=""))
        , height = 555, dynamicTicks = T)  # )
  } )  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    # content = function(file) {
        filename = function() {
          paste("GHG-plotdata_",input$gas.species,".csv", sep="")
        },
        content = function(file) {
        fwrite(ghg.file.plot, file, row.names = FALSE, sep = ";")
      }

  )
  
  output$pics <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg$
    # filename <- normalizePath(file.path('pics/21_03_17_CS9_1.png'))
    filename <- normalizePath(file.path('./pics',
                                        paste('21_03_18_CS9_1.png', sep='')))
    pics.select = input$plot.table2_rows_selected
    # filename <- normalizePath(file.path("./pics/",
    #                                     paste0(ghg.file$ID[pics.select],".png", sep="")))
    # Return a list containing the filename
    list(src = filename)
  }, deleteFile = FALSE) 
  
} # end server function



# Run the application 
shinyApp(ui = ui, server = server)
