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
# library(EBImage)  # installation of fftw-devel on fedora required



# Define UI for application
ui <- fluidPage(
  img(src = "ETH_logo.jpg", height = 70, width = 200, align = "right"),
  
   # Application title
   titlePanel("Soil GHG Flux shiny tool"),
   
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
         numericInput("f.detect", "Enter the minimal detectable flux:", 0.0072, min = 0.001, max = 0.1),
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
       # conditionalPanel(
       #   condition = "is.null(input$read.input$datapath) == 'TRUE'",
       # h4("Diverfarming's Dataset"),
       # 
       # checkboxGroupInput("site","select Case Study sites:", c("CS09 - Trier DE" = "CS9",
       #                                                         "CS10 - Jakobsalash HU" = "CS10",
       #                                                         "CS11 - Villany HU" = "CS11"),
       #                    # ghg.file$site[1] = ,
       #                    selected = "CS10")
       # ),
       
       uiOutput("siteInput"),
       

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
                  # selectInput("gasflux.pic",
                  #             label = "Select an individual chamber flux:",
                  #             choices = c("Treatment",
                  #                         "Chamber",
                  #                         "Block",
                  #                         "Position"),
                  #             selected = "Treatment"),
                  plotOutput("pics"),
                  br(),
                  downloadButton("downloadGasfluxes", "Download"),
                  verbatimTextOutput("value"),
                  br(),
                  DT::dataTableOutput("plot.table2")
                  ),
         # ),
         
         tabPanel(p(icon("question-circle"), "Help"),
            includeMarkdown("help.Rmd")
         )  ## maybe not "hahahah"
         
         # fluidRow(column(3, verbatimTextOutput("value")))
         ) # end of tab panel
       
       
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
  output$siteInput <- renderUI({
    if(is.null(input$read.input$datapath) == TRUE) {
      ghg.file <- fread(paste0("data/gasflux-",input$gas.species,"_DiverFarming.csv"), sep = ";")
    } else {
      ghg.file <- fread(input$read.input$datapath,
                        header = T,
                        sep = ";")
      }
    sites <- unique(ghg.file$site)
    checkboxGroupInput("sites", "Choose sites", sites, selected = ghg.file$site[1])
  })
  
  get.flux.data <- reactive({
    
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
          # observeEvent(input$gasfluxes.go, {   # this is not working here
            
           ghg.file <- gasfluxes.get()
            # session$sendCustomMessage(type = 'testmessage',
            #                           message = 'eimalzwätschgezweimalzwätschgedrümalzwätschge... mach ma hüüüü')
           
          # }, once = T)

        }
      }
      if (input$ghgtask == "fluxvisual" & is.null(input$read.input$datapath) == TRUE){
        ghg.file <- fread(paste0("data/gasflux-",input$gas.species,"_DiverFarming.csv"), sep = ";")
        }

        # ghg.file  <- fread("/media/shiny-sae/gasflxvis/data/gasflux-CH4_DiverFarming.csv", sep = ";")

    return(ghg.file)
  })
  
    get.plot.data <- reactive({
      
        ghg.file <- get.flux.data()
        if (input$ghgtask == "gasfluxes") {
          ghg.file <- gasfluxes.get()
        }
          tryCatch(
            {
              ghg.file[ ,date.strp := as.POSIXct(date,format = "%Y-%m-%d")]
              # ghg.file <- ghg.file[is.finite(linear.f0),]
                ghg.file[is.na(robust.linear.f0), robust.linear.f0 := linear.f0]  # use linear for if only 3 datapoints are available
                ghg.file[is.na(HMR.f0),              HMR.f0 := robust.linear.f0]  # use robust linear if HMR could not be fitted
                
                ghg.file[, block := as.factor(block)] # blocks as factors
                ghg.file[, chamber.nr := as.factor(chamber.nr)] # chamber number as factor
                ghg.file[, sub_factor := as.factor(sub_factor)] # chamber number as factor
            })
        
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
                             "Further factor"     = "sub_factor")
                      
        # 
        # ghg.file.melted <- ghg.file.melt[site %in% input$site]
        ghg.file.melted <- ghg.file.melt[site %in% input$sites]
        
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
    # observeEvent(input$gasfluxes.go, {
      
    withProgress(message = ' Calculating gas fluxes. Please wait ...', {
        # n <- length(ghg.file$ID)
        # updateProgress <- function(detail = NULL) {
        #   progress$inc(amount = 1/n, detail = detail)
        # }
      flux.file <- wrap_gasfluxes(gc.samples = get.flux.data(), gas.species = input$gas.species)
    
    })
      return(flux.file)
      

      
    # }, once = T)
  })

  
  # m = c('toDateString', 'toLocaleDateString', 'toLocaleString', 'toUTCString')
  
  output$plot.table <- DT::renderDataTable(datatable(get.plot.data(), options = list(pageLength = 15,lengthChange=T)) 
                                           %>% formatSignif(c(5:8),3)
                                           %>% formatDate(1) )
  # if (input$ghgtask == "fluxvisual") {
  #   output$plot.table2 <- DT::renderDataTable(datatable(get.flux.data(), options = list(pageLength = 12,lengthChange=T))
  #                                             %>% formatSignif(c(4:8),3))
  # }
  
  gasfluxes.table <- reactive({
    if (input$ghgtask == "fluxvisual") gasflx.data <- get.flux.data()
    else if (input$ghgtask == "gasfluxes") gasflx.data <- gasfluxes.get()
    return(gasflx.data)
  })
  
  output$plot.table2 <- DT::renderDataTable(datatable(gasfluxes.table(), options = list(pageLength = 12,lengthChange=T))
                                            %>% formatSignif(c(4:8),3))
  
  # output$value <- renderPrint({input$plot.table2_rows_selected })
  
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
        ggplot(data = get.plot.data(), aes_string(x ="date.strp", y = "mean", colour = group.var2)) + 
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
        fwrite(get.plot.data(), file, row.names = FALSE, sep = ",")
      }

  )
  
  output$downloadGasfluxes <- downloadHandler(
    # content = function(file) {
    filename = function() {
      paste("GHG-gasfluxes_",input$gas.species,".csv", sep="")
    },
    content = function(file) {
      fwrite(gasfluxes.table(), file, row.names = FALSE, sep = ",")
    }
    
  )
  
  output$pics <- renderImage({
    # When input$n is 1, filename is ./images/image1.jpeg$
    
    # filename <- normalizePath(file.path('./pics',
    #                                     paste('28_04_18_CS11_18.png', sep='')))
    pics.select <- input$plot.table2_rows_selected
    pics.length <- length(pics.select)
    # filename <- NA
    # for(s in 1:pics.length) filename <- c(filename, normalizePath(file.path('./pics',
    #                                                                           paste0(ghg.file$ID[s],'.png', sep=''))))
    # img = readImage("/pics/pics/10_05_18_CS10_1.png", type = "png")
    # 
    #                                                   )) (filename[1],normalizePath(file.path('./pics',
    #                                         paste0(ghg.file$ID[pics.select],'.png', sep=''))) )
    if(is.null(input$plot.table2_rows_selected) == FALSE) {
   filename <- normalizePath(file.path('./pics',
                                        paste0(gasfluxes.get()$ID[pics.select[pics.length]],'.png', sep='')))
    # filename <- normalizePath(file.path('./pics',
    #                                          paste0(ghg.file$ID[c(2)],'.png', sep='')))

    # filename <- normalizePath(file.path("./pics/",
    #                                     paste0(ghg.file$ID[pics.select],".png", sep="")))
    # Return a list containing the filename
    # test <- combine(filename)
    # xt = tile(test, 4)
    list(src = filename)
    }
  }, deleteFile = FALSE) 
  
} # end server function



# Run the application 
shinyApp(ui = ui, server = server)
