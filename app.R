###########################################################################
### Soil GreenHouseGas Flux Visualisation and calculation tool          ###
###########################################################################
## Author: Roman Hüppi
## Date : Oct. 2018
## Version: 0.95

# libraries ---------------------------------------------------------------
library(shiny)
library(ggplot2)
# library(ggthemes)
library(plotly)
library(data.table)
library(DT)
library(gasfluxes)
library(ggsignif)
source("wrap-gasfluxes.R")
# library(EBImage)  # installation of fftw-devel on fedora required

options(shiny.sanitize.errors = FALSE)

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
                                "Calculate gasfluxes" = "gasfluxes"),
                                # "Cumulative fluxes" = "cumflux"),
                    selected = 'fluxvisual'),
       
       radioButtons("gas.species", "GHG of your interest",
                    choices = c("Nitrous oxide" = "N2O",    # eval(c(expression("N"[2]*"O"))
                                "Carbon dioxide" = "CO2",
                                "Methane" = "CH4"),
                    selected = 'N2O'),
       radioButtons("separator","Input file separator", c("Comma" = ",","Semicolon" = ";"), 
                    selected = ",", inline=T),
        # Input: Select a file ----
       fileInput("read.input", "Upload input file:",
                 multiple = FALSE,
                 accept = c("text/csv",
                            "text/comma-separated-values,text/plain",
                            ".csv")),
       
 
       
       conditionalPanel(
         condition = "input.ghgtask == 'gasfluxes'",
         numericInput("f.detect", "Enter the minimal detectable flux:", 0.02, min = 0.001, max = 1.1),
         helpText("Use the 'minflxlim' shiny app to calculate f.detect:",
                  "https://sae-interactive-data.ethz.ch/minflxlim/"),
         
         actionButton("gasfluxes.go", "Estimate gasfluxes")
         ),
  
       # Horizontal line ----
       tags$hr(),
       
       # h4("Diverfarming's Dataset"),
       # 
       # checkboxGroupInput("site","select Case Study sites:", c("CS09 - Trier DE" = "CS9",
       #                                                         "CS10 - Jakobsalash HU" = "CS10",
       #                                                         "CS11 - Villany HU" = "CS11"),
       #                    # ghg.file$site[1] = ,
       #                    selected = "CS10")
       # ),
       h4("Visualisation options"),
       uiOutput("siteInput"),
       

       # })
       # tags$hr(),
       
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
       radioButtons("reg.method", "Choose a flux calculation method",
                    choices = c("dynamic kappa.max HMR" = "dynamic.kappa.f0",
                                "robust linear" = "robust.linear.f0",
                                "non-linear - HMR" = "HMR.f0"),
                    selected = 'dynamic.kappa.f0'),
       
       checkboxGroupInput("aggflux","Flux aggregation", c("none" = "no",
                                                    "cumulative" = "cumsum",
                                                    "aggregated" = "aggflx"),inline=T,
                    selected = "no")
                    
       
     ),  # end sidebarPanel
     
     # Main panel for displaying outputs ----
     mainPanel(
       
       tabsetPanel(
         tabPanel(p(icon("bar-chart"), "Timeline plots"), 
                  plotlyOutput("timelinePlot"),
                  plotlyOutput("cumPlot"),
                  plotOutput("aggPlot", width = "60%")), 
         
         # Data 
         tabPanel(p(icon("table"), "Plot Data"),
          # dataTableOutput(outputId="dTable")
          # verbatimTextOutput("value"),
          downloadButton("downloadData", "Download"),
          
          DT::dataTableOutput("plot.table")
                   
         ), #, end of "Dataset" tab panel
         
         # conditionalPanel(
           # condition = "input.ghgtask == 'gasfluxes'",
           
         tabPanel(p(icon("line-chart"), "Chamber details"),
                  # selectInput("gasflux.pic",
                  #             label = "Select an individual chamber flux:",
                  #             choices = c("Treatment",
                  #                         "Chamber",
                  #                         "Block",
                  #                         "Position"),
                  #             selected = "Treatment"),
                  plotOutput("pics"),
                  # plotOutput("pics"),
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
  img(src = "sae.png", height = 48, align = "left"),
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
      ghg.file <- fread(paste0("data/gasflux-",input$gas.species,"_DiverFarming.csv"))  # , sep = input$separator
    } else {
      ghg.file <- fread(input$read.input$datapath,
                        header = T,
                        sep = input$separator)
      }
    sites <- unique(ghg.file$site)
    checkboxGroupInput("sites", "Choose sites", sites, selected = ghg.file$site[2], inline=T)
  })
  
  # get.flux.data <- reactive({
    get.flux.data <- function(){
      
      if (input$ghgtask == "fluxvisual" & is.null(input$read.input$datapath) == TRUE){   # 
        ghg.file <- fread(paste0("data/gasflux-",input$gas.species,"_DiverFarming.csv")) #, sep = input$separator)
        # tryCatch(
        #   {
        #     ghg.file[ ,date.strp := as.POSIXct(date,format = "%Y-%m-%d")]
        #     # ghg.file <- ghg.file[is.finite(linear.f0),]
        #     ghg.file[is.na(robust.linear.f0), robust.linear.f0 := linear.f0]  # use linear for if only 3 datapoints are available
        #     ghg.file[is.na(HMR.f0),              HMR.f0 := robust.linear.f0]  # use robust linear if HMR could not be fitted
        #     
        #     ghg.file[, block := as.factor(block)] # blocks as factors
        #     ghg.file[, chamber.nr := as.factor(chamber.nr)] # chamber number as factor
        #     ghg.file[, sub_factor := as.factor(sub_factor)] # sub_factor as factor
        #   })
        # ghg.file
        }
      
      if(is.null(input$read.input$datapath) == FALSE){  #  == FALSE & input$ghgtask == "gasfluxes"
        # tryCatch(
          # {
            ghg.file <- fread(input$read.input$datapath,
                           header = T,
                           sep = input$separator
                           # quote = input$quote
                           )
      } # end if
      # else {
      #   stop()
      # }
      

    return(ghg.file)
  }  # end get.flux.data()
  
    get.plot.data <- reactive({
      
        ghg.file <- get.flux.data()
        if (input$ghgtask == "gasfluxes" & is.null(input$read.input$datapath) == FALSE) {  # 
          ghg.file <- gasfluxes.get()
        }
          tryCatch(
            {
              ghg.file[ ,date.strp := as.POSIXct(date,format = "%Y-%m-%d")]
              # ghg.file <- ghg.file[is.finite(linear.f0),]
              ghg.file[is.na(robust.linear.f0), robust.linear.f0 := linear.f0]  # use linear for if only 3 datapoints are available
              ghg.file[is.na(HMR.f0),              HMR.f0 := robust.linear.f0]  # use robust linear if HMR could not be fitted
# browser()
              ghg.file[, block := as.factor(block)] # blocks as factors
              ghg.file[, chamber.nr := as.factor(chamber.nr)] # chamber number as factor
              ghg.file[, sub_factor := as.factor(sub_factor)] # sub_factor as factor
            })
        
        
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
        setkey(ghg.file.melted, date.strp)
        # if (input$aggflux == "cumsum") {ghg.file.melted$N2O.flux <- cumsum(ghg.file.melted$N2O.flux) }
        
        ghg.file.plot <- ghg.file.melted[, .(mean = mean(N2O.flux),  # site == select.site
                                             sd = sd(N2O.flux,na.rm = TRUE),
                                             n = length(N2O.flux)),
                                         # , se = sd/sqrt(n)),
                                         by = c("date.strp","site","method", group.var2)]
        ghg.file.plot[, se := sd/sqrt(n)]
        ghg.file.plot[, cums := cumsum(mean), group.var2]

        ghg.file.plotsd <- ghg.file.melted[, .(mean = mean(N2O.flux),  # site == select.site
                                             # sd = sd(N2O.flux,na.rm = TRUE),
                                             n = length(N2O.flux)),
                                         # , se = sd/sqrt(n)),
                                         by = c("date.strp","site","method","chamber.nr", "treatment_fact1", "treatment_fact2", 
                                                "treatment_fact3", "block","sub_factor")]
        ghg.file.plotsd[, cums := cumsum(mean), "chamber.nr"]
        ghg.file.plotsd[, agg.flx := agg.fluxes(mean,date.strp, timeunit = "days"), by = "chamber.nr"]
        ghg.file.plot$cums.sd <- ghg.file.plotsd[, .(cums.sd = sd(cums)), by = c("date.strp","site","method", group.var2)]$cums.sd  # plugs in data for aggregation back in the the original dataframe
        ghg.file.plot[, cums.se := cums.sd/sqrt(n)]
        
        ghg.file.plot[, agg.flx  := agg.fluxes(mean,date.strp, timeunit = "days"), by = group.var2]  # use gasflux agg.fluxes function to aggregate daily fluxes
        
        # ghg.file.plot$agg.flx <- ghg.file.plotsd$agg.flx[1:length(ghg.file.plot$agg.flx)]  # knit the chamberwise results into the output table
       
        # ghg.file.merge <- ghg.file.plotsd[unique(agg.flx),.(agg.flx,treatment_fact1)]
        # 
        # merge(ghg.file.plot, ghg.file.merge, by = group.var2, all.x = TRUE, all.y = FALSE, allow.cartesian = TRUE)
        
        # ghg.file.plot <- ghg.file.plotsd[, .(mean = mean(N2O.flux),  # site == select.site
        #                                      sd = sd(N2O.flux,na.rm = TRUE),
        #                                      n = length(N2O.flux)),
        #                                  # , se = sd/sqrt(n)),
        #                                  by = c("date.strp","site","method", group.var2)]
        # 
        # ghg.file.plot[, sd.cums := sd(cumsum(N2O.flux)), chamber.nr]
        # ghg.file.plotsd[, .(agg.flx := agg.fluxes(mean,date.strp, timeunit = "days")), by = group.var2]
        
        # ghg.file.boxplot <- ghg.file.plot[, .(agg.flx = agg.fluxes(mean,date.strp, timeunit = "days")), by = c("date.strp","site","method", group.var2)]
        
        # agg.fluxes(ghg.file.plot$mean,ghg.file.plot$date.strp, timeunit = "hours")
        # setkey(ghg.file.plot, date.strp)
      return(ghg.file.plot)
      
      # })
  })
    
  get.agg.data <- function(){
    ghg.file <- get.flux.data()
    if (input$ghgtask == "gasfluxes" & is.null(input$read.input$datapath) == FALSE) {  # 
      ghg.file <- gasfluxes.get()
    }    # if(is.null(ghg.file$date.strp) == TRUE){

          ghg.file[ ,date.strp := as.POSIXct(date,format = "%Y-%m-%d")]
          # ghg.file <- ghg.file[is.finite(linear.f0),]
          ghg.file[is.na(robust.linear.f0), robust.linear.f0 := linear.f0]  # use linear for if only 3 datapoints are available
          ghg.file[is.na(HMR.f0),              HMR.f0 := robust.linear.f0]  # use robust linear if HMR could not be fitted
          ghg.file[, block := as.factor(block)] # blocks as factors
          ghg.file[, chamber.nr := as.factor(chamber.nr)] # chamber number as factor
          ghg.file[, sub_factor := as.factor(sub_factor)] # sub_factor as factor
          # }

    
    ghg.file.melt  <- melt(ghg.file, id.vars = c("date.strp","site","chamber.nr","treatment_fact1","block","treatment_fact2","treatment_fact3","sub_factor"), 
                           measure.vars = input$reg.method, value.name = "N2O.flux", variable.name = "method")
    
    ghg.file.melted <- ghg.file.melt[site %in% input$sites]
    setkey(ghg.file.melted, date.strp)
    
    ghg.file.plotsd <- ghg.file.melted[, .(mean = mean(N2O.flux),  # site == select.site
                                           # sd = sd(N2O.flux,na.rm = TRUE),
                                           n = length(N2O.flux)),
                                       # , se = sd/sqrt(n)),
                                       by = c("date.strp","site","method","chamber.nr", "treatment_fact1", "treatment_fact2", 
                                              "treatment_fact3", "block","sub_factor")]
    # ghg.file.plotsd[, cums := cumsum(mean), "chamber.nr"]
    # browser()
    
    ghg.file.plotsd[, agg.flx := agg.fluxes(mean,date.strp, timeunit = "hours"),
                    by = c("site","method","chamber.nr", "treatment_fact1", "treatment_fact2", 
                           "treatment_fact3", "block","sub_factor")]
    
    ghg.file.plotsd.boxpl <- ghg.file.plotsd[, .(agg.flx = mean(agg.flx)),  # site == select.site
                                           # sd = sd(N2O.flux,na.rm = TRUE),
                                       # , se = sd/sqrt(n)),
                                       by = c("site","method","chamber.nr", "treatment_fact1", "treatment_fact2", 
                                              "treatment_fact3", "block","sub_factor")]

    return(ghg.file.plotsd.boxpl)
    
  } # end get.agg.data function
      

  gasfluxes.get <- reactive({

    # observeEvent(input$gasfluxes.go, {
    input$gasfluxes.go
    withProgress(message = ' Calculating gas fluxes. Please wait ...', {
        # n <- length(ghg.file$ID)
        # updateProgress <- function(detail = NULL) {
        #   progress$inc(amount = 1/n, detail = detail)
        # }

      flux.file <- wrap_gasfluxes(gc.samples = get.flux.data(), gas.species = input$gas.species, f.detect = input$f.detect)

    })

    return(flux.file)
    # }, once = T)
  })


  gasfluxes.table <- reactive({
    if (input$ghgtask == "fluxvisual") {
      gasflx.data <- get.flux.data()
      gasflx.data <- fread("data/gasflux-N2O_DiverFarming-Rshiny.csv")
      gasflx.data[, c("ID","linear.f0.se","linear.f0.p","linear.C0","linear.AIC","linear.AICc","linear.RSE","linear.diagnostics",
                        "robust.linear.f0.se","robust.linear.f0.p","robust.linear.C0","robust.linear.weights","robust.linear.diagnostics",
                        "HMR.f0.se","HMR.f0.p","HMR.phi","HMR.AIC","HMR.AICc","HMR.RSE","day","month","year","f.detect.n2o","f.detect.co2","f.detect.ch4",
                        "t.meas","flux.se","flux.p","height [cm]","diameter [cm]") := NULL]
      setcolorder(gasflx.data,
                  c("site","date","treatment_fact1","treatment_fact2","treatment_fact3","sub_factor","block","chamber.nr",
                    "linear.f0","linear.r","robust.linear.f0","HMR.f0","HMR.kappa","kappa.max","dynamic.kappa.f0","method","HMR.diagnostics"))
    }
    
    else if (input$ghgtask == "gasfluxes") {
      gasflx.data <- gasfluxes.get()
      # # gasflx.data <- fread("data/gasflxvis_gasfluxes_example_input.csv")
      # Cols.chosen <- c("linear.f0.se","linear.f0.p","linear.C0","linear.AIC","linear.AICc","linear.RSE","linear.diagnostics",
      #                  "robust.linear.f0.se","robust.linear.f0.p","robust.linear.C0","robust.linear.weights","robust.linear.diagnostics",
      #                  "HMR.f0.se","HMR.f0.p","HMR.phi","HMR.AIC","HMR.AICc","HMR.RSE","t.meas","flux.se","flux.p")
      # gasflx.data[, (Cols.chosen) := NULL]
      # setcolorder(gasflx.data,
      #             c("site","date","treatment_fact1","treatment_fact2","treatment_fact3","sub_factor","block","chamber.nr",
      #               "CO2.TCD.mol","N2O.ECD.mol","CH4.FID.mol","chamber.A","chamber.V","vial.time","chamber.time"))
      
      gasflx.data[, c("ID","linear.f0.se","linear.f0.p","linear.C0","linear.AIC","linear.AICc","linear.RSE","linear.diagnostics",
                      "robust.linear.f0.se","robust.linear.f0.p","robust.linear.C0","robust.linear.weights","robust.linear.diagnostics",
                      "HMR.f0.se","HMR.f0.p","HMR.phi","HMR.AIC","HMR.AICc","HMR.RSE","day","month","year","f.detect.n2o","f.detect.co2","f.detect.ch4",
                      "t.meas","flux.se","flux.p","height [cm]","diameter [cm]") := NULL]
      setcolorder(gasflx.data,
                  c("site","date","treatment_fact1","treatment_fact2","treatment_fact3","sub_factor","block","chamber.nr",
                    "linear.f0","linear.r","robust.linear.f0","HMR.f0","HMR.kappa","kappa.max","dynamic.kappa.f0","method","HMR.diagnostics"))
      
    }
    return(gasflx.data)
  })
  
  
  output$plot.table <- DT::renderDataTable(datatable(get.plot.data(), options = list(pageLength = 15,lengthChange=T)) 
                                           %>% formatSignif(c(5:8),3)
                                           %>% formatDate(1) )
  
  output$plot.table2 <- DT::renderDataTable(datatable(gasfluxes.table(), options = list(pageLength = 15,lengthChange=T))
                                           %>% formatSignif(c(9:15),3))
  
  # output$value <- renderPrint({input$plot.table2_rows_selected })
  
  output$timelinePlot <- renderPlotly({
    if(any(input$aggflux == "no") == TRUE) {
      
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
    
    ggplotly(
      ggplot(data = get.plot.data(), aes_string(x ="date.strp", y = "mean", colour = group.var2)) + 
        geom_point(size = 0.95, show.legend = T) +
        # ggplot(data = ghg.file.plot, aes(x = date.strp, y = mean, colour = group.var)) + 
        geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2) +
        geom_line() +  #ylim(0,1) +
        # geom_smooth(method = "loess") +
        theme_light() +  # theme_gdocs() +   # theme_dark() + 
        # theme(legend.position="top") +  # this does not work in ggplotly
        ylab(paste0("flux [",gas.unit," m<sup>-2</sub> h<sup>-1</sub>]")) + xlab("date") + labs(colour = paste0(" grouped by:\n ",input$group.var,sep=""))  # 
      , height = 400, dynamicTicks = T) %>%layout(legend = list(orientation = "v",   # show entries horizontally
                                                                xanchor = "right" ))  # ) ,  # use center of legend as anchor
    
    } # end if
  })  # end tielinePlot
    
  output$cumPlot <- renderPlotly({
      
    if(any(input$aggflux == "cumsum") == TRUE) {

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
      ggplotly(
        ggplot(data = get.plot.data(), aes_string(x ="date.strp", y = "cums", colour = group.var2)) + 
          geom_point(size = 0.95, show.legend = T) +
          # ggplot(data = ghg.file.plot, aes(x = date.strp, y = mean, colour = group.var)) + 
          geom_errorbar(aes(ymin=cums-cums.se, ymax=cums+cums.se), width=.2) +
          geom_line() +  #ylim(0,1) +
          theme_light() +  # theme_gdocs() + 
          ylab(paste0("flux [",gas.unit," m<sup>-2</sub> h<sup>-1</sub>]")) + xlab("date") + labs(colour = paste0(" grouped by:\n ",input$group.var,sep=""))  # 
        , height = 400, dynamicTicks = T) %>%layout(legend = list(orientation = "v",   # show entries horizontally
                                                                  xanchor = "right" ))  # ) ,  # use center of legend as anchor
                                                       #  x = 0.5)  
    } # end if
  }) # end cumflux plot
    
  output$aggPlot <- renderPlot({
    
    if(any(input$aggflux == "aggflx") == TRUE)  {
      
    boxplot.data <- get.agg.data()
    group.var2 <- switch(input$group.var,
                         "Treatment factor 1" = "treatment_fact1",
                         "Treatment factor 2" = "treatment_fact2",
                         "Treatment factor 3" = "treatment_fact3",
                         "Chamber number"     = "chamber.nr" ,
                         "Block"              = "block")
    
    gas.unit <- switch(input$gas.species,
                       "N2O" = "mg N[2]O",
                       "CO2" = "g CO[2]" ,
                       "CH4" = "mg CH[4]")
    
    # eval(parse(text = group.var2))
      # ggplotly(
        ggplot(data = boxplot.data, aes_string(x = group.var2, y = "agg.flx", fill = group.var2)) + 
          theme_light() +  # theme_gdocs() + 
          geom_signif(comparisons = combn(unique(eval(parse(text = paste("boxplot.data$",group.var2)))), 2, simplify = FALSE),  #ghg.file.plotsd.boxpl list(unique(boxplot.data$group.var2)),
                      map_signif_level=TRUE) +
          ylab(paste0("aggregated emissions [mg N2O]")) + xlab("treatments") + # ",gas.unit,"
          # ylab(paste0(bquote('aggregated emissions [',gas.unit,']'))) +
          geom_boxplot()
        
      # , width = 300)
    } # end if aggflx
  } )  # end aggPlot
  
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
    #                                     paste('15-06-18_CS10_12_oat_c2_interrow_2_50.png', sep='')))
    # # pics.select <- input$plot.table2_rows_selected
    pics.select <- input$plot.table2_row_last_clicked
    # pics.length <- length(pics.select)
    # filename <- NA
    # for(s in 1:pics.length) filename <- c(filename, normalizePath(file.path('./pics',
    #                                                                           paste0(ghg.file$ID[s],'.png', sep=''))))
    # img = readImage("/pics/pics/10_05_18_CS10_1.png", type = "png")
    # 
    #                                                   )) (filename[1],normalizePath(file.path('./pics',
    #                                         paste0(ghg.file$ID[pics.select],'.png', sep=''))) )
    # if(is.null(input$plot.table2_rows_selected) == FALSE) {
   # filename <- normalizePath(file.path('./pics',
   #                                      paste0(gasfluxes.get()$ID[pics.select[pics.length]],'.png', sep='')))
    
   # pic.date <- as.Date(gasfluxes.get()$date[pics.select],format = "%d-%m-%y")
    gasflux.pics <- gasfluxes.get()
    
   filename <- normalizePath(file.path('./pics', paste0(gasflux.pics$date[pics.select],'_',  # paste0('./pics_',input$gas.species)
                                                        gasflux.pics$site[pics.select],'_',
                                                        gasflux.pics$chamber.nr[pics.select],'_',
                                                        gasflux.pics$treatment_fact1[pics.select],'_',
                                                        gasflux.pics$treatment_fact2[pics.select],'_',
                                                        gasflux.pics$treatment_fact3[pics.select],'_',
                                                        gasflux.pics$block[pics.select],'_',
                                                        gasflux.pics$sub_factor[pics.select],
                                              '.png', sep='')))

    list(src = filename)
    # }
  }, deleteFile = FALSE) 
  
} # end server function



# Run the application 
shinyApp(ui = ui, server = server)
