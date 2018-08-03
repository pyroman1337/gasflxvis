###########################################################################
# Flux calculation wrapper for shiny gasfluxes -------------------#-#-#-#-#
###########################################################################
## this function wrap "GC-samples-outp..." files around to be fed to GHG vis shiny app
# 
# gc.samples  <- read.table(paste(savingDir,"/GC-samples-outp_",folder.name,"_fixed.csv",sep=""),sep=";",header=T)  # this can be used if the "GC-samples-outp" was manipulated manually

wrap_gasfluxes <- function(gc.samples, treatment1 = "comment", gas.species){
  
  gas.concentration <- switch(gas.species,
                         "N2O" = gc.samples$N2O.ECD.mol,
                         "CO2" = gc.samples$CO2.TCD.mol ,
                         "CH4" = gc.samples$CH4.FID.mol)
  gas.molarweight <- switch(gas.species,
                         "N2O" = (2 * 14 + 16),
                         "CO2" = (12 + 2 * 16),
                         "CH4" = (12 + 4 * 1))
  
  #### prepare table for HMR/gasfluxes input  ####
  ## specify flux units here !! ###
  n.seq <- length(gc.samples$sample.nr) # getting the number of samples
  hmr.input <- as.data.table(matrix(NA, nrow=n.seq, ncol=5))  # prepare data frame for the output
  names(hmr.input) <- c("ID","V","A","time","C")
  hmr.input$ID   <- gc.samples$sample.nr          # the unique flux identifier created above
  hmr.input$V    <- gc.samples$chamber.V          # [m^3] also chamber height only could be inserted here because the flux is calculated as V/A
  hmr.input$A    <- gc.samples$chamber.A          # [m^2] hence A could chosen equal 1 and chamber volume could be set as chamber height
  hmr.input$time <- gc.samples$vial.time / 3600   # [h] choose time unit to calculate the flux (sec,min,hour according to input in the file)
  
  
  # N2O ECD flux calculation #
  ### please choose the unit for the gas you want to use for the flux (mass or mol: i.e. g[N2O-N]/(m^2)=> N2O.ECD.N or mol/L => N2O.ECD.mol)
  # hmr.input$C    <- gc.samples$N2O.ECD.mol * 3600 * 28 * 10000 * 1E-9    ## g-N2O-N / ha / h   for Ben: * 3600 (in hours) * 28 (in ng-N2O) * 10000 (per hectare)  
  hmr.input$C    <-  gas.concentration * gas.molarweight    * 1E-6    ## mg-N2O / m2 / h    DiverFarming unit suggested by Kristiina Regina 
  # write.table(hmr.input, paste(savingDir,"/gasflux-input-N2O_",folder.name,".csv",sep=""), sep=";", dec=".",row.names=F,col.names=T, append=F)   # writes data from GC extration to output file
  # hmr.input      <- na.omit(hmr.input)     # remove input without values
  
  gasflux.output <- gasfluxes(na.omit(hmr.input[order(ID,time)]),methods = c("linear", "robust linear", "HMR")
                              ,k_HMR= log(1.5),plot = T)   # use gasflux package by Roland to actually calculate the fluxes with different models
  
  gasflux.backup <- gasflux.output  # just in case you have to restart from a fresh gasfluxes output
  # translate chamber ID back to its component and add f.detect  --------
  
  flux.ID          <- strsplit(as.character(gasflux.output$ID) ,"_") # colsplit(fluxes$serie,"_",names=c("...","..","."))
  flux.ID.frame    <- data.table(t(sapply(flux.ID,c)),stringsAsFactors = FALSE)  #,c
  setnames(flux.ID.frame, c("day","month","year","site","chamber.nr"))
  flux.ID.frame$ID <- gasflux.output$ID
  site.fdetect     <- data.table(site = c("CS9","CS10","CS11"), 
                                 f.detect.n2o = c(0.0072,0.0027,0.0027),
                                 f.detect.co2 = c(0.01,  0.003, 0.003 ),
                                 f.detect.ch4 = c(0.007, 0.0025,0.0025))
  flux.ID.frame  <- merge(flux.ID.frame,site.fdetect,by=c("site"),sort = F)
  tmeas.table    <- as.data.table(gc.samples)[, .(t.meas = max(vial.time)/3600), by = sample.nr]
  setnames(tmeas.table,"sample.nr","ID")   
  flux.ID.frame  <- merge(flux.ID.frame, tmeas.table, sort =F, by = "ID")
  
  gasflux.output <- merge(gasflux.output,flux.ID.frame, sort = F, by = "ID")
  
  # gasflux.select <- selectfluxes(gasflux.output, select = "kappa.max", f.detect = 0.0027, t.meas = 0.5)
  gasflux.select <- selectfluxes(gasflux.output, select = "kappa.max", f.detect = gasflux.output$f.detect.n2o, t.meas = gasflux.output$t.meas)
  setnames(gasflux.select,"flux","dynamic.kappa") # rename output flux column to kappa max dynamic selection
  ## additional flux selection algorithms could be inserted here
  # gasflux.output <- merge(gasflux.output,gc.samples, by.x="ID", by.y="sample.nr",all.x=T)
  # add covariables to output #
  
  # 
  # flux.ID.frame$X5       <- as.numeric(flux.ID.frame$X5)
  gasflux.meta <- gasflux.select[,c("ID")]
  gasflux.meta$date <- as.IDate(strptime(paste(flux.ID.frame$day,flux.ID.frame$month,flux.ID.frame$year),format="%d %m %y"))
  gasflux.meta$time <- gc.samples$chamber.time[gc.samples$vial.time == 0]    # as.ITime(strptime(paste(flux.ID.frame$X3),format="%H"))
  gasflux.meta$site <- flux.ID.frame$site
  gasflux.meta$chamber.nr  <- as.numeric(flux.ID.frame$chamber.nr)  # 
  
  
  input.chamber <- fread("data/DiverFarming_chamber.input.csv",sep = ";",header = T)
  
  gasflux.meta <- merge(gasflux.meta,input.chamber,by=c("chamber.nr","site"))
  
  gasflux.select[, chamber.nr := as.numeric(chamber.nr), ]
  gasflux.select <- merge(gasflux.select,gasflux.meta,by=c("ID","site","chamber.nr"), no.dubs = T)
  gasflux.select[is.na(robust.linear.f0), robust.linear.f0 := linear.f0]  # if no robust lin.; use linear
  gasflux.select[is.na(HMR.f0), HMR.f0 := robust.linear.f0]   # if no HMR, use robust lin.
  
  ## maybe the output can be reduced at this point
  # write.table(gasflux.select, paste(savingDir,"/gasflux-output-N2O_",folder.name,".csv",sep=""), sep=";", dec=".",row.names=F,col.names=T, append=F)   # writes data from GC extration to output file
  write.table(gasflux.select, paste("data/gasflux-",gas.species,"_DiverFarming-Rshiny.csv",sep = ""), sep=";", dec=".",row.names=F,col.names=T, append=F)   # writes data from GC extration to output file
  # write.table(gasflux.select, "/home/hueppir/naslocal/GC Tools/ghg_gc_script/shinyGHG_FluxVis/data/gasflux-N2O_DiverFarming.csv", sep=";", dec=".",row.names=F,col.names=T, append=F)   # writes data from GC extration to output file
  return(gasflux.select) # return calculated data
}
