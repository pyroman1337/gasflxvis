# gasflxvis Rshiny App

This shiny app has been developed by the Sustainable Agroecosystem Group at ETH Zurich (http://www.sae.ethz.ch/). The live version can be found here: https://sae-interactive-data.ethz.ch/gasflxvis

The app was created for DiverFarming (www.diverfarming.eu) in order to visualise field GHG data for the project partners that send their field GHG samples to our lab in Zurich. For the processing of the GC data to following workflow has been used: https://bitbucket.org/pyro1337/ghg-gc-data-processing

Available functions:

### Soil GHG flux visualisation

* timeline plots, aggregated by different treatment factor, single chambers or block
* cumulative fluxes on timeline (adding up all measurements without interpolation)
* aggregated boxplot (interpolated daily aggregate) incl. significance t-test
* view and download data table behind the time line plot

### gasfluxes calculation

* upload raw data from GC
* calculate linear and non-linear flux estimates (gasfluxes package by Roland Fuss is applied: https://bitbucket.org/ecoRoland/gasfluxes)
* select fluxes according to minimum detectable flux (see  https://doi.org/10.1371/journal.pone.0200876)
* check chamber details to verify how the different models fit
* view and download data table of the flux calculation results (this table can be uploaded for later visualisation again)

refer to help tab in the app for more details

contact the author for feedback (roman.hueppi@usys.ethz.ch)
