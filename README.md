# Network-Visualization Tool

The tool has been developed in R using the `shiny` package. In addition to `shiny`, we require the following packages installed:

* igraph: to plot the networks, calculate network centrality measures

  * to install igraph type `install.packages('igraph')` in the R command prompt

* rcolorbrewer: to color the nodes in the network (in the code, this is done on the basis of industry; this can be changed)

  * to install rcolorbrewer, type `install.packages('RColorBrewer')` in the R command prompt

* shinysky: provides an autocomplete textbox, which is not included in shiny

  * to install shinysky, you need to first install a library called devtools by typing `install.packages('devtools')`. Once devtools in installed type in the following command `devtools::install_github('AnalytixWare/ShinySky')`
