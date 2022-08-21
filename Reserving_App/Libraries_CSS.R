library(shiny)
library(ChainLadder)
library(markdown)
library(shinydashboard)
library(copula)  #load the copula package
library(distr)  #distribution library
library(scatterplot3d)  #scatterplot3d - not always needed
library(matrixcalc)
library(shinythemes)
library(shinyLP)
library(shinyBS)
library(shinyjs)
library(scales) # used for adding commas as separators for numbers
library(DT) # for fancy datatables
library(ggplot2)  #  for good graphs

options(shiny.sanitize.errors = TRUE)
#Special CSS for loading page.
#appCSS <- "
#loading-content {
#position: absolute;
#background: #000000;
#opacity: 0.9;
#z-index: 100;
#left: 0;
#right: 0;
#height: 100%;
#text-align: center;
#color: #FFFFFF;
#}
#"