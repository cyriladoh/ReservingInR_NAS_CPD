#Author:
#Date:
#Purpose:

#load libraries and CSS
#source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/ReservingScripts/LoadData.R")
#source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/ReservingScripts/Premiums.R")
#source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/ReservingScripts/Claims.R")
#source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/ReservingScripts/Reserving_Model.R")

library(shiny)
library(shinythemes)
library(shinyjs)
library(shinydashboard)
library(scales)
library(ChainLadder)

header <-  dashboardHeader(disable = T)

sidebar <- dashboardSidebar(disable = T)
  
body <- dashboardBody(fluidPage(theme=shinytheme("cerulean"),
                                useShinyjs(),
  tags$style(type="text/css", "body {padding-top: 50px;}", ".help-block {color: red !important;}"),
  tags$head(HTML('<link rel="image_src", href="nas_logo.jpeg"/>')),
  # div(style="padding: 1px 0px; width: '100%'",
  #     titlePanel(title="", windowTitle="Reserving Model")
  # ),
  navbarPage("",position="fixed-top",collapsible = TRUE,
             source("Menu.R", local=TRUE)$value
  )
  ))

if(interactive()) { dashboardPage(header, sidebar, body) }