#Author:
#Date:
#Purpose:

#load libraries and CSS
source("Libraries_CSS.R")$value
#source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/HealthReserving/ConnectAccessDB.R")
#source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/HealthReserving/Premiums.R")
#source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/HealthReserving/ClaimsTriangle.R")
#source("~/Desktop/ReservingInR_NAS_CPD/Reserving_App/HealthReserving/Health_Reserving_Model.R")
library(shiny)
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
           #  source("Menu_Intro.R", local=TRUE)$value,
             source("Menu_Health.R", local=TRUE)$value,
            # source("Menu_NonLife.R", local=TRUE)$value,
            # source("Menu_Life.R", local=TRUE)$value,
            # source("Menu_ResultSummary.R", local=TRUE)$value,
             #source("Menu_Help.R", local=TRUE)$value,
             #source("Menu_QuitReboot.R", local=TRUE)$value
  )
  ))

if(interactive()) { dashboardPage(header, sidebar, body) }