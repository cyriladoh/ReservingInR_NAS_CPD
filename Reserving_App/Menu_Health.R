tabPanel("Health Reserving",icon=icon("medkit"),
         fluidPage(
           fluidRow(
             column(3,
                    wellPanel(id="ctrlPanel1",
                              wellPanel(
                                radioButtons("prd_selector", label = "Health Plans",
                                             choices = list("Product1" = 1, "Product2" = 2), 
                                             selected = NULL),
                              ),
                              wellPanel(
                                radioButtons("ldf_selector", label = "Loss Development Factor",
                                             choices = list("Simple Average" = 1, "Weighted Average (WA)" = 2, "WA Latest 12months" = 3, "WA Latest 24months" = 4), 
                                             selected = 2),
                              ),
                              wellPanel(
                                selectInput("ielr_s", label = "Update IELR Assumption For Cohorts",
                                             choices = list("Dec-2021" = 0,"Nov-2021"=1,"Oct-2021"=2, "Sept-2021"=3), 
                                             selected = 0),
                                         textInput("ielr_input", "Enter IELR for XX: ", 0.8)
                                
                              ),
                             
                              fluidRow(actionButton("ResetCLinputs","Reset inputs",icon = icon("refresh")))
                    ) # close overall assumption wellPanel
             ),
             column(9,
                  tabBox(width = NULL,  id = "tabs",
                      # tabPanel("Data", fluidRow(
                      #   box(
                      #     "Gross Written Premium", br(), tags$p(year(now())-1),
                      #     DT::dataTableOutput("t_gwp")
                      #   ),
                      #   
                      #   box(
                      #     DT::dataTableOutput("t_clms")
                      #   ),
                      #   
                      #   box(
                      #     "Earned Premium", br(), tags$p(year(now())-1),
                      #     DT::dataTableOutput("t_ep")
                      #   ),
                      #   box(
                      #     "Unearned Written Premium", br(), tags$p(year(now())-1),
                      #     DT::dataTableOutput("t_upr")
                      #   ),
                      #   box(
                      #     "Gross Written Premium", br(), tags$p(paste0("December"," ", year(now())-1)),
                      #     DT::dataTableOutput("m_gwp")
                      #   ),
                      #   
                      #   box(
                      #     "Total Claims Paid", br(), tags$p(paste0("December"," ", year(now())-1)),
                      #     DT::dataTableOutput("m_clms")
                      #   )
                      # )),
                      tabPanel("Product 1",
                               tabBox(width = NULL,
                                 tabPanel("Incremental Triangle",
                                          h5("Data triangle"),
                                          DT::dataTableOutput("incr_tri_pd1")
                                 ),
                                 tabPanel("Cumulative Triangle",
                                          h5("Data triangle"),
                                          DT::dataTableOutput("cum_tri_pd1")
                                          ),
                                  tabPanel("Age-To-Age Factors",
                                          h5("Age-To-Age Factors"),
                                          DT::dataTableOutput("ata_tri_pd1"),
                                          hr(),
                                          h5("Selected"),
                                          DT::dataTableOutput("selected_df_1"),
                                          
                                  ),
                                 tabPanel("Result Table",
                                          h5("Result Table"),
                                          DT::dataTableOutput("res_pd1")
                                 )
                                 )),
                      tabPanel("Product 2",
                               tabBox(width = NULL,
                                 tabPanel("Incremental Triangle",
                                          h5("Data triangle"),
                                          DT::dataTableOutput("incr_tri_pd2")),
                                 tabPanel("Cumulative Triangle",
                                          h5("Data triangle"),
                                          DT::dataTableOutput("cum_tri_pd2")),
                                 tabPanel("Age-To-Age Factors",
                                          h5("Age-To-Age Factors"),
                                          DT::dataTableOutput("ata_tri_pd2"),
                                          hr(),
                                          h5("Selected"),
                                          DT::dataTableOutput("selected_df_2")
                                          
                                          ),
                                 tabPanel("Result Table",
                                          h5("Result Table"),
                                          DT::dataTableOutput("res_pd2")
                                )
                               )),
                      tabPanel("Results",
                               h5("Total"),
                               fluidRow(
                                 valueBoxOutput("TotalIBNR", width = 4),
                                 valueBoxOutput("TotalUPR", width = 4),
                                 valueBoxOutput("TotalRes", width = 4)
                               ),
                               hr(),
                               h5("Product 1"),
                               fluidRow(
                                 valueBoxOutput("P1_IBNR", width = 4),
                                 valueBoxOutput("P1_UPR", width = 4),
                                 valueBoxOutput("P1_Res", width = 4)
                               ),
                               h5("Product 2"),
                               fluidRow(
                                 valueBoxOutput("P2_IBNR", width = 4),
                                 valueBoxOutput("P2_UPR", width = 4),
                                 valueBoxOutput("P2_Res", width = 4)
                               )
                               )
             )
             )
             )
           )
         )# end of TabPanel for main CL menu  



