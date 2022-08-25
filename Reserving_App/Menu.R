tabPanel("Reserving",icon=icon("medkit"),
         fluidPage(
           fluidRow(
             column(3,
                    wellPanel(id="ctrlPanel1",
                              wellPanel(
                                radioButtons("ldf_selector", label = "Loss Development Factor",
                                             choices = list("Simple Average" = 1, "Weighted Average (WA)" = 2, "WA Latest 12months" = 3, "WA Latest 24months" = 4), 
                                             selected = 2),
                              ),

                             
                              fluidRow(actionButton("ResetCLinputs","Reset inputs",icon = icon("refresh")))
                    ) # close overall assumption wellPanel
             ),
             column(9,
                  tabBox(width = NULL,  id = "tabs",
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
                               )
                               )
             )
             )
             )
           )
         )# end of TabPanel for main CL menu  



