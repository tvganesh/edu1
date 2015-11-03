
# Define UI for application that draws a histogram
shinyUI(navbarPage("Crimes against women in India",
                   tabPanel("Literacy in  India",
                            # Application title
                            titlePanel("India Literacy"),
                            
                            
                            fluidRow(
                                column(3,
                                       selectizeInput(
                                           "state", label = "States", choices = NULL,multiple=FALSE,selected="Assam",
                                           options = list(create = TRUE,placeholder = 'Choose the state')
                                           
                                       ),
                                       radioButtons("region", label = h3("Region"),
                                                    choices = list("Total" = "Total",
                                                                   "Rural" = "Rural", 
                                                                   "Urban" = "Urban"),                                                   
                                                    selected = "Total"),
                                       radioButtons("type", label = h3("Who"),
                                                    choices = list("Persons" = "Persons",
                                                                   "Males" = "Males", 
                                                                   "Females" = "Females"),                                                                  
                                                    selected = "Persons")
                                ),
                                
                                # Show a plot of the generated distribution
                                
                                column(8,
                                       plotOutput("distPlot")
                                )
                               
                                
                            )      
                            
                            
                            
                   ),
                   tabPanel("Statewise literacy",
                            
                        
                                     titlePanel("Literacy in each state"),
                                     
                                     
                                     fluidRow(
                                         column(3,
                                                selectizeInput(
                                                    "state1", label = "States", choices = NULL,multiple=FALSE,selected="Assam",
                                                    options = list(create = TRUE,placeholder = 'Choose the state')
                                                    
                                                ),
                                                radioButtons("region1", label = h3("Region"),
                                                             choices = list("Total" = "Total",
                                                                            "Rural" = "Rural", 
                                                                            "Urban" = "Urban"),                                                   
                                                             selected = "Total"),
                                                radioButtons("type1", label = h3("Who"),
                                                             choices = list("Persons" = "Persons",
                                                                            "Males" = "Males", 
                                                                            "Females" = "Females"),                                                                  
                                                             selected = "Persons"),
                                                radioButtons("literacy1", label = h3("Literacy"),
                                                             choices = list("Attending Edn" = "Edu",
                                                                            "Illiterate" = "Illiterate", 
                                                                            "Literate" = "Literate"),                                                                  
                                                             selected = "Edu")
                                         ),
                                         
                                         # Show a plot of the generated distribution
                                         
                                         column(8,
                                                plotOutput("statePlot")
                                         )
                                         
                            
                            )
                            
                  ),
                  tabPanel("District-wiseLiteracy",
                           # Application title
                           titlePanel("Literacy withion states"),
                           
                           
                           fluidRow(
                               column(3,
                                      selectizeInput(
                                          "state2", label = "States", choices = NULL,multiple=FALSE,selected="Assam",
                                          options = list(create = TRUE,placeholder = 'Choose the state')
                                          
                                      )
                                      
                               ),
                               
                               # Show a plot of the generated distribution
                               
                               column(6,
                                      plotOutput("districtPlot")
                               )
                               
                               
                           )      
                           
                           
                           
                  )
               
))

