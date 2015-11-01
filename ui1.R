
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
                   tabPanel("Statewise crimes",
                            
                            tabPanel("Statewise crimes",
                                     # Application title
                                     titlePanel("Crimes in each state"),
                                     
                                     # Sidebar with a slider input for the number of bins
                                     fluidRow(
                                         column(6,
                                                
                                                selectizeInput(
                                                    'state', label = "State", choices = NULL,multiple=FALSE,selected="Andhra Pradesh",
                                                    options = list(create = TRUE,placeholder = 'Choose the state')
                                                    
                                                ),
                                                radioButtons("radio1", label = h3("Radio buttons"),
                                                             choices = list("RAPE" = "RAPE",
                                                                            "DOWRY DEATH" = "DOWRY DEATH", 
                                                                            "ASSAULT ON WOMEN" = "ASSAULT ON WOMEN WITH INTENT TO OUTRAGE HER MODESTY",
                                                                            "CRUELTY BY HUSBAND OR RELATIVES" = "CRUELTY BY HUSBAND OR RELATIVES",
                                                                            "IMMORAL TRAFFIC" = "IMMORAL TRAFFIC(PREVENTION)ACT",
                                                                            "INDECENT REPRESENTATION OF WOMEN" = 'INDECENT REPRESENTATION OF WOMEN(PREVENTION)ACT',
                                                                            "INSULT TO THE MODESTY OF WOMEN"="INSULT TO THE MODESTY OF WOMEN",
                                                                            "KIDNAPPING & ABDUCTION" = "KIDNAPPING & ABDUCTION",
                                                                            "TOTAL CRIMES AGAINST WOMEN" = 'TOTAL CRIMES AGAINST WOMEN'), 
                                                             selected = "RAPE")
                                         ),
                                         
                                         # Show a plot of the generated distribution
                                         
                                         column(6,
                                                plotOutput("statePlot")
                                         )
                                         
                                     )       
                            
                            )
                            
                  ),
                   tabPanel("Component 3")
               
))

