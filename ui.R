
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
                  tabPanel("Districtwise Literacy",
                           # Application title
                           titlePanel("Literacy within states"),
                           
                           
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
                           
                  ),
                  tabPanel("Literacy across India",
                           # Application title
                           titlePanel("Literacy across India"),
                           
                           
                           fluidRow(
                               column(3,
                                      radioButtons("region2", label = h3("Region"),
                                                   choices = list("Total" = "Total",
                                                                  "Rural" = "Rural", 
                                                                  "Urban" = "Urban"),                                                   
                                                   selected = "Total"),
                                      radioButtons("type2", label = h3("Who"),
                                                   choices = list("Persons" = "Persons",
                                                                  "Males" = "Males", 
                                                                  "Females" = "Females"),                                                                  
                                                   selected = "Persons"),
                                      radioButtons("literacyLevel", label = h3("Literacy Level"),
                                                   choices = list("Attending Education Inst" = "AttendingEdu",
                                                                  "Illiterate" = "Illiterate", 
                                                                  "Literate with no edn" = "LiterateNoEdu",
                                                                  "Below Primary" = "BelowPrimary",
                                                                  "Primary" = "Primary",
                                                                  "Middle school" = "Middle",
                                                                  "Matric or Secondary" = "MatricSecondary",
                                                                  "HigherSec-Intermdt-PU" = "HigherSecIntmdtPU",
                                                                  "Non Technical Diploma" = "NonTechnicalDiploma",
                                                                  "Technical Diploma" = "TechnicalDiploma",
                                                                  "Graduate and Above" = "GraduateAndAbove",
                                                                  "Unclassified" = "Unclassified"),
                                                                                                                     
                                                   selected = "AttendingEdu")
                                      
                               ),
                               
                               # Show a plot of the generated distribution
                               
                               column(6,
                                      plotOutput("indiaLiteracy")
                               )
                               
                               
                           )                   
                           
                  )
               
))

