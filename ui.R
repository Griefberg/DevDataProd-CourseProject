shinyUI(
        navbarPage("Census Income Database",
                   tabPanel("Exploratory analysis",
                            mainPanel(tabsetPanel(
                                    tabPanel('All income groups',
                                             column(12, plotOutput("age")),
                                             column(12, plotOutput("sex")),
                                             column(12, plotOutput("race")),
                                             column(12, plotOutput("marital")),
                                             column(12, plotOutput("education")),
                                             column(12, plotOutput("occupation")),
                                             column(12, plotOutput("country"))),
                                    tabPanel('Low income group',
                                             column(12, plotOutput("age_low")),
                                             column(12, plotOutput("sex_low")),
                                             column(12, plotOutput("race_low")),
                                             column(12, plotOutput("marital_low")),
                                             column(12, plotOutput("education_low")),
                                             column(12, plotOutput("occupation_low")),
                                             column(12, plotOutput("country_low"))),
                                    tabPanel('High income group',
                                             column(12, plotOutput("age_high")),
                                             column(12, plotOutput("sex_high")),
                                             column(12, plotOutput("race_high")),
                                             column(12, plotOutput("marita_high")),
                                             column(12, plotOutput("education_high")),
                                             column(12, plotOutput("occupation_high")),
                                             column(12, plotOutput("country_high")))))),
        tabPanel('Income Prediction',
                 fluidPage(
                         sidebarPanel(numericInput("age", "Your age:", 17 , min = 17, max = 90),
                                      selectInput("sex", "Choose your sex:", 
                                                  choices = unique(as.character(census$sex))),
                                      selectInput("race", "Choose your race:", 
                                                  choices = unique(as.character(census$race))),
                                      selectInput("marital", "Choose your marital status:", 
                                                  choices = unique(as.character(census$marital_status))),
                                      selectInput("education", "Choose your education level:", 
                                                  choices = unique(as.character(census$education))),
                                      selectInput("occupation", "Choose your occupation status:", 
                                                  choices = unique(as.character(census$occupation))),
                                      numericInput("hours", "Your working hours per week:", 1 , min = 1, max = 99),
                                      selectInput("country", "Choose your native country:", 
                                                  choices = unique(as.character(census$native_country))),
                                      submitButton("Submit")
                         ),
                         mainPanel(textOutput("text1")
                         )
                 )),
        tabPanel('About', mainPanel(includeMarkdown("README.rmd")))
        ))
        
                                                                  