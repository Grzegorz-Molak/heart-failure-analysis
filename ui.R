library(shiny)


df= read.csv("heart_failure_clinical_records_dataset.csv")

# Define UI for slider demo app ----
navbarPage("Heart failure",
           tabPanel("Classification",
  
  # App title ----
  titlePanel("Custom entity"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      sliderInput("ageC", "Age:",
                  min = 1, max = 120,
                  value = 30, step = 1),
      
      # Input: Decimal interval with step value ----
      selectInput("anaemiaC", "Anaemia:",
                  c("No", "Yes")),
      
      # Input: Specification of range within an interval ----
      sliderInput("creatinine_phosphokinaseC", "Creatinine phosphokinase:",
                  min = 5, max = 8000,
                  value = 50, step=10),
      
      # Input: Specification of range within an interval ----
      selectInput("diabetesC", "Diabetes:",
                  c("No", "Yes")),
      
      # Input: Specification of range within an interval ----
      sliderInput("ejection_fractionC", "Ejection fraction:",
                  min = 0, max = 100,
                  value = 40, step = 2),
      
      # Input: Specification of range within an interval ----
      selectInput("high_blood_pressureC", "High blood pressure:",
                  c("No", "Yes")),
      
      # Input: Specification of range within an interval ----
      sliderInput("plateletsC", "Platelets:",
                  min = 0, max = 1000,
                  value = 500, step = 10),
      
      # Input: Specification of range within an interval ----
      sliderInput("serum_creatinineC", "Serum creatinine:",
                  min = 0, max = 10,
                  value = 1, step = 0.25),
      
      # Input: Specification of range within an interval ----
      sliderInput("serum_sodiumC", "Serum sodium:",
                  min = 100, max = 150,
                  value = 137, step = 1),
      
      # Input: Specification of range within an interval ----
      selectInput("sexC", "Sex:",
                  c("woman", "men")),
      
      # Input: Specification of range within an interval ----
      selectInput("smokingC", "Smoking:",
                  c("No", "Yes")),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      h4("Probability of death event based on:"),
      h5("Random forest votes:"),
      textOutput("forest_prob"),
      h5("Random forest decision based on it's threshold of 0.37"),
      strong(textOutput("rf_res")),
      h5("Logistic regression response:"),
      textOutput("glm_prob"),
      h5("Logistic regression decision based on it's threshold of 0.21"),
      strong(textOutput("glm_res")),
      
      
    )
  )
  ),
  # ###################################################################
  tabPanel("Data",
           h4("Source of data:"),
           h5("Heart failure clinical records. (2020). UCI Machine Learning Repository. https://doi.org/10.24432/C5Z89R."),
           titlePanel("Filter data"),
           
           # Sidebar layout with input and output definitions ----
           sidebarLayout(
             
             # Sidebar to demonstrate various slider options ----
             sidebarPanel(
               
               # Input: Simple integer interval ----
               sliderInput("age", "Age:",
                           min = min(df$age), max = max(df$age),
                           value = c(min(df$age), max(df$age)), step = 1),
               
               # Input: Decimal interval with step value ----
               sliderInput("anaemia", "Anaemia:",
                           min = 0, max = 1,
                           value = c(0,1), step = 1),
               
               # Input: Specification of range within an interval ----
               sliderInput("creatinine_phosphokinase", "Creatinine phosphokinase:",
                           min = min(df$creatinine_phosphokinase), max = max(df$creatinine_phosphokinase),
                           value = c(min(df$creatinine_phosphokinase), max(df$creatinine_phosphokinase)), step=10),
               
               # Input: Specification of range within an interval ----
               sliderInput("diabetes", "Diabetes:",
                           min = 0, max = 1,
                           value = c(0,1), step = 1),
               
               # Input: Specification of range within an interval ----
               sliderInput("ejection_fraction", "Ejection fraction:",
                           min = min(df$ejection_fraction), max = max(df$ejection_fraction),
                           value = c(min(df$ejection_fraction), max(df$ejection_fraction)), step = 2),
               
               # Input: Specification of range within an interval ----
               sliderInput("high_blood_pressure", "High blood pressure:",
                           min = 0, max = 1,
                           value = c(0,1), step = 1),
               
               # Input: Specification of range within an interval ----
               sliderInput("platelets", "Platelets:",
                           min = min(df$platelets), max = max(df$platelets),
                           value = c(min(df$platelets), max(df$platelets)), step = 10),
               
               # Input: Specification of range within an interval ----
               sliderInput("serum_creatinine", "Serum creatinine:",
                           min = min(df$serum_creatinine), max = max(df$serum_creatinine),
                           value = c(min(df$serum_creatinine),max(df$serum_creatinine)), step = 0.25),
               
               # Input: Specification of range within an interval ----
               sliderInput("serum_sodium", "Serum sodium:",
                           min = min(df$serum_sodium), max = max(df$serum_sodium),
                           value = c(min(df$serum_sodium),max(df$serum_sodium)), step = 1),
               
               # Input: Specification of range within an interval ----
               sliderInput("sex", "Sex:",
                           min = 0, max = 1,
                           value = c(0,1), step = 1),
               
               # Input: Specification of range within an interval ----
               sliderInput("smoking", "Smoking:",
                           min = 0, max = 1,
                           value = c(0,1), step = 1),
               # Input: Specification of range within an interval ----
               sliderInput("deathevent", "Death Event:",
                           min = 0, max = 1,
                           value = c(0,1), step = 1),),
             
             
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Table summarizing the values entered ----
                 dataTableOutput("filteredTable")
                 
               )
             )
           ),
  #######################################################################
  tabPanel("Models evaluations",
           mainPanel(
             h2("Dimensonality reduction"),
             h3("Importances by random forest's ",em("importances")," field"),
             plotOutput("importances"),
             h1("RFE algorithm choices:"),
             p(actionButton("go", "Calculate"), "(can take a while)"),
             textOutput('selectedFeatures'),
             h3("ROC curves for random forest and logistic regression models"),
             plotOutput("roc"),
             h3("ROC curves for the same models trained only on 3 chosen features"),
             plotOutput("roc3"),
             h3("Scatter plot of 2 most important features"),
             plotOutput("scatter", width = "100%"),
             h3("Models metrics:"),
             sliderInput("threshold", "Decision threshold:",
                         min = 0, max = 1,
                         value = 0.5, step = 0.05),
             tableOutput("scores")
           )),
  #######################################################################
  tabPanel("Plots",
           mainPanel(
             h2("Age"),
             plotOutput("agePlot"),
             h2("Anaemia"),
             plotOutput("anaemiaPlot"),
             h2("Creatinine"),
             plotOutput("creatininePlot"),
             h2("Diabetes"),
             plotOutput("diabetesPlot"),
             h2("Ejection fraction"),
             plotOutput("ejectionPlot"),
             h2("High blood pressure"),
             plotOutput("pressurePlot"),
             h2("Platelets"),
             plotOutput("plateletsPlot"),
             h2("Serum creatinine"),
             plotOutput("serumcreatininePlot"),
             h2("Sodium"),
             plotOutput("sodiumPlot"),
             h2("Sex"),
             plotOutput("sexPlot"),
             h2("Smoking"),
             plotOutput("smokingPlot"),
             h2("Death"),
             plotOutput("deathPlot")
             )
           )
  
  # END ############################################################
)

