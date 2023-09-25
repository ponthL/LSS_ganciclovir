### appli IV


library(shiny)
library(tidymodels)
library(tidyverse)
library(xgboost)
library(shinyvalidate)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(lubridate)
library(stacks)

#load model
model_shinny_IV<- readRDS("final_wf_xgb_1_6_IV.rds")


##App shiny


ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = tagList(
                      tags$span(
                        class = "logo-mini", "G1D"
                      ),
                      tags$span(
                        class = "logo-lg", "ganciclovir LSS "
                      )
                    )),
                    dashboardSidebar(collapsed = T,
                                     sidebarMenu(
                                       menuItem("Home", tabName = "Home",icon = icon("home")),
                                       menuItem("ganciclovir", tabName = "ganciclovir",icon = icon("medkit"))
                                     )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Home",
                                userBox(
                                  title = userDescription(
                                    title = " Application of machine learning models to predict the ganciclovir and valganciclovir exposure in children using a limited sampling strategy ",
                                    subtitle = "Laure Ponthier PhD works",
                                    type = 2,
                                    image = "hex-V1D.png"
                                  ),
                                  width = 12,
                                  status = "primary",
                                  collapsible = F,
                                  gradient = TRUE,
                                  background = "navy",
                                  boxToolSize = "l",  
                                  "For research purposes only, not dedicated to clinical use"
                                ),
                                box(width=12, title = "Abstract of study",
                                    
                                    status = 'navy',
                                    h3("Introduction:"), h5("Ganciclovir (GCV) and Valganciclovir (VGCV) display significant variability in pharmacokinetics, particularly in children. The best pharmacokinetic parameter for therapeutic drug monitoring is the area under the concentration-time (AUC). Machine Learning (ML) algorithms represent an interesting alternative to Maximum-a-Posteriori Bayesian-estimators (MAP-BE) for AUC estimation. The goal of our study was to develop and validate a ML-based limited sampling strategy (LSS) approach to predict GCV and VGCV AUC0–24 in children. 
" ),
                                    h3("Methods"),
                                    h5("The PK parameters of 4 literature population pharmacokinetic models in addition to the WHO growth curve for children were used in the mrgsolve R package to simulate 10800 PK profiles of children. Different ML algorithms were trained to predict AUC based on different combinations of 2 samples. Performances were evaluated in a simulated test set and in an external dataset of “real” patients. "), 
                                    h3("Results:"), h5("The best estimation performances in the test set were obtained with the Xgboost algorithm and 2 and 6 hours post dose LSS for VGCV (rMPR=0.49%, rRMSE=7.27%) and 1 and 6 hours post dose LSS for GCV (rMPR= 0.9%, rRMSE=9.4%). In the external dataset, the performance based on 2 samples were good: rMPR=2.8%, rRMSE= 16.6%  for VGCV and rMPR=-2.7% , rRMSE=24.6% for GCV. 
"), 
                                    h3("Conclusion:"), h5("The Xgboost algorithm developed yielded accurate individual estimation using only 2 blood samples allowing to improve the use of GCV TDM based on AUC in children.
")
                                )
                        ),
                        #### ganciclovir dashboard-----
                        tabItem(tabName = "ganciclovir",
                                box(width = 12, title = "Patient data",  
                                    numericInput(inputId = "Pds",label = "Current weight (Kg):", value= NULL, min = 5, max = 120),
                                    helpText(" Weight values between 5 and 120 Kg are accepted,  model has not been tested for values out of this range"), 
                                    numericInput(inputId = "taille",label = "Current height (cm):", value= NULL, min = 68, max = 195),
                                    helpText(" Height values between 68 and 195 cm are accepted,  model has not been tested for values out of this range"), 
                                    numericInput(inputId = "age",label = "Age (years)", value= NULL, min = 1, max = 18),
                                    helpText(" Age values between 1 and 18 years are accepted,  model has not been tested for values out of this range"), 
                                    
                                    numericInput(inputId = "Clcreat",label = "Creatinine clearance values (mL/ min/1.73 m2):",value= NULL, min = 10, max = 340),
                                    helpText(" Serum creatinine clearance values between 10 and 340 µmol/L are accepted, model has not been tested for values out  of this range"),
                                    
                                    numericInput(inputId = "amt",label = "dose IV ganciclovir (mg):",value= NULL, min = 10, max = 900),
                                    helpText("doses between 10 and 900 mg are accepted, model has not been tested for values out  of this range"),
                                    
                                    selectInput(inputId ="greffe", "Type of transplantation",c("solid" = "solide", "stem cell transplant" = "moelle")),
                                    selectInput(inputId ="DI", "interdose",c("12" = "12", "24" = "24")),
                                    selectInput(inputId ="SEX", "Gender",
                                                c("girl" = "fille",
                                                  "boy" = "garcon"
                                                )),
                                    numericInput(inputId = "out_1",label = "concentration at 1 hours (mg/L):",value= NULL, min = 0, max = 40),
                                    helpText(""),
                                    numericInput(inputId = "out_6",label = "concentration at 6 hours (mg/L):",value= NULL, min = 0, max = 40),
                                    helpText("")),
                                valueBoxOutput("AUC")
                        ))))





# Define server logic ----
server <- function(input, output) {
  
  
  # Reactive expression to create data frame of all input values ----
  
  output$AUC <- renderValueBox({
    
    
    
    test <- tibble(
      ID = 1,
      Pds= ifelse(between(input$Pds,5,120),input$Pds, NULL),
      taille= ifelse(between(input$taille,68,195),input$taille, NULL),
      age= ifelse(between(input$age,1,18),input$age, NULL),
      Clcreat= ifelse(between(input$Clcreat,10,340),input$Clcreat, NULL),
      SEX = input$SEX,
      greffe = input$greffe,
      DI=input$DI,
      amt = case_when(DI=="12"~2*(input$amt),DI=="24"~(input$amt)),
      out_1= input$out_1,
      out_6= input$out_6,
      SC_mosteller= sqrt((input$taille*input$Pds)/3600))
    
    
    
    #round(predict(model_shinny,test), digits = 0)
    
    AUC <- test %>% mutate(AUC2= case_when(DI=="12"~2*(round(predict(model_shinny_IV,test), digits = 0)),DI=="24"~(round(predict(model_shinny_IV,test), digits = 0)))) %>% select(AUC2)
    #AUC <- round(predict(model_shinny,test), digits = 0)
    
    
    valueBox(
      paste(AUC$AUC2, " mg*h/L ", sep = " ") ,"AUC ",  icon = icon("chart-area"),
      color = "blue"
    )
  })
  
  
  ## validation  Inputs
  
  #iv <- InputValidator$new()
  #iv$add_rule("Pds", sv_required())
  #iv$add_rule("taille", sv_required())
  #iv$add_rule("age", sv_required())
  #iv$add_rule("Clcreat", sv_required())
  #iv$add_rule("Pds", sv_between(5, 120))
  #iv$add_rule("taille", sv_between(68, 195))
  #iv$add_rule("age", sv_between(1, 18))
  #iv$add_rule("Clcreat", sv_between(10, 340))
  #iv$enable()
  
  
  
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)



