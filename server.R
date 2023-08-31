#Source of data
#Heart failure clinical records. (2020). UCI Machine Learning Repository. https://doi.org/10.24432/C5Z89R.
library(shiny)
library(randomForest)
library(dplyr)
library(ggplot2)
library(ggExtra)
library(extraoperators)
library(caret)
library(plotROC)
library(ModelMetrics)
library(patchwork)
##############################################################################
mycolors <- c("#00BFC4", "#F8766D") 
######################################
df= read.csv("heart_failure_clinical_records_dataset.csv")
df_classification <- df %>% select(-"time")
df_classification$DEATH_EVENT = factor(df$DEATH_EVENT, levels = c(0, 1))
#############################################################################3

## Split data
set.seed(0)
ind <- createDataPartition(df_classification$DEATH_EVENT, p = 0.8, list = FALSE)
trainingset <- df_classification[ind,]
testingset <- df_classification[-ind,]
#######

## MODELS #############
## MAIN ###
fit_glm <- glm(DEATH_EVENT ~ ., data = trainingset, family = "binomial")
fit_rf <- randomForest(DEATH_EVENT ~ ., data = trainingset, ntree = 100)
##
#Thresholds:
glm_threshold <- 0.21
rf_threshold <- 0.37
## 3 features ####
subset <-  df_classification %>% select("serum_creatinine", "ejection_fraction", "age", "DEATH_EVENT")
sub_trainingset <- df_classification[ind,]
sub_testingset <- df_classification[-ind,]
fit_3glm <- glm(DEATH_EVENT ~ ., data = sub_trainingset, family = "binomial")
fit_3rf <- randomForest(DEATH_EVENT ~ ., data = sub_trainingset, ntree = 100)
## Predictions on testing set ###
results_glm <- predict(fit_glm, testingset, type = "response")
results_rf <- predict(fit_rf, testingset, type = "prob")
results_3glm <- predict(fit_3glm, sub_testingset, type = "response")
results_3rf <- predict(fit_3rf, sub_testingset, type = "prob")
#####
# Sensitivities - for ROC
metrics <- function(predictions){
  actual <- actual <- as.numeric(testingset$DEATH_EVENT) - 1
  thresholds <- seq(0,1, by=.005)
  sensitivities <- c()
  specificities <- c()
  for(threshold in thresholds){
    sensitivities <- append(sensitivities,
                            sensitivity(actual, predictions, threshold))
    specificities <- append(specificities,
                            specificity(actual, predictions, threshold))
  }
  return(data.frame(sens = sensitivities, spec = specificities))
}
######
## Importance model ###
imp_rf <- randomForest(DEATH_EVENT ~ ., data = df_classification, ntree = 100)
importances <- imp_rf$importance;
importances = data.frame(rownames(importances), importances)
colnames(importances) <- c("Feature", "MeanDecreaseGini")
rownames(importances) <- NULL
##############################################################################


function(input, output, session) {
  
  ## Classification ##
  classification_input <- reactive({
    user_input <- data.frame("age" = input$ageC,
                       "anaemia"=as.numeric(input$anaemiaC == "Yes"),
                       "creatinine_phosphokinase"=input$creatinine_phosphokinaseC,
                       "diabetes"= as.numeric(input$diabetesC == "Yes"),
                       "ejection_fraction"=input$ejection_fractionC,
                       "high_blood_pressure"=as.numeric(input$high_blood_pressureC == "Yes"),
                       "platelets"=input$plateletsC,
                       "serum_creatinine"=input$serum_creatinineC,
                       "serum_sodium"=input$serum_sodiumC,
                       "sex"=as.numeric(input$sexC == "men"),
                       "smoking"=as.numeric(input$smokingC == "Yes"))
    return(user_input)
  })
  
  output$forest_prob <- reactive(paste(forest_votes()*100, "%", sep="")) 
  output$glm_prob <- reactive(paste(round(glm_response()*100,3), "%", sep=""))
  forest_votes <- function(){
    predict(fit_rf, classification_input(), type="prob")[,2]
  }
  glm_response <- function(){
    predict(fit_glm, classification_input(), type="response")
  }
  
  output$glm_res <- reactive(ifelse(round(as.numeric(glm_response()),3) > glm_threshold, "Death", "Survival" ))
  output$rf_res <- reactive(ifelse(forest_votes() > rf_threshold, "Death", "Survival" ))
  
###############################
  ## Data exploration ###
  filterDataTable <- reactive({
    df%>% filter(
      age %gele% input$age,
      anaemia %gele% input$anaemia,
      creatinine_phosphokinase %gele% input$creatinine_phosphokinase,
      diabetes %gele% input$diabetes,
      ejection_fraction %gele% input$ejection_fraction,
      high_blood_pressure %gele% input$high_blood_pressure,
      platelets %gele% input$platelets,
      serum_creatinine %gele% input$serum_creatinine,
      serum_sodium %gele% input$serum_sodium,
      sex %gele% input$sex,
      smoking %gele% input$smoking,
      DEATH_EVENT %gele% input$deathevent
    )
  })
  
  output$filteredTable <- renderDataTable({
    filterDataTable()
  })
  
  ############################################3
  ### Statistics ###
  scores <- function(){
    actual = as.numeric(testingset$DEATH_EVENT) - 1
    methods = c("Logistic regression", "3 features logistic regression",
                  "Random forest", "3 features random forest")
    auc <- c(ModelMetrics::auc(actual, results_glm),
             ModelMetrics::auc(actual, results_3glm),
             ModelMetrics::auc(actual, results_rf[,2]),
             ModelMetrics::auc(actual, results_3rf[,2]))
    sens <- c(ModelMetrics::sensitivity(actual, results_glm, input$threshold),
              ModelMetrics::sensitivity(actual, results_3glm, input$threshold),
              ModelMetrics::sensitivity(actual, results_rf[,2], input$threshold),
              ModelMetrics::sensitivity(actual, results_3rf[,2], input$threshold))
    spec <- c(ModelMetrics::specificity(actual, results_glm, input$threshold),
              ModelMetrics::specificity(actual, results_3glm, input$threshold),
              ModelMetrics::specificity(actual, results_rf[,2], input$threshold),
              ModelMetrics::specificity(actual, results_3rf[,2], input$threshold))
    f1sc <- c(ModelMetrics::f1Score(actual, results_glm, input$threshold),
              ModelMetrics::f1Score(actual, results_3glm, input$threshold),
              ModelMetrics::f1Score(actual, results_rf[,2], input$threshold),
              ModelMetrics::f1Score(actual, results_3rf[,2], input$threshold))
    
    
    res <- data.frame(methods, auc, sens, spec,f1sc)
    colnames(res) <- c("Method", "AUC", "Sensitivity", "Specificity", "F1 score")
    return(res)
  }
  
  output$importances <- renderPlot({
    importances %>%
      ggplot( aes(x=Feature, y=MeanDecreaseGini)) +
      geom_segment( aes(xend=Feature, yend=0)) +
      geom_point( size=4, color="orange") +
      coord_flip() +
      theme_bw() +
      xlab("")
  })
  
  output$selectedFeatures <- eventReactive(input$go,
    {
    set.seed(0)
    control_rfe = rfeControl(functions = rfFuncs)
    train_x <- df_classification %>% select(-"DEATH_EVENT")
    train_y <- df_classification$DEATH_EVENT
    withProgress(message = 'Calculation in progress', {
    result_rfe = rfe(x = train_x, 
                     y = as.factor(train_y), 
                     sizes = c(2:3),
                     rfeControl = control_rfe,
                     metric = "Accuracy")
    return(result_rfe$optVariables)
  })
    })

  output$roc <- renderPlot({
     metrics_glm <- metrics(results_glm)
     metrics_rf <- metrics(results_rf[,2])
      ggplot() + 
      geom_path(data = metrics_glm, aes(x = 1-spec, y = sens, colour=mycolors[1]), linewidth=2) +
      geom_path(data = metrics_rf, aes(x = 1-spec, y = sens, colour=mycolors[2]), linewidth=2) +
      scale_colour_discrete(name=NULL,
                            breaks = mycolors[c(1,2)],
                            labels=c("Logistic regression", "Random Forest")) +
      labs(title="ROC curves", x="True positive rate", y="False positivie rate") +
      theme(legend.position="bottom")
  })
  
  output$roc3 <- renderPlot({
    metrics_glm <- metrics(results_3glm)
    metrics_rf <- metrics(results_3rf[,2])
    ggplot() + 
      geom_path(data = metrics_glm, aes(x = 1-spec, y = sens, colour=mycolors[1]), linewidth=2) +
      geom_path(data = metrics_rf, aes(x = 1-spec, y = sens, colour=mycolors[2]), linewidth=2) +
      scale_colour_discrete(name=NULL,
                            breaks = mycolors[c(1,2)],
                            labels=c("Logistic regression", "Random Forest")) +
      labs(title="ROC curves", x="True positive rate", y="False positivie rate") +
      theme(legend.position="bottom")
  })
  
  output$scatter <- renderPlot({
    ggMarginal(
      ggplot(df_classification, aes(x=serum_creatinine, y=ejection_fraction, color=DEATH_EVENT)) + 
                 geom_point(alpha=0.7, size=3) +
                 scale_color_manual(name="Outcome", labels=c("Survival", "Death"), values=mycolors) + 
                 theme(legend.position = "bottom"),
          type="density", groupFill = TRUE)  
  }, height = 400, width = 800
  )
  
  output$scores <- renderTable({
    scores()
  })
  

#########################################################################################
  
  custom_plot <- function(col){
    colname <- as.name(col)
    p1 <- df %>%
      group_by(!!colname) %>%
      summarize(count = n()/nrow(df)) %>%
      ggplot(aes(x = as.factor(!!colname), y = count, fill=as.factor(!!colname))) + 
      geom_bar(stat = "identity") +
      theme(legend.position="none")+
      scale_x_discrete(labels=if(col == "sex") c("Woman", "Man") else c(paste("No ",col), col))+
      scale_fill_manual(values=mycolors) +
      labs(title="rate", x=NULL) +
      geom_text(aes(label = paste(round(count,2)*100,"%")), position=position_dodge(width=0.9), vjust=-0.25)
    
    p2 <- df %>%
      group_by(!!colname) %>%
      summarize(surv = (1-mean(DEATH_EVENT)))  %>%
      ggplot(aes(x = as.factor(!!colname), y = surv, fill=as.factor(!!colname))) + 
      geom_bar(stat = "identity") +
      theme(legend.position="none")+
      scale_x_discrete(labels=if(col == "sex") c("Woman", "Man") else c(paste("No ",col), col))+
      scale_fill_manual(values=mycolors) +
      labs(title="survival rate", x=NULL) +
      geom_text(aes(label = paste(round(surv,2)*100,"%")), position=position_dodge(width=0.9), vjust=-0.25)
    
    return(p1 + p2)
  }
  
  
  
  ## Features plots ###
  output$agePlot <- renderPlot({
     df_classification %>%
       ggplot(aes(x = age, fill=DEATH_EVENT)) +
       geom_density(alpha=0.8) +
       labs(title="Age") +
       scale_fill_manual(name="Outcome", labels=c("Survival", "Death"), values=mycolors)
  })
  
  output$anaemiaPlot <- renderPlot({
    custom_plot("anaemia")
  })
  
  output$creatininePlot <- renderPlot({
    df %>%
      ggplot(aes(x = creatinine_phosphokinase, fill=as.factor(DEATH_EVENT))) +
      geom_density(alpha = 0.7) + 
      scale_fill_manual(name="Outcome", labels=c("Survival", "Death"), values=mycolors) +
      labs(title="Creatinine phospokinase levels", x="level")
  })
  
  output$diabetesPlot <- renderPlot({
    custom_plot("anaemia")
  })
  
  
  output$ejectionPlot <- renderPlot({
    df %>%
      ggplot(aes(x = ejection_fraction, fill=as.factor(DEATH_EVENT))) +
      geom_boxplot(outlier.colour="black", outlier.shape=16,
                   outlier.size=2) +
      labs(title="Ejection fraction", x="Ejection fraction[%]") +
      scale_fill_manual(name="Outcome", labels=c("Survival", "Death"), values=mycolors)
    
  })
  
  output$pressurePlot <- renderPlot({
    custom_plot("high_blood_pressure")
  })
  
  output$plateletsPlot <- renderPlot({
    df %>%
      ggplot(aes(as.factor(DEATH_EVENT), platelets, fill=as.factor(DEATH_EVENT))) +
      geom_violin() +
      labs(title="Platelets", x=NULL) +
      scale_x_discrete(labels=c("Survival", "Death"))+
      scale_fill_manual(name="Outcome", labels=c("Survival", "Death"), values=mycolors)
    
  })
  
  output$serumcreatininePlot <- renderPlot({
    df %>%
      ggplot(aes(x = serum_creatinine, fill=as.factor(DEATH_EVENT))) +
      geom_histogram(position = "identity", bins = 30) +
      scale_fill_manual(name="Outcome", labels=c("Survival", "Death"), values=mycolors) +
      labs(title="Serum creatinine levels", x="level")
  })
  
  output$sodiumPlot <- renderPlot({
    df %>%
      ggplot(aes(x = serum_sodium, fill=as.factor(DEATH_EVENT))) +
      geom_boxplot() +  
      labs(title="Serum sodium") +
      scale_fill_manual(name="Outcome", labels=c("Survival", "Death"), values=mycolors)
  })
  
  output$sexPlot <- renderPlot({
    custom_plot("sex") 
  })
  
  
  output$smokingPlot <- renderPlot({
    custom_plot("smoking")
  })
  
  
  output$deathPlot <- renderPlot({
    df %>%
      group_by(DEATH_EVENT) %>%
      summarize(count = n()/nrow(df)) %>%
      ggplot(aes(x = as.factor(DEATH_EVENT), y = count, fill=as.factor(DEATH_EVENT))) + 
      geom_bar(stat = "identity") +
      theme(legend.position="none")+
      scale_x_discrete(labels=c("Survival", "Death"))+
      scale_fill_manual(values=mycolors) +
      labs(title="Death event rate", x=NULL) +
      geom_text(aes(label = paste(round(count,2)*100,"%")), position=position_dodge(width=0.9), vjust=-0.25)
  })

}

