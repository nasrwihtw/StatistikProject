# This is a Shiny web application for visualizing and exploring a Titanic dataset.

# Load required libraries
library(bit)
library(shiny)
library(readr)
library(ggplot2)
library(hrbrthemes)
library(corrplot)
library(ggcorrplot)
library(naniar)
library(vcd)
library(ggmosaic)
library(tidyverse)
library(dplyr)
library(rsconnect)
library(ggExtra)
#https://r-graphics.org/
# Read the Titanic dataset from a CSV file
dataset <-
  read_csv("./titanic_data.csv")
dataset_backup <- dataset
#Publishing
#rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
# Create a binary variable 'Adult' based on age - https://www.kaggle.com/code/galvaowesley/survivors-classification-on-titanic-using-r/notebook
dataset$Adult[dataset$Age >= 18] <- 1
dataset$Adult[dataset$Age < 18] <- 0
# Calculate the median of Age grouped by the Pclass and Sex = male
Mgroup1 <- subset(dataset, Pclass == 1 & Sex == 'male')
Mmedian1 <- median(Mgroup1$Age, na.rm = TRUE)
Mgroup2 <- subset(dataset, Pclass == 2 & Sex == 'male')
Mmedian2 <- median(Mgroup2$Age, na.rm = TRUE)
Mgroup3 <- subset(dataset, Pclass == 3 & Sex == 'male')
Mmedian3 <- median(Mgroup3$Age, na.rm = TRUE)

# Calculate the median grouped by the Pclass and Sex = female
Fgroup1 <- subset(dataset, Pclass == 1 & Sex == 'female')
Fmedian1 <- median(Fgroup1$Age, na.rm = TRUE)
Fgroup2 <- subset(dataset, Pclass == 2 & Sex == 'female')
Fmedian2 <- median(Fgroup2$Age, na.rm = TRUE)
Fgroup3 <- subset(dataset, Pclass == 3 & Sex == 'female')
Fmedian3 <- median(Fgroup3$Age, na.rm = TRUE)

# Fill the Age's NULL values with median of respective group
for (i in 1:dim(dataset)[1]) {
  if (dataset$Sex[i] == 'male' && is.na(dataset$Age[i])) {
    if (dataset$Pclass[i] == 1)
      dataset$Age[i] <- Mmedian1
    if (dataset$Pclass[i] == 2)
      dataset$Age[i] <- Mmedian2
    if (dataset$Pclass[i] == 3)
      dataset$Age[i] <- Mmedian3
  }
  if (dataset$Sex[i] == 'female' && is.na(dataset$Age[i])) {
    if (dataset$Pclass[i] == 1)
      dataset$Age[i] <- Fmedian1
    if (dataset$Pclass[i] == 2)
      dataset$Age[i] <- Fmedian2
    if (dataset$Pclass[i] == 3)
      dataset$Age[i] <- Fmedian3
  }
}
# Define the user interface
ui <- fluidPage(titlePanel("Titanic Surviver Analysis"),
                #theme = bslib::bs_theme(bootswatch = "darkly"),
                sidebarLayout(
                  sidebarPanel(
                    checkboxGroupInput(
                      "filter_input_sex",
                      label = "Gender",
                      
                      inline = TRUE,
                      choices = c("male", "female"),
                      selected = c("male", "female")
                    ),
                    checkboxGroupInput(
                      "filter_input_pclass",
                      label = "Passenger Class",
                      inline = T,
                      choices = sort(unique(dataset$Pclass)),
                      selected = sort(unique(dataset$Pclass))
                    ),
                    checkboxGroupInput(
                      "filter_input_emb",
                      label = "Embarked from Port",
                      inline = T,
                      choiceValues = sort(unique(dataset$Embarked)),
                      choiceNames = c("Cherbourg", "Queenstown", "Southampton"),
                      selected = sort(unique(dataset$Embarked))
                    ),
                    sliderInput(
                      "age_range",
                      "Age:",
                      min = (min(dataset$Age, na.rm = TRUE)),
                      max = (max(dataset$Age, na.rm = TRUE)),
                      value = c(min(dataset$Age, na.rm = TRUE), max(dataset$Age, na.rm = TRUE))
                    ),
                    textOutput("SliderText"),
                    fileInput("file", "Choose CSV file", accept = ".csv"),
                    hr(),
                    verbatimTextOutput("rowc"),
                    verbatimTextOutput("rownc"),
                    shinythemes::themeSelector(),
                  ),
                  mainPanel(
                    style = "overflow-y: scroll; max-height:  90vh;",
                    tabsetPanel(
                      type = "tabs",
                      tabPanel(
                        "Summary of Data",
                        #h4("Characteristics:"),
                        #htmlOutput("characteristics"),
                        h3("Summary of the Titanic dataset"),
                        verbatimTextOutput("summaryD"),
                        h3("Summary of the filterable Titanic dataset"),
                        verbatimTextOutput("summaryF"),
                        hr(),
                        h3("Handling Missing Data"),
                        plotOutput("vis_missPlot"),
                        h4("Missing Data in Category: Embarked"),
                        verbatimTextOutput("missingEmbarked"),
                        verbatimTextOutput("missingEmbarkedExpl"),
                        h4("Missing Data in Category: Age"),
                        verbatimTextOutput("missingAge"),
                        verbatimTextOutput("missingAgeExpl"),
                        hr(),
                        
                      ),
                      tabPanel(
                        "Plots",
                        plotOutput("corPlot"),
                        hr(),
                        plotOutput("milan_mosaicplot"),
                        radioButtons(
                          "demo_radio",
                          label = "Choose a Feature for Plot",
                          choices = c("Sex", "Pclass", "Adult", "Embarked"),
                          selected = c("Sex"),
                          inline = TRUE
                        ),
                        plotOutput("milan_mosaicplot2"),
                        hr(),
                        plotOutput("nasr_mosaicplot"),
                        selectInput(
                          "featureChoice",
                          "Choose a Feature:",
                          choices = c("Sex", "Pclass", "Embarked"),
                          selected = "Sex"
                        ),
                        plotOutput("barPlot"),
                        hr(),
                        h3("Boxplots"),
                        plotOutput("boxPlot"),
                        hr(),
                        plotOutput("boxPlot2"),
                        hr(),
                        h3("Scatterplots mit Fare < 300"),
                        plotOutput("scatterPlot"),
                        hr(),
                        sliderInput(
                          "bins",
                          "Size of the margins:",
                          min = 1,
                          max = 15,
                          value = 10
                          
                        ),
                        plotOutput("scatterPlot2")
                      ),
                      
                      tabPanel(
                        "Table of Data",
                        selectInput(
                          "row_selector",
                          "Select Rows to Show:",
                          choices = c("Show Excluded Rows", "Show Included Rows")
                        ),
                        
                        tableOutput("table")
                      ), tabPanel("Sources and Inspirations",
                                  verbatimTextOutput("Src"),
                      ),
                      
                    )
                  )
                ))

# Define the server logic
server <- function(input, output) {
  # Define the dataRE() function
  
  output$rowc <- renderText({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
      )
    paste("Count of selected Rows:", nrow(filtered_dataset))
  })
  output$rownc <- renderText({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
        
      )
    paste("Count of excluded Rows:", (nrow(dataset) - nrow(filtered_dataset)))
    #paste("Count of excluded Rows:",nrow(dataset)-nrow(filtered_dataset))
    
  })
  output$missingEmbarked <- renderPrint({
    str(subset(dataset_backup, is.na(Embarked)))
  })
  output$missingEmbarkedExpl <- renderPrint({
    cat("We chose to remove those Entries.", sep = "\n")
  })
  
  output$missingAge <- renderPrint({
    str(subset(dataset_backup, is.na(Age)))
  })
  output$missingAgeExpl <- renderPrint({
    cat("We chose to calculate the median of Age grouped by the Pclass and Sex after https://www.kaggle.com/code/galvaowesley/survivors-classification-on-titanic-using-r/notebook
", "Mgroup1 <- subset(dataset, Pclass == 1 & Sex == 'male')", " Mmedian1 <- median(Mgroup1$Age, na.rm = TRUE)","if (dataset$Sex[i] == 'male' && is.na(dataset$Age[i])) {
    if (dataset$Pclass[i] == 1)
      dataset$Age[i] <- Mmedian1", sep = "\n")
  })
  my_range <- reactive({
    c(input$age_range[1], input$age_range[2])
  })
  output$characteristics <- renderText({
    HTML(
      "PassengerId: Passenger ID<br>Survived: 0=no, 1=yes<br>Pclass: Passenger class<br>Name<br>Sex<br>Age<br>SibSp: Number of siblings/spouses Aboard<br>Parch: Number of parents/children Aboard<br>Ticket: Ticket number<br>Fare: Price of ticket<br>Cabin<br>Embarked: Port of embarkation"
    )
  })
  output$summaryD <- renderPrint({
    summary(dataset)
  })
  output$summaryF <- renderPrint({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
      )
    summary(filtered_dataset)
  })
  #https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
  output$vis_missPlot <- renderPlot({
    # Filter the dataset based on selected inputs
    filtered_dataset <-
      subset(
        dataset_backup,
        Sex %in% input$filter_input_sex &
          (Embarked %in% input$filter_input_emb | is.na(Embarked)) &
          (Age >= input$age_range[1] &
             Age <= input$age_range[2] | is.na(Age)),
      )
    #
    vis_miss(filtered_dataset) + ggtitle("Missing Data Visualization") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$Src <- renderPrint({
    cat("Missing Data Plot from: ","https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html",
        "VCD Mosaic Plots inspired by:"," https://cran.r-project.org/web/packages/vcd/vcd.pdf"
        ,"GG2 MosaicPlot from","  https://www.kaggle.com/code/dhafer/mosaics-plots-using-ggmosaic",
        "Many inspirations from ", "https://r-graph-gallery.com/","and","https://r-graphics.org/",
        "Barplots inspired by: ","https://rstudio-pubs-static.s3.amazonaws.com/151051_5b082e3b8fbd4faaa34cbb352bafb815.html"
        ,"Missing Values and Boxplots from","Graph from https://www.kaggle.com/code/galvaowesley/survivors-classification-on-titanic-using-r/notebook",
        "Scatterplots from: ","https://daattali.com/shiny/ggExtra-ggMarginal-demo/",
        sep = "\n")
  })
  #https://www.kaggle.com/code/dhafer/mosaics-plots-using-ggmosaic
  output$nasr_mosaicplot <- renderPlot({
    titanic <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
      )
    titanic$Pclass <-factor(titanic$Pclass)
    p1 <- ggplot(data = titanic) +
      geom_mosaic(aes(x = product(Survived, !!sym(input$featureChoice)), fill = !!sym(input$featureChoice)))
    
    p1d <- ggplot_build(p1)$data %>% as.data.frame() %>% filter(.wt > 0)
    
    compt_perc <- function(x) {
      d <- c(x, 1) - c(0, x)
      d[-length(d)]
    }
    
    x <- tapply(p1d$ymax, factor(p1d$fill, levels = unique(p1d$fill)), compt_perc)
    x <- unlist(x)
    
    p1d$percentage <- paste0(round(100 * x, 2), "%")
    
    # Chi-Quadrat-Test durchführen
    contingency_table <- table(dataset$Survived, dataset[[input$featureChoice]])
    chi_squared <- chisq.test(contingency_table)
    
    # Ergebnis des Chi-Quadrat-Tests
    chi_squared_title <- paste("Pearson's Chi-squared test:",
                               "Chi-Square Statistic =", round(chi_squared$statistic, 4),
                               "p-value =", round(chi_squared$p.value, 4))
    
    p2 <- p1 +
      geom_label(
        data = p1d,
        aes(x = (xmin + xmax)/2,
            y = (ymin + ymax)/2,
            label = percentage)
      ) +
      xlab(paste0(input$featureChoice)) +
      ylab("Survived") +
      ggtitle(chi_squared_title) +
      theme_bw() +
      theme(legend.position = "none")
    
    p2
  })
  #https://cran.r-project.org/web/packages/vcd/vcd.pdf
  output$milan_mosaicplot <- renderPlot({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
      )
    vcd::mosaic(
      as.formula(paste("~", input$demo_radio, "+ Survived")),
      data = filtered_dataset,
      highlighting = "Survived",
      highlighting_fill = c( "lightblue","tomato"),
      main = paste("Mosaicplot 1 ", input$demo_radio , " vs Survived"),
      labeling_args = list(
        set_varnames = c(Survived = "Status"),
        set_labels = list(Survived = c("Survived", "Not Survived"))
      ),
      labeling = labeling_values
    )
  })
  output$milan_mosaicplot2 <- renderPlot({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
      )
    inp <- ""
    lastinp <- ""
    if (input$demo_radio != "Embarked") {
      inp <- input$demo_radio
      lastinp <- inp
    } else
    {
      inp <- lastinp
    }
    vcd::mosaic(
      as.formula(paste("~", inp, "+ Embarked")),
      data = filtered_dataset,
      highlighting = "Embarked",
      highlighting_fill = c("lightblue","tomato","lightgreen"),
      main = paste("Mosaicplot 1 ", inp , " vs Embarked"),
      labeling_args = list(
        set_varnames = c(Embarked = "Port"),
        set_labels = list(Embarked =  c(
          "C", "Q", "S"
        ))
      ),
      labeling = labeling_values
    )
  })
  #https://rstudio-pubs-static.s3.amazonaws.com/151051_5b082e3b8fbd4faaa34cbb352bafb815.html
  output$barPlot <- renderPlot({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
      )
    # ifelse(df$Embarked == "Q", 1, 2))
    labels <- c("0" = "Deceased", "1" = "Survived")
    # Create a stacked bar chart of survival by passenger class and gender
    ggplot(data = filtered_dataset, aes(x = factor(Pclass), fill = factor(Sex))) +
      geom_bar(position = position_dodge()) +
      geom_text(
        aes(label = paste0(round((after_stat(count) / sum(nrow(
          filtered_dataset
        ))) * 100
        ), "%")),
        stat = "count",
        size = 3,
        vjust = 1.5,
        position = position_dodge(.9)
      ) +
      labs(x = "Passengerclass", y = "Count", fill = "Sex") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('Relation between Pclass, Survived, and Sex') +
      facet_wrap( ~ Survived, labeller = labeller(Survived = labels))
  })
  
  output$barPlot2 <- renderPlot({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
      )
    # ifelse(df$Embarked == "Q", 1, 2))
    labels <- c("0" = "Deceased", "1" = "Survived")
    # Create a stacked bar chart of survival by passenger class and embarked port
    ggplot(data = filtered_dataset, aes(x = factor(Pclass), fill = factor(Embarked))) +
      geom_bar(position = position_dodge()) +
      geom_text(
        aes(label = paste0(round((after_stat(count) / sum(nrow(
          filtered_dataset
        ))) * 100
        ), "%")),
        stat = "count",
        size = 3,
        vjust = 1.5,
        position = position_dodge(.9)
      ) +
      labs(x = "Passengerclass", y = "Count", fill = "Sex") +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle('Relation between Pclass, Survived, and Sex') +
      facet_wrap( ~ Survived, labeller = labeller(Survived = labels))
  })
  output$corPlot <- renderPlot({
    filtered_dataset <-
      subset(
        dataset_backup,
        Sex %in% input$filter_input_sex &
          Embarked %in% input$filter_input_emb,
        na.rm = TRUE
      )
    df <- filtered_dataset
    df$Sex <- ifelse(df$Sex == "male", 0, 1)
    df$Embarked <-
      ifelse(df$Embarked == "S", 0, ifelse(df$Embarked == "Q", 1, 2))
    df <-
      subset(df, select = -c(Name, Ticket, Cabin))
    df <- subset(df, complete.cases(df$Age))
    correlation <- cor(df)
    p.mat <- cor_pmat(df)
    ggcorrplot(
      correlation,
      lab = TRUE,
      outline.col = "gray",
      colors = c("#6D9EC1", "white", "#E46726"),
      type = "lower",
      p.mat = p.mat,
      insig = "blank"
    ) +
      ggtitle('Correlation between characteristics')
  })
  data <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  #Graph from https://www.kaggle.com/code/galvaowesley/survivors-classification-on-titanic-using-r/notebook
  output$boxPlot <- renderPlot({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
      )
    # Create a boxplot of age by sex, grouped by passenger class
    ggplot(filtered_dataset) +
      geom_boxplot(aes(
        x = Sex,
        y = Age,
        fill = as.factor(Pclass)
      ), na.rm = TRUE) +
      labs(x = 'Sex', y = 'Age') +
      scale_fill_discrete(name = "Pclass") +
      theme_bw() +
      ggtitle('Boxplot: Age vs. Sex grouped by Pclass') +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$boxPlot2 <- renderPlot({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2]
      )
    # Create a boxplot of age by sex, grouped by passenger class
    ggplot(filtered_dataset) +
      geom_boxplot(aes(
        x = Sex,
        y = Fare,
        fill = as.factor(Pclass)
      ), na.rm = TRUE) +
      labs(x = 'Sex', y = 'Fare') +
      scale_fill_discrete(name = "Pclass") +
      theme_bw() +
      ggtitle('Boxplot: Fare vs. Sex grouped by Pclass') +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$scatterPlot <- renderPlot({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2] &
          Fare <= 300
      )
    #binss <- seq(min(filtered_dataset$Age), max(filtered_dataset$Age), length.out = input$bins + 1)
    # Create a boxplot of age by sex, grouped by passenger class
    p <-
      ggplot(filtered_dataset,
             aes(
               x = Age,
               y = Fare,
               color = as.factor(Pclass),
               shape = Sex
             )) +
      geom_point(size = 2) +
      theme_bw(15) +
      theme(legend.position = "bottom")
    ggtitle('Scatterplot: Fare vs. Age grouped by Sex') +
      theme(plot.title = element_text(hjust = 0.5))
    ggMarginal(
      p,
      size = input$bins,
      type = "density" ,
      groupColour = TRUE,
      groupFill = TRUE
    )
  })
  output$scatterPlot2 <- renderPlot({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] & Age <= input$age_range[2] &
          Fare <= 300
      )
    # Create a boxplot of age by sex, grouped by passenger class
    p <-
      ggplot(filtered_dataset,
             aes(
               x = Age,
               y = Fare,
               color = as.factor(Survived),
               shape = Sex
             )) +
      geom_point(size = 3) +
      theme_ipsum() +
      theme(legend.position = "bottom")
    ggtitle('Scatterplot: Fare vs. Age grouped by Sex') +
      theme(plot.title = element_text(hjust = 0.5))
    ggMarginal(
      p,
      size = input$bins,
      type = "histogram" ,
      groupColour = TRUE,
      groupFill = TRUE
    )
  })
 
  
  output$table <- renderTable({
    filtered_dataset <-
      subset(
        dataset,
        Sex %in% input$filter_input_sex &
          Pclass %in% input$filter_input_pclass &
          Embarked %in% input$filter_input_emb &
          Age >= input$age_range[1] &
          Age <= input$age_range[2]#| is.na(Age))
        
      )
    
    if (input$row_selector == "Show Excluded Rows") {
      anti_join(dataset, filtered_dataset, join_by("Name"))
    } else if (input$row_selector == "Show Included Rows") {
      semi_join(dataset, filtered_dataset, join_by("Name"))
    }
    
    
    
    # Ausgabe des gefilterten Dataframes
  })
}

# Run the application
shinyApp(ui = ui, server = server)
#runApp('TitanicMNM', display.mode = "showcase")