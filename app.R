#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Load libraries
library(shiny)
library(haven)
library(dplyr)
library(ggplot2)
library(pscl)
library(MASS)
library(glmmTMB)
library(plotly)
library(faraway)
library(gridExtra)
library(xtable)
library(psych)
library(shinythemes)

# Load and clean data
pu_ssocs18_r <- read_sas("./data/pu_ssocs18.sas7bdat")

DF <- pu_ssocs18_r[, -c(203:424)]
keeps <- c("C0562", "C0560", "C0534", "C0536", "C0532", "FR_URBAN", "FR_LVEL", "FR_SIZE", "VIOINC18")
df <- DF[keeps]
df[df == -1] <- NA

df = df%>%mutate_at("FR_SIZE", as.factor)
df = df%>%mutate_at("FR_URBAN", as.factor)
df = df%>%mutate_at("FR_LVEL", as.factor)
df$FR_LVEL <- trimws(df$FR_LVEL, which=c("left"))
poisson_model <- glm(VIOINC18 ~  FR_URBAN + FR_LVEL + FR_SIZE + C0562 + C0560 + C0534 + C0536 + C0532, data=df, family='poisson')
zip_model <- zeroinfl(VIOINC18 ~  FR_URBAN + FR_LVEL + FR_SIZE + C0562 + C0560 + C0534 + C0536 + C0532, data=df)
nb_model <- glm.nb(VIOINC18 ~  FR_URBAN + FR_LVEL + FR_SIZE + C0562 + C0560 + C0534 + C0536 + C0532, data=df)
randint_nb_model <- readRDS("randint.rds")


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("slate"),

    # Application title
    titlePanel("School Survey on Crime and Safety, 2018"),
        # Show a plot of the generated distribution
        mainPanel(width=10,
            tabsetPanel(type="tabs",
                        tabPanel("About", br(),
                                 h3("The Data"),
                                 "The",
                                 a("School Survey on Crime and Safety", href="https://nces.ed.gov/surveys/ssocs/", target="_blank"),
                                 "is a collection of data from a cross-sectional survey of public schools in the United States collected in 2018. Its focus is reporting measures of both violent and non-violent incidents and measures of attempts to mitigate those incidents. This project constructs a number of models to estimate the count of total violent incidents recorded within the survey year given predictors such as the level of crime where students live and percentage of students scoring below 15th percentile on standardized tests. The original dataset contains 2762 responses for over 100 different variables. For this analysis only several of those variables were selected.",
                                 br(),
                                 h3("The Analysis"),
                                 "The dependent variable of interest in this analysis was the total count of violent incidents recorded within the survey year. Because this value is a count value, a Poisson model is an obvious choice. However, there are two issues with this dataset, namely",
                                 a("zero inflation", href="https://en.wikipedia.org/wiki/Zero-inflated_model", target="_blank"),
                                 "and",
                                 a("overdispersion.", href="https://en.wikipedia.org/wiki/Overdispersion", target="_blank"),
                                 "For more information on these issues when analyzing count data using Poisson models see the linked pages. In order to correct for these two issues, two more additional models were fit: a zero-inflated Poisson model and a Negative Binomial model. Unfortunately, both of these models only succeeded and solving one of the issues. The Negative Binomial model still has issues with zero-inflation, and the zero-inflated Poisson model still has issues with overdispersion. Finally, a zero-inflated Negative Binomial Mixed-effects model was fit to the data to attempt to solve both the issues, and to address any random effects present in the data.",
                                 br(),
                                 "The 'Data Visualization' tab displays a table of summary statistics for the variables included in this analysis as well as a few plots to visualize the data. The 'Data Analysis' tab displays the summary output for each of these models, showing the parameter significance and othe measures of fit including AIC and Residual Deviance. The final tab, 'Predictions' will allow the user to input specific values to estimate the total number of violent incidents that could occur at a given school using one of the four models created.",
                                 ),
                        tabPanel("Data Visualization",br(),
                                 h3("Summary Statistics"),
                                 "The table below displays certain summary statistics for each of the variables in the dataset including the mean response values, standard deviations and ranges. For more information on the other statistics, please see",
                                 a("the psych package for R.", href="http://personality-project.org/r/psych/vignettes/intro.pdf", target="_blank"),
                                 "The variables listed in order are: level of crime around the school, level of crime around where students live, percent of students expected to go to college, percent of students for whom academic achievement is important, percent of students scoring below the 15th percentile on standardized examinations, urbanicity, grade level, size and violent incidents recorded.",
                                 br(),
                                 tableOutput("summ"),
                                 sidebarPanel(
                                     radioButtons("splitornot",
                                                  "Separate Visualizations by Grade Level?",
                                                  choices = list("Yes" = 1,
                                                                 "No" = 2),
                                                  selected = 1)),
                                 fluidRow(
                                     column(plotOutput("viz"), width=12),
                                     column(plotOutput("hist"), width=12))),
                        tabPanel("Data Analysis", br(),
                                 sidebarPanel(
                                     selectInput("modeltype",
                                                  "Select Model Type:",
                                                  choices = list("Poisson" = 1,
                                                                 "Zero-inflated Poisson" = 2,
                                                                 "Negative Binomial" = 3,
                                                                 "Mixed-effects Negative Binomial" = 4),
                                                  selected = 1)),
                                 verbatimTextOutput("regoutput")),
                        tabPanel("Predictions", br(),
                                 sidebarPanel(width=6,
                                     selectInput("modeltype2",
                                                 "Select Model Type:",
                                                 choices = list("Poisson" = 1,
                                                                "Zero-inflated Poisson" = 2,
                                                                "Negative Binomial" = 3,
                                                                "Mixed-effects Negative Binomial" = 4),
                                                 selected = 1),
                                     radioButtons("schoolcrime",
                                                  "Level of Crime Around School:",
                                                  choices = list("High" = 1,
                                                                 "Moderate" = 2,
                                                                 "Low" = 3),
                                                  selected = 1),
                                     radioButtons("livecrime",
                                                  "Level of Crime Where Students Live:",
                                                  choices = list("High" = 1,
                                                                 "Moderate" = 2,
                                                                 "Low" = 3,
                                                                 "Varying" = 4),
                                                  selected = 1),
                                     sliderInput("expcollege",
                                                 "Percent of Students Expected to Go to College:",
                                                 min=0,
                                                 max=100,
                                                 value = 75),
                                     sliderInput("fifteenth",
                                                 "Percent of Students Scoring Below 15th Percentile in Standardized Examinations",
                                                 min=0,
                                                 max=100,
                                                 value=50),
                                     sliderInput("acacheive",
                                                 "Percent of Students for whom Academic Achievement is Important",
                                                 min=0,
                                                 max=100,
                                                 value=50),
                                     selectInput("size",
                                                 "School Size",
                                                 choices = list("Less than 300 students" = 1,
                                                                "300-499 students" = 2,
                                                                "500-999 students" = 3,
                                                                "1,000 or more students" = 4),
                                                 selected = 1),
                                     selectInput("urb",
                                                 "Urbanicity",
                                                 choices = list("City" = 1,
                                                                "Suburb" = 2,
                                                                "Town" = 3,
                                                                "Rural" = 4),
                                                 selected = 1),
                                     selectInput("lvel",
                                                 "Grade Levels",
                                                 choices = list("Elementary" = 1,
                                                                "Middle" = 2,
                                                                "High" = 3,
                                                                "Mixed" = 4),
                                                 selected = 1)),
                                 h2("Predicted Count of Violent Incidents"),
                                 h3(textOutput("prediction")))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    modelchoice <- reactive({input$modeltype})
    
    output$regoutput <- renderPrint({
        if (modelchoice() == 1){
            summary(poisson_model)
        } else if (modelchoice() == 2){
            summary(zip_model)
        } else if (modelchoice() == 3){
            summary(nb_model)
        } else {
            # randint_nb_model <- glmmTMB(VIOINC18 ~ FR_URBAN + FR_SIZE + C0562 + C0560 + C0534 + C0536 + C0532 + (1|FR_LVEL), data = df, ziformula = ~1, family = "nbinom2")
            # saveRDS(randint_nb_model, "randint.rds")
            summary(randint_nb_model)
        }
    })
    
    splitornot <- reactive({input$splitornot})
    
    output$summ <- renderTable({
        xtable(describe(df)[1:10])
    })
    output$viz <- renderPlot({
        
        if (splitornot()==1){
            g <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0562, y=VIOINC18, colour = FR_LVEL), size = 0.3) + facet_wrap(~FR_LVEL)+ theme(legend.position='none') + xlab("Crime Around School")+ ylab("Number of Violent Incidents Recorded")
            h <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0560, y=VIOINC18, colour = FR_LVEL), size = 0.3)+ facet_wrap(~FR_LVEL)+ theme(legend.position='none') + xlab("Crime Where Students Live")+ ylab("Number of Violent Incidents Recorded")
            i <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0534, y=VIOINC18, colour = FR_LVEL), size = 0.3)+ facet_wrap(~FR_LVEL)+ theme(legend.position='none') + xlab("Expected College")+ ylab("Number of Violent Incidents Recorded")
            j <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0536, y=VIOINC18, colour = FR_LVEL), size = 0.3)+ facet_wrap(~FR_LVEL)+ theme(legend.position='none') + xlab("Academic Acheivment Motivation")+ ylab("Number of Violent Incidents Recorded")
            l <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0532, y=VIOINC18, colour = FR_LVEL), size = 0.3)+ facet_wrap(~FR_LVEL)+ theme(legend.position='none') + xlab("Percent Below 15th Percentile")+ ylab("Number of Violent Incidents Recorded")
            m <- ggplot() + geom_jitter(data=df, mapping=aes(x=FR_SIZE, y=VIOINC18, colour = FR_LVEL), size = 0.3)+ facet_wrap(~FR_LVEL) + theme(legend.position='none') + xlab("School Size") + ylab("Number of Violent Incidents Recorded")
        } else {
            g <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0562, y=VIOINC18, colour = FR_LVEL), size = 0.3) +  theme(legend.position='none') + xlab("Crime Around School")+ ylab("Number of Violent Incidents Recorded")
            h <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0560, y=VIOINC18, colour = FR_LVEL), size = 0.3)+  theme(legend.position='none') + xlab("Crime Where Students Live")+ ylab("Number of Violent Incidents Recorded")
            i <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0534, y=VIOINC18, colour = FR_LVEL), size = 0.3)+  theme(legend.position='none') + xlab("Expected College")+ ylab("Number of Violent Incidents Recorded")
            j <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0536, y=VIOINC18, colour = FR_LVEL), size = 0.3)+ theme(legend.position='none') + xlab("Academic Acheivment Motivation")+ ylab("Number of Violent Incidents Recorded")
            l <- ggplot() + geom_jitter(data=df, mapping=aes(x=C0532, y=VIOINC18, colour = FR_LVEL), size = 0.3)+ theme(legend.position='none') + xlab("Percent Below 15th Percentile")+ ylab("Number of Violent Incidents Recorded")
            m <- ggplot() + geom_jitter(data=df, mapping=aes(x=FR_SIZE, y=VIOINC18, colour = FR_LVEL), size = 0.3) + theme(legend.position='none') + xlab("School Size") + ylab("Number of Violent Incidents Recorded")
        }
        grid.arrange(g, h, i, j, l, m, ncol=3, nrow=2)
    })
    
    output$hist <- renderPlot({
        if (splitornot()==1){
            n <- ggplot(data=df, aes(x = VIOINC18, fill=FR_LVEL))+ facet_wrap(~FR_LVEL) + geom_bar(stat = "count") + xlim(-1, 50) + xlab("Number of Violent Incidents Recorded") + ylab("Count") + scale_fill_discrete("School Grade Level")
        } else {
            n <- ggplot(data=df, aes(x = VIOINC18, fill=FR_LVEL)) + geom_bar(stat = "count") + xlim(-1, 50) + xlab("Number of Violent Incidents Recorded") + ylab("Count") + scale_fill_discrete("School Grade Level")
        }
        n
    })
    
    newdata <- reactive({
        FR_URBAN = factor(input$urb, levels=c(1, 2, 3, 4))
        FR_LVEL = factor(input$lvel, levels=c(1, 2, 3, 4))
        FR_SIZE = factor(input$size, levels=c(1, 2, 3, 4))
        C0562 = as.numeric(input$schoolcrime)
        C0560 = as.numeric(input$livecrime)
        C0534 = input$expcollege
        C0536 = input$acacheive
        C0532 = input$fifteenth
        data.frame(FR_URBAN, FR_LVEL, FR_SIZE, C0562, C0560, C0534, C0536, C0532)
    })
    
    modelchoice2 <- reactive({input$modeltype2})
    
    output$prediction <- renderText({
        new_df <- newdata()
        if (modelchoice2()==1){
            predict.glm(poisson_model,new_df, type="response")[[1]]
        } else if (modelchoice2()==2){
            predict(zip_model, new_df, type="response")[[1]]
        } else if (modelchoice2()==3){
            predict.glm(nb_model, new_df, type="response")[[1]]
        } else {
            predict(randint_nb_model, new_df, type="response")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
