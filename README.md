# Data Science Project Exploring School Crime and Safety
## September 2021
## Lincoln Kartchner

This repo contains three things:

1. The R code underlying the shiny app that performs the data analysis, visualization as well as all the shiny. (app.R)
2. A saved R model called by app.R in order to speed up model predictions, required in the same directory as app.R. (randint.rds)
3. A directory (data) containing the dataset used in app.R.

app.R requires the following libraries, all of which can be installed using the base 'install.packages()' function, i.e. they're all on CRAN repos.

1. shiny
2. haven
3. dplyr
4. ggplot2
5. pscl
6. MASS
7. glmmTMB
8. plotly
9. faraway
10. gridExtra
11. xtable
12. psych
13. shinythemes

Any questions about the code in app.R and its requirements can be directed to lincoln.kartchner@gmail.com.

The hosted app is linked at:

[Final Project Link](https://lincolnck.shinyapps.io/ds4ph-bme-final/)

The goal of this project was to analyze the School Survey on Crime and Safety from 2018. The SSOCS is a survey conducted by the US Department of Education in order to explore various factors conributing to and mitigating the level of crime and violence within schools. It measures such factors as the level of crime in the neighborhood of the school and whether or not parents are involved in disciplinary actions at school. It is a large dataset with over 2500 responses for over 100 different variables.
The particular emphasis of this project was to analyze only a small subset of those variables, namely: level of crime around the school, level of crime around where students live, percent of students expected to go to college, percent of students for whom academic achievement is important, percent of students scoring below the 15th percentile on standardized examinations, urbanicity, grade level, size and how they relate to the number of violent incidents recorded.
Since the number of violent incidents reported is count data, a poisson model is an obvious choice to fit the the data. However, the dataset has two issues that are common for Poisson models: zero-inflation (a much larger number of zero responses than would be expected), and overdispersion (when there is more variance in the dataset that would be expected given the parameter of the underlying Poisson distribution). The address these issues, I fit four separate models to the data, a Poisson model, a zero-inflated Poisson model, a Negative Binomial model, and a zero-inflated Negative Binomial Mixed-effects model.
Of these four models, the model that had the best fit to the data (as confirmed in outside analysis by comparing the AIC and performing a Pearsons Chi-square test for model fit), is the zero-inflated Negative Binomial mixed effects model. This was to be expected as it is the only model that addresses both the issues in the underlying data. 
The app displays a brief summary of the data and the analysis. It shows a couple plots to visualize the data, along with summary statistics for the variables included and the summary output for the different fitted models. Lastly it allows the user to specify a set of new data in order to predict the number of estimated violent incidents at a school.
