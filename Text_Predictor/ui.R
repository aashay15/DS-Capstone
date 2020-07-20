#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Text Predictor DS Capstone"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h2("How the Data product works:"), 
            h5("1. Enter a word or phrases in the text box."),
            h5("2. The predicted next word prints below it in green."),
            h5("3. No need to hit enter of submit."),
            h5("4. A question mark means no prediction, due to typo or swear words inserted"),
            a("Source Code GitHub Repository", href = "https://github.com/aashay15/DS-Capstone")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
                         textInput("user_input", h3("Please Enter some text:"), 
                                   value = "Your words"),
                         h3("Prediction for the Next Word:"),
                         h4(em(span(textOutput("ngram_output"), style="color:green")))
                
               
        )
            
        )
    )

)