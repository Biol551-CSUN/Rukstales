#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(here)
library(lubridate)

babydata <- read_csv(here("Week_10", "Data", "HatchBabyExport.csv"))


ui <- fluidPage(theme = shinytheme("spacelab"),

    # Application title
    titlePanel("Baby Blakely and Micah's weight progress over 5 weeks"),

    # Sidebar with radio buttons for choosing which baby
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "baby",
                         label = "Baby:",
                         choices = c("Blakely", "Micah"),
                         selected = "Blakely")
        ),
        
        mainPanel(
           plotOutput("weight_plot")
        )
    )
)


server <- function(input, output) {
    babydata <- babydata %>%
        rename(name = "Baby Name",
               start = "Start Time") %>%
        filter(Activity == "Weight") %>%
        separate(col = start,
                 into = c("date", "time", "am_pm"),
                 sep = " ") %>%
        mutate(date = mdy(date),
               time = hm(time),
               Amount = as.numeric(Amount))
    
    rbabies <- reactive({
        babydata %>%
            filter(name == input$baby & !is.na(Amount))
    })
    output$weight_plot <- renderPlot({
        rbabies() %>%
            ggplot(aes(x = date,
                       y = Amount,
                       group = 1)) +
            geom_line(color = "#30bfbd",
                      size = 1) +
            scale_y_continuous(breaks = seq(5, 8),
                               expand = c(0, 0)) +
            theme_bw() +
            theme(axis.title = element_text(size = 16),
                  axis.text = element_text(size = 12)) +
            labs(x = "\n Date",
                 y = "Weight (lbs) \n")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
