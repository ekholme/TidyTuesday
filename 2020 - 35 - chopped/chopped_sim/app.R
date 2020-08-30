

# Setup, Data Clean, Function Def -----------------------------------------


library(shiny)
library(tidyverse)
library(glue)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

judges <- chopped %>%
    select(judge1, judge2, judge3) %>%
    pivot_longer(cols = everything(),
                 names_to = "key",
                 values_to = "judge") %>%
    count(judge, sort = TRUE) %>%
    filter(n > 10) %>%
    pull(judge)

judges <- judges[-c(10, 13)]

ingredients_df <- chopped %>%
    select(appetizer, entree, dessert) %>%
    pivot_longer(cols = everything(),
                 names_to = "course",
                 values_to = "ingredient") %>%
    separate_rows(ingredient, sep = ", ")

get_rand_ingred <- function(course, n = 4) {
    temp <- ingredients_df %>%
        filter(course == course) %>%
        distinct(ingredient) %>%
        pull(ingredient) %>%
        sample(size = n, replace = FALSE)
    
    glue("Your { course } must include { temp[[1]] }, { temp[[2]] }, { temp[[3]] }, and { temp[[4]] }.")
}

get_judges <- function() {
    temp <- sample(judges, size = 3)
    
    glue("Your judges will be { temp[[1]] }, { temp[[2]] }, and { temp[[3]] }.\n")
}


# UI ----------------------------------------------------------------------

ui <- fluidPage(

    titlePanel("Chopped Episode Simulator"),

    sidebarLayout(
        sidebarPanel(width = 3,
            actionButton("generate", "Generate Episode!")
        ),

        mainPanel(width = 9,
           
           h2("Appetizer Round"),
            
           textOutput("apps"),
           
           h2("Entree Round"),
           
           textOutput("entree"),
           
           h2("Dessert Round"),
            
           textOutput("dess"),
           
           h2("Judges"),
           
           textOutput("judges")
        )
    )
)


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
    
    txt <- eventReactive(input$generate, {
        map_chr(c("appetizer", "entree", "dessert"),
                get_rand_ingred) %>%
            paste(sep = "\n")
    })
    
    jdgs <- eventReactive(input$generate, {
        get_judges()
    })

    output$apps <- renderText({
        txt()[[1]]
    })
    
    output$entree <- renderText({
        txt()[[2]]
    })
    
    output$dess <- renderText({
        txt()[[3]]
    })
    
    output$judges <- renderText({
        jdgs()
    })
}


# Run App -----------------------------------------------------------------

 
shinyApp(ui = ui, server = server)
