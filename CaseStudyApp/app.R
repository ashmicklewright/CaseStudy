# case study ER app
# chapter 4 in Hadley's textbook

library(shiny)
library(vroom)
library(tidyverse)

dir.create("neiss", showWarnings = FALSE)

download <- function(name) {
  url <- "https://raw.github.com/hadley/mastering-shiny/main/neiss/"
  download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
}

download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

injuries <- vroom::vroom("neiss/injuries.tsv.gz")
products <- vroom::vroom("neiss/products.tsv")
population <- vroom::vroom("neiss/population.tsv")

prod_codes <- setNames(products$prod_code, products$title)

count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  titlePanel("ER Injury Explorer"),
  
  fluidRow(
    column(
      8,
      selectInput(
        "code",
        "Product",
        choices = prod_codes,
        width = "100%"
      )
    ),
    column(
      4,
      selectInput(
        "y",
        "Y axis",
        choices = c("rate", "count")
      )
    )
  ),
  
  fluidRow(
    column(4, h4("Diagnosis"), tableOutput("diag")),
    column(4, h4("Body Part"), tableOutput("body_part")),
    column(4, h4("Location"), tableOutput("location"))
  ),
  
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  
  fluidRow(
    column(2, actionButton("story", "Tell me a story")),
    column(10, textOutput("narrative"))
  )
)


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



server <- function(input, output, session) {
  
  selected <- reactive({
    injuries %>%
      filter(prod_code == input$code)
  })
  
  output$diag <- renderTable(
    count_top(selected(), diag),
    width = "100%"
  )
  
  output$body_part <- renderTable(
    count_top(selected(), body_part),
    width = "100%"
  )
  
  output$location <- renderTable(
    count_top(selected(), location),
    width = "100%"
  )
  
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        labs(
          x = "Age",
          y = "Estimated number of injuries",
          colour = "Sex"
        )
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        labs(
          x = "Age",
          y = "Injuries per 10,000 people",
          colour = "Sex"
        )
    }
  }, res = 96)
  
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>%
      pull(narrative) %>%
      sample(1)
  )
  
  output$narrative <- renderText({
    narrative_sample()
  })
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
 
shinyApp(ui, server)







