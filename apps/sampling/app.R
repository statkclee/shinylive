library(shiny)
library(ggplot2)
library(ggforce)
library(dplyr)
library(showtext)

# Google Font 로드
font_add(family = "notosanskr", regular = str_glue("{here::here()}/assets/NotoSerifKR-Regular.otf"))
showtext_auto()

ui <- fluidPage(
  titlePanel("모집단에서 표본 추출"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sample_size", "표본 크기", min = 1, max = 100, value = 10),
      verbatimTextOutput("sample_summary")
    ),
    mainPanel(
      plotOutput("population_plot"),
      plotOutput("sample_plot")
    )
  )
)

server <- function(input, output) {
  set.seed(123)
  n <- 100
  angles <- runif(n, 0, 2*pi)
  radii <- sqrt(runif(n, 0, 1))
  x <- radii * cos(angles)
  y <- radii * sin(angles)
  population <- tibble(x = x, y = y) %>%
    mutate(gender = c(rep("남자", 50), rep("여자", 50)))

  output$population_plot <- renderPlot({
    ggplot(population) +
      geom_point(aes(x, y, color = gender), size = 3) +
      geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "gray30") +
      theme_void() +
      scale_color_manual(values = c("여자" = "red", "남자" = "blue")) +
      theme(legend.position = "top",
            text = element_text(family = "notosanskr")) +
      coord_fixed() +
      labs(title = "모집단", color = "성별")
  })

  sample <- reactive({
    population %>%
      sample_n(input$sample_size, replace = FALSE)
  })

  output$sample_plot <- renderPlot({
    ggplot(sample()) +
      geom_point(aes(x, y, color = gender), size = 3) +
      geom_circle(aes(x0 = 0, y0 = 0, r = 1), color = "gray30") +
      theme_void() +
      scale_color_manual(values = c("여자" = "red", "남자" = "blue")) +
      theme(legend.position = "top",
            text = element_text(family = "notosanskr")) +
      coord_fixed() +
      labs(title = "추출된 표본", color = "성별")
  })

  output$sample_summary <- renderText({
    male_count <- sum(sample()$gender == "남자")
    female_count <- sum(sample()$gender == "여자")
    paste("추출된 표본 요약:\n",
          "남자:", male_count, "명\n",
          "여자:", female_count, "명")
  })
}

shinyApp(ui, server)
