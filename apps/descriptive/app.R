library(shiny)
library(ggplot2)
library(dplyr)

## R 내장 데이터셋
builtin_data <- list(자동차 = cars |> set_names(c("속도", "거리")))

## 범주형 데이터셋
set.seed(123) # 재현 가능한 결과를 위해 시드 설정

gender <- sample(c("남성", "여성"), 10, replace = TRUE)
age_group <- sample(c("10대", "20대", "30대", "40대", "50대 이상"), 10, replace = TRUE)
region <- sample(c("서울", "경기", "충청", "전라", "경상", "강원", "제주"), 10, replace = TRUE)

category_data <- data.frame(
  성별 = as.factor(gender),
  연령대 = as.factor(age_group),
  거주지역 = as.factor(region)
)

## 데이터셋 결합
datasets <- c(builtin_data, list(인적정보 = category_data))

library(shiny)
library(ggplot2)
library(dplyr)

# 0. 데이터셋 ---------------------------------------------------------------
## R 내장 데이터셋
builtin_data <- list(자동차 = cars |> set_names(c("속도", "거리")))

## 범주형 데이터셋
set.seed(123) # 재현 가능한 결과를 위해 시드 설정

gender <- sample(c("남성", "여성"), 10, replace = TRUE)
age_group <- sample(c("10대", "20대", "30대", "40대", "50대 이상"), 10, replace = TRUE)
region <- sample(c("서울", "경기", "충청", "전라", "경상", "강원", "제주"), 10, replace = TRUE)

category_data <- data.frame(
  성별 = as.factor(gender),
  연령대 = as.factor(age_group),
  거주지역 = as.factor(region)
)

## 데이터셋 결합
datasets <- c(builtin_data, list(고객정보 = category_data))

# 1. UI ---------------------------------------------------------------
ui <- fluidPage(
  titlePanel("기술통계"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "데이터셋 선택", choices = names(datasets)),
      uiOutput("var_select")
    ),
    mainPanel(
      fluidRow(
        column(6, plotOutput("plot_bar")),
        column(6, plotOutput("plot_pie"))
      ),
      uiOutput("summary")
    )
  )
)

# 2. 서버 ---------------------------------------------------------------
server <- function(input, output, session) {
  data <- reactive({
    datasets[[input$dataset]]
  })

  output$var_select <- renderUI({
    selectInput("var", "변수 선택", choices = names(data()))
  })

  output$plot_bar <- renderPlot({
    req(input$var)
    if (is.factor(data()[[input$var]])) {
      ggplot(data(), aes_string(x = input$var)) +
        geom_bar(fill = "skyblue", color = "white") +
        labs(title = "막대 그래프", x = input$var, y = "빈도") +
        theme_minimal()
    } else if (is.numeric(data()[[input$var]])) {
      ggplot(data(), aes_string(x = input$var)) +
        geom_histogram(fill = "skyblue", color = "white", bins = 30) +
        labs(title = "히스토그램", x = input$var, y = "빈도") +
        theme_minimal()
    }
  })

  output$plot_pie <- renderPlot({
    req(input$var)
    if (is.factor(data()[[input$var]])) {
      freq_table <- summary(data()[[input$var]])
      pie_data <- data.frame(
        "범주" = names(freq_table),
        "빈도" = as.vector(freq_table)
      )
      ggplot(pie_data, aes(x = "", y = 빈도, fill = 범주)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(title = "원 그래프") +
        theme_void()
    } else {
      NULL
    }
  })

  freq_df <- reactive({
    req(input$var)
    if (is.factor(data()[[input$var]])) {
      freq_table <- summary(data()[[input$var]])
      freq_df <- data.frame(
        "범주" = names(freq_table),
        "빈도" = as.vector(freq_table),
        check.names = FALSE
      )
      total <- sum(freq_df$빈도)
      freq_df$비율 <- paste0(round(freq_df$빈도 / total * 100, 2), "%")
      freq_df <- rbind(freq_df, c("총합", total, "100%"))
      freq_df
    }
  })

  output$summary <- renderUI({
    req(input$var)
    if (is.factor(data()[[input$var]])) {
      table_output <- tableOutput("freq_table")
      bootstrapPage(
        fluidRow(
          column(12,
                 h4("범주형 변수 요약"),
                 table_output
          )
        )
      )
    } else if (is.numeric(data()[[input$var]])) {
      x <- data()[[input$var]]
      summary_output <- renderPrint({
        list(
          "평균" = mean(x),
          "중앙값" = median(x),
          "최솟값" = min(x),
          "최댓값" = max(x),
          "범위" = max(x) - min(x),
          "분산" = var(x),
          "표준편차" = sd(x)
        )
      })
      bootstrapPage(
        fluidRow(
          column(12,
                 h4("연속형 변수 요약"),
                 summary_output
          )
        )
      )
    }
  })

  output$freq_table <- renderTable({
    freq_df()
  })
}

shinyApp(ui, server)
