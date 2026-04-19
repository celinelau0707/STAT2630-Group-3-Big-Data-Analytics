# 1. 安裝並載入必要的工具包
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("mongolite")) install.packages("mongolite")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("plotly")) install.packages("plotly")
if (!require("dplyr")) install.packages("dplyr")
if (!require("wordcloud")) install.packages("wordcloud")
if (!require("RColorBrewer")) install.packages("RColorBrewer")
if (!require("tidyr")) install.packages("tidyr")
if (!require("scales")) install.packages("scales")
if (!require("tidytext")) install.packages("tidytext")



# 1. 載入必要套件
library(shiny)
library(shinydashboard)
library(mongolite)
library(ggplot2)
library(plotly)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(tidyr)
library(scales)
library(tidytext)

# 2. MongoDB 連接設定
model_pred <- mongo(collection = "Model_Predictions", db = "Cluster1",
                    url = "mongodb+srv://teresali:123abc@cluster1.ow8c4i0.mongodb.net/Cluster1?retryWrites=true&w=majority")
rating_sig <- mongo(collection = "Rating_Significance", db = "Cluster1",
                    url = "mongodb+srv://teresali:123abc@cluster1.ow8c4i0.mongodb.net/Cluster1?retryWrites=true&w=majority")
sent_freq <- mongo(collection = "Sentiment_Frequency_Analysis", db = "Cluster1",
                   url = "mongodb+srv://teresali:123abc@cluster1.ow8c4i0.mongodb.net/Cluster1?retryWrites=true&w=majority")
sent_score <- mongo(collection = "Sentiment_Score_Analysis", db = "Cluster1",
                    url = "mongodb+srv://teresali:123abc@cluster1.ow8c4i0.mongodb.net/Cluster1?retryWrites=true&w=majority")
ratings <- mongo(collection = "ratings_data", db = "Cluster1",
                 url = "mongodb+srv://teresali:123abc@cluster1.ow8c4i0.mongodb.net/Cluster1?retryWrites=true&w=majority")

# 讀取資料
data_pred <- model_pred$find()
data_sig <- rating_sig$find()
data_freq <- sent_freq$find()
data_score <- sent_score$find()
data_ratings <- ratings$find()

# --- UI 前端介面 ---
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Roblox Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("1. Rating Trend", tabName = "trend", icon = icon("line-chart")),
      menuItem("2. Sentiment Score", tabName = "sent_score", icon = icon("smile")),
      menuItem("3. Word Cloud", tabName = "wordcloud", icon = icon("cloud")),
      menuItem("4. Significance Test", tabName = "sig_test", icon = icon("balance-scale")),
      menuItem("5. Rating Forecast", tabName = "forecast", icon = icon("robot"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # 1. Rating Trend
      tabItem(tabName = "trend",
              fluidRow(box(title = "Historical Rating Trend", status = "primary", solidHeader = TRUE, 
                           plotlyOutput("plotTrend"), width = 10))),

      # 2. Sentiment Score (加入了你原本的 input$mode 控制選項)
      tabItem(tabName = "sent_score",
              fluidRow(
                box(title = "Controls", status = "warning", width = 10,
                    radioButtons("mode", "Select Display Mode:", choices = c("Percentages", "Net Score"), inline = TRUE)),
                box(title = "Sentiment Score", status = "success", solidHeader = TRUE, 
                    plotlyOutput("plotSentScore"), width = 10)
              )),
      # 3. Word Cloud (加入了你原本的 input$topN 控制拉桿)
      tabItem(tabName = "wordcloud",
              fluidRow(
                box(title = "Controls", status = "warning", width = 10,
                    sliderInput("topN", "Number of Words:", min = 5, max = 30, value = 5)),
                box(title = "Word Cloud", status = "primary", solidHeader = TRUE, 
                    plotOutput("plotWC", height = "600px"), width = 10))),
      
    
      # 4. Significance Test
      tabItem(tabName = "sig_test",
              fluidRow(box(title = "Welch T-Test", status = "danger", solidHeader = TRUE, 
                           plotlyOutput("plotSigTest"), width = 10))),

      # 5. Rating Forecast
      tabItem(tabName = "forecast",
              fluidRow(
                box(title = "Supervised Learning: Rating Forecast ", status = "warning", solidHeader = TRUE, 
                    plotlyOutput("plotForecast"), width = 10)
              ))
    )
  )
)

# --- Server 後端邏輯 ---
server <- function(input, output) {
  
  # 1. Rating Trend
  output$plotTrend <- renderPlotly({
    p <- ggplot(data_ratings, aes(x = as.Date(date), y = avg_rating)) +
      geom_line(color = "blue", size=1.2) +
      scale_x_date(labels = date_format("%b %Y", locale="C")) +
      labs(title = "Google Play Rating Trend (Nov 2025 – Apr 2026)",
           x = "Date", y = "Average Rating") +
      theme_minimal(base_size = 10)
    ggplotly(p)
  })
  # 2. Sentiment Score (還原你的 input$mode 邏輯)
  output$plotSentScore <- renderPlotly({
    if (input$mode == "Percentages") {
      df <- data_score %>%
        select(source_type, negative, positive) %>%
        pivot_longer(cols=c("negative","positive"),
                     names_to="sentiment", values_to="percentage")
      
      p <- ggplot(df, aes(x=source_type, y=percentage, fill=sentiment)) +
        geom_bar(stat="identity", position="dodge") +
        scale_fill_manual(values=c("negative"="red","positive"="blue")) +
        labs(title="Sentiment Distribution ", x="Source", y="Percentage") +
        theme_minimal(base_size = 14)
    } else {
      p <- ggplot(data_score, aes(x=source_type, y=net_sentiment_score, fill=source_type)) +
        geom_bar(stat="identity") +
        geom_hline(yintercept=0, linetype="dashed") +
        scale_fill_brewer(palette="Set2") +
        labs(title="Net Sentiment Score ", x="Source", y="Net Sentiment Score") +
        theme_minimal(base_size = 14)
    }
    ggplotly(p)
  })
  # 3. Word Cloud (修正重複 Bug 版本)
  output$plotWC <- renderPlot({
    topN <- input$topN
    
    # 修正點：先將重複的單字頻率加總 (Deduplication)
    wc_clean <- data_freq %>%
      group_by(word) %>%
      summarise(n = sum(n, na.rm = TRUE), .groups = "drop") %>%
      # 確保數據乾淨後，再取前 N 名
      top_n(topN, n)
    
    par(mar=c(0,0,0,0))
    wordcloud(words = wc_clean$word, 
              freq = wc_clean$n,
              max.words = topN, 
              colors = brewer.pal(8, "Set1"),
              random.order = FALSE, # 讓最大的字在中間，看起來更專業
              scale=c(8,0.5))       # 調整比例，避免字體太大超出邊框
  })
  # 4. Significance Test (融合 MongoDB 統計數據與實時繪圖)
  # 4. Significance Test (修復 Hover NA + 增加圖例說明)
  output$plotSigTest <- renderPlotly({
    
    validate(need(nrow(data_sig) > 0, "Waiting for Significance Test data..."))
    validate(need(exists("data_ratings"), "Waiting for rating data..."))
    
    # 1. 提取在 MongoDB 的統計檢驗結果
    p_val    <- data_sig$p_value[1]
    diff_val <- data_sig$avg_diff[1]
    ci_low   <- data_sig$conf_int_low[1]
    ci_high  <- data_sig$conf_int_high[1]
    is_sig   <- data_sig$is_significant[1]
    
    # 2. 準備原始數據
    df_clean <- data_ratings %>% filter(!is.na(avg_rating))
    
    # 3. 手動計算各組的平均值與 95% CI (解決 Plotly 顯示 NA 的問題)
    df_summary <- df_clean %>%
      group_by(phase) %>%
      summarise(
        mean_val = mean(avg_rating),
        sd_val = sd(avg_rating),
        n = n(),
        se = sd_val / sqrt(n),
        # 計算單組的 95% Confidence Interval
        error_low = mean_val - qt(0.975, df = n-1) * se,
        error_high = mean_val + qt(0.975, df = n-1) * se,
        .groups = 'drop'
      )
    
    # 4. 準備副標題與圖形說明 (明確告知小點與菱形代表什麼)
    status_text <- if(is_sig) "Significant Drop" else "No Significant Change"
    
    # 在這裡加入明確的圖例說明 (Point Guide)
    subtitle_text <- paste0(
      "Welch T-Test: p = ", p_val, " (", status_text, ")<br>",
      "Mean Diff: ", diff_val, " | 95% CI of Diff: [", ci_low, ", ", ci_high, "]<br>",
      "<b>* Visual Guide: Small dots = Raw Data Points | ♦ Diamonds = Group Means</b>"
    )
    
    # 5. 繪圖 (分離 raw data 和 summary data)
    p <- ggplot() +
      # 圖層 1: 原始數據小點
      geom_jitter(data = df_clean, 
                  aes(x = phase, y = avg_rating, color = phase,
                      text = paste("Phase:", phase, "<br>Raw Rating:", round(avg_rating, 2))),
                  width = 0.2, alpha = 0.4, size = 2) +
      
      # 圖層 2: 誤差條 (Error Bars)
      geom_errorbar(data = df_summary, 
                    aes(x = phase, ymin = error_low, ymax = error_high, color = phase), 
                    width = 0.2, size = 1) +
      
      # 圖層 3: 均值菱形 (獨立設定 text 以解決 NA 問題)
      geom_point(data = df_summary, 
                 aes(x = phase, y = mean_val, color = phase,
                     text = paste("Phase:", phase, "<br>Group Mean:", round(mean_val, 3))), 
                 shape = 18, size = 5) +
      
      scale_x_discrete(limits = c("before rollout", "after rollout")) +
      scale_color_manual(values = c("before rollout" = "#2E8B57", "after rollout" = "#CD5C5C")) +
      
      labs(title = "Average Rating Before vs After Policy", 
           subtitle = subtitle_text,
           x = "Phase", y = "Average Rating") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none") # 隱藏右側預設圖例
    
    # 6. 輸出動態圖表
    ggplotly(p, tooltip = "text") %>% 
      layout(margin = list(t = 100)) # 增加頂部空間給三行副標題
  })
  # 5. Rating Forecast
  output$plotForecast <- renderPlotly({
    
    # 1. 強制 X 軸月份顯示為英文
    # "C" 代表標準電腦格式 (English)，這會讓月份變成 Jan, Feb...
    Sys.setlocale("LC_TIME", "C")
    
    # 2. 抓取 MongoDB 參數
    m_pred <- mongo(collection = "Model_Predictions", url = "mongodb+srv://teresali:123abc@cluster1.ow8c4i0.mongodb.net/Cluster1?retryWrites=true&w=majority")
    model_params <- m_pred$find()
    
    validate(need(nrow(model_params) > 0, "Waiting for model parameters..."))
    
    intercept_val <- as.numeric(model_params$intercept[1])
    slope_val     <- as.numeric(model_params$slope[1])
    
    # 3. 建立預測曲線 (從 1月1日 開始)
    start_date <- as.Date("2026-01-01")
    future_dates <- seq(start_date, as.Date("2026-06-30"), by = "day")
    
    df_forecast <- data.frame(
      date = future_dates,
      time_index = as.numeric(future_dates - start_date)
    ) %>%
      mutate(pred_rating = intercept_val + (slope_val * time_index))
    
    # 4. 處理歷史數據 (加入一月起的過濾條件)
    validate(need(exists("data_ratings"), "Historical data missing"))
    
    df_actual <- data_ratings %>%
      mutate(date = as.Date(date),
             avg_rating = as.numeric(avg_rating)) %>%
      filter(date >= as.Date("2026-01-01")) %>% # 修改處：強制從一月開始
      filter(!is.na(avg_rating))
    
    # 5. 繪圖
    p <- ggplot() +
      # 藍線：將 color 放入 aes 內以產生圖例
      geom_line(data = df_actual, 
                aes(x = date, y = avg_rating, group = 1, color = "Actual Data",
                    text = paste("Date:", date, "<br>Actual:", round(avg_rating, 2))), 
                size = 1) +
      
      # 紅線：將 color 放入 aes 內以產生圖例
      geom_line(data = df_forecast, 
                aes(x = date, y = pred_rating, group = 1, color = "Predicted Trend",
                    text = paste("Date:", date, "<br>Predicted:", round(pred_rating, 2))), 
                size = 1) +
      
      # 分界虛線
      geom_vline(xintercept = as.numeric(max(df_actual$date, na.rm = TRUE)), 
                 linetype = "dashed", color = "darkgrey") +
      
      # 手動指定顏色 (藍色對應 Actual, 紅色對應 Predicted)
      scale_color_manual(name="", 
                         values = c("Actual Data" = "blue", "Predicted Trend" = "red")) +
      
      # 修改 X 軸顯示格式為月 (Jan, Feb...)
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      
      labs(title = "Rating Trend Forecast: May - Jun",
           x = "Date", y = "Average Rating") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") # 將圖例放在底部
    
    # 6. 輸出
    ggplotly(p, tooltip = "text") %>%
      layout(
        # 增加 b (bottom) 的數值，給 legend 和 X 軸標題留出更多空間
        margin = list(t = 70, b = 100, l = 60, r = 50), 
        
        # 設定 X 軸標題的距離
        xaxis = list(
          title = list(text = "Date", standoff = 40) # standoff 會把 "Date" 字樣往下推
        ),
        
        # 調整 Legend 的位置，y = -0.3 會讓它比原本的 -0.2 更低
        legend = list(
          orientation = "h", 
          x = 0.5, 
          xanchor = "center", 
          y = -0.3, # 數值越小，位置越低
          yanchor = "top"
        )
      )
  })
}

shinyApp(ui = ui, server = server)
