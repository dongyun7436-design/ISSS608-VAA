library(shiny)
library(fable)
library(tsibble)
library(feasts)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(leaflet)
library(sf)
library(rnaturalearth)
library(countrycode)
library(ggplot2)
Sys.setlocale("LC_TIME", "English")
process_wide_data <- function(file_path, type_label) {
  df <- read_excel(file_path)
  
  df %>%
    # 修复日期转换：针对 01/2005 这种格式
    mutate(date_col = floor_date(as.Date(parse_date_time(Month, c("my", "ymd"))), "month")) %>% 
    pivot_longer(
      cols = -c(Month, date_col), 
      names_to = "country", 
      values_to = "total"
    ) %>%
    mutate(
      # 自动识别大洲：如果是具体国家，会返回大洲名；如果本身就是大洲名（如Asia），会返回 NA
      region = countrycode(country, origin = "country.name", destination = "continent",
                           custom_match = c("china-hk" = "Asia", "Russia" = "Europe", "USA" = "Americas")),
      iso3 = countrycode(country, origin = "country.name", destination = "iso3c",
                         custom_match = c("china-hk" = "HKG", "Russia" = "RUS", "USA" = "USA")),
      category = type_label
    ) %>%
    # 关键：过滤掉数据中原本就有的“大洲总计”列，只留具体国家
    filter(!is.na(region))
}


# 加载数据
data_import_long <- process_wide_data("data/Import.xlsx", "import")
data_export_long <- process_wide_data("data/Export.xlsx", "export")


world_sf <- ne_countries(scale = "medium", returnclass = "sf")


ui <- navbarPage(
  title = "国际贸易分析与预测系统（待统一）",
  theme = shinythemes::shinytheme("flatly"),
  
  tabPanel("Global Trade Dynamic Map",
           sidebarLayout(
             sidebarPanel(
               radioButtons("map_type", "Select Trade Flow:", 
                            choices = c("Import" = "import", "Export" = "export")),
               sliderInput("map_date", "Select Time:", 
                           min = min(data_import_long$date_col), 
                           max = max(data_import_long$date_col),
                           value = min(data_import_long$date_col), 
                           timeFormat = "%Y-%m", animate = TRUE),
               helpText("Note: The map matches by country name; continent-level data will not be displayed on the map.White represents data that is approximately 0 or missing"),
               helpText("Note: Units are in Millions of Singapore Dollars (S$M).")
             ),
             mainPanel(leafletOutput("world_map", height = "750px"))
           )
  ),
  tabPanel("Country Analysis & Forecast",
           sidebarLayout(
             sidebarPanel(
               selectInput("region_select", "1. Select Continent:", choices = NULL),
               selectInput("country_select", "2. Select Country:", choices = NULL),
               radioButtons("analysis_mode", "3. Function Mode:", 
                            choices = c("Time Series Diagnosis" = "diag", "Trend Forecast" = "fore")),
               hr(),
               radioButtons("type_select", "4. Select Trade Flow:", 
                            choices = c("Import" = "import", "Export" = "export"),
                            selected = "import"),
               hr(),
               helpText("Note: Units are in Millions of Singapore Dollars (S$M)."),
               conditionalPanel(
                 condition = "input.analysis_mode == 'fore'",
                 helpText("Model: Automated Optimal ETS Algorithm"),
                 helpText("Horizon: Future 12 Months"),
                 helpText("The blue part is the prediction.")
               ),
               conditionalPanel(
                 condition = "input.analysis_mode == 'diag'",
                 helpText("Includes: Historical Trend, ACF, and PACF plots.")
               )
             ),
             mainPanel(
               uiOutput("analysis_ui")
             )
           )
  )
)
server <- function(input, output, session) {
  
  # 预加载地图（确保包含 iso_a3 列）
  world_sf <- ne_countries(scale = "medium", returnclass = "sf")
  
  # 基础地图
  output$world_map <- renderLeaflet({
    leaflet(world_sf) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(10, 20, 2)
  })
  
  observe({
    # 1. 选择并过滤数据
    current_raw <- if(input$map_type == "import") data_import_long else data_export_long
    target_month <- format(input$map_date, "%Y-%m")
    
    plot_data <- current_raw %>% 
      filter(format(date_col, "%Y-%m") == target_month)
    
    if(nrow(plot_data) == 0) return()
    
    # 2. 【核心步骤】自动识别国家名并转为 ISO3 编码
    plot_data <- plot_data %>%
      mutate(iso3 = countrycode(country, 
                                origin = "country.name", 
                                destination = "iso3c",
                                # 针对一些 countrycode 可能识别不到的特殊缩写进行手动微调
                                custom_match = c("Hong Kong SAR (China)" = "HKG", "USA" = "USA", "Russia" = "RUS")))
    
    # 3. 关联地图 (使用你之前成功的 iso_a3 匹配)
    map_sf <- world_sf %>% 
      left_join(plot_data, by = c("iso_a3" = "iso3"))
    
    # 4. 颜色映射逻辑
    # 提取有效数值
    vals <- map_sf$total[!is.na(map_sf$total) & map_sf$total > 0]
    if(length(vals) == 0) return()
    
    log_vals <- log1p(vals)
    current_domain <- range(log_vals)
    if(diff(current_domain) == 0) current_domain <- c(current_domain[1], current_domain[1] + 1)
    
    pal <- colorNumeric(palette = "Greens", domain = current_domain, na.color = "#F5F5F5")
    
    # 5. 更新地图
    leafletProxy("world_map", data = map_sf) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(log1p(total)),
        weight = 0.5, color = "white", fillOpacity = 0.8,
        label = ~paste0(name, ": ", ifelse(is.na(total), "No Data", round(total, 2))),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      removeControl("map_legend") %>%
      addLegend(
        "bottomright", pal = pal, values = current_domain,
        title = paste(input$map_type, "(Log)"),
        labFormat = labelFormat(transform = function(x) round(expm1(x), 0)),
        layerId = "map_legend"
      )
  })
  
  all_data_combined <- reactive({
    bind_rows(data_import_long, data_export_long)
  })
  
  observe({
    regions <- sort(unique(all_data_combined()$region))
    updateSelectInput(session, "region_select", choices = regions)
  })
  
  observeEvent(input$region_select, {
    req(input$region_select)
    countries <- all_data_combined() %>% 
      filter(region == input$region_select) %>% 
      pull(country) %>% unique() %>% sort()
    updateSelectInput(session, "country_select", choices = countries)
  })
  
  # 2. 补全动态 UI 逻辑 (这是解决你现在页面没反应/空白的关键)
  output$analysis_ui <- renderUI({
    if (input$analysis_mode == "diag") {
      plotOutput("ts_diagnosis", height = "600px") # 显示诊断图
    } else {
      tabsetPanel(
        tabPanel("Forecast Plot", plotOutput("forecast_plot", height = "500px")), # 显示预测图
        tabPanel("Model Summary", verbatimTextOutput("model_summary"))
      )
    }
  })

  selected_series <- reactive({
    req(input$country_select)
    df <- if(input$type_select == "import") data_import_long else data_export_long
    df %>% filter(country == input$country_select) %>%
      mutate(Month_idx = yearmonth(date_col)) %>%
      as_tsibble(index = Month_idx, key = c(country, category)) %>% 
      fill_gaps() # 填充缺失月份
  })
  output$ts_diagnosis <- renderPlot({
    req(nrow(selected_series()) > 0)
    selected_series() %>%
      gg_tsdisplay(total, plot_type = 'partial') +
      labs(title = paste("TS Diagnosis Report for", input$country_select),
           subtitle = "Showing original trend, ACF, and PACF",
           x = "Year (Month)", y = "Trade Volume") +
      theme_minimal()
  })
  
  # 预测图
  output$forecast_plot <- renderPlot({
    req(nrow(selected_series()) > 0)
    selected_series() %>%
      model(best_ets = ETS(total)) %>%
      forecast(h = 12) %>%
      autoplot(selected_series()) +
      labs(title = paste(input$type_select, "Trend Forecast for", input$country_select),
           subtitle = "Based on Optimal ETS Exponential Smoothing Model (Next 12 Months)",
           x = "Time (Month)", y = "Value") +
      theme_minimal()
  })
  
  # 模型文本报告
  output$model_summary <- renderPrint({
    req(nrow(selected_series()) > 0)
    selected_series() %>% 
      model(best_ets = ETS(total)) %>% 
      report()
  })
}

# --- 4. 启动 ---
shinyApp(ui = ui, server = server)


