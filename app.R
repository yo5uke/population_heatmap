library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(leaflet)

source("helpers.R")

# 1. ヘッダー部分のレイアウト調整
custom_header <- dashboardHeader(
  title = tags$div(
    style = "display: flex; align-items: center; justify-content: space-between; width: 100%;",
    tags$span(
      "人口ヒートマップ",
      style = "font-weight:bold; font-size: 1.6rem; letter-spacing:0.08em; color: #fff;"
    ),
    tags$span(
      style = "display: flex; gap: 15px;",
      tags$a(href = "https://yo5uke.com/pages/software/", icon("home"), title = "ソフトウェア一覧", target = "_blank", style = "color:white; font-size:22px;"),
      tags$a(href = "https://github.com/yo5uke/population_heatmap", icon("github"), title = "GitHub", target = "_blank", style = "color:white; font-size:22px;")
    )
  ),
  titleWidth = "100%"
)

ui <- dashboardPage(
  skin = "green",
  header = custom_header,
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("地図", tabName = "map", icon = icon("map")),
      menuItem("用語・出典", tabName = "help", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .skin-green .main-header .logo,
      .skin-green .main-header .navbar {
        background-color: #78c2ad !important;
        color: #fff !important;
      }
      .skin-green .main-header .navbar .sidebar-toggle { color: #fff !important; }
      .skin-green .main-header .navbar .sidebar-toggle:hover { background: rgba(255,255,255,0.08) !important; }
  
      /* 設定boxタイトルバー */
      .box.box-solid.box-success > .box-header {
        color: #fff !important;
        background: #78c2ad !important;
        background-color: #78c2ad !important;
      }
      /* 設定boxの枠もminty色に */
      .box.box-solid.box-success {
        border: 1px solid #78c2ad !important;
      }
    "))),
    tabItems(
      tabItem(
        tabName = "map",
        fluidRow(
          column(
            width = 4,
            box(
              width = 12,
              title = "設定",
              solidHeader = TRUE, status = "success",
              helpText(
                tags$p("市町村ごとの人口や年齢別人口とその割合、人口の増加率を国勢調査ベースで可視化しています。"),
                tags$p("表示したい指標や年を選択してください。")
              ),
              selectInput("valueType", "値の種類",
                          choices = c("人口", "人口割合", "人口増加率"),
                          selected = "人口"),
              selectInput("ageCategory", "年齢区分",
                          choices = c("総人口", "15歳未満人口", "15歳～64歳人口", "65歳以上人口"),
                          selected = "総人口"),
              sliderTextInput("year", "表示する年",
                              choices = names(map_data_list),
                              selected = max(names(map_data_list)),
                              grid = TRUE),
              tags$div(
                style = "margin-top:18px; display: flex; gap:12px; justify-content: flex-start;",
                actionButton("show_help_modal", "🛈 用語説明"),
                actionButton("show_source_modal", "📦 データ出典")
              )
            )
          ),
          column(
            width = 8,
            box(
              width = 12,
              leafletOutput("map", height = "720px")
            )
          )
        )
      ),
      tabItem(
        tabName = "help",
        fluidRow(
          column(
            width = 8, offset = 2,
            box(
              width = 12,
              title = "アプリの用語・データ出典・ライセンス",
              solidHeader = TRUE, status = "success",
              div(
                style = "line-height:1.8;",
                HTML("
                  <h4>📊 データ出典</h4>
                  <ul>
                    <li>
                      総務省統計局『国勢調査』（1980～2020年）<br>
                      <a href='https://www.e-stat.go.jp/stat-search?page=1&toukei=00200521&survey=%E5%9B%BD%E5%8B%A2%E8%AA%BF%E6%9F%BB' target='_blank'>
                        e-Statの国勢調査ページ
                      </a>
                    </li>
                    <li>
                      国土数値情報ダウンロードサイト：<br>
                      <a href='https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-2024.html' target='_blank'>
                        2024年1月1日時点の行政区域データ
                      </a><br>
                      ※ データ軽量化のため <code>rmapshaper::ms_simplify()</code> を使用して簡素化しています。
                    </li>
                  </ul>
                  <h4>📝 ライセンス</h4>
                  <p>
                    このアプリは <strong>MITライセンス</strong> の下で公開されています。<br>
                    自由に利用・改変が可能ですが、著作権表示とライセンス文の保持が必要です。
                  </p>
                  <h4>🔗 ソースコード</h4>
                  <p>
                    GitHub リポジトリはこちら：<br>
                    <a href='https://github.com/yo5uke/population_heatmap' target='_blank'>
                      https://github.com/yo5uke/population_heatmap
                    </a>
                  </p>
                  <hr>
                  <h4>用語</h4>
                  <ul>
                    <li><b>人口割合</b>：各年齢階層の人口 ÷ 当該年の総人口 × 100（%）</li>
                    <li>
                      <b>人口増加率</b>：<br>
                      （当該年の人口 − 前回調査年の人口） ÷ 前回調査年の人口 × 100（%）<br>
                      ※ 正の値：増加、負の値：減少
                    </li>
                  </ul>
                ")
              )
            )
          )
        )
      )
    )
  )
)

# ---- サーバーロジック ----
server <- function(input, output, session) {
  # 動的UI
  observe({
    if (input$valueType == "人口") {
      updateSelectInput(session, "ageCategory",
                        choices = c("総人口", "15歳未満人口", "15～64歳人口", "65歳以上人口"),
                        selected = "総人口"
      )
    } else if (input$valueType == "人口割合") {
      updateSelectInput(session, "ageCategory",
                        choices = c("15歳未満人口割合", "15～64歳人口割合", "65歳以上人口割合"),
                        selected = "15歳未満人口割合"
      )
    } else if (input$valueType == "人口増加率") {
      updateSelectInput(session, "ageCategory",
                        choices = c("人口増加率", "15歳未満人口増加率", "15～64歳人口増加率", "65歳以上人口増加率"),
                        selected = "人口増加率"
      )
    }
  })
  
  observeEvent(input$valueType, {
    if (input$valueType == "人口増加率") {
      updateSliderTextInput(session, "year",
                            choices = names(map_data_list)[-1],
                            selected = tail(names(map_data_list), 1)
      )
    } else {
      updateSliderTextInput(session, "year",
                            choices = names(map_data_list),
                            selected = tail(names(map_data_list), 1)
      )
    }
  })
  
  selected_col <- reactive({
    switch(input$valueType,
           "人口" = c(
             "総人口" = "population",
             "15歳未満人口" = "pop_u15",
             "15～64歳人口" = "pop_15to64",
             "65歳以上人口" = "pop_o65"
           )[input$ageCategory],
           "人口割合" = c(
             "15歳未満人口割合" = "pct_u15",
             "15～64歳人口割合" = "pct_15to64",
             "65歳以上人口割合" = "pct_o65"
           )[input$ageCategory],
           "人口増加率" = c(
             "人口増加率" = "pct_inc_pop",
             "15歳未満人口増加率" = "pct_inc_u15",
             "15～64歳人口増加率" = "pct_inc_15to64",
             "65歳以上人口増加率" = "pct_inc_o65"
           )[input$ageCategory]
    )
  })
  
  filtered_gis <- reactive({
    req(input$year, selected_col())
    dat <- map_data_list[[as.character(input$year)]]
    col <- selected_col()
    dat$val <- dat[[col]]
    if (input$valueType == "人口") {
      dat$val_bin <- pmax(pmin(dat$val, 3000000), 0)
    } else if (input$valueType == "人口割合") {
      if (col == "pct_u15") {
        dat$val_bin <- pmax(pmin(dat$val, 30), 0)
      } else if (col == "pct_o65") {
        dat$val_bin <- pmax(pmin(dat$val, 40), 0)
      } else {
        dat$val_bin <- pmax(pmin(dat$val, 70), 0)
      }
    } else if (input$valueType == "人口増加率") {
      dat$val_bin <- pmax(pmin(dat$val, 15), -15)
    }
    dat
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = 138.36834, lat = 38.01827, zoom = 6)
  })
  
  observe({
    dat <- filtered_gis()
    req(nrow(dat) > 0)
    
    if (input$valueType == "人口増加率") {
      breaks <- c(-20, -15, -10, -5, 0, 5, 10, 15, 20)
      pal <- colorBin("RdYlBu", domain = dat$val_bin, bins = breaks, reverse = TRUE)
    } else if (input$valueType == "人口割合") {
      pal <- colorBin("YlGnBu", domain = dat$val_bin, bins = pretty(dat$val_bin, 8))
    } else {
      breaks <- c(0, 10000, 50000, 100000, 200000, 300000, 500000, 1000000, 2000000, 3000000)
      pal <- colorBin("YlOrRd", domain = dat$val_bin, bins = breaks)
    }
    
    legend_title <- switch(input$valueType,
                           "人口" = paste0(input$ageCategory, "（万人）<br>", input$year, "年"),
                           "人口割合" = paste0(input$ageCategory, "（%）<br>", input$year, "年"),
                           "人口増加率" = paste0(input$ageCategory, "（%）<br>", input$year, "年")
    )
    
    popup_text <- if (input$valueType == "人口") {
      if_else(
        is.na(dat$val),
        paste0(dat$name_muni, "<br>", input$year, "年国勢調査<br>データが欠損しています。"),
        paste0(dat$name_muni, "<br>", input$year, "年国勢調査<br>", format(dat$val, big.mark = ","), "人")
      )
    } else if (input$valueType == "人口割合") {
      if_else(
        is.na(dat$val),
        paste0(dat$name_muni, "<br>", input$year, "年国勢調査<br>割合：データが欠損しています。"),
        paste0(dat$name_muni, "<br>", input$year, "年国勢調査<br>割合：", dat$val, "%")
      )
    } else {
      if_else(
        is.na(dat$val),
        paste0(dat$name_muni, "<br>", input$year, "年国勢調査<br>前回国勢調査比：データが欠損しています。"),
        if_else(
          dat$val >= 0,
          paste0(dat$name_muni, "<br>", input$year, "年国勢調査<br>前回国勢調査比：+", dat$val, "%"),
          paste0(dat$name_muni, "<br>", input$year, "年国勢調査<br>前回国勢調査比：", dat$val, "%")
        )
      )
    }
    
    leafletProxy("map", data = dat) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor = ~pal(val_bin),
        color = "white",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.7,
        label = dat$name_muni,
        popup = popup_text
      ) |>
      addLegend(
        pal = pal,
        values = dat$val_bin,
        title = legend_title,
        position = "bottomright"
      )
  })
  
  observeEvent(input$show_help_modal, {
    showModal(modalDialog(
      title = "📝 用語説明",
      HTML("
        <ul style='padding-left: 20px;'>
          <li><b>人口割合</b>：各年齢階層の人口 ÷ 当該年の総人口 × 100（%）</li>
          <li>
            <b>人口増加率</b>：<br>
            （当該年の人口 − 前回調査年の人口） ÷ 前回調査年の人口 × 100（%）<br>
            ※ 正の値：増加、負の値：減少
          </li>
        </ul>
      "),
      easyClose = TRUE,
      footer = modalButton("閉じる"),
      size = "l"
    ))
  })
  
  observeEvent(input$show_source_modal, {
    showModal(modalDialog(
      title = "📦 データ出典・ライセンス・ソースコード",
      HTML("
        <div style='line-height: 1.6; font-size: 95%;'>
          <h4>📊 データ出典</h4>
          <ul>
            <li>
              総務省統計局『国勢調査』（1980～2020年）<br>
              <a href='https://www.e-stat.go.jp/stat-search?page=1&toukei=00200521&survey=%E5%9B%BD%E5%8B%A2%E8%AA%BF%E6%9F%BB' target='_blank'>
                e-Statの国勢調査ページ
              </a>
            </li>
            <li>
              国土数値情報ダウンロードサイト：<br>
              <a href='https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-N03-2024.html' target='_blank'>
                2024年1月1日時点の行政区域データ
              </a><br>
              ※ データ軽量化のため <code>rmapshaper::ms_simplify()</code> を使用して簡素化しています。
            </li>
          </ul>
          <h4>📝 ライセンス</h4>
          <p>
            このアプリは <strong>MITライセンス</strong> の下で公開されています。<br>
            自由に利用・改変が可能ですが、著作権表示とライセンス文の保持が必要です。
          </p>
          <h4>🔗 ソースコード</h4>
          <p>
            GitHub リポジトリはこちら：<br>
            <a href='https://github.com/yo5uke/population_heatmap' target='_blank'>
              https://github.com/yo5uke/population_heatmap
            </a>
          </p>
        </div>
      "),
      easyClose = TRUE,
      footer = modalButton("閉じる"),
      size = "l"
    ))
  })
}

shinyApp(ui, server)
