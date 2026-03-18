library(shiny)
library(leaflet)
library(dplyr)

df <- read.csv("data/test_preds_shiny.csv", stringsAsFactors = FALSE)

pred_vars <- c("log_d_mean_gb", "oxygen_mean_0_50", "oxygen_surface",
               "salt_surface",  "temp_mean_0_50")

var_labels <- c(
  log_d_mean_gb    = "Log Groundfish Biomass",
  oxygen_mean_0_50 = "Mean Oxygen 0–50m",
  oxygen_surface   = "Surface Oxygen",
  salt_surface     = "Surface Salinity",
  temp_mean_0_50   = "Mean Temp 0–50m"
)

var_units <- c(
  log_d_mean_gb    = "log units",
  oxygen_mean_0_50 = "µmol/kg",
  oxygen_surface   = "µmol/kg",
  salt_surface     = "PSU",
  temp_mean_0_50   = "°C"
)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@300;400;500;600&display=swap"),
    tags$style(HTML("

      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        font-family: 'IBM Plex Sans', sans-serif;
        background: #f0f4f8;
        color: #2d3f50;
        height: 100vh;
        overflow: hidden;
      }

      .container-fluid { padding: 0 !important; height: 100vh; }
      .row { height: 100vh; margin: 0 !important; display: flex; }

      /* ── sidebar ── */
      .col-sm-4 {
        width: 280px !important;
        flex: 0 0 280px !important;
        max-width: 280px !important;
        background: #ffffff;
        border-right: 1px solid #d6e4ef;
        display: flex;
        flex-direction: column;
        padding: 0 !important;
        height: 100vh;
        overflow-y: auto;
      }

      /* ── main panel ── */
      .col-sm-8 {
        flex: 1 1 auto !important;
        width: auto !important;
        max-width: none !important;
        padding: 0 !important;
        height: 100vh;
      }

      /* ── header ── */
      .app-header {
        padding: 20px 20px 16px;
        background: #2a7f62;
        color: white;
      }
      .app-title {
        font-size: 15px;
        font-weight: 600;
        letter-spacing: 0.2px;
        color: #ffffff;
      }
      .app-sub {
        font-size: 11px;
        color: rgba(255,255,255,0.65);
        margin-top: 3px;
        font-weight: 300;
      }

      /* ── controls ── */
      .controls-section {
        padding: 18px 20px;
        border-bottom: 1px solid #e8f0f7;
      }

      .ctrl-label {
        font-size: 10px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: #7a9bb5;
        margin-bottom: 6px;
        display: block;
      }

      .form-group { margin-bottom: 14px !important; }
      .form-group:last-child { margin-bottom: 0 !important; }
      .form-group label { display: none; }

      select.form-control {
        background: #f5f8fb !important;
        border: 1px solid #c8dce9 !important;
        border-radius: 6px !important;
        color: #2d3f50 !important;
        font-family: 'IBM Plex Sans', sans-serif !important;
        font-size: 13px !important;
        padding: 8px 32px 8px 11px !important;
        height: auto !important;
        cursor: pointer;
        transition: border-color 0.15s, box-shadow 0.15s;
        appearance: none;
        background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='11' height='7' viewBox='0 0 11 7'%3E%3Cpath d='M1 1l4.5 4.5L10 1' stroke='%237a9bb5' stroke-width='1.5' fill='none' stroke-linecap='round' stroke-linejoin='round'/%3E%3C/svg%3E\") !important;
        background-repeat: no-repeat !important;
        background-position: right 11px center !important;
      }
      select.form-control:focus {
        border-color: #2a7f62 !important;
        box-shadow: 0 0 0 3px rgba(42,127,98,0.1) !important;
        outline: none !important;
        background-color: #ffffff !important;
      }

      /* ── stats ── */
      .stats-section {
        padding: 18px 20px;
        flex: 1;
      }
      .stats-title {
        font-size: 10px;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.8px;
        color: #7a9bb5;
        margin-bottom: 12px;
      }
      .stats-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 8px;
      }
      .stat-card {
        background: #f5f8fb;
        border: 1px solid #dce9f3;
        border-radius: 7px;
        padding: 10px 12px;
      }
      .stat-card.full-width { grid-column: 1 / -1; }
      .stat-card-label {
        font-size: 10px;
        color: #7a9bb5;
        font-weight: 500;
        margin-bottom: 3px;
      }
      .stat-card-value {
        font-size: 15px;
        font-weight: 600;
        color: #2d3f50;
      }
      .stat-card-unit {
        font-size: 10px;
        color: #9fb8cc;
        margin-top: 1px;
      }

      /* ── badge ── */
      .hw-badge {
        display: inline-block;
        font-size: 10px;
        font-weight: 600;
        padding: 3px 10px;
        border-radius: 20px;
        margin-bottom: 14px;
        letter-spacing: 0.3px;
      }
      .hw-pre  { background: #ddeef9; color: #2a6496; border: 1px solid #b8d8ef; }
      .hw-post { background: #fce8e6; color: #b94038; border: 1px solid #f5c5c2; }

      /* ── map ── */
      #map { width: 100% !important; height: 100vh !important; }

      /* ── leaflet legend ── */
      .leaflet-control.legend {
        background: rgba(255,255,255,0.95) !important;
        border: 1px solid #d6e4ef !important;
        border-radius: 8px !important;
        color: #2d3f50 !important;
        font-family: 'IBM Plex Sans', sans-serif !important;
        font-size: 12px !important;
        padding: 10px 14px !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08) !important;
      }

    "))
  ),
  
  div(class = "container-fluid",
      div(class = "row",
          
          div(class = "col-sm-4",
              div(class = "app-header",
                  div(class = "app-title", "Ocean Conditions Explorer"),
                  div(class = "app-sub",   "California Current · Heat Wave Analysis")
              ),
              div(class = "controls-section",
                  span(class = "ctrl-label", "Time Frame"),
                  selectInput("timeframe", label = NULL,
                              choices = c("Pre Heat Wave" = "PreHW",
                                          "Post Heat Wave" = "PostHW")),
                  span(class = "ctrl-label", "Variable"),
                  selectInput("variable", label = NULL,
                              choices  = setNames(pred_vars, var_labels[pred_vars]),
                              selected = "log_d_mean_gb")
              ),
              div(class = "stats-section",
                  div(class = "stats-title", "Summary Statistics"),
                  uiOutput("stats_cards")
              )
          ),
          
          div(class = "col-sm-8",
              leafletOutput("map", height = "100vh")
          )
      )
  )
)

server <- function(input, output, session) {
  
  filtered <- reactive({
    df %>% filter(time_frame == input$timeframe)
  })
  
  pal <- reactive({
    colorNumeric("viridis", domain = df[[input$variable]])
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -121, lat = 37.5, zoom = 6)
  })
  
  observe({
    d   <- filtered()
    var <- input$variable
    cp  <- pal()
    lbl <- var_labels[var]
    un  <- var_units[var]
    
    popup_html <- paste0(
      "<div style='font-family:IBM Plex Sans,sans-serif;min-width:150px;padding:2px'>",
      "<div style='font-size:11px;font-weight:600;color:#7a9bb5;text-transform:uppercase;",
      "letter-spacing:0.5px;margin-bottom:4px'>", lbl, "</div>",
      "<div style='font-size:20px;font-weight:600;color:#2d3f50;line-height:1.1'>",
      round(d[[var]], 3), "</div>",
      "<div style='font-size:11px;color:#9fb8cc;margin-bottom:8px'>", un, "</div>",
      "<div style='font-size:11px;color:#aaa;border-top:1px solid #eef2f6;padding-top:6px'>",
      round(d$lat, 3), "°N · ", round(d$lon, 3), "°E</div>",
      "</div>"
    )
    
    leafletProxy("map") %>%
      clearShapes() %>%
      removeControl("legend") %>%
      addCircles(
        data        = d,
        lng         = ~lon,
        lat         = ~lat,
        radius      = 3000,
        fillColor   = ~cp(d[[var]]),
        color       = ~cp(d[[var]]),
        fillOpacity = 0.85,
        weight      = 0,
        popup       = popup_html
      ) %>%
      addLegend(
        position = "bottomright",
        pal      = cp,
        values   = d[[var]],
        title    = paste0(lbl, "<br><span style='font-weight:300;font-size:10px'>", un, "</span>"),
        layerId  = "legend",
        opacity  = 0.9
      )
  })
  
  output$stats_cards <- renderUI({
    d    <- filtered()
    var  <- input$variable
    un   <- var_units[var]
    vals <- d[[var]]
    
    badge_class <- if (input$timeframe == "PreHW") "hw-badge hw-pre" else "hw-badge hw-post"
    badge_label <- if (input$timeframe == "PreHW") "Pre Heat Wave" else "Post Heat Wave"
    
    tagList(
      div(class = badge_class, badge_label),
      div(class = "stats-grid",
          div(class = "stat-card",
              div(class = "stat-card-label", "Mean"),
              div(class = "stat-card-value", round(mean(vals, na.rm=TRUE), 3)),
              div(class = "stat-card-unit",  un)
          ),
          div(class = "stat-card",
              div(class = "stat-card-label", "Median"),
              div(class = "stat-card-value", round(median(vals, na.rm=TRUE), 3)),
              div(class = "stat-card-unit",  un)
          ),
          div(class = "stat-card",
              div(class = "stat-card-label", "Std Dev"),
              div(class = "stat-card-value", round(sd(vals, na.rm=TRUE), 3)),
              div(class = "stat-card-unit",  un)
          ),
          div(class = "stat-card",
              div(class = "stat-card-label", "n cells"),
              div(class = "stat-card-value", format(nrow(d), big.mark=",")),
              div(class = "stat-card-unit",  "observations")
          ),
          div(class = "stat-card full-width",
              div(class = "stat-card-label", "Range"),
              div(class = "stat-card-value",
                  paste0(round(min(vals, na.rm=TRUE), 2), " – ", round(max(vals, na.rm=TRUE), 2))),
              div(class = "stat-card-unit",  un)
          )
      )
    )
  })
}

shinyApp(ui, server)