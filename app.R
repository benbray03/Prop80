library(shiny)
library(leaflet)
library(dplyr)

df <- read.csv("data/test_preds_shiny.csv", stringsAsFactors = FALSE)

predictors <- c(
  "Log Mean Groundfish Biomass" = "log_d_mean_gb",
  "Mean Oxygen 0–50m"           = "oxygen_mean_0_50",
  "Surface Oxygen"              = "oxygen_surface",
  "Surface Salinity"            = "salt_surface",
  "Mean Temperature 0–50m"      = "temp_mean_0_50"
)

units <- c(
  log_d_mean_gb    = "log units",
  oxygen_mean_0_50 = "µmol/kg",
  oxygen_surface   = "µmol/kg",
  salt_surface     = "PSU",
  temp_mean_0_50   = "°C"
)

div_palette <- c("#313695","#4575b4","#74add1","#abd9e9","#e0f3f8",
                 "#ffffbf","#fee090","#fdae61","#f46d43","#d73027","#a50026")

ui <- fluidPage(
  tags$head(tags$style(HTML("
    * { box-sizing: border-box; }
    body, html { margin: 0; padding: 0; height: 100%; overflow: hidden;
                 font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
    #map { position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: 1; }

    #controls {
      position: fixed; top: 16px; left: 16px; z-index: 1000;
      background: white; border-radius: 10px;
      box-shadow: 0 2px 14px rgba(0,0,0,0.18);
      padding: 16px 18px; width: 290px;
    }
    #controls h4 { margin: 0 0 14px; font-size: 15px; font-weight: 700;
                   color: #1a237e; }
    #controls label { font-size: 11px; color: #666; font-weight: 600;
                      text-transform: uppercase; letter-spacing: 0.5px;
                      display: block; margin-bottom: 5px; }
    #controls select { width: 100%; padding: 8px 10px; border: 1px solid #ddd;
                       border-radius: 6px; font-size: 13px; margin-bottom: 14px;
                       background: #fafafa; outline: none; }
    .hw-toggle { display: flex; gap: 8px; }
    .hw-btn {
      flex: 1; padding: 8px 0; border: 1.5px solid #ccc;
      border-radius: 6px; font-size: 12px; font-weight: 600;
      cursor: pointer; background: white; color: #999;
      transition: all 0.15s; text-align: center;
    }
    .hw-btn.active-pre  { background: #1565C0; border-color: #1565C0; color: white; }
    .hw-btn.active-post { background: #B71C1C; border-color: #B71C1C; color: white; }

    #legend-panel {
      position: fixed; bottom: 36px; left: 16px; z-index: 1000;
      background: white; border-radius: 10px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.15);
      padding: 12px 16px; min-width: 220px;
    }
    .leg-title { font-size: 11px; font-weight: 700; color: #444;
                 text-transform: uppercase; letter-spacing: 0.4px; margin-bottom: 7px; }
    .leg-bar { height: 12px; border-radius: 3px; margin-bottom: 5px;
               background: linear-gradient(to right,
                 #313695,#4575b4,#74add1,#abd9e9,#e0f3f8,
                 #ffffbf,#fee090,#fdae61,#f46d43,#d73027,#a50026); }
    .leg-labels { display: flex; justify-content: space-between;
                  font-size: 11px; color: #666; }

    #stats-panel {
      position: fixed; top: 16px; right: 16px; z-index: 1000;
      background: white; border-radius: 10px;
      box-shadow: 0 2px 12px rgba(0,0,0,0.15);
      padding: 14px 16px; min-width: 210px;
    }
    #stats-panel h5 { margin: 0 0 10px; font-size: 13px; font-weight: 700;
                      color: #1a237e; }
    .stat-row { display: flex; justify-content: space-between;
                font-size: 12px; padding: 4px 0;
                border-bottom: 1px solid #f2f2f2; }
    .stat-row:last-child { border-bottom: none; }
    .stat-key { color: #888; }
    .stat-val { font-weight: 700; color: #222; }
    .delta-pos { color: #B71C1C; }
    .delta-neg { color: #1565C0; }
    .section-label { font-size: 10px; font-weight: 700; color: #aaa;
                     text-transform: uppercase; letter-spacing: 0.5px;
                     padding: 6px 0 2px; }
  "))),
  
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateBtnClass', function(msg) {
      document.getElementById('btn_pre').className  = msg.pre;
      document.getElementById('btn_post').className = msg.post;
    });
  ")),
  
  leafletOutput("map", width = "100%", height = "100vh"),
  
  div(id = "controls",
      tags$h4("Ocean Conditions Explorer"),
      tags$label(`for` = "var_select", "Predictor variable"),
      selectInput("var_select", label = NULL,
                  choices = predictors, selected = "log_d_mean_gb"),
      tags$label("Time frame"),
      div(class = "hw-toggle",
          actionButton("btn_pre",  "Pre Heat Wave",  class = "hw-btn active-pre"),
          actionButton("btn_post", "Post Heat Wave", class = "hw-btn active-post")
      )
  ),
  
  div(id = "legend-panel",
      div(class = "leg-title", textOutput("legend_title", inline = TRUE)),
      div(class = "leg-bar"),
      div(class = "leg-labels",
          span(textOutput("legend_min", inline = TRUE)),
          span(textOutput("legend_max", inline = TRUE))
      )
  ),
  
  div(id = "stats-panel",
      tags$h5(textOutput("stats_title", inline = TRUE)),
      uiOutput("stats_table")
  )
)

server <- function(input, output, session) {
  
  active_tf <- reactiveVal(c("PreHW", "PostHW"))
  
  observeEvent(input$btn_pre, {
    cur <- active_tf()
    if ("PreHW" %in% cur && length(cur) > 1) active_tf(setdiff(cur, "PreHW"))
    else if (!"PreHW" %in% cur) active_tf(sort(c(cur, "PreHW")))
  })
  
  observeEvent(input$btn_post, {
    cur <- active_tf()
    if ("PostHW" %in% cur && length(cur) > 1) active_tf(setdiff(cur, "PostHW"))
    else if (!"PostHW" %in% cur) active_tf(sort(c(cur, "PostHW")))
  })
  
  observe({
    cur <- active_tf()
    session$sendCustomMessage("updateBtnClass", list(
      pre  = if ("PreHW"  %in% cur) "hw-btn active-pre"  else "hw-btn",
      post = if ("PostHW" %in% cur) "hw-btn active-post" else "hw-btn"
    ))
  })
  
  filt <- reactive({
    df %>% filter(time_frame %in% active_tf())
  })
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -121, lat = 37.5, zoom = 6)
  })
  
  observe({
    d   <- filt()
    var <- input$var_select
    req(nrow(d) > 0)
    
    vals <- d[[var]]
    cp   <- colorNumeric(palette = div_palette, domain = range(vals, na.rm = TRUE))
    un   <- units[var]
    vlab <- names(predictors)[predictors == var]
    tf_label <- ifelse(d$time_frame == "PreHW", "Pre Heat Wave", "Post Heat Wave")
    
    popup_txt <- paste0(
      "<b style='color:#1a237e'>", vlab, "</b><br>",
      "<b>", round(vals, 3), "</b> ", un, "<br>",
      "<span style='color:#666'>", tf_label, "</span><br>",
      "<span style='color:#999; font-size:11px'>",
      round(d$lat, 3), "°N, ", round(d$lon, 3), "°E</span>"
    )
    
    leafletProxy("map") %>%
      clearShapes() %>%
      addCircles(
        data        = d,
        lng         = ~lon,
        lat         = ~lat,
        radius      = 2800,
        fillColor   = ~cp(vals),
        color       = ~cp(vals),
        fillOpacity = 0.85,
        weight      = 0,
        popup       = popup_txt
      )
  })
  
  output$legend_title <- renderText({
    var <- input$var_select
    paste0(names(predictors)[predictors == var], " (", units[var], ")")
  })
  
  output$legend_min <- renderText({
    d <- filt(); var <- input$var_select
    req(nrow(d) > 0)
    round(min(d[[var]], na.rm = TRUE), 2)
  })
  
  output$legend_max <- renderText({
    d <- filt(); var <- input$var_select
    req(nrow(d) > 0)
    round(max(d[[var]], na.rm = TRUE), 2)
  })
  
  output$stats_title <- renderText({
    tfs <- active_tf()
    if (length(tfs) == 2)       "Pre vs Post Summary"
    else if ("PreHW" %in% tfs)  "Pre Heat Wave"
    else                         "Post Heat Wave"
  })
  
  output$stats_table <- renderUI({
    d   <- filt()
    var <- input$var_select
    req(nrow(d) > 0)
    
    row <- function(key, val, cls = "stat-val") {
      div(class = "stat-row",
          span(class = "stat-key", key),
          span(class = cls, val)
      )
    }
    section <- function(label) div(class = "section-label", label)
    
    rows <- list()
    
    if ("PreHW" %in% active_tf()) {
      pre <- d %>% filter(time_frame == "PreHW") %>% pull(!!var)
      rows <- c(rows, list(
        section("Pre Heat Wave"),
        row("Mean",   round(mean(pre,   na.rm=TRUE), 3)),
        row("Median", round(median(pre, na.rm=TRUE), 3)),
        row("SD",     round(sd(pre,     na.rm=TRUE), 3))
      ))
    }
    
    if ("PostHW" %in% active_tf()) {
      post <- d %>% filter(time_frame == "PostHW") %>% pull(!!var)
      rows <- c(rows, list(
        section("Post Heat Wave"),
        row("Mean",   round(mean(post,   na.rm=TRUE), 3)),
        row("Median", round(median(post, na.rm=TRUE), 3)),
        row("SD",     round(sd(post,     na.rm=TRUE), 3))
      ))
    }
    
    if (all(c("PreHW","PostHW") %in% active_tf())) {
      pre_mn  <- mean(d[d$time_frame=="PreHW",  var], na.rm=TRUE)
      post_mn <- mean(d[d$time_frame=="PostHW", var], na.rm=TRUE)
      delta   <- round(post_mn - pre_mn, 3)
      pct     <- round((delta / abs(pre_mn)) * 100, 2)
      cls     <- if (delta >= 0) "stat-val delta-pos" else "stat-val delta-neg"
      arrow   <- if (delta >= 0) "\u25b2" else "\u25bc"
      rows <- c(rows, list(
        section("Change"),
        row("Delta (Post\u2212Pre)", paste(arrow, delta), cls),
        row("% change", paste0(pct, "%"), cls)
      ))
    }
    
    do.call(div, rows)
  })
}

shinyApp(ui, server)