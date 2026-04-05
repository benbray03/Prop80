library(shiny)
library(leaflet)
library(dplyr)
library(tidyr)
library(RColorBrewer)

# ── Load data ──────────────────────────────────────────────────────────────────
dat <- readRDS("data/CPFV_PA_sp_decade_roms_n_ensemble_shiny.rds")

# Parse lon/lat from cell_coord_id  (format: "lon-lat", e.g. "-120.5-34.2")
# Handles negative longitudes: split on the LAST dash that is preceded by a digit
dat <- dat %>%
  mutate(
    lat = as.numeric(sub("-.*", "", cell_coord_id)),        # everything before the first dash
    lon = -as.numeric(sub(".*-", "", cell_coord_id))        # everything after the last dash, negated
  )

keys <- dat %>%
  distinct(species, decade) %>%
  arrange(species, decade) %>%
  mutate(key = paste(species, decade, sep = "||")) %>%
  pull(key)

dat_split <- dat %>%
  arrange(species, decade) %>%
  group_by(species, decade) %>%
  group_split() %>%
  setNames(keys)

species_list <- sort(unique(dat$species))
decade_list  <- sort(unique(dat$decade))

# ── Colour palette (shared, rescaled per species+decade subset) ────────────────
pal_fun <- colorNumeric(
  palette = rev(brewer.pal(11, "RdYlBu")),
  domain  = c(0, 1),   # pa values are probabilities 0-1
  na.color = "transparent"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
             background: #f4f6f9; margin: 0; }
      .app-header { background: #1a3a5c; color: white; padding: 14px 24px;
                    display: flex; align-items: center; gap: 16px; }
      .app-header h2 { margin: 0; font-size: 1.25rem; font-weight: 600; }
      .app-header .subtitle { font-size: 0.85rem; opacity: 0.75; }
      .sidebar-panel { background: white; border-radius: 8px;
                       box-shadow: 0 1px 4px rgba(0,0,0,.12);
                       padding: 18px; margin: 16px 8px 16px 16px; }
      .map-panel    { margin: 16px 16px 16px 8px; border-radius: 8px;
                      overflow: hidden;
                      box-shadow: 0 1px 4px rgba(0,0,0,.15); }
      .species-btn  { display: block; width: 100%; text-align: left;
                      padding: 8px 12px; margin-bottom: 6px; border: none;
                      border-radius: 6px; cursor: pointer;
                      background: #eef2f7; color: #1a3a5c;
                      font-size: 0.88rem; font-weight: 500;
                      transition: background .15s, color .15s; }
      .species-btn:hover  { background: #c8d8ec; }
      .species-btn.active { background: #1a3a5c; color: white; }
      .decade-row   { display: flex; flex-wrap: wrap; gap: 8px;
                      margin-top: 12px; }
      .decade-btn   { flex: 1 1 calc(50% - 4px); padding: 7px 4px;
                      border: 2px solid #c0cfe0; border-radius: 6px;
                      background: white; color: #1a3a5c; font-size: 0.82rem;
                      font-weight: 600; cursor: pointer; text-align: center;
                      transition: all .15s; }
      .decade-btn:hover  { background: #eef2f7; }
      .decade-btn.active { background: #1a3a5c; color: white;
                           border-color: #1a3a5c; }
      .section-label { font-size: 0.75rem; font-weight: 700; letter-spacing: .06em;
                       text-transform: uppercase; color: #7a90a8;
                       margin: 16px 0 8px; }
      .stat-box { background: #eef2f7; border-radius: 6px; padding: 10px 14px;
                  margin-top: 14px; font-size: 0.82rem; color: #1a3a5c; }
      .stat-box b { font-size: 1rem; }
    "))
  ),
  
  # Header
  div(class = "app-header",
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/2942/2942909.png",
               height = "36px"),
      div(
        h2("CPFV Species Distribution — Decadal Presence / Absence"),
        div(class = "subtitle", "ROMS ensemble model · ensemble mean PA by decade")
      )
  ),
  
  fluidRow(
    # ── Sidebar ──
    column(3,
           div(class = "sidebar-panel",
               div(class = "section-label", "Species"),
               uiOutput("species_buttons"),
               div(class = "section-label", "Decade"),
               uiOutput("decade_buttons"),
               div(class = "stat-box",
                   uiOutput("stats_out")
               )
           )
    ),
    # ── Map ──
    column(9,
           div(class = "map-panel",
               leafletOutput("map", height = "82vh")
           )
    )
  ),
  
  # JS to handle button clicks → update hidden inputs
  tags$script(HTML("
    $(document).on('click', '.species-btn', function() {
      $('.species-btn').removeClass('active');
      $(this).addClass('active');
      Shiny.setInputValue('selected_species', $(this).data('val'), {priority: 'event'});
    });
    $(document).on('click', '.decade-btn', function() {
      $('.decade-btn').removeClass('active');
      $(this).addClass('active');
      Shiny.setInputValue('selected_decade', $(this).data('val'), {priority: 'event'});
    });
  "))
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    species = species_list[1],
    decade  = decade_list[1]
  )
  
  observeEvent(input$selected_species, { rv$species <- input$selected_species })
  observeEvent(input$selected_decade,  { rv$decade  <- input$selected_decade  })
  
  # Species buttons
  output$species_buttons <- renderUI({
    lapply(species_list, function(sp) {
      cls <- if (sp == rv$species) "species-btn active" else "species-btn"
      tags$button(class = cls, `data-val` = sp, sp)
    })
  })
  
  # Decade buttons
  output$decade_buttons <- renderUI({
    div(class = "decade-row",
        lapply(decade_list, function(dec) {
          cls <- if (dec == rv$decade) "decade-btn active" else "decade-btn"
          tags$button(class = cls, `data-val` = dec, dec)
        })
    )
  })
  
  # Filtered subset
  subset_dat <- reactive({
    key <- paste(rv$species, rv$decade, sep = "||")
    dat_split[[key]]
  })
  
  # Stats
  output$stats_out <- renderUI({
    d <- subset_dat()
    if (nrow(d) == 0) return(p("No data for this selection."))
    HTML(sprintf(
      "<b>%s</b><br>Decade: %s<br><br>
       Cells: <b>%d</b><br>
       Mean PA: <b>%.3f</b><br>
       Range: <b>%.3f – %.3f</b>",
      rv$species, rv$decade,
      nrow(d),
      mean(d$pa_decade_mean_pa, na.rm = TRUE),
      min(d$pa_decade_mean_pa,  na.rm = TRUE),
      max(d$pa_decade_mean_pa,  na.rm = TRUE)
    ))
  })
  
  # Base map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -122, lat = 36, zoom = 6) %>%
      addLegend(
        position = "bottomright",
        pal      = pal_fun,
        values   = seq(0, 1, by = 0.1),
        title    = "Mean PA",
        opacity  = 0.85
      )
  })
  
  # Update circles when selection changes
  observe({
    d <- subset_dat()
    
    leafletProxy("map") %>%
      clearGroup("pa_points")
    
    if (nrow(d) == 0) return()
    
    leafletProxy("map") %>%
      addCircleMarkers(
        data        = d,
        lng         = ~lon,
        lat         = ~lat,
        group       = "pa_points",
        radius      = 5,
        color       = NA,
        fillColor   = ~pal_fun(pa_decade_mean_pa),
        fillOpacity = 0.85,
        popup       = ~paste0(
          "<b>", species, "</b><br>",
          "Decade: ", decade, "<br>",
          "Cell: ", cell_coord_id, "<br>",
          "PA: <b>", round(pa_decade_mean_pa, 3), "</b><br>",
          "Model: ", roms_model
        )
      )
  })
}

shinyApp(ui, server)
