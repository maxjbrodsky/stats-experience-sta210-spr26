library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)


cases <- read_csv("cases.csv") 
cases <- na.omit(cases)

world <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(name, iso_a3, geometry)

ui <- fluidPage(
  titlePanel("Measles and Rubella Cases by Country"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      pickerInput(
        inputId = "disease",
        label = "Disease",
        choices = c("Measles", "Rubella"),
        multiple = TRUE,
        selected = c("Measles", "Rubella"),
        options = list(`actions-box` = TRUE)
      ),
      pickerInput(
        inputId = "case_type",
        label = "Case Type",
        choices = c(),  
        multiple = TRUE,
        options = list(`actions-box` = TRUE)
      ),
      sliderInput(
        inputId = "year",
        label = "Year",
        min = min(cases$year, na.rm = TRUE),
        max = max(cases$year, na.rm = TRUE),
        value = c(min(cases$year, na.rm = TRUE), max(cases$year, na.rm = TRUE)),
        step = 1,
        sep = ""
      )
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("map", height = "650px")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$disease, {
    choices <- c()
    if ("Measles" %in% input$disease) {
      choices <- c(choices, "Total", "Lab Confirmed", "Epi Linked", "Clinical")
    }
    if ("Rubella" %in% input$disease) {
      choices <- c(choices, "Total", "Lab Confirmed", "Epi Linked", "Clinical")
    }
    updatePickerInput(session, "case_type", choices = unique(choices), selected = unique(choices))
  }, ignoreNULL = FALSE)

  output$map <- renderLeaflet({
    leaflet(world) |> addTiles()
  })

  reactive({
    req(input$disease, input$case_type, input$year)

    filtered <- cases |>
      filter(year >= input$year[1], year <= input$year[2])

    selected_cols <- c()
    if ("Measles" %in% input$disease) {
      if ("Total" %in% input$case_type) selected_cols <- c(selected_cols, "measles_total")
      if ("Lab Confirmed" %in% input$case_type) selected_cols <- c(selected_cols, "measles_lab_confirmed")
      if ("Epi Linked" %in% input$case_type) selected_cols <- c(selected_cols, "measles_epi_linked")
      if ("Clinical" %in% input$case_type) selected_cols <- c(selected_cols, "measles_clinical")
    }
    if ("Rubella" %in% input$disease) {
      if ("Total" %in% input$case_type) selected_cols <- c(selected_cols, "rubella_total")
      if ("Lab Confirmed" %in% input$case_type) selected_cols <- c(selected_cols, "rubella_lab_confirmed")
      if ("Epi Linked" %in% input$case_type) selected_cols <- c(selected_cols, "rubella_epi_linked")
      if ("Clinical" %in% input$case_type) selected_cols <- c(selected_cols, "rubella_clinical")
    }

    filtered <- filtered |>
      rowwise() |>
      mutate(sum_m_r_selected = sum(c_across(all_of(selected_cols)), na.rm = TRUE)) |>
      ungroup()

    country_data <- filtered |>
      group_by(iso3) |>
      summarise(sum_m_r_selected = sum(sum_m_r_selected, na.rm = TRUE), .groups = "drop")

    map_df <- world |>
      left_join(country_data, by = c("iso_a3" = "iso3"))

    map_df <- map_df |>
      rowwise() |>
      mutate(
        popup_text = if (length(input$disease) == 2) {
          paste0("<b>", name, "</b><br>Measles and Rubella<br>Cases: ", sum_m_r_selected)
        } else if ("Measles" %in% input$disease) {
          paste0("<b>", name, "</b><br>Measles<br>Cases: ", sum_m_r_selected)
        } else {
          paste0("<b>", name, "</b><br>Rubella<br>Cases: ", sum_m_r_selected)
        }
      ) |>
      ungroup()

    pal <- colorNumeric(
      palette = if (length(input$disease) == 2) "Purples" else if ("Measles" %in% input$disease) "Reds" else "Greens",
      domain = map_df$sum_m_r_selected,
      na.color = "#f0f0f0"
    )

    leafletProxy("map", data = map_df) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor = ~pal(sum_m_r_selected),
        fillOpacity = 0.7,
        color = "#444",
        weight = 0.5,
        popup = ~popup_text
      ) |>
      addLegend(
        pal = pal,
        values = map_df$sum_m_r_selected,
        title = "Cases",
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)
