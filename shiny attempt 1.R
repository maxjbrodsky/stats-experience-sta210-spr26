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
        choices = c("Measles", "Rubella", "Both"),
        multiple = FALSE,
        selected = c("Both"),
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
      choices <- c(choices, "Lab Confirmed", "Epi Linked", "Clinical")
    }
    if ("Rubella" %in% input$disease) {
      choices <- c(choices, "Lab Confirmed", "Epi Linked", "Clinical")
    }
    if ("Both" %in% input$disease) {
      choices <- c(choices, "Lab Confirmed", "Epi Linked", "Clinical")
    }
    updatePickerInput(session, "case_type", choices = unique(choices), selected = unique(choices))
  }, ignoreNULL = FALSE)

  output$map <- renderLeaflet({
    leaflet(world) |> addTiles()
  })

  observe({
    
    req(input$disease, input$case_type, input$year)

    filtered <- cases |>
      filter(year >= input$year[1], year <= input$year[2])

    selected_cols <- c()
    if ("Measles" %in% input$disease) {
      if ("Lab Confirmed" %in% input$case_type) selected_cols <- c(selected_cols, "sum_lab_m")
      if ("Epi Linked" %in% input$case_type) selected_cols <- c(selected_cols, "sum_epi_m")
      if ("Clinical" %in% input$case_type) selected_cols <- c(selected_cols, "sum_clinic_m")
    }
    if ("Rubella" %in% input$disease) {
      if ("Lab Confirmed" %in% input$case_type) selected_cols <- c(selected_cols, "sum_lab_r")
      if ("Epi Linked" %in% input$case_type) selected_cols <- c(selected_cols, "sum_epi_r")
      if ("Clinical" %in% input$case_type) selected_cols <- c(selected_cols, "sum_clinic_r")
    }
    if ("Both" %in% input$disease) {
      if ("Lab Confirmed" %in% input$case_type) selected_cols <- c(selected_cols, "sum_lab")
      if ("Epi Linked" %in% input$case_type) selected_cols <- c(selected_cols, "sum_epi")
      if ("Clinical" %in% input$case_type) selected_cols <- c(selected_cols, "sum_clinic")
    }

    filtered <- filtered |>
      rowwise() |>
      mutate(sum_selected = sum(c_across(all_of(selected_cols)), na.rm = TRUE)) |>
      ungroup()

    country_data <- filtered |>
      group_by(iso3) |>
      summarise(sum_selected = sum(sum_selected, na.rm = TRUE), .groups = "drop")

    map_df <- world |>
      left_join(country_data, by = c("iso_a3" = "iso3"))

    map_df <- map_df |>
      rowwise() |>
      mutate(
        popup_text = if ("Rubella" %in% input$disease) {
          paste0("<b>", name, "</b><br>Rubella<br>Cases: ", sum_selected)
        } else if ("Measles" %in% input$disease) {
          paste0("<b>", name, "</b><br>Measles<br>Cases: ", sum_selected)
        } else {
          paste0("<b>", name, "</b><br>Measles and Rubella<br>Cases: ", sum_selected)
        }
      ) |>
      ungroup()

    pal <- colorNumeric(
      palette = if ("Rubella" %in% input$disease) {"Purples"} 
      else if ("Measles" %in% input$disease) {"Reds"} 
      else {"Greens"},
      domain = map_df$sum_selected,
      na.color = "#f0f0f0"
    )

    leafletProxy("map", data = map_df) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor = ~pal(sum_selected),
        fillOpacity = 0.7,
        color = "#444",
        weight = 0.5,
        popup = ~popup_text
      ) |>
      addLegend(
        pal = pal,
        values = map_df$sum_selected,
        title = "Number of Cases",
        position = "bottomright"
      )
  })
}

shinyApp(ui, server)
