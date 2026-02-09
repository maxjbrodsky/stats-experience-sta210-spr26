library(shiny)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# cases_year <- read.csv("casesdf.csv")

cases_year <- na.omit(cases_year)
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
        min = min(cases_year$year, na.rm = TRUE),
        max = max(cases_year$year, na.rm = TRUE),
        value = c(min(cases_year$year, na.rm = TRUE), max(cases_year$year, na.rm = TRUE)),
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
      choices <- unique(
        c(choices, c("Total", "Lab Confirmed", "Epi Linked", "Clinical"))
        )
    }
    
    if ("Rubella" %in% input$disease) {
      choices <- unique(
        c(choices, c("Total", "Lab Confirmed", "Epi Linked", "Clinical"))
        )
    }
    
    updatePickerInput(
      session,
      "case_type",
      choices = choices,
      selected = choices
    )
  }, ignoreNULL = FALSE)

  output$map <- renderLeaflet({
    leaflet(world) |> addTiles()
  })

  observe({
    
    req(input$disease, input$case_type, input$year)

    filtered_data <- cases_year |>
      filter(
        year >= input$year[1], year <= input$year[2]
        )
    
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

    filtered_data <- filtered_data |>
      rowwise() |>
      mutate(
        total_cases = sum(c_across(all_of(selected_cols)), na.rm = TRUE)
        ) |>
      ungroup()

    country_data <- filtered_data |>
      group_by(iso3) |>
      summarise(
        total_cases = sum(total_cases, na.rm = TRUE), .groups = "drop"
        )

    map_df <- world |>
      left_join(
        country_data, by = c("iso_a3" = "iso3")
        )

    pal <- colorNumeric(
      # palette = case_when(
      #   (length(input$disease) == 2) ~ "Purples",
      #   (input$disease == "Measles") ~ "Reds",
      #   input$disease == "Rubella" ~ "Greens"),
      palette = if (length(input$disease) ==2) "Purples" else "Reds",
      domain  = map_df$total_cases,
      na.color = "#f0f0f0"
    )

    leafletProxy("map", data = map_df) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor = ~pal(total_cases),
        fillOpacity = 0.7,
        color = "#444",
        weight = 0.5,
        popup = ~paste0("<b>", name, "</b><br>", input$disease, "</b><br>", "Cases: ", ifelse(is.na(total_cases), 0, total_cases))
      ) |>
      addLegend(
        pal = pal,
        values = map_df$total_cases,
        title = "Total cases",
        position = "bottomright"
      )
    
  })
  
}
shinyApp(ui, server)