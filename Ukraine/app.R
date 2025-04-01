# -------------------------------
# Global Setup and Data Loading
# -------------------------------
library(dplyr) 
library(tidyverse)
library(openxlsx)
library(purrr)
library(here)
library(writexl)
library(sf)
library(ggplot2)
library(cowplot)
library(patchwork)
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(viridis)

############################################################################
# The original shape file is omitted in this code as the file is too large.
# This code will start from the ligther shape file "admin1_boundaries.rds"
############################################################################
# shapefile downloaded from https://data.humdata.org/dataset/cod-ab-ukr
# shapefile_folder <- "data/ukr_admbnd_sspe_20240416_ab_shp"

# Find the shapefile containing "adm1" in its name
# adm1_shapefile_path <- list.files(shapefile_folder, pattern = "adm1.*\\.shp$", full.names = TRUE)

# Check if the files are found
#if (length(adm1_shapefile_path) == 0) {
#  stop("No shapefile with 'adm1' in its name was found.")
#}

# Load the shapefiles
#admin1_boundaries <- st_read(adm1_shapefile_path)

# Save as RDS
# saveRDS(admin1_boundaries, file = "admin1_boundaries.rds")
#########################################################################

# Load ADM1 boundaries (light version)
admin1_boundaries <- readRDS("admin1_boundaries.rds")

# Load SSSU data
file_path <- here("SSSU_dataset.csv")
SSSU_dataset <- read.csv(file_path)

# -------------------------------
# Data Cleaning and Wrangling
# -------------------------------
# 3.1 Cleaning SSSU Population Data
SSSU_dataset <- SSSSU_dataset %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    values_to = "Total",
    names_prefix = "X"
  ) %>%
  filter(Total != "NA") %>%
  filter(
    Indicator == "Size of the constant population",
    Frequency == "Annual",
    Age %in% c("Total", "0 year", "0-4 years old", "9 years old", "10-14 years old", 
               "0-17 years old", "15-49 years old", "15-19 years old", "20-24 years old",
               "60 years and older", "65 years and older", "0-14 years old"),
    Terrain.type == "Total"
  ) %>%
  select(where(~ !all(is.na(.)))) %>%
  mutate(
    Gender = case_when(
      Gender == "Male"   ~ "M",
      Gender == "Female" ~ "F",
      Gender == "Total"  ~ "T",
      TRUE               ~ Gender
    ),
    Region = case_when(
      Region == "Avtonomna Respublika Krym" ~ "Autonomous Republic of Crimea",
      TRUE                                 ~ Region
    )
  ) %>%
  select(-Indicator, -Frequency, -Terrain.type) %>%
  rename(ADM1_EN = Region) %>%
  filter(Year >= 2015)

# Join geometry and ADM1 code from boundaries
SSSU_dataset <- SSSU_dataset %>%
  left_join(admin1_boundaries %>% select(ADM1_EN, ADM1_PCODE, geometry),
            by = "ADM1_EN") %>%
  mutate(
    ADM1_PCODE = if_else(is.na(ADM1_PCODE), "UA", ADM1_PCODE)
  )

# Create a combined record for age "9-14 years old"
SSSU_age_9_14 <- SSSU_dataset %>%
  filter(Age %in% c("9 years old", "10-14 years old")) %>%
  group_by(ADM1_EN, ADM1_PCODE, Year, Gender) %>%
  summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  mutate(Age = "9-14 years old")

SSSU_dataset <- bind_rows(SSSU_dataset, SSSU_age_9_14)

# Create Age_Category according to your rules and preserve geometry
SSSU_clean <- SSSU_dataset %>%
  mutate(
    Year = suppressWarnings(as.integer(Year)),
    Age_Category = case_when(
      Age %in% c("0 year")           ~ "inf",
      Age %in% c("0-4 years old")      ~ "chi4",
      Age %in% c("9-14 years old")     ~ "hpvvac",
      Age %in% c("0-14 years old")     ~ "youdep",
      Age %in% c("0-17 years old")     ~ "chi17",
      Age %in% c("15-19 years old", "20-24 years old") ~ "you",
      Age %in% c("15-49 years old")    ~ "wra",
      Age %in% c("60 years and older") ~ "eld",
      Age %in% c("65 years and older") ~ "olddep",
      Age == "Total"                 ~ "total",
      TRUE                           ~ NA_character_
    )
  ) %>%
  rename(Population = Total, Sex = Gender) %>%
  group_by(ADM1_PCODE, ADM1_EN, Year, Age_Category, Sex) %>%
  summarise(
    Population = sum(Population, na.rm = TRUE),
    geometry = first(geometry),  # Preserve geometry from boundaries
    .groups = "drop"
  )

# 1. Extract combinations
SSSU_filler <- SSSU_clean %>%
  ungroup() %>%
  select(Year, Age_Category, Sex) %>%
  distinct()

# 2. Create filler rows (dummy entries with NA population)
SSSU_filler <- SSSU_filler %>%
  tidyr::crossing(ADM1_PCODE = c("UA01", "UA85")) %>%
  left_join(admin1_boundaries %>% select(ADM1_PCODE, ADM1_EN, geometry), by = "ADM1_PCODE") %>%
  mutate(
    Population = NA_real_,
    pct_Change = NA_real_
  )

# 3. Add to full dataset
dataset <- bind_rows(SSSU_clean, SSSU_filler)

# 4. Calculating Percentage Changes
calculate_pct_Change <- function(data) {
  data %>%
    arrange(ADM1_PCODE, Age_Category, Sex, Year) %>%
    group_by(ADM1_PCODE, Age_Category, Sex) %>%
    mutate(
      Population_Lag = lag(Population),
      pct_Change = round((Population - Population_Lag) / Population_Lag * 100, 2)
    ) %>%
    ungroup() %>%
    select(ADM1_PCODE, ADM1_EN, Year, Age_Category, Sex, Population, pct_Change, geometry)
}

final_dataset <- calculate_pct_Change(dataset) %>%
  mutate(ADM1_EN = if_else(is.na(ADM1_EN), "Ukraine", ADM1_EN))

# 5. Adding National Totals and Regional Percentages
national_totals <- final_dataset %>%
  filter(ADM1_PCODE == "UA") %>%
  select(Year, Age_Category, Sex, National_Total = Population)

final_dataset <- final_dataset %>%
  left_join(national_totals, by = c("Year", "Age_Category", "Sex")) %>%
  mutate(
    pct = if_else(
      !is.na(National_Total),
      round((Population / National_Total) * 100, 2),
      NA_real_
    )
  )

# Define available year choices (from 2021 onward)
year_choices <- sort(unique(final_dataset %>% filter(Year >= 2021) %>% pull(Year)))

# -------------------------------
# Summary Table for Oblast-Level Data
# -------------------------------
# First, create a summary for groups "wra" and "hpvvac" (which apply only to females)
wra_oblast <- final_dataset %>%
  filter(Age_Category %in% c("wra", "hpvvac") & Sex == "F") %>%
  group_by(ADM1_PCODE, ADM1_EN, Year, Sex, Age_Category) %>%
  summarise(
    Population = sum(Population, na.rm = TRUE),
    pct_Change = sum(pct_Change, na.rm = TRUE),
    pct = sum(pct_Change, na.rm = TRUE),
    .groups = "drop"
  )

# Then, take the remaining data (excluding "wra" and "hpvvac")
ADM1_table <- final_dataset %>%
  filter(!(Age_Category %in% c("wra", "hpvvac"))) %>%
  select(ADM1_PCODE, ADM1_EN, Year, Sex, Age_Category, Population, pct_Change, pct)  # using 'pct' as regional %

# Bind the two datasets together and recode Age_Category labels
ADM1_table <- bind_rows(ADM1_table, wra_oblast) %>%
  mutate(Age_Category = case_when(
    Age_Category == "inf"    ~ "Children under 1 (00)",
    Age_Category == "chi4"   ~ "Children (00-04)",
    Age_Category == "hpvvac" ~ "Children for HPV vaccine (09-14)",
    Age_Category == "youdep" ~ "Children (00-14)",
    Age_Category == "chi17"  ~ "Children (00-17)",
    Age_Category == "you"    ~ "Youth (15 - 24) Population",
    Age_Category == "wra"    ~ "Women of reproductive age (15-49)",
    Age_Category == "eld"    ~ "Elderly Population (60+)",
    Age_Category == "dep"    ~ "Dependent population",
    Age_Category == "olddep" ~ "Elderly Dependent population (65+)",
    Age_Category == "total"  ~ "Total Population",
    TRUE                   ~ Age_Category
  )) %>%
  rename(
    Oblast_Name = ADM1_EN,
    Age_Group = Age_Category,
    Population_Count = Population,
    Yearly_Population_Change_Percent = pct_Change,
    Oblast_level_of_population_share = pct  # using regional % calculated earlier
  )

# Write summary table (non-spatial) to Excel
ADM1_table_nongeom <- st_drop_geometry(ADM1_table)
write.xlsx(ADM1_table_nongeom, "population_of_interest_Oblast.xlsx", rowNames = FALSE)

# -------------------------------
# Prepare Data for Shiny Mapping
# -------------------------------
# Before creating the Shiny app, join the summary table back to the boundaries.
# (This will add the geometry column back.)
ADM1_table <- admin1_boundaries %>%
  left_join(ADM1_table_nongeom, by = "ADM1_PCODE") %>%
  mutate(Raion_Name = ADM1_EN)  # Here we set Raion_Name equal to ADM1_EN

# Create a Shiny-ready table (non-spatial) if needed.
ADM1_table_shiny <- ADM1_table %>%
  filter(!is.na(Year)) %>%
  select(Year, ADM1_EN, ADM1_PCODE, Oblast_Name, Sex, Age_Group, Population_Count, Yearly_Population_Change_Percent, Oblast_level_of_population_share)
ADM1_table_shiny <- st_drop_geometry(ADM1_table_shiny)

# Pre-calculate available year choices (from ADM1_table)
year_choices <- sort(unique(ADM1_table %>% filter(Year >= 2015) %>% pull(Year)))

# -------------------------------
# End of Data Cleaning and Analysis
# -------------------------------





# -------------------------------
# Shiny App: UI and Server
# -------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Population of Interest"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Heatmap", tabName = "heatmap", icon = icon("th")),
      menuItem("Oblast Map", tabName = "oblast_map", icon = icon("globe", style = "color: cornflowerblue;")),
      menuItem("Table", tabName = "table", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # Heatmap Tab (show all years)
      tabItem(tabName = "heatmap",
              fluidRow(
                box(width = 12, title = "Select Population Group for Heatmap",
                    selectInput("heatmapPopGroup", "Population Group:",
                                choices = sort(unique(ADM1_table$Age_Group)),
                                selected = "Elderly Population (60+)")
                )
              ),
              fluidRow(
                box(width = 12, title = "Heatmap Visualization",
                    plotOutput("heatmapPlot", height = "500px"))
              )
      ),
      # Oblast Map Tab (with year selection)
      tabItem(tabName = "oblast_map",
              fluidRow(
                box(width = 6, title = "Select Population Group for Map",
                    selectInput("mapPopGroup", "Population Group:",
                                choices = sort(unique(ADM1_table$Age_Group)),
                                selected = "Elderly Population (60+)")
                ),
                box(width = 6, title = "Select Year for Map",
                    selectInput("mapYear", "Year:",
                                choices = year_choices,
                                selected = year_choices[1])
                )
              ),
              fluidRow(
                box(width = 12, title = "Geographical Map",
                    leafletOutput("mapPlot", height = "1100px"))
              )
      ),
      # Table Tab
      tabItem(tabName = "table",
              fluidRow(
                box(width = 12, title = "Summary Table",
                    dataTableOutput("summaryTable"))
              )
      )
    )
  )
)

# -------------------------------
# Shiny App: Server
# -------------------------------
server <- function(input, output, session) {
  
  #### Reactive Heatmap Data (ADM1_table) ####
  output$heatmapPlot <- renderPlot({
    req(input$heatmapPopGroup)
    # Determine desired sex: for female‑only groups, use "F"; otherwise "T"
    desired_sex <- if (input$heatmapPopGroup %in% c("Women of reproductive age (15-49)", "Children for HPV vaccine (09-14)")) "F" else "T"
    
    # Filter ADM1_table for the selected population group, desired sex, and exclude national totals ("UA")
    heat_data <- ADM1_table %>%
      filter(
        Age_Group == input$heatmapPopGroup,
        Sex == desired_sex,
        ADM1_PCODE != "UA"
      )
    
    # If pct_Change is missing, generate dummy values
    if (!"pct_Change" %in% colnames(heat_data) || all(is.na(heat_data$pct_Change))) {
      heat_data <- heat_data %>% mutate(pct_Change = runif(n(), -20, 20))
    }
    
    # Create a heatmap that shows all years (x-axis as factor of Year)
    ggplot(heat_data, aes(x = as.factor(Year), y = ADM1_EN, fill = pct_Change)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(pct_Change, 1),
                    color = ifelse(pct_Change < 0, "white", "black")),
                size = 3) +
      scale_fill_viridis_c(
        option = "viridis", limits = c(-20, 20),
        oob = scales::squish, na.value = "grey60",
        name = paste(input$heatmapPopGroup, "change (%)")
      ) +
      scale_color_identity() +
      theme_minimal() +
      labs(title = paste("Heatmap for", input$heatmapPopGroup, "Population Change (2015 - 2022)"),
           x = "Year", y = "Region") +
      theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  #### Reactive Oblast Map Data ####
  reactive_map_data <- reactive({
    # For groups that apply only to females, use "F"; otherwise "T"
    desired_sex <- if (input$mapPopGroup %in% c("Women of reproductive age (15-49)", "Children for HPV vaccine (09-14)")) "F" else "T"
    
    ADM1_table %>%
      filter(
        Age_Group == input$mapPopGroup,
        Year == as.numeric(input$mapYear),
        Sex == desired_sex
      ) %>%
      filter(ADM1_PCODE != "UA")  # Exclude national-level data
  })
  
  #### Oblast Map Output ####
  output$mapPlot <- renderLeaflet({
    req(input$mapPopGroup, input$mapYear)
    map_data <- reactive_map_data()
    
    # Check for valid geometry
    if(nrow(map_data) == 0 || !"geometry" %in% names(map_data)) {
      return(leaflet() %>% addTiles() %>% 
               addPopups(lng = 0, lat = 0, "No data available for the selected criteria"))
    }
    
    # Transform to WGS84
    map_data <- st_transform(map_data, crs = 4326)
    
    # Define a color palette using viridis.
    # For polygons with NA values and for two specific ADM1_PCODE values ("UA01" and "UA85"),
    # we force the fill color to dark grey ("#A9A9A9").
    pal <- colorNumeric(
      palette = "viridis",
      domain = range(map_data$Oblast_level_of_population_share, na.rm = TRUE),
      na.color = "#A9A9A9"
    )
    
    leaflet(map_data) %>%
      addTiles(options = providerTileOptions(opacity = 0.3)) %>%
      addPolygons(
        fillColor = ~ifelse(ADM1_PCODE %in% c("UA01", "UA85"),
                            "#A9A9A9",
                            ifelse(is.na(Oblast_level_of_population_share),
                                   "#A9A9A9",
                                   pal(Oblast_level_of_population_share))),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        # For the label, if the value is NA (or for the specified codes), show the ADM1 name.
        label = ~ifelse(ADM1_PCODE %in% c("UA01", "UA85") | is.na(Oblast_level_of_population_share),
                        ADM1_EN,
                        paste0(ADM1_EN, ": ", round(Oblast_level_of_population_share, 1), "%"))
      ) %>%
      addLegend(
        pal = pal, 
        values = ~Oblast_level_of_population_share[!is.na(Oblast_level_of_population_share)], 
        title = "Oblast share (%)",
        na.label = ""
      )
  })
  
  #### Summary Table Output ####
  output$summaryTable <- renderDataTable({
    # Here we assume ADM1_table_shiny is a version of ADM1_table with geometry dropped for table display.
    datatable(ADM1_table_shiny %>% st_drop_geometry(), options = list(pageLength = 10), filter = "top")
  })
}

# Run the Shiny app
shinyApp(ui, server)
