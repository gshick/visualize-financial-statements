
# Activate required libraries
################################################################################
library(readxl)
library(dplyr)
library(networkD3)
library(htmlwidgets)

# Read in Excel file - automate this to scrape from web later
################################################################################
raw = readxl::read_xlsx(., sheet = "ER Income Statement")

# Data manipulation and clean up
################################################################################
income = raw %>%
  rename(category = `THE CHARLES SCHWAB CORPORATION`, value = `...7`) %>% 
  select(category, value) %>% 
  slice(8:41) %>% 
  filter(!is.na(value) & value != 0) %>% 
  mutate(category = category %>% gsub(pattern = "\\(1)", replacement = "")) %>% 
  mutate(group = case_when(between(row_number(), 1, 8) ~ "Revenue",
                          between(row_number(), 9, 18) ~ "Expenses",
                          between(row_number(), 19, 23) ~ "Income"
                          )) %>% 
  mutate(category = as.factor(case_when(group == "Revenue" & category == "Other" ~ "Other revenue",
                              group == "Expenses" & category == "Other" ~ "Other expenses",
                              TRUE ~ category)))

# Create links for Sankey chart
################################################################################
links = rbind(

  # Revenue inflows  
  income %>% 
    filter(group == "Revenue" & 
           category %in% c("Net interest revenue",
                           "Asset management and administration fees ",
                           "Trading revenue",
                           "Bank deposit account fees",
                           "Other revenue")) %>%
    rename(source = category) %>% 
    mutate(target = "Total net revenues") %>% 
    select(source, target, value),
  
  # Expenses group
  income %>%
    filter(group == "Expenses" &
             category %in% c("Total expenses excluding interest")) %>%
    rename(target = category) %>%
    mutate(source = "Total net revenues") %>%
    select(source, target, value),
  
  # Expense outflows
  income %>% 
    filter(group == "Expenses" & 
           category %in% c("Compensation and benefits",
                           "Professional services",
                           "Occupancy and equipment",
                           "Advertising and market development",
                           "Communications",
                           "Depreciation and amortization",
                           "Amortization of acquired intangible assets",
                           "Regulatory fees and assessments",
                           "Other expenses")) %>%
    rename(target = category) %>% 
    mutate(source = "Total expenses excluding interest") %>% 
    select(source, target, value),
  
  # Income group
  income %>%
    filter(group == "Income" &
             category %in% c("Income before taxes on income")) %>%
    rename(target = category) %>%
    mutate(source = "Total net revenues") %>%
    select(source, target, value),
  
  # Net income subtotal
  income %>%
    filter(group == "Income" &
             category %in% c('Taxes on income',
                             "Net Income")) %>%
    rename(target = category) %>%
    mutate(source = "Income before taxes on income") %>%
    select(source, target, value),
  
  # Net income final
  income %>%
    filter(group == "Income" &
             category %in% c("Preferred stock dividends and other",
                             "Net Income Available to Common Stockholders"
             )) %>%
    rename(target = category) %>%
    mutate(source = "Net Income") %>%
    select(source, target, value)
  
) %>% 
  select(source, target, value)

# Create nodes for Sankey
################################################################################
node <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% 
    unique()
)

nodes = node %>% 
  left_join(income, by=c("name"="category")) %>% 
  mutate(label = as.character(paste0(name,", ", scales::dollar(as.numeric(value)/1000000000,prefix = "$", suffix = "B", accuracy = 0.1))))


# Create numeric id's for Sankey, cannot use character values
################################################################################
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Set custom colors
################################################################################
my_color <- 'd3.scaleOrdinal() .domain(["Revenue", "Expenses", "Income"]) .range(["#425563", "#F7A800", "#00A0DF"])'



# Make the Sankey network
################################################################################
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "label", NodeGroup="group",
                   fontSize = 20, nodeWidth = 30,
                   width = 1925, height = 500,
                   colourScale = my_color, 
                   sinksRight=FALSE)

p 

# Add HTML titles to plot object
################################################################################
sankey = htmlwidgets::prependContent(p, htmltools::tags$h1("THE CHARLES SCHWAB CORPORATION"), htmltools::tags$h2("2022 Year-End Income Statement"))

sankey



