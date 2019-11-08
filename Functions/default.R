### disable scientific notation
options(scipen = 999)

### default packages
library(tidyverse)
library(lubridate)
library(janitor)
library(kableExtra)
library(naniar)
library(readxl)
library(sf)
library(ggmap)
library(data.table)
library(ggplot2)
library(devtools)
library(glue)
library(RColorBrewer)
library(zoo)
library(DT)
library(ggsci)
library(DataExplorer)
library(skimr)

# Colors
#yl_rd <- RColorBrewer::brewer.pal(8, "YlOrRd") %>% adjust_luminance(+1.0)
#rd_bl <- RColorBrewer::brewer.pal(8, "RdYlBu") %>% adjust_luminance(+1.0)

### Functions for formatting tables
dt_no_filter <- function(table) {
  
  names(table) <- str_to_title(gsub("_", " ", names(table)))
  table %>%
    datatable(rownames = FALSE,
              extensions = "FixedColumns",
              options = list(pageLength=10, 
                             pagingType="full",
                             autoWidth=FALSE, 
                             scrollX = TRUE, 
                             fixedColumns = list(leftColumns = 2)),
              class = "compact row-border cell-border stripe") %>%
    formatStyle(columns = c(1:100), fontSize = '80%') 
}

dt_filter <- function(table) {
  
  names(table) <- str_to_title(gsub("_", " ", names(table)))
  table %>%
    datatable(rownames = FALSE,
              extensions = "FixedColumns",
              options = list(pageLength=10, 
                             pagingType="full",
                             autoWidth=FALSE, 
                             scrollX = TRUE, 
                             fixedColumns = list(leftColumns = 2)),
              filter = list(
                position = 'top', clear = FALSE, plain = TRUE
              ),
              class = "compact row-border cell-border stripe") %>%
    formatStyle(columns = c(1:100), fontSize = '80%') 
}

dt_simple <- function(table) {
  
  names(table) <- str_to_title(gsub("_", " ", names(table)))
  table %>%
    datatable(rownames = FALSE,
              extensions = "FixedColumns",
              options = list(pageLength=10,
                             pagingType="full",
                             scrollX = TRUE, 
                             fixedColumns = list(leftColumns = 2),
                             dom = 'tps'), 
              class = "compact row-border cell-border stripe") %>%
    formatStyle(columns = c(1:100), fontSize = '80%') 
  
}

dt_simple_long <- function(table) {
  
  names(table) <- str_to_title(gsub("_", " ", names(table)))
  table %>%
    datatable(rownames = FALSE,
              extensions = "FixedColumns",
              options = list(pageLength=30,
                             pagingType="full",
                             scrollX = TRUE, 
                             fixedColumns = list(leftColumns = 2),
                             dom = 'tps'), 
              class = "compact row-border cell-border stripe") %>%
    formatStyle(columns = c(1:100), fontSize = '80%') 
  
}

dt_bare <- function(table) {
  
  names(table) <- str_to_title(gsub("_", " ", names(table)))
  table %>%
    datatable(rownames = FALSE,
              extensions = "FixedColumns",
              options = list(pageLength=40,
                             pagingType="full",
                             scrollX = TRUE, 
                             fixedColumns = list(leftColumns = 2),
                             dom = 't'), 
              class = "compact row-border cell-border stripe") %>%
    formatStyle(columns = c(1:100), fontSize = '80%') 
  
}

### Functions for making reports

# make report function
make_report <- function(table) {
  table <- table %>% 
    mutate(`# Records` = row_number())
  missing <- profile_missing(table)
  summaries <- table %>%   
    remove_empty("cols") %>%
    summarize_all(funs(distinct = n_distinct,
                       count = n())) %>% 
    gather() %>%
    extract(key, into = c("feature", "stat"), "^(.+)_(.+)$") %>% 
    spread(stat, value) %>%
    right_join(missing, by = "feature")
  
  summaries %>%
    mutate_at(vars(count, distinct), as.integer) %>%
    mutate(#pct_missing = round(100*pct_missing,1),
      pct_distinct = distinct/count,
      num_values = count-num_missing) %>%
    arrange(pct_missing, desc(distinct)) %>%
    select(
      Column = feature,
      `# Distinct` = distinct,
      `# Not Missing` = num_values,
      `# Missing` = num_missing,
      `% Missing` = pct_missing,
      `% Distinct` = pct_distinct) %>%
    
    datatable(rownames = FALSE,
              extensions = "FixedColumns",
              options = list(pageLength=10, 
                             pagingType="full",
                             autoWidth=FALSE, 
                             scrollX = TRUE, 
                             fixedColumns = list(leftColumns = 2)),
              filter = list(position = 'top',
                            clear = FALSE,
                            plain = TRUE),
              class = "compact row-border cell-border stripe") %>%
    formatStyle('% Missing',
                backgroundColor = styleInterval(c(0, 
                                                  0.1, 0.3, 
                                                  0.5, 0.7, 
                                                  0.9, 
                                                  1), 
                                                c("#ffffff", 
                                                  "#fee5d9", "#fcbba1",
                                                  "#fc9272", "#fb6a4a",
                                                  "#ef3b2c", "#cb181d",
                                                  "#99000d"))) %>%
    formatPercentage(c("% Missing","% Distinct"), 1) %>%
    formatStyle(columns = c(1:100), fontSize = '80%') 
}

# profile missing formatted function
profile_missing_f <- function(table) {
  
  profile_missing(as.data.table(table)) %>%
    select(Column = feature,
           `# Missing` = num_missing,
           `% Missing` = pct_missing) %>%
    datatable(rownames = FALSE,
              extensions = "FixedColumns",
              options = list(pageLength=15, 
                             pagingType="full",
                             autoWidth=FALSE, 
                             scrollX = TRUE, 
                             fixedColumns = list(leftColumns = 2)),
              filter = list(position = 'top', clear = FALSE, plain = TRUE),
              class = "compact row-border cell-border stripe") %>%
    formatStyle('% Missing',
                backgroundColor = styleInterval(c(0, 
                                                  0.1, 0.3, 
                                                  0.5, 0.7, 
                                                  0.9, 
                                                  1), 
                                                c("#ffffff", 
                                                  "#fee5d9", "#fcbba1",
                                                  "#fc9272", "#fb6a4a",
                                                  "#ef3b2c", "#cb181d",
                                                  "#99000d"))) %>%
    formatPercentage(c("% Missing"), 1) %>%
    formatStyle(columns = c(1:3), fontSize = '80%') 
}


### Adorn tables
adorn_pct_across <- function(table)
{
  table %>%
    adorn_totals("col") %>%
    adorn_totals("row") %>%
    adorn_percentages("row") %>%
    adorn_pct_formatting(0) %>%
    adorn_ns()
}

adorn_pct_down <- function(table)
{
  table %>%
    adorn_totals("col") %>%
    adorn_totals("row") %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(0) %>%
    adorn_ns()
}

adorn_one_col <- function(table) {
  table %>%
    adorn_totals("row") %>%
    adorn_percentages("col") %>%
    adorn_pct_formatting(0) %>%
    adorn_ns()
}
