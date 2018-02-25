library(tidyxl);library(unpivotr)
# http://www.dof.ca.gov/Reports/Demographic_Reports/American_Community_Survey/documents/Web_ACS2010_Inc-Pov-Emp.xls
base_link <- "http://www.dof.ca.gov/Reports/Demographic_Reports/American_Community_Survey/documents/Web_ACS"
years <- 2010:2016
link_type <- c(
  "Pop-Race",
  "Inc-Pov-Emp",
  "HealthIns",
  "Educ",
  "Housing"
)
xlsx_links <- c(2014:2016 %>% paste(base_link, ., "_", rep(link_type, each = 3), ".xlsx", sep = "")) %>% order()

# links <- c(2014:2016 %>% paste(base_link, ., "_", rep(link_type, each = 3), ".xlsx", sep = ""),
#           2012:2013 %>% paste(base_link, ., "_", rep(link_type, each = 2), ".xls", sep = ""),
#           2010:2011 %>% paste(base_link, ., "_", rep(link_type[-1], each = 2), ".xls", sep = ""))

skips <- c("Race and Hispanic" = 4,
           "Total Pop & Median Age" = 4,
           "Income" = 3,
           "Poverty" = 3,
           "Employment Status" = 3,
           "Health Insurance" = 4,
           "Educational Attainment" = 4,
           "Earnings by Educ" = 5,
           "Occupancy" = 3,
           "Tenure" = 3,
           "Units" = 3,
           "Mortgage Status" = 3,
           "Value" = 3,
           "Owner Costs " = 4,
           "Renter Costs" = 4)
sheet_names = list(
  "Pop-Race" = c("Total Pop & Median Age", "Race and Hispanic"),
  "Inc-Pov-Emp" = c("Income", "Poverty", "Employment Status"),
  "HealthIns" = c("Health Insurance"),
  "Educ" = c("Educational Attainment", "Earnings by Educ"),
  "Housing" = c("Occupancy", "Tenure", "Units", "Mortgage Status", "Value", "Owner Costs ", "Renter Costs")
)

read_acs_sheet = function(file_title = "ACS2014_Housing.xlsx", sheet = "Occupancy") {
  skip <- skips[sheet]
  print(paste0("Reading File: ", file_title, ", Sheet: ", sheet, ", Skipping: ", skip))
  headers <-
    xlsx_cells(file_title, sheet) %>% filter(row %in% (skip + 1:2)) %>%
    select(row, col, header = character) %>% filter(isnt.na(header)) %>%
    split(.$row)
  df <-
    xlsx_cells(file_title, sheet) %>% filter(row > (skip + 2)) %>% NNW(headers[[1]]) %>% N(headers[[2]]) %>%
    select(row, col, character, numeric, header.data, header.header) %>%
    filter(header.header %notin% c("Summary Level", "County", "Place")) %>%
    mutate(numeric =
             ifelse(isnt.na(numeric),
                    numeric,
                    ifelse(
                      str_detect(character, "\\*\\*"),
                      0,
                      ifelse(
                        str_detect(character, "\\+"),
                        str_extract(character, "[0-9,]+") %>%
                          gsub(pattern = ",", replacement = "") %>% as.numeric(),
                        numeric
                      )
                    ))) %>%
    mutate(Geography = ifelse(is.na(numeric), character, NA)) %>%
    group_by(row) %>% fill(Geography, .direction = "down") %>% filter(isnt.na(numeric)) %>% ungroup() %>%
    select(
      Geography,
      `Statistic Description` = header.data,
      Statistic = header.header,
      Value = numeric
    )
  return(df)
}

read_acs_file = function(link = "http://www.dof.ca.gov/Reports/Demographic_Reports/American_Community_Survey/documents/Web_ACS2014_Housing.xlsx") {
  file_title = str_extract(link, pattern = "ACS[0-9]*_[A-Za-z\\-]*\\..*")
  file_type = file_title %>% str_extract("_[A-Za-z\\-]+\\.") %>% str_sub(2, -2)
  download.file(link, file_title)
  sheets = sheet_names %>% `[[`(file_type)
  dfs <- lapply(sheets, function(s){read_acs_sheet(file_title, s)})
  unlink(file_title)
  return(dfs)
}

read_acs_all = function(links = xlsx_links) {
  sapply(links, read_acs_file)
}

acs_dfs <- read_acs_all()
