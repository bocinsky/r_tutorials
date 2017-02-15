## Download and crop FCC data on cell tower licenses
library(readr)
library(readxl)
library(magrittr)
library(dplyr)
library(sp)

# Remove output if already exists
unlink("./data/cell_towers.csv",
       recursive = TRUE,
       force = TRUE)

# Create a directory for extraction
dir.create("./data/cell_towers",
           showWarnings = FALSE,
           recursive = TRUE)

# Download data
download.file("http://wireless.fcc.gov/uls/data/complete/l_cell.zip",
              destfile = "./data/cell_towers/l_cell.zip")

# Download field definition file
download.file("http://wireless.fcc.gov/uls/documentation/pa_ddef50.xls",
              destfile = "./data/cell_towers/pa_ddef50.xls")

# Uncompress data
unzip("./data/cell_towers/l_cell.zip",
      exdir = "./data/cell_towers")

lat.convert <- function(deg, min, sec){
  new("DMS",
      WS = FALSE,
      deg = deg,
      min = min,
      sec = sec,
      NS = TRUE) %>%
    as("numeric")
}

long.convert <- function(deg, min, sec){
  new("DMS",
      WS = TRUE,
      deg = deg,
      min = min,
      sec = sec,
      NS = FALSE) %>%
    as("numeric")
}

# Load field definitions
readxl::read_excel("./data/cell_towers/pa_ddef50.xls",
                   sheet = "LO",
                   skip = 1)$`Data Element`

cell_towers <- readr::read_delim("./data/cell_towers/LO.dat",
                  delim = "|",
                  col_names = readxl::read_excel("./data/cell_towers/pa_ddef50.xls",
                                                 sheet = "LO",
                                                 skip = 1)$`Data Element`,
                  col_types = cols(
                    `Record Type                                    [LO]` = col_character(),
                    `Unique System Identifier` = col_integer(),
                    `ULS File Number` = col_character(),
                    `EBF Number` = col_character(),
                    `Call Sign` = col_character(),
                    `Location Action Performed` = col_character(),
                    `Location Type Code` = col_character(),
                    `Location Class Code` = col_character(),
                    `Location Number` = col_integer(),
                    `Site Status` = col_character(),
                    `Corresponding Fixed Location` = col_character(),
                    `Location Address` = col_character(),
                    `Location City` = col_character(),
                    `Location County/Borough/Parish` = col_character(),
                    `Location State` = col_character(),
                    `Radius of  Operation` = col_character(),
                    `Area of Operation Code` = col_character(),
                    `Clearance Indicator` = col_character(),
                    `Ground Elevation` = col_double(),
                    `Latitude Degrees` = col_double(),
                    `Latitude Minutes` = col_double(),
                    `Latitude Seconds` = col_double(),
                    `Latitude Direction` = col_character(),
                    `Longitude Degrees` = col_double(),
                    `Longitude Minutes` = col_double(),
                    `Longitude Seconds` = col_double(),
                    `Longitude Direction` = col_character(),
                    `Max Latitude Degrees` = col_character(),
                    `Max Latitude Minutes` = col_character(),
                    `Max Latitude Seconds` = col_character(),
                    `Max Latitude Direction` = col_character(),
                    `Max Longitude Degrees` = col_character(),
                    `Max Longitude Minutes` = col_character(),
                    `Max Longitude Seconds` = col_character(),
                    `Max Longitude Direction` = col_character(),
                    Nepa = col_character(),
                    `Quiet Zone Notification Date` = col_character(),
                    `Tower Registration Number` = col_integer(),
                    `Height of Support Structure` = col_double(),
                    `Overall Height of Structure` = col_double(),
                    `Structure Type` = col_character(),
                    `Airport ID` = col_character(),
                    `Location Name` = col_character(),
                    `Units Hand Held       ` = col_character(),
                    `Units Mobile       ` = col_character(),
                    `Units Temp Fixed       ` = col_character(),
                    `Units Aircraft       ` = col_character(),
                    `Units Itinerant       ` = col_character(),
                    `Status Code` = col_character(),
                    `Status Date` = col_date(format = "mm/dd/yyyy"),
                    `Earth Station Agreement` = col_character()
                  )) %>%
  filter(!is.na(`Latitude Degrees`),
         !is.na(`Latitude Minutes`),
         !is.na(`Latitude Seconds`),
         !is.na(`Longitude Degrees`),
         !is.na(`Longitude Minutes`),
         !is.na(`Longitude Seconds`),
         !is.na(`Overall Height of Structure`),
         `Location Type Code` %in% c("F","P")) %>%
  dplyr::bind_cols(
    tibble::tibble(
      Latitude = mapply(FUN = lat.convert,
                        deg = .$`Latitude Degrees`,
                        min = .$`Latitude Minutes`,
                        sec = .$`Latitude Seconds`),
      Longitude = mapply(FUN = long.convert,
                         deg = .$`Longitude Degrees`,
                         min = .$`Longitude Minutes`,
                         sec = .$`Longitude Seconds`)
    )
  ) %>%
  dplyr::select(`Unique System Identifier`,
                `Call Sign`,
                `Overall Height of Structure`,
                Latitude,
                Longitude) %>%
  dplyr::distinct() %>%
  readr::write_csv("./data/cell_towers.csv")

unlink("./data/cell_towers",
       recursive = TRUE,
       force = TRUE)

