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
download.file("http://wireless.fcc.gov/uls/data/complete/r_tower.zip",
              destfile = "./data/cell_towers/r_tower.zip")

# Download field definition file
download.file("http://wireless.fcc.gov/uls/documentation/patower-2.xls",
              destfile = "./data/cell_towers/patower-2.xls")

# Uncompress data
unzip("./data/cell_towers/r_tower.zip",
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

registrations <- readr::read_delim("./data/cell_towers/RA.dat",
                                   delim = "|",
                                   col_names = c("Record Type",
                                                 "Content Indicator",
                                                 "File Number",
                                                 "Registration Number",
                                                 "Unique System Identifier",
                                                 "Application Purpose",
                                                 "Previous Purpose",
                                                 "Input Source Code",
                                                 "Status Code",
                                                 "Date Entered",
                                                 "Date Received",
                                                 "Date Issued",
                                                 "Date Constructed",
                                                 "Date Dismantled",
                                                 "Date Action",
                                                 "Archive Flag Code",
                                                 "Version",
                                                 "Signature First Name",
                                                 "Signature Middle Initial",
                                                 "Signature Last Name",
                                                 "Signature Suffix",
                                                 "Signature Title",
                                                 "Invalid Signature",
                                                 "Structure_Street Address",
                                                 "Structure_City",
                                                 "Structure_State Code",
                                                 "County Code",
                                                 "ZIP Code",
                                                 "Height of Structure",
                                                 "Ground Elevation",
                                                 "Overall Height Above Ground",
                                                 "Overall Height AMSL",
                                                 "Structure Type",
                                                 "Date FAA Determination Issued",
                                                 "FAA Study Number",
                                                 "FAA Circular Number",
                                                 "Specification Option",
                                                 "Painting and Lighting",
                                                 "Proposed Marking and Lighting",
                                                 "Marking and Lighting Other",
                                                 "FAA EMI Flag",
                                                 "NEPA Flag"),
                                   col_types = cols(
                                     .default = col_character(),
                                     `Registration Number` = col_integer(),
                                     `Unique System Identifier` = col_integer(),
                                     `Height of Structure` = col_double(),
                                     `Ground Elevation` = col_double(),
                                     `Overall Height Above Ground` = col_double(),
                                     `Overall Height AMSL` = col_double(),
                                     `Specification Option` = col_integer(),
                                     `Proposed Marking and Lighting` = col_integer()
                                   )) %>%
  dplyr::filter(`Content Indicator` == "REG",
                !is.na(`Height of Structure`),
                `Status Code` == "C",
                `Archive Flag Code` == "C") %>%
  dplyr::select(`Unique System Identifier`,
                `Height of Structure`,
                `Application Purpose`,
                `Date Action`) %>%
  dplyr::distinct()

entities <- readr::read_delim("./data/cell_towers/EN.dat",
                              delim = "|",
                              col_names = c("Record Type",
                                            "Content Indicator",
                                            "File Number",
                                            "Registration Number",
                                            "Unique System Identifier",
                                            "Contact Type",
                                            "Entity Type",
                                            "Entity Type - Other",
                                            "Licensee ID",
                                            "Entity Name",
                                            "First Name",
                                            "MI",
                                            "Last Name",
                                            "Suffix",
                                            "Phone",
                                            "Fax Number",
                                            "Internet Address",
                                            "Street Address",
                                            "Street Address 2",
                                            "PO Box",
                                            "City",
                                            "State",
                                            "Zip Code",
                                            "Attention",
                                            "FRN"),
                              col_types = cols(
                                .default = col_character(),
                                `Registration Number` = col_integer(),
                                `Unique System Identifier` = col_integer(),
                                Phone = col_double(),
                                `Fax Number` = col_double()
                              )) %>%
  dplyr::filter(!is.na(`Licensee ID`),
                `Content Indicator` == "REG",
                `Unique System Identifier` %in% registrations$`Unique System Identifier`) %>%
  dplyr::select(`Unique System Identifier`,
                `Entity Name`) %>%
  dplyr::distinct() %>%
  dplyr::left_join(registrations, by = "Unique System Identifier") %>%
  dplyr::arrange(`Unique System Identifier`)

locations <- readr::read_delim("./data/cell_towers/CO.dat",
                               delim = "|",
                               col_names = c("Record Type",
                                             "Content Indicator",
                                             "File Number",
                                             "Registration Number",
                                             "Unique System Identifier",
                                             "Coordinate Type",
                                             "Latitude Degrees",
                                             "Latitude Minutes",
                                             "Latitude Seconds",
                                             "Latitude Direction",
                                             "Latitude_Total_Seconds",
                                             "Longitude Degrees",
                                             "Longitude Minutes",
                                             "Longitude Seconds",
                                             "Longitude Direction",
                                             "Longitude Total Seconds",
                                             "Array Tower Position",
                                             "Array Total Tower"),
                               col_types = cols(
                                 `Record Type` = col_character(),
                                 `Content Indicator` = col_character(),
                                 `File Number` = col_character(),
                                 `Registration Number` = col_integer(),
                                 `Unique System Identifier` = col_integer(),
                                 `Coordinate Type` = col_character(),
                                 `Latitude Degrees` = col_integer(),
                                 `Latitude Minutes` = col_integer(),
                                 `Latitude Seconds` = col_double(),
                                 `Latitude Direction` = col_character(),
                                 Latitude_Total_Seconds = col_double(),
                                 `Longitude Degrees` = col_integer(),
                                 `Longitude Minutes` = col_integer(),
                                 `Longitude Seconds` = col_double(),
                                 `Longitude Direction` = col_character(),
                                 `Longitude Total Seconds` = col_double(),
                                 `Array Tower Position` = col_character(),
                                 `Array Total Tower` = col_character()
                               ))%>%
  dplyr::right_join(entities, by = "Unique System Identifier") %>%
  filter(!is.na(`Latitude Degrees`),
         !is.na(`Latitude Minutes`),
         !is.na(`Latitude Seconds`),
         !is.na(`Longitude Degrees`),
         !is.na(`Longitude Minutes`),
         !is.na(`Longitude Seconds`),
         `Coordinate Type` == "T",
         `Content Indicator` == "REG",
         `Unique System Identifier` %in% registrations$`Unique System Identifier`)  %>%
  dplyr::distinct() %>%
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
                `Entity Name`,
                `Height of Structure`,
                Latitude,
                Longitude) %>%
  dplyr::distinct(`Unique System Identifier`, .keep_all = TRUE) %>%
  readr::write_csv("./data/cell_towers.csv")

unlink("./data/cell_towers",
       recursive = TRUE,
       force = TRUE)

