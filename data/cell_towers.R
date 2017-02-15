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
download.file("https://cdn.radiocells.org/us.sqlite",
              destfile = "./data/cell_towers/us.sqlite")

# To connect to a database first create a src:
cell_towers <- src_sqlite(path = "./data/cell_towers/us.sqlite")

# Then reference a tbl within that src
tbl(cell_towers, "cell_zone") %>%
  dplyr::filter(technology == "LTE") %>%
  dplyr::select(cid, latitude, longitude) %>%
  dplyr::distinct() %>%
  tibble::as_tibble() %>%
  readr::write_csv("./data/cell_towers.csv")

unlink("./data/cell_towers",
       recursive = TRUE,
       force = TRUE)

