library(readxl)
tb_data <- read_xlsx(path = "data-raw/tb_ppreport.xlsx", sheet = "tb_data")
tb_ug <- read_xlsx(path = "data-raw/tb_ppreport.xlsx", sheet = "tb_ug", col_types = "text")
tb_gnd <- read_xlsx(path = "data-raw/tb_ppreport.xlsx", sheet = "tb_gnd", col_types = "text")
tb_acao <- read_xlsx(path = "data-raw/tb_ppreport.xlsx", sheet = "tb_acao", col_types = "text")
usethis::use_data(tb_data, tb_ug, tb_gnd, tb_acao,
                  internal = TRUE,
                  overwrite = TRUE)
