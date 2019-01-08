library(odbc)
library(tidyverse)
library(DBI)


# Make connection ---------------------------------------------------------

# Use Microsoft SQL Server Management Studio to view heirarchy (tables) and columns and keys, 
# MUCH easier. Use KEY folder to get the actual name of the key.

lsp <- function(package, all.names = FALSE, pattern)
{
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}

#lsp(odbc)
#lsp(DBI)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "TSWIKIAPPS",
                 Database = "WikiWarehouse",
                 UID = rstudioapi::askForPassword("Database user"),
                 PWD = rstudioapi::askForPassword("Database password"),
                 Trusted_Connection = "True")

get_fields <- function(table, con) {
  fields <- dbListFields(con, table)
  tibble(table_name = table, field_name = fields)
}

databases <- dbListTables(con, table_name = 'Main%')
databases

save_table <- function(database, connection) {
  dbReadTable(connection, database) %>% as.tibble() %>%
    saveRDS(paste0('./data/',database,'.rds'))
}
databases %>% lapply(save_table, con)
