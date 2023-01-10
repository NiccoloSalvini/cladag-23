## load data 
## "2023-01-09"

## 1  load default data from `coresoir` ----
## currently used 
library(coresoi)
data("mock_data_core")

## 2 load data from `corebigquery`  ----
## not working


## 3 load data directly from BigQuery ----
## not working

# Load the BigQuery library
library(bigrquery)

# Set up authentication
# bq_auth(path = "auth.json")
# 
# # Set up the query parameters
# project <- "my-project"
# dataset <- "my-dataset"
# table <- "my-table"
# fields <- c("field1", "field2", "field3")
# where_clause <- "WHERE field1 > 5"
# limit <- 1000 # number of rows to return, max is 10,000
# 
# # Run the query and store the results in a data frame
# df <- bq_project_query(project, paste0("SELECT ", paste0(fields, collapse = ", "), 
#                                        " FROM `", dataset, ".", table, "` ", where_clause, 
#                                        " LIMIT ", limit))
