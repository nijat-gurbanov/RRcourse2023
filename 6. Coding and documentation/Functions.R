# Functions ---------------------------------------------------------------


# 1. Calc_totals ----------------------------------------------------------

# This function is for calculating totals for each country
calc_totals <- function(df_vector, country){
  
  vectors <- mget(df_vector, envir = globalenv())
  
  assign(paste0("total_", country), 
         Reduce("+", lapply(vectors, "[[", country)))
}
