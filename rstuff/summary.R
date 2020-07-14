library(dplyr)
library(tidyr)
library(rstackdeque)

load("../../subset_data.Rdat")

str(data)

#s <- rstack()
#for(id in data$person.csv$person_id) {
#  cat("Pulling data for person ", id, "...\n")
#  sublist <- lapply(data, function(df) {
#    return(filter(df, person_id == person_id))
#  })
#  s <- insert_top(s, sublist)
#}
