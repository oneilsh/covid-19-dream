library(readr)   # easy data import from csvs
library(sqldf)   # treat csvs like SQL
library(dplyr)
library(tidyr)

data_path <- "../../synthetic_data/" # location of unzipped .csvs
defs_file <- "../../data_dictionary.csv"
output_rdat <- "../../subset_data.Rdat"  # file for cleaned R objects
do_subset <- FALSE                       # work on a subset of person_ids?


# read defs
defs <- read_csv(defs_file)
print(head(defs))
# there's one duplicate concept_id here:
#4196147,Peripheral oxygen saturation,observation
#4196147,Peripheral oxygen saturation,measurement
# since I don't care what table they're in I'll drop the table col and take unique rows
defs$table <- NULL
defs <- unique(defs)


# make it a named list of filenames so we get a named list of dataframes back out from lapply
filenames <- as.list(list.files(data_path, pattern = "*.csv", full.names = TRUE))
names(filenames) <- list.files(data_path, pattern = "*.csv", full.names = FALSE)


# read other files, selecting only records where person_id % 10 is 0 (10% of data by person) if do_subset is TRUE
data <- lapply(
  filenames,
  
  function(filename) {
    cat("Reading ", filename, "...\n")
    # read all data if there's no person_id col
    ret_code <- system(paste0("head -n 1 ", filename,  " | grep person_id"))
    if(ret_code == 0 & do_subset) {
      return(read.csv.sql(filename, "select * from file where person_id % 10 == 0"))
    } else {
      return(read.csv.sql(filename, "select * from file"))
    }
  }
  
)
str(data)


# mapping concept_ids to concepts (with factors), using the mapping defined in defs
data_named <- lapply(data,
                     
                     function(df) {
                       is_concept_col <- grepl("concept_id", colnames(df))
                       concept_cols_indices <- which(is_concept_col)
                       
                       for(i in concept_cols_indices) {
                         df[[i]] <- factor(df[[i]], levels = defs$concept_id, labels = defs$concept_name)
                       }
                       
                       return(df)
                     })

str(data_named)



# let's separate the tables with person_id from those without
has_person_data <- lapply(data_named,
                          function(df) {
                            return(any(colnames(df) == "person_id"))
                          })

print(has_person_data)
# it's just location that doesn't have any person_id, lol
# let's split it out...
location <- data_named$location.csv
data_named$location.csv <- NULL

## and lets merge in the location data
data_named_location <- lapply(data_named, 
                              function(df){
                                if(any(colnames(df) == "location_id")) {
                                  # all.x = TRUE for left join
                                  df <- merge(df, location, by = "location_id", all.x = TRUE)
                                } 
                                return(df)
                              })

# write it out and start a new script...
save(data_named_location, file = output_rdat)


# may as well write .tsvs too
for(tablename in names(data_named_location)) {
  table <- data_named_location[[tablename]]
  write.table(table, 
              file = paste0(data_path, "w_concepts_", tablename), 
              sep = "\t", 
              row.names = FALSE,
              quote = FALSE)
}

