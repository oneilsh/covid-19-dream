#!/usr/bin/env Rscript

####################
### vectors
####################


######## vectorized operations and recycling
nums <- c(10, 20, 30, 40)
mult <- c(10, -10)

print(nums * mult) # 100, -200, 300, 400

## single elements are length-1 vectors:
mult <- 100 # a length-1 vector
print(nums * mult) # 1000, 2000, 3000, 4000

## other vector types:

# character vector (vector of strings)
names <- c("Joe", "Jim", "Kim")

# logical 
checkit <- c(FALSE, TRUE, FALSE)

## vectors can't mix types - you'll get autoconversion
test <- c(1.2, as.integer(4), "hi") # "1.2", "4", "hi"

######### selection/subsetting
## by index #
subnums <- nums[c(3, 2)] # second and third element (300, 200)
subnum <- nums[3]       # just the 3rd (300)

## replacement:
nums[c(3, 2)] <- c(-1, -2)  # change third and second element
nums[c(1, 2, 3)] <- NA      # entries are recyled if they are shorter; NA is a special not available type

## by logical
nums[c(TRUE, FALSE, TRUE, FALSE)] <- c(52, 42)

# combining vectorized operations with local operators
large_values <- nums[nums > median(nums)]   # select w/ logical selection, produced by recycled > operator

########### named vectors: a bit weird
nums <- c(10, 20, 30, 40)

# setting the names attribute
attr(nums, "names") <- c("A", "B", "C", "D")

# more canonical:
names(nums) <- c("A", "B", "C", "D")

# now we can index by character vector:
nums[c("C", "A")] <- c(3, 1)

############### handy vector functions, logic
nums <- seq(1, 20, 0.2)  # 0 to 20 in steps of 0.2
nums <- seq(1, 20)       # steps of 1
nums <- 1:20             # sugar

print(length(nums))      # 20 (also works on lists)

nums_sample <- sample(nums) # a random permutation
nums_sample <- sample(nums, size = 5) # get 5 random elements
nums_sample <- sample(nums, size = 5, replace = TRUE)  # sample w/ replacement

nums_sample <- rnorm(100, mean = 20, sd = 4)   # sample from a normal distribution

nums_sample[4] <- NA # replace the 4th entry with NA (unknown/not available) for exposition

# return a logical indicating which entries are NA
where_na <- is.na(nums_sample)  # FALSE FALSE FALSE TRUE FALSE FALSE FALSE ...

nums_mean <- mean(nums_sample)   # will be NA since there's an NA in the mix
nums_mean <- mean(nums_sample, na.rm = TRUE)  # remove NA's during computation

nums_mean <- mean(nums_sample[!is.na(nums_sample)])   # no thank you, I can remove NAs myself. (! for negation)
# see also: sd(), median(), sum(), max(), min()

# logical operators are & and | (&& and || exist but don't operate in the same vectorized way as other operators like +, *, >, etc)
a<- c(TRUE, TRUE, FALSE, FALSE)
b<- c(TRUE, FALSE, TRUE, FALSE)
print(a & b) # T F F F
print(a | b) # T T T F


####################
### lists
####################

## lists can hold anything - other lists, etc. they are often named
person_list <- list(36, "male", c("Fido", "Fluffy"))
names(person_list) <- c("age", "gender", "pets")

# directly:
person_list <- list(age = 36, 
                    gender = "male", 
                    pets = c("Fido", "Fluffy"))

# accessing w/ [] returns a sublist:
person_no_pets <- person_list[c(2, 3)]
# aka, by name
person_no_pets <- person_list[c("gender", "pets")]

# but you'll get a list of 1 if you ask for it this way:
pets_only_list <- person[3]        # not a vector of len 2, but rather a list of lenght 1 holding a vector of length 2

# double-brackets are used for that
pets <- person[[3]]                # vector of length 2
# by name
pets <- person[["pets"]]
# syntactic sugar:
pets <- person$"pets"
# syntactic sugar (if name is simple, no funky chars)
pets <- person$pets


# we can work with items inside lists
person$pets[2] <- "DemonCat"    # renaming pet # 2

# and add new entries by name
person$pet_types <- c("dog", "cat")


############## lists as hashes
# we can use lists like hashes (lookup is fast, but they don't grow efficiently, see the hash package for a better alternative: https://cran.r-project.org/web/packages/hash/index.html)
# to so though, we need to watch out for the sugar

myhash <- list() # an empty list
new_key <- "Joe"
new_value <- 36

# this won't work because myhash$new_key is sugar for myhash$"new_key" (is sugar for myhash[["new_key"]])
# myhash$new_key <- new_value

# but this does
myhash[[new_key]] <- new_value


############## lists as objects
# lists are often complex, and they're the de-facto way to store structured (non-rectangular) data. the str() function prints their structure summary
str(person)

# many R functions actually return lists;
samp1 <- rnorm(40, mean = 4, sd = 1)
samp2 <- rnorm(40, mean = 8, sd = 5)
result <- t.test(samp1, samp2)

print(result)  # fancy formatting
str(result)    # show the list structure

# the "class" attribute determines what methods will be dispatched to from generic functions. (in the S3 object system)

attr(person, "class") <- c("Adult", "Person") 
# canonically:
class(person) <- c("Adult", "Person")

# when we run 
print(person)

# because print is generic, it will try print.Adult(person), if not found print.Person(person), and finally if not found print.default(person). 
print(methods(print))   # show all print.* functions
print(methods(class = "list")) # show all *.list functions

######### lapply (map)
# the lapply() function acts as a map; first param is a list, second is a function to call on each element
samples <- list(s1 = rnorm(4),
                s2 = rnorm(50),
                s3 = rnorm(25))
medians <- lapply(samples, median)

# optional follow-on parameters can be specifid for each call in the call to lapply:
medians_ignore_nas <- lapply(samples, median, na.rm = TRUE)


###################
### Misc data types and their caveats
###################

######## matrices and arrays
m <- matrix(1:4, nrow = 2, ncol = 2)

multidimArray <- array(1:12, dim = c(2, 3, 3))

# matrix is a special case of array
print(class(m))               # "matrix", "array"
print(class(multidimArray))   # "array"

## WARNING
# both matrices and arrays are backed by vectors (with metadata on dimensionality for lookup by index),
# meaning both types are limited in the same way as vectors: they can't mix types; there are numeric matrices, character matrices, logical
# matrices, etc.

## WARNING 2
# R's lapply() is nice, and it can also work on a vector input (producing a list output for each element of the vector)
# R also has apply() and sapply() - sapply() tries to convert the output into an appropriate type (vector, list, matrix...) by guessing 
# ugh
# apply() applies a function over dimensions of a matrix or array
# don't use it on a dataframe: it will first convert the dataframe to a matrix (coercing all the data to be the same type)
# https://www.talyarkoni.org/blog/2012/06/08/r-the-master-troll-of-statistical-languages/


######## factors

# factors are annoying, basically a way to efficiently store string vectors and put restrictions on them.
s <- as.factor(c("Good", "Bad", "OK", "Bad"))
print(s)

# to see what's really going on, we remove the class attribute (so we don't get dispatched to factor-specific output)
str(unclass(s))
# output:
#  int [1:4] 2 1 3 1
#  - attr(*, "levels")= chr [1:3] "Bad" "Good" "OK"
# thus: a factor is an integer vector, with an attribute called "levels" that maps integers to their representation

# too much to say about factors here... normally they aren't worth worrying about at first but machine learning in R uses them frequently
# more on factors (in factors section, they broke my anchor links): https://open.oregonstate.education/computationalbiology/chapter/character-and-categorical-data/ 


########### dates and times

# R has native support for these w/ POSIXct and POSIXlt vector types
# the lubridate package adds functions for these types that are actually reasonable


####################
### data frames
####################

# data frames are lists of vectors, one per column, and they keep their columns the same length (recyling entries when creating new columns if necessary, 
# or throwing an error if you try to add a column that's too long)

data <- data.frame(colA = c("A", "B", "C"),
                   colB = c(1, 2, 3),
                   colC = c(TRUE, FALSE, TRUE),
                   stringsAsFactors = FALSE)       # set this if you don't want your char cols turned to factors (this if finally default to false in R 4.0)

# because dataframes are lists of vectors, we can all the stuff we can w/ lists

print(names(data))  # names are the column names
print(data$colB)   # 1 2 3
data$colC[1]  <- FALSE    # set an entry to false

# when we craeat a new entry, e.g. by name, it's recycled:
data$likes_music <- NA   # recycled to NA NA NA

print(data)
#  colA colB  colC likes_music
#1    A    1 FALSE          NA
#2    B    2 FALSE          NA
#3    C    3  TRUE          NA

# colnames() is the column vector names (same as returned by names())
print(colnames(data))  # "colA" "colB" "colC" "likes_music"

# the 1, 2, 3 on the left are not row indices, they are row *names* - stored as a character vector
print(rownames(data))  # "1" "2" "3"

rownames(data) <- c("A1", "A2", "A3")
print(data)
#   colA colB  colC likes_music
#A1    A    1 FALSE          NA
#A2    B    2 FALSE          NA
#A3    C    3  TRUE          NA

# Notice that the quotations are also left off of colA, making it hard to distinguish column types (is colB a character, factor, integer, or numeric vector?!)
# the tidyverse folks have created tibbles - an extension of data.frames that inherit data frame methods but provide nicer versions for
# some operations

library(tibble)
print(as_tibble(data))

## A tibble: 3 x 4
#  colA   colB colC  likes_music
#  <chr> <dbl> <lgl> <lgl>      
#1 A         1 FALSE NA         
#2 B         2 FALSE NA         
#3 C         3 TRUE  NA 


##### base-R indexing

# vectors and lists can be indexed with [], lists and dataframes can be indexed with [[]], and dataframes can be indexed with [ , ]
# where the syntax is [<row_selector>, <col_selector>]; either of these can be a numeric or integer vector (to select by row or column
# index), character vector (to select by row or column name), or logical vector (to select by logical keep/don't keep)

subdata <- data[c(1, 3), c(TRUE, FALSE, FALSE, TRUE)]   # rows 1 and 3, cols 1 and 4

# get rows with colB greater than the median, all cols
subdata <- data[data$colB >= median(data$colB), ]



########################33
### tidyverse
##########################

# in the last ~decade there's been a growth of packages aimed at cleaning up the R user experience,
# particularly around common data-munging tasks
# since many default R functions have varying parameter names for common parameters, etc.
# these also aimed at providing a user-friendliness & compactness
# the main downside is they tend to verge on being DSLs with specialized functions, sometimes broad API for each one
# most tidyverse packages have focused on dataframes, but more recent additions have expanded to include functions for lists,
# arrays, etc.


entries <- data.frame(colA = c("A", "B", "C"),
                   colB = c(1, 2, 3),
                   colC = c(TRUE, FALSE, TRUE),
                   stringsAsFactors = FALSE)    

# this is a base R function that illustrates R's use of non-standard-evaluation to allow working with column names
# as unquoted entries

# changes colB >= median(colB) to entries[["colB"]] >= median(entries[["colB"]] ) before execution
sub_data <- subset(entries, colB >= median(colB))

# tidyverse *loves* these unqouted things
# regular R functions are spotty about doing this, and spotty about which argument is the data (here it's first)
# tidyverse functions strive to take the data argument first; here's dplyrs filter which does the same thing
library(dplyr)
sub_data <- filter(entries, colB >= median(colB))


# to create a new column which is colX = 5 * colB
sub_data$colX <- sub_data$colB * 5    # old-school
sub_data <- mutate(sub_data, colX = 5 * colB)


# the %>% supplies the result of it's left hand side as the first argument to the function on the right-hand side (also using non-
# standard evaluation to accept the calling-form of the right side funtion)

sub_data <- filter(entries, colB >= median(colB)) %>%
  mutate(colX = 5 * colB)   # could also be mutate(., colX = 5. colB), where . is interpreted to mean the input from the LHS

