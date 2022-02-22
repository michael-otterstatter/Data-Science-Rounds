######################################################################
# BCCDC Biostats - Efficient Data Import in R
# Michael Otterstatter, Feb 15, 2019
#
# Much of this material is based on the book 'R for Data Science' by
# Garrett Grolemund and Hadley Wickham
# Freely available online at: https://r4ds.had.co.nz/data-import.html
#
# This script was written to work on the BCCDC Analytics Platform
#
######################################################################

setwd("O:/BCCDC/Groups/Analytics_Resources/Training/Biostats/Sessions/Feb 15 2019 - data import")


# Set checkpoint to ensure reproducibility
#  By defining a 'checkpoint' date, R will use only those versions of R functions available
#  at the specified date, thus ensuring subsequent updates to R will not cause
#  the code to break.  For more information on checkpoint, see:
#  https://cran.r-project.org/web/packages/checkpoint/vignettes/checkpoint.html
library(checkpoint)
checkpoint("2017-06-30") # set checkpoint date (June 30, 2017 matches R version on analytics platform)



library(tidyverse)
# Here, the particular tidyverse package of interest is readr
# which contains a variety of efficient data importation tools
# that work seemlessly with the tidy data manipulation tools
# (see BCCDC Biostats session on Tidy Data - Jan 19, 2019)
# for more information on readr see https://readr.tidyverse.org/
# we will focus primarily on importing csv files with readr 
# using the function read_csv

# read_csv has many options that can be specified;
# however, we usually need to specify only the file for importing,
# and can use default settings for all remaining options

# read_csv(file,
#          col_names = TRUE,
#          col_types = NULL,
#          locale = default_locale(),
#          na = c("", "NA"),
#          quoted_na = TRUE,
#          quote = "\"",
#          comment = "",
#          trim_ws = TRUE,
#          skip = 0,
#          n_max = Inf,
#          guess_max = min(1000, n_max),
#          progress = show_progress(),
#          skip_empty_rows = TRUE)


# location of data file (note: in R, must use forward slashes not back slashes)
indat <- "O:/BCCDC/Groups/Analytics_Resources/Training/Biostats/Sessions/Feb 15 2019 - data import"

# example file name
infile <- "bccdc_reportable_disease_age_sex.txt"

# note, we can maximize the flexibility of the code by specifying the file path 
# and file name separately and then combining these using paste(), for example
paste(indat, infile, sep = "/")



# In most cases, only the file needs to be specified
#  readr will intelligently do the rest
example.data <- read_csv(paste(indat, infile, sep = "/"))

example.data


# note, that readr guesses the correct column formats and tells you explicity
#  what formats were assigned.  If you prefer to specify these manually, simply
#  copy and paste the column specifications shown by readr and modified as desired
#  for example, change Report Year column format to character instead of numeric:
example.data <- read_csv(paste(indat, infile, sep = "/"), col_types = 
  cols(
    Disease = col_character(),
    `Report Year` = col_character(),
    Sex = col_character(),
    `Age Group` = col_character(),
    Count = col_double()
  ))

example.data



# For comparison, what if we had imported the same data using the base R 
# fucntion read.csv?
example.data.old <- read.csv(paste(indat, infile, sep = "/"))

example.data.old

glimpse(example.data.old)


# For illustration purposes, imagine you've received a data file like the following,
# which contains a mix of unneeded text interspersed with the data:
messy <- "Here is the data file I promised you \n
note that I did not include the column names so these will need to be added manually \n
also I have put some comments throughout to help clarify the data elements \n
# here are the counts by HA \n
FHA,2015-01-01,1,5,3 \n
IHA,2015-05-01,4,*missing*,9 \n
NHA,2016-03-13,6,8,2 \n
# here are the corresponding ages \n
FHA,2015-01-01,34,45,13 \n
IHA,2015-05-01,56,N/A,23 \n
NHA,2016-03-13,56,23,63"

cat(messy)

# cleaning this up with read_csv is surprisingly easy, as we only need to specify
# the options for column names, the number of initial rows to skip, the symbol(s)
# used to identify comment rows, and the codes used to indicating missing data
# (by default read_csv will also drop rows that are empty)
clean <- read_csv(messy, 
         col_names = c("HA","Date","Early","Mid","Late"), 
         skip = 6, 
         comment = "#",
         na = c("*missing*", "N/A"))

clean


# dealing with dates
# read_csv will recognize dates if they are uniformly presented
# e.g., consistently in order year, month, day
uniform.dates <- data_frame(Dates = c("1809-02-11","1809-02-11","1809/02/11")) # create example dates
write_csv(uniform.dates, paste(indat,"uniform.dates.csv", sep = "/")) # write example dates as csv file
read_csv(paste(indat, "uniform.dates.csv", sep = "/")) # import csv file with example dates

# but if dates are inconsistently presented, read_csv will simply code these
# as character strings (i.e., maintain the original structure) and leave the 
# clean-up to you
mixed.dates <- data_frame(Dates = c("1809-02-11","1809/2/11","11-02-1908"))
write_csv(mixed.dates, paste(indat,"mixed.dates.csv", sep = "/"))
read_csv(paste(indat, "mixed.dates.csv", sep = "/"))


# fortunately, the lubridate package (part of the tidyverse) makes working with dates easy
library(lubridate)
raw.dates <- data_frame(Dates = c("1809-02-11","1809-2-11","1809/02/11",
               "1809/2/11","1809:02:11"))
write_csv(raw.dates, paste(indat,"raw.dates.csv", sep = "/"))
raw.dates2 <- read_csv(paste(indat, "raw.dates.csv", sep = "/"))

raw.dates2

# with lubridate, we need only specify the ordering of the date components
# e.g., ymd (meaning dates are in the form year, month, day)
clean.dates <- raw.dates2 %>%
  mutate(Dates = ymd(Dates))

clean.dates
         
      


################################################################################################
# A note about speed: read_csv tends to be much faster than read.csv
# for comparison, you can create arbitrarily large csv files
# and then see how long read_csv takes to import vs read.csv

n <- 10000000
lrg.data <- tibble(x = runif(n, 0, 1), 
                   y = runif(n, 0, 1), 
                   z = sample(c("a","b","c","d"), n, replace = TRUE))

outfile <- "large.data.example.csv"
write_csv(lrg.data, paste(indat, outfile, sep = "/"))

test.new <- read_csv(paste(indat, outfile, sep = "/")) # about 11 sec for 10 million rows
test.old <- read.csv(paste(indat, outfile, sep = "/")) # about 200 sec for 10 million rows
################################################################################################





