

# Define working directory of where the data files reside.
setwd("~/Darcy/Ryerson/Capstone (CKME136)/Yelp Data")

# "readr" is used to read in the records from the json data files.
#install.packages("readr")
library(readr)

# "stringr" is used to combine the data from each file into a single json string for processing
#install.packages("stringr")
library(stringr)

# "jsonlite" is used to parse the json string
#install(jsonlite)
library(jsonlite)

# "dplyr" is needed by "jasonlite" to enable a tribble dataframe
#install.packages("dplyr")
library(dplyr)

# "n_max = 2,500,000" is used with the reviews dataset because of memory limitations but dataset will be
# large enough for this analysis
lines <- read_lines("yelp_academic_dataset_review.json", n_max = 2500000, progress = TRUE)

# Each line in "review_lines" is combined into a single JSON string to optimize processing
combined <- str_c("[", str_c(lines, collapse = ", "), "]")

# The single JSON string is parsed and stored as a tribble dataframe, "flatten" is used to extract any
# nested JSON data, "%>%" is a pipe function to cycle through all records
reviews <- fromJSON(combined) %>%
  flatten() %>%
  tbl_df()

# Same process is used to load the business JSON file, although the whole file is read
lines <- read_lines("yelp_academic_dataset_business.json", progress = TRUE)
combined <- str_c("[", str_c(lines, collapse = ", "), "]")
business <- fromJSON(combined) %>%
  flatten() %>%
  tbl_df()

# Remove spaces in business$categories to prepare for word frequency analysis
business$categories <- gsub('\\s+', '', business$categories)


# Word frequency in categories to determine what is the best way to capture businesses that are resturants
# by using the table function to seperate each phrase split by a comma and stort for most frequent words
# what is found is "restaurants" will provide the most businesses
category_freq_work <- data.frame(table(unlist(strsplit(tolower(business$categories), ","))))
category_freq_work[order(-category_freq_work$Freq),]

# Next is to refine the business dataframe to only include rows that have "restaurants"
# in the category field
business <- dplyr::filter(business, grepl('Restaurant', categories))

# To determine which cuisine to use, I will pick the top 10 most popular cuisines
category_freq_work <- data.frame(table(unlist(strsplit(tolower(business$categories), ","))))
category_freq_work[order(-category_freq_work$Freq),]

# american(traditional)
# american(new)
# italian
# mexican
# chinese
# japanese
# sushibars
# asianfusion
# canadian(new)
# mediterranean

# Business dataframe is filter to include only the 10 most popular cuisines
business <- dplyr::filter(business, grepl('american\\(traditional\\)|american\\(new\\)|italian|mexican|chinese|japanese|sushibars|asianfusion|canadian\\(new\\)|mediterranean', categories, ignore.case = TRUE))

# Create a new column in business called "cuisine" as it will be used for analysis
business$cuisine <-  ifelse(grepl("american\\(traditional\\)", business$categories, ignore.case = T), "american(traditional)", 
                     ifelse(grepl("american\\(new\\)", business$categories, ignore.case = T), "american(new)",
                     ifelse(grepl("italian", business$categories, ignore.case = T), "italian",
                     ifelse(grepl("mexican", business$categories, ignore.case = T), "mexican",
                     ifelse(grepl("chinese", business$categories, ignore.case = T), "chinese",
                     ifelse(grepl("japanese", business$categories, ignore.case = T), "japanese",
                     ifelse(grepl("sushibars", business$categories, ignore.case = T), "sushibars",
                     ifelse(grepl("asianfusion", business$categories, ignore.case = T), "asianfusion",
                     ifelse(grepl("canadian\\(new\\)", business$categories, ignore.case = T), "canadian(new)", "mediterranean")))))))))

# To determine which cities to use, I will pick the top 10 most popular cities and filter to only
# include businesses in those cities
category_freq_work <- data.frame(table(unlist(strsplit(tolower(business$city), ","))))
category_freq_work[order(-category_freq_work$Freq),]
business <- dplyr::filter(business, grepl("las vegas|toronto|phoenix|montréal|charlotte|pittsburgh|calgary|scottsdale|cleveland|mesa", business$city, ignore.case = TRUE))
business$city <- trimws(business$city)

# Some cities names need to be consolidated
business$city[which(business$city=="charlotte")]<-"Charlotte"
business$city[which(business$city=="East Cleveland")]<-"Cleveland"
business$city[which(business$city=="Cleveland Heights")]<-"Cleveland"
business$city[which(business$city=="North Las Vegas")]<-"Las Vegas"
business$city[which(business$city=="N. Las Vegas")]<-"Las Vegas"
business$city[which(business$city=="N Las Vegas")]<-"Las Vegas"
business$city[which(business$city=="South Las Vegas")]<-"Las Vegas"
business$city[which(business$city=="Montréal-Nord")]<-"Montréal"
business$city[which(business$city=="Communauté-Urbaine-de-Montréal")]<-"Montréal"
business$city[which(business$city=="Montréal-Ouest")]<-"Montréal"
business$city[which(business$city=="Montréal-West")]<-"Montréal"
business$city[which(business$city=="East Pittsburgh")]<-"Pittsburgh"
business$city[which(business$city=="North Toronto")]<-"Toronto"

# Filter the reviews dataframe to exclude any businesses that are not in the refined business
# dataframe based on business id
reviews <- reviews %>%
  filter(business_id %in% business$business_id)

# Based on business id, populate the reviews dataframe with the city and cuisine values from the business
# dataframe
reviews$city <- business$city[match(reviews$business_id, business$business_id)]
reviews$cuisine <- business$cuisine[match(reviews$business_id, business$business_id)]

# Display dataset size by city and cuisine
with(reviews, table(city, cuisine))


