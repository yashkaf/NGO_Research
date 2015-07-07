@@ -0,0 +1,73 @@
run_all <- function(homeurl="http://reporting.waterforpeople.org") {
# This function runs the script and pulls all the data from all the districts linked to on the homepage into a single data frame.
library(stringr) # For searching and manipulating raw strings
water_links <- get_links(homeurl)
water_data <- init_data()
for(i in seq_along(water_links)) {
    water_data[i,] <- pull_data(water_links[i])
  }
water_data
}


get_links <- function(homeurl="http://reporting.waterforpeople.org") {
# This function pulls all the links to the districts into a single vector.
  # Connect and read lines from the reporting homepage.
  con <- url(homeurl,open="rt") # Read only access
  homepage <- readLines(con)
  close(con) # Closing connection to website

  # Find all links to WFP reporting districts, based on website HTML structure
  links <- str_extract_all(homepage,"(?<=(href=\"))http://reporting.waterforpeople.org/(?!home).{3,30}(?=(\" >.{3,30}</a> </li>))")[[1]]
}

init_data <-function() {
# This function initializes an empty data frame with all 13 variables.
    table<-data.frame(country = character(), region = character(), watwp = numeric(), sanwp = numeric(), everyone = numeric(), forever = numeric(), sanitation = numeric(), 
                    wfpinv = numeric(), govinv = numeric(), cominv = numeric(), othinv = numeric(), totinv = numeric(), saninv = numeric(), stringsAsFactors = FALSE)
}

pull_data <- function(suburl) {
# This function extracts 13 variables of data from a webpage of a single district.
  # Connect and read lines from the subdistrict page
  con <- url(suburl,open="rt") # Read only access
  subpage <- readLines(con)
  close(con) # Closing connection to website
  
  # Country and district name
  coun <- str_extract(subpage,"[:alpha:]{2,30}(?=</a></span></li><li class=\"last\">)")
  dist <- str_extract(subpage,"(?<=<head> <title>)[:print:]{3,40}(?= - Water For People RIR</title>)")
  
  # Number of measuring waterpoints
  wawp <- str_extract(subpage,"(?<=showWaterServiceMetrics.{20,200}Water Points=)[:digit:]+")
  suwp <- str_extract(subpage,"(?<=showSanitationMetrics.{20,200}Water Points=)[:digit:]+")
  
  # Everyone, forever and sanitation scores
  eone <- str_extract(subpage,"[:digit:]{0,2}%(?=.{10,30}chartWaterService)")
  fore <- str_extract(subpage,"[:digit:]{0,2}%(?=.{10,30}chartWaterSustainability)")
  ssco <- str_extract(subpage,"[:digit:]{0,2}%(?=.{10,30}chartSanitationData)")
  
  # Investment breakdown
  winv<-str_extract(subpage,"(?<=(window.wfpExpenditures).{0,10}(Water For People).{0,10}[$])[:digit:]{1,3}(,[:digit:]{3})+")
  winv<-as.numeric(gsub(",","",winv))
  if(is.na(winv)) winv <- 0 # Expenditure by Water for People
  ginv<-str_extract(subpage,"(?<=(window.wfpExpenditures).{10,50}(Government).{0,10}[$])[:digit:]{1,3}(,[:digit:]{3})+")
  ginv<-as.numeric(gsub(",","",ginv))
  if(is.na(ginv)) ginv <- 0 # Expenditure by the government
  cinv<-str_extract(subpage,"(?<=(window.wfpExpenditures).{30,150}(Community).{0,10}[$])[:digit:]{1,3}(,[:digit:]{3})+")
  cinv<-as.numeric(gsub(",","",cinv))
  if(is.na(cinv)) cinv <- 0 # Expenditure by the local community
  oinv<-str_extract(subpage,"(?<=(window.wfpExpenditures).{50,200}(Other).{0,10}[$])[:digit:]{1,3}(,[:digit:]{3})+")
  oinv<-as.numeric(gsub(",","",oinv))
  if(is.na(oinv)) oinv <- 0 # Expenditure by others
  tinv <- winv + ginv + cinv + oinv  # Total expenditure in district
  
  # Total expenditure on sanitation from all sources
  sinv<-str_extract(subpage,"(?<=(<div class=\"surveyLabel\" title=\"Sanitation\">Sanitation</div>).{0,1000}(<div class=\"surveyValue\">)[$])[:digit:]{1,3}(,[:digit:]{3})+")
  sinv<-as.numeric(gsub(",","",sinv))

  c(coun,dist,wawp,suwp,eone,fore,ssco,winv,ginv,cinv,oinv,tinv,sinv)
}


