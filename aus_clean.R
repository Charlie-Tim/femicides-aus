library("selectr")
library("xml2")
library("rvest")
library("stringr")
library("magrittr")
library("tidyverse")
library("lubridate")

#the following script can be used to scrape information from the AUSTRALIAN FEMICIDE &
#CHILD DEATH MAP. This is a a companion project to The RED HEART Campaign's Memorial 
#to Women and Children Lost to Violence.

url <- "https://www.google.com/maps/d/u/0/viewer?mid=1WEjK9o99Qm734bKyizsOY_4XPDUgvnp2&ll=-32.889276537388845%2C147.88251818099047&z=5"
webpage <- read_html(url)

#script node
text <- webpage %>% html_nodes("script")
#get text
test <- text[2] %>% html_text()

#where Victim_name appears in the string
idvic <-gregexpr(pattern = "Victim_name",test)[[1]]

#start point for each observation
idbr <- gregexpr(pattern = "\\[\\[\\[.*?\\\"\\]",test)[[1]]
idbr <- idbr[idbr > (idvic[1] -200)]

#store observations (info for each victim) in a list
obvs <- list()
for(i in 1:(length(idbr) - 1)){
  obvs[[i]] <- substr(test, start = idbr[i], stop =  idbr[(i + 1)])
}
length(obvs)

#get rid of 2002, 2003, 2004 which aren't observations
obvs = obvs[-(2002:2004)]


#get spatial information
#long lat coordinates are the first 2 numbers
processLine <- function(line){
  as.numeric(unlist(regmatches(line,
                               gregexpr("-?\\d+\\.\\d+",line))
  )      )[1:2] 
}

aus_coords <- do.call("rbind",lapply(obvs,processLine))


#clean up for simplicity
new_obvs <- obvs %>%
  str_replace_all(pattern = "\\\\n", replacement = "") %>%
  str_replace_all(pattern = "[\\^]", replacement = " ") %>%
  str_replace_all(pattern = '\\\\"', replacement = "")

#function to extract key info (using regex and pattern match)
processLine2 <- function(line){
  name = gsub("^.+Victim_name,?\\[","",line)
  name = trimws(gsub("?\\].+", "", name))
  date= gsub("^.+(STORY.+?:).+$","\\1",line)
  date = gsub(".+\\[(.+):$","\\1", date)
  age = gsub("^.+(Age_at_death,.+?\\]).+$","\\1",line)
  age = gsub(".+\\[(.+)\\]$","\\1", age)
  year = gsub("^.+(Year_of_death,.+?\\]).+$","\\1",line)
  year = gsub(".+\\[(.+)\\]$","\\1", year)
  cause = gsub("^.+(Cause_of_death,.+?\\]).+$","\\1",line)
  cause = gsub(".+\\[(.+)\\]$","\\1", cause)
  killer = gsub("^.+(relationship_to_victim,.+?\\]).+$","\\1",line)
  killer = gsub(".+\\[(.+)\\]$","\\1", killer)
  killer_gender = gsub("^.+(perpetrator_gender,.+?\\]).+$","\\1",line)
  killer_gender = gsub(".+\\[(.+)\\]$","\\1", killer_gender)
  c(name,date, age, year, cause, killer, killer_gender)
}

#run for all data
ausdata <- do.call("rbind",lapply(new_obvs,processLine2))

ausdata <- cbind(aus_coords, ausdata)
dimnames(ausdata) <- list(c(),c("Latitude","Longitude","Name", "Date", "Age", "Year", "Cause",
                                "Relationship to killer", "Killer gender"))
ausdata <-  as.data.frame(ausdata)

#year is represented differently in the html after obsv 2203
ausdata$Year[grep("null", ausdata$Year)] <-  gsub(".+,", "",ausdata$Year[grep("null", ausdata$Year)])
ausdata$Year <-  as.numeric(ausdata$Year)


###look at date format
good_dates <-  grep("[A-Za-z]+ \\d{1,2}, \\d{4}", ausdata$Date)
bad_dates <- setdiff(1:nrow(ausdata), good_dates)
ausdata$Date[which(nchar(ausdata$Date) > 100)] <-  NA

#some dates have to be cleaned up, but we have year anyway so should be fine

#tidy up a few obsv
ausdata$Name[nchar(ausdata$Name) > 50] <-  NA
ausdata$Cause[which(nchar(ausdata$Cause) > 100)] <- "Domestic violence"

#Our dataframe includes spatial information (Latitude, Longitude), the Year (and date),
#the age of the victim, the cause of death, the relationship of the killer to the victim and 
#the gender of the killer

#there are 2286 observations in total (as of 30/11/20), 1275 of which occurred in 2000 0r later


