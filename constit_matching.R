setwd("C:/Users/dapon/Dropbox/Smith-Daponte-Smith/constituencies")
library(tidyverse)
library(sf)
#note: const2007 are the constituencies defined by the electoral amendment act of 2009 and used at the 2011 election
      # we know this because Kerry North West Limerick appears for the first time in this data
#const2013 are those defined by the electoral amendment act of 2013 and used at the 2016 election
#const2017 are those defined by the electoral amndment act of 2017 and used at the 2020 election
#const98 are those defined by the electoral amendment act of 1998 and used at hte 2002 election
#this means we're missing the 2007 election shapefiles 

#read in place names, which contains coordinates 
places <- read_csv("IE_place_names.csv") 

#eliminate NA entries 
places <- places %>% filter(!is.na(latitude) & !is.na(longitude) & 
                              !is.na(const90) & !is.na(const95) & !is.na(const98))

#create row index in places - will be used for merging with constits later 
places$index <- seq(1, nrow(places), 1)

#read in 2017 district shapefile 
shp <- st_read("6674493c-ac73-4998-b33c-542767be56ff2020328-1-1e1htqf.0cjo.shp")
shp_crs <- st_crs(shp)

#transform coordinates of places into sf file
test <- sf::st_as_sf(places, coords=c("longitude", "latitude"), crs = "WGS84")
#test <- test %>% st_set_crs(., shp_crs)
#test <- st_transform(test, "+proj=longlat +datum=WGS84")
test <- st_transform(test, crs = shp_crs)

#the col.id column corresponds to constituencies by their row placement in shp
inter <- sf::st_intersects(test, shp, sparse = T) %>% as_tibble()

#create index to match with the col.id column 
shp$index <- seq(1,39,1)

const <- NULL

#get vector of constituency names
constits <- (shp$ENGLISH_NA) %>% as_tibble()

#loop through inter$col.id to match the numbers to constituency names 
for( i in 1:nrow(inter)) {
  const[i] <- constits[inter$col.id[i],]
}
const <- unlist(const) %>% as_tibble()

#bind the constituencies with the row ids for merging with places data 
const <- cbind(const, inter$row.id) 
names(const) <- c("const2017", "index")

t2 <- left_join(places, const, by = "index")
#note that there are 34 rows that have values for const83 and 90 but are missing cost95 and 98 ... 
#and these are all in Wicklow, which is interesting 

#there are 5 rows with values for all const variables except 98, and all in Cork East 


#repeat with 2013 constituencies 
bound2013 <- st_read("boundaries_2013.shp")
crs_2013 <- st_crs(bound2013)
test <- st_transform(test, crs = crs_2013)
inter2013 <- sf::st_intersects(test, bound2013, sparse = T) %>% as_tibble()
bound2013$index <- seq(1,40,1)


const<-NULL
#get vector of constituency names
constits <- (bound2013$MAX_CON_NA) %>% as_tibble()

#loop through inter$col.id to match the numbers to constituency names 
for( i in 1:nrow(inter2013)) {
  const[i] <- constits[inter2013$col.id[i],]
}
const <- unlist(const) %>% as_tibble()

#bind the constituencies with the row ids for merging with places data 
const <-cbind(const, inter2013$row.id)
names(const) <- c("const2013", "index")

t3 <- left_join(t2, const, by = "index")


#repeat for 2007 constituencies 
#note that these 2007 constituencies are actually those defined by the electoral amendment act of 2009 and used at the 2011
#election - we know this because they contain Kerry North West Limerick, used for the first time in that year 
bound2007 <- st_read("Census2011_Constituencies_2007.shp")
crs_2007 <- st_crs(bound2007)
test <- st_transform(test, crs = crs_2007)
inter2007 <- sf::st_intersects(test, bound2007, sparse = T) %>% as_tibble()
bound2007$index <- seq(1,43,1)


const<-NULL
#get vector of constituency names
constits <- (bound2007$NAME) %>% as_tibble()

#loop through inter$col.id to match the numbers to constituency names 
for( i in 1:nrow(inter2007)) {
  const[i] <- constits[inter2007$col.id[i],]
}
const <- unlist(const) %>% as_tibble()

#bind the constituencies with the row ids for merging with places data 
const <-cbind(const, inter2007$row.id)
names(const) <- c("const2007", "index")

t4 <- left_join(t3, const, by = "index")


#Also need to standardize the constituency names across the datasets to match 83-98 versions 

t4$const2013 <- as.character(t4$const2013)
t4$const2013[t4$const2013 == "Cavan-Monaghan (4)\n"] <- "Cavan Monaghan" # There's one district called "D" ... which one is that? One of the Dublins? 
t4$const2017 <- as.character(t4$const2017)
t4$const2017[t4$const2017 == "D"] <- "Dún Laoghaire"


#write function to make cosntituency names match the 85-98 versions 
const_clean_func <- function(const_vec) { 
  const_vec <- as.character(const_vec)
  try <- str_remove(const_vec, "[(]")
  try <- str_remove(try, "[)]")
  try <- str_remove(try, " 5")
  try <- str_remove(try, " 4")
  try <- str_remove(try, " 3")
  try <- str_replace(try, "-"," ")
  try <- str_remove(try, "\n")
  try <- str_remove(try, "\n")
  try <- str_replace(try, "Laois Offaly","Laoighis Offaly")
  
  return(try)
}

#apply function to 2017 vector 
t4$const2017 <- const_clean_func(t4$const2017)

#apply to 2013 vector
t4$const2013 <- const_clean_func(t4$const2013)

#apply to 2007 vector
t4$const2007 <- const_clean_func(t4$const2007)

try <- str_remove(t4$const2013, c("[(]","[)]"))


