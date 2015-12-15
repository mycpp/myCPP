source("code/apikey.R") #get API key for EIA
require(RJSONIO)

querytype = c("series", "category")

getUrl <- function(type, ORISCode)
{
  root = c('http://api.eia.gov/','/?api_key=', '&')
  u <-   paste0(root[1], type,
                root[2], apikey,
                root[3], type, 
                "_id=ELEC.PLANT.CONS_EG_BTU.",
                ORISCode,"-ALL-ALL.A&out=json")
  return(URLencode(u))
}

plant2012data <- read.csv("https://docs.google.com/spreadsheets/d/1ZbDI31sSKatBoEVKo70TV_A4VwCBHK4pIoCWXB7yfx0/pub?gid=1659543673&single=true&output=csv")

pattern <- "\\:\\s\\D*\\s" # ": [name]"

getLocations <- function(ORISCode){
  type = "series"
target <- getUrl(type, ORISCode)
datafromJSON   <- fromJSON(target)

rawName <- datafromJSON$series[[1]]$name

leftOfName <- regexpr(pattern, rawName, perl = TRUE)
rightOfName <- leftOfName  + attr(leftOfName, "match.length") - 1


name <- substr(rawName, leftOfName+2, rightOfName-1)

latitude <- datafromJSON$series[[1]]$lat
longitude <- datafromJSON$series[[1]]$lon
geography <- datafromJSON$series[[1]]$geography

returnRow <- t(c(name, ORISCode, latitude, longitude, geography))
return (returnRow)
}
ORIS.Codes <- unique(plant2012data$ORIS.Code)
#x[1:15]
#t(sapply(x[1:15], getLocations))
#sapply(x[1:100], getLocations, simplify = "array")
y1 <- sapply(ORIS.Codes[1:100], getLocations, simplify = "TRUE")

v1 <- matrix(NA, nrow = 100, ncol = 5)
v2 <- vapply(ORIS.Codes[1:100], getLocations, FUN.VALUE = t(c(rep("x", 5))))
