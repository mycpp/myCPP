source("code/apikey.R") #get API key for EIA
require(RJSONIO)

#querytype = c("series", "category")

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
statePattern <- "\\-\\w*"

getLocations <- function(ORISCode){
  type = "series"
target <- getUrl(type, ORISCode)
datafromJSON   <- fromJSON(target)

## Name -------
rawName <- datafromJSON$series[[1]]$name

leftOfName <- regexpr(pattern, rawName, perl = TRUE)
rightOfName <- leftOfName  + attr(leftOfName, "match.length") - 1

name <- substr(rawName, leftOfName+2, rightOfName-1)

## State -------
rawState <- datafromJSON$series[[1]]$geography # e.g. USA-AL

leftOfState <- regexpr(statePattern, rawState, perl = TRUE)
rightOfState <- leftOfName  + attr(leftOfState, "match.length") - 1

State <- substr(rawState, leftOfState+1, rightOfState-1) # e.g. AL

## Parameters -------
latitude <- datafromJSON$series[[1]]$lat
longitude <- datafromJSON$series[[1]]$lon

## Return -----
returnRow <- matrix(c(name, ORISCode, latitude, longitude, State), nrow = 1, ncol = 5)
colnames(returnRow) <- c("Name", "Code", "Lat", "Lon", "State")
return (returnRow)
# -----
}

## ORIS Codes -----
ORIS.Codes <- unique(plant2012data$ORIS.Code)
#x[1:15]
#t(sapply(x[1:15], getLocations))
#sapply(x[1:100], getLocations, simplify = "array")
y1 <- t(sapply(ORIS.Codes, getLocations, simplify = "TRUE"))
colnames(y1) <- c("Name", "Code", "Lat", "Lon", "State")

#v1 <- matrix(NA, nrow = 100, ncol = 5)
#v2 <- vapply(ORIS.Codes[1:100], getLocations, FUN.VALUE = t(c(rep("x", 5))))
