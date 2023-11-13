library(shadowr)
library(rvest)
library(RSelenium)
library(dplyr)
library(stringr)
library(RMariaDB)
library(DBI)
library(colorfindr)
library(logr)
library(xml2)

# Selenium-server running in Docker-container
remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",port=4445)
remDr$open()

# create database connection
condb <- dbConnect(MariaDB(),
                   db="edc",
                   host="localhost",
                   port=3306,
                   user="root",
                   password="root123")

# logfile
fn <- paste0("edclog_",as.numeric(Sys.time()),".log")
tmp <- file.path("/Users/thor/Git/edc",fn)
lf <- log_open(tmp)
log_print("Start")

# links
x <- scan("linkstoedc", what="", sep="\n")
#ll <- list(x)
#selenium-stuff
#startlink <- 'https://www.edc.dk/sog/?ejd-typer=1'
#rD <- rsDriver(port = 4539L, browser = c('firefox'))
#rclient <- rD[['client']]
remDr$navigate(x[1])
#shadow_rd <- shadow(remDr) 
#element <- find_elements(shadow_rd,'button')

# TEST ANOTHER SITE
#myurl <- remDr$navigate("https://myterminal.ect.nl/app/object-schedule")
#myrul <- 'https://sts.ect.nl/Account/Login?ReturnUrl=%2Fconnect%2Fauthorize%2Fcallback%3Fclient_id%3D463CCD95-88AA-4CF1-90D7-00F145B0F291%26redirect_uri%3Dhttps%253A%252F%252Fmyterminal.ect.nl%252Fapp%252Fcallback%26response_type%3Did_token%2520token%26scope%3Dopenid%2520profile%2520email%2520ProfilesWebAPI.Profile%2520MyTerminalAPI%2520truckvisitapi%26state%3D19b6e43843474ca58f15c3ed0761ba01%26nonce%3D14319b7925fe441d9082877016f1fcd5'
#remDr$navigate(myurl)
#shadow_rd <- shadow(remDr) 
#element <- find_elements(shadow_rd, 'button[class="_ect-button"]')[[5]]
#element$getElementText()[[1]] #to preview the element we found

#houspsource <- rclient$getPageSource()
# handle cookie
#webElem <- remDr$findElement(using = "class", "desktop")
#webElem$
#remDr$click()


#rvest
#basepaginglink <- 'https://www.edc.dk/sog/?ejd-typer=1&antal=1000&side='
#endpageinglink <- '#lstsort'
#mainhousetag=".propertyitem__wrapper"
mainhousetag=".propertyitem--list"
housedfnames=initnames()
totaldf = data.frame(matrix(ncol=14, nrow=0))
colnames(totaldf) = housedfnames
nnd = c(housedfnames[1],housedfnames[2],housedfnames[3],housedfnames[4],housedfnames[5],housedfnames[6],housedfnames[7],housedfnames[8])


maxlimit=39

#testhouse = houselist[[4]]

#rclient$navigate(startlink)

for (counter in (12:maxlimit)) {
  #tmplink <- paste0(basepaginglink,counter,endpageinglink)
  tmplink <- x[counter]
  remDr$navigate(tmplink)
  Sys.sleep(10)
  log_print(c("link: ",tmplink))
  tmpsource <- remDr$getPageSource()
  househtml <- read_html(tmpsource[[1]])
  #househtml <- read_html(tmplink)
  houselist <- househtml %>% html_nodes(mainhousetag)
  #pagesource <- rclient$getPageSource()
  #carhtml <- read_html(pagesource[[1]])
  #print(tmplink)
  Sys.sleep(3)
  
  for (house in houselist) {
    #tmpdf = as.data.frame(matrix(nrow = 1, ncol = 13))
    #colnames(tmpdf) <- housenames
    #house=testhouse
    tlink <- ""
    housedf <- gethouselist(house)
    # get link
    linktag =".propertyitem__openhouselink"
    altlinktag =".propertyitem__link"
    tlink <- getlink(house,linktag)
    if (nchar(tlink) < 20) {
      tlink <- getlink(house,altlinktag)
    }
    
    housedf$link=tlink
    log_print(c("tlink: ",tlink))
    
    #get sagsnr
    sagsnr <- getsagsnr(house)
    log_print(c("Sagsnr: ",sagsnr))
    housedf$id=sagsnr
    
    #get price
    houseprice <- gethouseprice(house)
    housedf$price=as.numeric(houseprice)
    
    # get region
    regtag = ".propertyitem__address--listview"
    address <-  house %>% html_nodes(regtag) %>% html_text()
    housedf$address=address
    
    housedf$scrapedate=Sys.time()
    housedf$solgt=FALSE
    
    # get dealer
    tryCatch({  
      colnames(housedf) = housedfnames
      colnames(totaldf) = housedfnames
      totaldf <- rbind(totaldf,housedf) 
      },
             warning = function(w) { print(c("Warning:",housedf$address)) },
             error = function(e) {print(c("Error:",housedf$address))} )
  }
  #resfromdb=dbWriteTable(condb,"houses",totaldf)
  lf <- log_open(tmp)
  log_print(c("Wrote to db ",resfromdb))
  
  #button=rclient$findElement(using = "class name","next")
  #button$clickElement()
  counter=counter+1
}
  
# save dataframe to fnile - RDS-format
saveRDS(totaldf,"edc.rds")


# save dataframe to mysql - first time only


dbWriteTable(condb,"edc3",totaldf,overwrite=T)
#dbWriteTable(con, "mtcars", datasets::mtcars, overwrite = TRUE)

# get cars from db
#fetchcars=dbSendQuery(condb,"Select * from cars")
resdb=dbGetQuery(condb,"Select * from cars")


getlink <- function(house,linktag){
  tlink <- house %>% 
    html_nodes(linktag) %>% 
    html_attr("href") %>% 
    paste0("https://edc.dk",.)
  return(tlink)
}

getsagsnr <- function(house) {
  sagsnr=0L
  log_print(c("gets - tlink: ",tlink))
  r=str_match(tlink,"viderestilling")
  if (is.na(r[1,1])) {
    hpattern='nr=([0-9]+)'
    resm=str_match(tlink,hpattern)
    sagsnr = resm[1,2]
    log_print(c("Sagsnr: ",sagsnr))
  } else {
    ss <- house %>% html_attrs()
    sagsnr = ss[[2]]
    log_print(c("Sagsnr: ",sagsnr))
  }
  return(sagsnr)
}

gethouselist <- function(house) {
    infolist = as.data.frame(matrix(nrow = 1, ncol = 8))
  tryCatch({
    infolistvalues <- house %>% html_nodes("td") %>% html_text()
      #infolist <- house %>% html_nodes("td") %>% html_table() %>% as.data.frame()
      colnames(infolist) <- nnd
      log_print(c("RAW: ",infolist))
      infolist <- as.data.frame(lapply(infolistvalues, function(x) gsub("[^0-9]","",x)))
    },
    error = function(e) { log_print(e) }
  )
  return(infolist)
}

gethouseprice <- function(house) {
  pricetag=".propertyitem__price"
  price <- house %>% html_nodes(pricetag) %>% html_text() 
  price <- gsub("[^0-9]","",price)
  return(price)
}
savelogo <- function(s) {
  #src = s %>% html_nodes(".listing-dealer-logo-sm") %>% html_attr("src")
  retval="NA"
  src = s %>% html_nodes(".listing-dealer-logo-sm") %>% html_attr("data-echo")
  log_print(c("S:",src))
  #src="https://billeder.bilbasen.dk/bilinfo/70da5fa5-6af4-45c0-b207-d85ca4de0caf.jpg?class=S400X400"
  tryCatch(
    {
      dealerm=str_match(src,"\\/([a-z0-9-]+)\\.jpg")
      log_print(c("DEAL: ",dealerm))
      retval=dealerm[1,2]
      dfile=paste0("./img/",str_match(src,"[0-9a-z-]+\\.jpeg"))
      #download.file(src, dfile, mode = "wb")
    },
    error=function(e) {
      log_print(e)
    }
  )
  return(retval)
}

initnames <- function(){
  nn2=character()
  nn2[1]="m2"
  nn2[2]="grund"
  nn2[3]="rum"
  nn2[4]="byggeaar"
  nn2[5]="liggetid"
  nn2[5]="plusminus"
  nn2[5]="liggetid"
  nn2[6]="plusminus"
  nn2[7]="prism2"
  nn2[8]="Ejerudgprmd"
  nn2[9]="link"
  nn2[10]="sagsnr"
  nn2[11]="pris"
  nn2[12]="adresse"
  nn2[13]="sold"
  nn2[14]="scrapedate"
  return(nn2)
}
