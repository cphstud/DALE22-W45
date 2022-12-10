library(shadowr)
library(rvest)
library(RSelenium)
library(dplyr)
library(stringr)
library(RMariaDB)
library(DBI)
library(colorfindr)
library(logr)

# create database connection

condb <- dbConnect(MariaDB(),
                   db="mybilbasen",
                   host="localhost",
                   port=3306,
                   user="root",
                   password="root123")

# logfile
tmp <- file.path(tempdir(),"bb.log")
lf <- log_open(tmp)

# links
x <- scan("linkstoedc", what="", sep="\n")
ll <- list(x)
#selenium-stuff
startlink <- 'https://www.edc.dk/sog/?ejd-typer=1'
rD <- rsDriver(port = 4539L, browser = c('firefox'))
rclient <- rD[['client']]
rclient$navigate(startlink)
rclient$navigate("https://www.edc.dk")
rclient$navigate(x[1])
#houspsource <- rclient$getPageSource()


#rvest
basepaginglink <- 'https://www.edc.dk/sog/?ejd-typer=1&antal=1000&side='
endpageinglink <- '#lstsort'
mainhousetag=".propertyitem__wrapper"
mainhousetag=".propertyitem--list"
totaldf = data.frame(matrix(ncol=14, nrow=0))
housedfnames=initnames()
colnames(totaldf) = housedfnames

maxlimit=40

#testhouse = houselist[[4]]

#rclient$navigate(startlink)

for (counter in (1:maxlimit)) {
  #tmplink <- paste0(basepaginglink,counter,endpageinglink)
  tmplink <- x[counter]
  rclient$navigate(tmplink)
  Sys.sleep(10)
  log_print(c("link: ",tmplink))
  tmpsource <- rclient$getPageSource()
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
    tryCatch({  totaldf <- rbind(totaldf,housedf) },
             warning = function(w) { print(c("Warning:",housedf$address)) },
             error = function(e) {print(c("Error:",housedf$address))} )
  }
  #button=rclient$findElement(using = "class name","next")
  #button$clickElement()
  counter=counter+1
}

# save dataframe to fnile - RDS-format
saveRDS(nresdf,"firsbb.rds")


# save dataframe to mysql - first time only


dbWriteTable(condb,"mycars2",nresdfbu,overwrite=T)
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
    colnames(infolist) <- nnd
  tryCatch({
      infolist <- house %>% html_nodes("table") %>% html_table() %>% as.data.frame()
      colnames(infolist) <- nnd
      log_print(c("RAW: ",infolist))
      infolist <- as.data.frame(lapply(infolist, function(x) gsub("[^0-9]","",x)))
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
