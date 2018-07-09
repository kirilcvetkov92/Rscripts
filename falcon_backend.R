rm(list=ls(all=TRUE))
library(jsonlite)
library(DBI)
library(ROracle)
library(RCurl)
library(plyr)
library(dplyr)
library(purrr)
library(data.table)
library(stringr)

#-------------------------------------------------------------------------------------------
# Functions

readQuery<-function(file) paste(readLines(file), collapse="\n") 

perClientInstrumentRankingFund <- function(client, instrument) {
  
  if (nrow(instrument) == 0){
    return (list())
  }
  
  instrument$Score <- 0
  
  #1. Currency Scores
  
  mainCCY <- instrumentDetails[instrumentDetails$fundId==instrument$fundId && instrumentDetails$isMainShareClass==TRUE,]$currency
  shareCCY <- instrument$currency
  clientCCY <- ''
  
  if (is.null(mainCCY) || length(mainCCY) == 0) {
    mainCCY <- "NaN"
  }
  if (is.atomic(client)) { 
    clientCCY <- client["CCY"]
  } else {
    clientCCY <- client$CCY
  }
  
  #1.1 client ref currency is scored in context of client
  #1.2 is currency of main share class
  #This only happens if the main share class is not part of the input which is unlikley
  
  if (shareCCY == clientCCY){ 
    instrument$Score <- instrument$Score +  60000}
  else if (shareCCY == mainCCY){ 
    instrument$Score <- instrument$Score +  50000}  
  else if (shareCCY == "USD"){ 
    instrument$Score <- instrument$Score +  40000}  
  else if (shareCCY == "EUR"){ 
    instrument$Score <- instrument$Score +  30000} 
  else if (shareCCY == "GBP"){ 
    instrument$Score <- instrument$Score +  20000} 
  else if (shareCCY == "CHF"){ 
    instrument$Score <- instrument$Score +  10000}
  
  # 2. Is Main Share Class
  if (instrument$isMainShareClass == TRUE) {
    instrument$Score <- instrument$Score + 1000} 
  
  # 3. Is Hedged
  if (instrument$hedged == TRUE) {
    instrument$Score <- instrument$Score + 100}  
  
  # 4. prefer distribution type accumulating
  if  (instrument$distribution == 'Accumulating' | instrument$distribution == 'accumulating'){
    instrument$Score <- instrument$Score + 10}
  
  # 5. Initial investment in relation with ongoing charge
  instrument$Score <- instrument$Score + instrument$chargeScore
  
  return(instrument)
}
perClientInstrumentRankingEquity <- function(client, instrument) {
  if (is.atomic(client)) { 
    clientCCY <- client["CCY"]
  } else {
    clientCCY <- client$CCY
  }
  if (clientCCY == instrument$currency) {
    instrument$Score <- 500
  } else {
    instrument$Score <- 100
  }
  return (instrument)
}
perClientInstrumentRanking <- function(client, instrument) {
  if(length(instrument$instrumentType)>0) {
    
    if (instrument$instrumentType == 'Fund') {
      return(perClientInstrumentRankingFund(client, instrument))  
    }else if (instrument$instrumentType =='Equity') {
      return(perClientInstrumentRankingEquity(client, instrument))
    } else {
      return (instrument)
    }
  } else {
    return (instrument)
  }
}
#--------------------------------------------------------------------------------------------
## LOAD DATA
# Connection
drv <- dbDriver("Oracle")
# Create the connection string
host <- "PPC1"
port <- 1702
sid <- "PPC1"
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")
con <- dbConnect(drv, username = "",
                 password = "",dbname=connect.string)
#--------------------------------------------------------------------------------------------
## load from Database

## read unique bmo context combinations
args <- commandArgs()
portfolios <-args[7]
isins <- args[8]
initative <- args[9]
type <- args[10]

#TEST DATA
if (length(args)==2){
  #portfolios <- "'480010033670003','480010074750001','480010039530002','480010044800002', '000030076190201','000000005710201'" #spanish ptfs booked in LU
  #portfolios <- "'450012850201'"
  #portfolios <- "'000030076190201','000000005710201','000031531870201'" #Other CD Andorra, CD CH
  portfolios <- "'131210201','145560201','485320201','905010201','1190720202','1614580201','1680900201','1771230201','1888340201','1900130201'"
  #portfolios <- "'000001190720202'"
    #portfolios <- "'31501390201','31557450203','22167370203','30748360201','451825200201','1585830201','31324430206','3610000201','453057150201','30560140201','480010165260002','30361650201','30869330201','480010129050001','31454420201','5312690201','31183360201','451814140201','451870530201'"    
  #portfolios <- "'155600203'"
  #portfolios <- "'480024800770001','480010187460002','480010179610001'" #lux booked in lux
  
  #isins <- 'LU1240770799'
  isins <- 'IE00B8449Z10,IE00BBL4VQ02,IE00B84WGD25,IE00BBL4VP94,IE00BBL4VR19,IE00BBT34H14,IE00BBL4VV54,IE00B7WMSM01,IE00B83XD802,IE00B8H6X308,IE00B831WC11,IE00BBL4VS26,IE00BBL4VW61,IE00BBL4VZ92,IE00B88WFS66,IE00BBL4VX78'
  #isins <- 'LU1687713633,LU1649332456'
  isins <- 'US30303M1027,US5949181045'
  type <- 'csv'
  initative <- 'Next Gen'
}
#in case ptf number is not DM ready
ptf_string <- strsplit(portfolios,',')
#FIXME this only works if randomly the first portfolio has not 16
#if (nchar(ptf_string[[1]][1]) != 17)
#{
  ptf_string <- substring(unlist(ptf_string), 2)
  ptf_string <- str_pad(as.character(ptf_string), 16 , pad = '0')
  ptf_string <- paste("'",ptf_string, sep = "")
  portfolios <- paste(ptf_string, collapse = ',')
#}

#system.time({
sql_string <- readQuery("D:\\Data\\data_processing\\SQL_Scripts\\Falcon\\distinct_bmo.sql")
sql_string <- gsub(":pf",portfolios, sql_string)
res  <- dbSendQuery(con, sql_string)
uniqeBMO <- fetch(res)

## read portfolio details
sql_string <- readQuery("D:\\Data\\data_processing\\SQL_Scripts\\Falcon\\portfolio_details.sql")
sql_string <- gsub(":pf",portfolios, sql_string)
res  <- dbSendQuery(con, sql_string)
portfolioDetails <- fetch(res)

## read portfolio details
sql_string <- readQuery("D:\\Data\\data_processing\\SQL_Scripts\\Falcon\\positions.sql")
sql_string <- gsub(":pf",portfolios, sql_string)
res  <- dbSendQuery(con, sql_string)
positionsDetails <- fetch(res)
#})

#--------------------------------------------------------------------------------------------
## load instrument data from PSP

myOpts <- curlOptions(  
                        postfields=jsonlite::toJSON(isins, pretty=TRUE) 
                        ) 

url <- 'http://dickeberta/falconfund/index.js'
h = basicTextGatherer()
a = curlPerform(url = url, writefunction = h$update, noprogress=FALSE, .opts = myOpts)
instrumentDetails <- jsonlite::fromJSON(h$value())

#TODO currently excluding all large initial investment share classses
instrumentDetails <- instrumentDetails[instrumentDetails$initialInvestment<500001 | is.na(instrumentDetails$initialInvestment) ,]

# TODO calculate score within same grouped_df_impl
instrumentDetails$chargeScore <- round((rank(with(instrumentDetails, 10-log10(charge*initialInvestment)))/nrow(instrumentDetails))*10)-1

#Add no context error instrument TODO only use if needed
#instrumentDetails[nrow(instrumentDetails)+1,] = c("nocontextfundid", "Context", "NOBmoCo", "CHF", "NA", "NA", "Error","","","","","","","","","")

#--------------------------------------------------------------------------------------------
## perform BMO call
# all relevant bmo contexts
# TODO alem, 3.4.2018: I am not able to query iap through a technical user, therefore cant get the regulation
bmoServiceINCTX <- list() # CTX is Client Domicile, Booking Center, RM Location
for(i in 1:nrow(uniqeBMO)) {
  row <- uniqeBMO[i,]
  bmo <- list(bookingCentreCode=row$BC, identifier=row$CTX, mandateOwnerCountryCode=row$CD, relationshipManagerCountryCode=row$RM, serviceModel="14",partner = list(clientClassifications=list(), domicileCountryCode=row$CD, isGBRnd=TRUE), solicitationType="Solicited")
  
  bmo$partner$clientClassifications[[length(bmo$partner$clientClassifications)+1]] <- list(regulation="CH_KAG", type='RetailClient') #row$EU_MiFID)
  bmo$partner$clientClassifications[[length(bmo$partner$clientClassifications)+1]] <- list(regulation="EU_MiFID", type='RetailClient') #type=row$EU_MiFID)
  #bmo$partner$clientClassifications[[length(bmo$partner$clientClassifications)+1]] <- list(regulation="HK_SFO", type=row$HK_SFO)
  #bmo$partner$clientClassifications[[length(bmo$partner$clientClassifications)+1]] <- list(regulation="SG_SFA", type=row$SG_SFA)
  bmoServiceINCTX[[length(bmoServiceINCTX)+1]] <- bmo
}

instrument_source <- strsplit(isins, ",")[[1]]
bmoServiceINInst <- list()
for (i in 1:length(instrument_source)) {
  row <- instrument_source[i]
  inst <- list(identifications= list(list(identifier=row, type='ISIN')))
  bmoServiceINInst[[length(bmoServiceINInst)+1]] <- inst
}

bmoIN <- list(contexts=bmoServiceINCTX, financialInstruments=bmoServiceINInst, ignoreEmptyCountryCodes=FALSE, messageTypeFilter=list())

#output the assembled bmo input
#jsonlite::toJSON(bmo, pretty=TRUE, auto_unbox=TRUE)

myOpts <- curlOptions(  httpheader=c(Authorization='Basic REVGQVVMVFx3ZWJzZXJ2aWNlLWV4ZWN1dG9yOjk4NTI3NCM2OF9M', 
                                     Accept="application/json",
                                     'Content-Type'="application/json; charset=utf-8"),
                        postfields=jsonlite::toJSON(bmoIN, pretty=TRUE, auto_unbox=TRUE), 
                        
                        ssl.verifyhost=FALSE, 
                        ssl.verifypeer=FALSE, followlocation=TRUE) 

url <- 'https://brs-be-exec.juliusbaer.com/executionserver/rest/1/ruleServices/BmoWebServices/versions/LATESTVERSION/rules/BmoWebServices/validateInstrumentOffering/executions'
h = basicTextGatherer()
a = curlPerform(url = url, writefunction = h$update, noprogress=FALSE, .opts = myOpts)
#read bmo output
bmoOUT <- jsonlite::fromJSON(h$value())
#♪jsonlite::toJSON(bmoOUT, pretty=TRUE, auto_unbox=TRUE)
#--------------------------------------------------------------------------------------------
## instrument Knock Out Rules
## Assemble valid instruments for each bmo context
bmoInstruments <- data.frame()
instrumentLen <- length(bmoOUT$response$contextIdentifier)
for (i in 1:instrumentLen) {
  
  CTX <- bmoOUT$response$contextIdentifier[[i]]
  ISIN <- bmoOUT$response$financialInstrument$identifications[[i]]$identifier[[1]]
  msgLen <- length(bmoOUT$response$messageIds[[i]])
  for (x in 1:msgLen) {
    if (length(bmoOUT$response$messageIds[[i]]) >0) {
      msgId <- bmoOUT$response$messageIds[[i]][[x]]
      col <- cbind(CTX,ISIN,msgId)
      #TODO fix context relevant msgs
      if (is.null(ISIN)) {
        ISIN <- 'Context'
        col <- cbind(CTX,ISIN,msgId)
      } else {
        colLen <- length(col)
        if(colLen == 3) { # removes non valid responses
          bmoInstruments <- rbind(bmoInstruments,col)  
        }  
      }
    } else {
      msgId <- 'nomsg'
      col <- cbind(CTX,ISIN,msgId)
      bmoInstruments <- rbind(bmoInstruments,col) 
    }
  }
}

messages <-  bmoOUT$messages[bmoOUT$messages$severity == 'Alert',c('id','severity', 'level')]
messages <- plyr::rename(messages, c("id"="msgId"))
#bmoInstruments$level <- 'Context'

#combine instrument with messages, the instruments have no alert will have an NA in the severity column
#bmoInstrumentValidity <- data.frame()
bmoInstrumentValidity <- merge(bmoInstruments, messages, by = "msgId", all.x=TRUE)


# filter out the valid instruments and reduce to unique combinations
invalidContext <- bmoInstrumentValidity[bmoInstrumentValidity$level=='Context',]
bmoInstrumentValidity <- bmoInstrumentValidity[ !bmoInstrumentValidity$CTX %in% invalidContext$CTX,]
bmoInstrumentValidity <- bmoInstrumentValidity[!is.na(bmoInstrumentValidity$severity),]
bmoInstrumentValidity <- unique(bmoInstrumentValidity[,c('CTX', 'ISIN', 'severity')]) 
invalidSpanish <- instrumentDetails[instrumentDetails$traspaso == 'No',]
invalidSpanish$CTX <- 'ESLUES'
invalidSpanish$severity <- 'Alert'

invalidSpanish <- unique(invalidSpanish[,c("CTX", "isin", "severity")])
invalidSpanish <- plyr::rename(invalidSpanish, c("isin"="ISIN"))
bmoInstrumentValidity <- rbind(bmoInstrumentValidity, invalidSpanish)
#All relevant BMO/Instrument combinations
bmoInstruments <- unique(bmoInstruments[,c("CTX", "ISIN")])
# BMO result
bmoResultat <- merge(bmoInstruments, bmoInstrumentValidity, all.x=TRUE)
bmoResultat <- bmoResultat[ !bmoResultat$CTX %in% invalidContext$CTX,]
bmoResultat <- bmoResultat[is.na(bmoResultat$severity),]

#--------------------------------------------------------------------------------------------
# Add valid Instruments to portfolio
# we filter the results from the checke engine according to inducement loaded for BC CH, Guerensy and LUX accrodingly:
bmoTemp <- merge(bmoResultat,instrumentDetails,by.x= "ISIN", by.y = "isin")
bmoResultat <- subset(bmoTemp, 
                      (instrumentType == 'Error') |
                      (instrumentType == 'Equity') |
                      
                    # BC Switzerland 
                  (substr(CTX,3,6) == "CHGB" & inducementloaded == "No") | 
                        ((substr(CTX,3,4) == "CH" & (substr(CTX,5,6) != "GB" & inducementloaded == "Yes") )
                   )
                  | #or for BC GE
                    (substr(CTX,3,6) == "GGGB" & inducementloaded == "No") | 
                    ((substr(CTX,3,4) == "GG" & (substr(CTX,5,6) != "GB" & inducementloaded == "Yes") )
                    )
                  | #or for BC LUX
                  (substr(CTX,3,6) == "LUES" & inducementloaded == "Yes") |
                      ((substr(CTX,3,4) == "LU" & (substr(CTX,5,6) != "ES" & inducementloaded == "No") ) 
                   )
              , select = c("CTX","ISIN","severity")
              )
bmoResultat <- merge(bmoResultat, portfolioDetails, by = "CTX")
#-------------------------------------------------------------------------------------------
# Calculate instrument ranking / per client knockout rules 
bmoOutput <- list()
uniqueBmoResultat <- unique(bmoResultat[,c('CD','BC', 'RM','CTX', 'COUNTRY_NAME', 'SERVICE_MODEL', 'SERVICE_MODEL_CODE', 'PORTFOLIOID','CCY', 'RM_NAME','EMAIL', 'OFFICE_PHONE_NUMBER', 'MOBILE_PHONE_NUMBER', 'AUM', 'RM_LOC', 'RM_TEAM', 'RM_TEAM_NAME')])
## till here its working
for (i in 1: nrow(uniqueBmoResultat)) {
  
  row <- uniqueBmoResultat[i,]
  uniqueResult <- list(BC = row$BC, CD=row$CD, RM=row$RM, CDName=row$COUNTRY_NAME,ServiceModel=row$SERVICE_MODEL,
                       ServiceModelcODE=row$SERVICE_MODEL_CODE,RM_name =row$RM_NAME, portfolio=row$PORTFOLIOID,
                       ccy=row$CCY, email=row$EMAIL, phone=row$OFFICE_PHONE_NUMBER, mobile=row$MOBILE_PHONE_NUMBER,
                       aum= row$AUM, type='investment', initative=initative, isins=
                              
                         lapply(unique(bmoResultat[bmoResultat$CTX == row$CTX,c('ISIN')]), function(frow) {
                                              
                                            #instrumentDetails[instrumentDetails$isin==frow,]
                                            #testinst<- instrumentDetails[instrumentDetails$isin=='IE00BD3B6H93',]
                                            #testuser <- uniqueBmoResultat[i,]
                                            
                                            return (perClientInstrumentRanking(uniqueBmoResultat[i,],instrumentDetails[instrumentDetails$isin==frow,]))
                                          }), RM_LOC=row$RM_LOC, RM_TEAM=row$RM_TEAM,RM_TEAM_NAME=row$RM_TEAM_NAME
                         )
  bmoOutput[[length(bmoOutput) + 1]] <- uniqueResult
}

# sort instruments by score and only show top 3
resultat <- list()
for (pf in 1:length(bmoOutput)) {
  #pf <- 1
  sortedList <- data.frame()
  if (length(bmoOutput[[pf]])>0) {
    if (length(bmoOutput[[pf]]$isins)>0) {
      for (i in 1:length(bmoOutput[[pf]]$isins)){
        #i <- 1
        row <- bmoOutput[[pf]]$isins[[i]]
        if (length(row$instrumentType)>0) {
          
          #--ptf risk ?
          
          if(row$instrumentType == "Fund") {
            #check for holding
            if (nrow(positionsDetails[ !is.na(positionsDetails$FUNDID) & positionsDetails$FUNDID==row$fundId & positionsDetails$PORTFOLIOID == bmoOutput[[pf]]$portfolio ,]) == 0) {
              sortedList <- rbind(sortedList,row)        
            }  
          } else if (row$instrumentType == "Equity") {
            #check for holding
            if (nrow(positionsDetails[ !is.na(positionsDetails$ISIN) & positionsDetails$ISIN==row$isin & positionsDetails$PORTFOLIOID == bmoOutput[[pf]]$portfolio ,]) == 0)  {
              sortedList <- rbind(sortedList,row)        
            }  
          }
        }
      }
    }
  }
  pfFunds <- sortedList[sortedList$instrumentType=='Fund',]
  pfEquities <- sortedList[sortedList$instrumentType=='Equity',]
  #for each fundID only give back the best (slice(1))
  topFunds <- data.frame()
  if (length(pfFunds)>0) {
    topFunds <- data.frame(pfFunds %>% group_by(fundId) %>% arrange(desc(Score)) %>% slice(1))  
  }
  topEquities <- data.frame()
  if (length(pfEquities)>0) {
    topEquities <- head(pfEquities[order(-pfEquities$Score),], 3)
  }
  
  
  #bmoOutput[[pf]]$isins <- rbind(head(pfFunds[order(-pfFunds$Score) ,], 1),head(pfEquities[order(-pfEquities$Score),], 3))
  allInstruments <- rbind(topFunds,topEquities)
  
  if (length(allInstruments)>0){
    bmoOutput[[pf]]$isins <- allInstruments
    resultat[[length(resultat) + 1]] <- bmoOutput[[pf]]
    
  }
}

#-------------------------------------------------------------------------------------------
# SEND RESPONSE
if (type == "csv") {
  dfs <- lapply(resultat, data.frame, stringsAsFactors = FALSE)
  flat_table <- rbindlist(dfs)
  #split again to individual ptfs
  #ptf <- portfolios
  #ptf <- gsub("'","",ptf)
  #ptf <- unlist(strsplit(ptf,",") )
  
  #portfolio not in resultat
  #ptf_no_offering <- portfolioDetails[ !(ptf %in% flat_table$portfolio),]
  #iterate through ptf with no offerings and add them to output
  #for (i in 1:length(ptf_no_offering[,1]))
  #{
    #reuse code above
   # row <- ptf_no_offering[i,]
    #tmp <- c(BC = row$BC, CD=row$CD, RM=row$RM, CDName=row$COUNTRY_NAME,ServiceModel=row$SERVICE_MODEL,
    #                     ServiceModelcODE=row$SERVICE_MODEL_CODE,RM_name =row$RM_NAME, portfolio=row$PORTFOLIOID,
    #                     ccy=row$CCY, email=row$EMAIL, phone=row$OFFICE_PHONE_NUMBER, mobile=row$MOBILE_PHONE_NUMBER,
    #                     aum= row$AUM, type='investment', initative=initative)
    #fill the 17 columns
    #tmpnames <- c(names(instrumentDetails),"Score")
    #null_vec <- rep("no offering",17)
    #names(null_vec) <-  paste("isins.", tmpnames , sep = "")
    #flat_table <- rbind(flat_table,t(data.frame(c(tmp,null_vec))))
  #}
  
  print(write.csv2(flat_table))
}
if (type == "json") {
print(jsonlite::toJSON(resultat,pretty=TRUE, auto_unbox=TRUE))
}
