rm(list=ls(all=TRUE))
library(ROracle)

readQuery<-function(file) paste(readLines(file), collapse="\n") 
sql_string <- readQuery("./SQL_Scripts/ISG_Datamart/master.sql")
#bla
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

#--- data ------------------------------------------------
res  <- dbSendQuery(con, sql_string)
#-------------------------------------------------
system.time({
  # Do something that takes time
  data <- fetch(res)
})
newnames <- substr(names(data), start=1, stop=30)
names(data) <- newnames

#==============================================================================
#remove all duplicated columns...such as ZRNo, DPID ect
data <- data[,unique(names(data))]

library(ROracle)
drv <- dbDriver("Oracle")
# Create the connection string
host <- "PPC1"
port <- 1702
sid <- "PPC1"
connect.string <- paste(
  "(DESCRIPTION=",
  "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
  "(CONNECT_DATA=(SID=", sid, ")))", sep = "")
con_write <- dbConnect(drv, username = "",
                       password = "",dbname=connect.string)
#--- trx ------------------------------------------------
Sys.setenv(TZ = "GMT")
Sys.setenv(ORA_SDTZ = "GMT")
if (dbExistsTable(con_write, "MASTER_DATA" ,"BL_ADV_DS"))
  dbRemoveTable(con_write, "MASTER_DATA")
dbWriteTable(con_write, "MASTER_DATA", data)

sql <- "Grant select on MASTER_DATA to BL_DM_ISG_CONS"
dbSendQuery(con_write, sql)

#write.csv2(data,"master_data.xlsx")

















































