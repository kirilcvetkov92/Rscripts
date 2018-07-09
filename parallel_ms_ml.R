rm(list=ls(all=TRUE))
library(ROracle)
library(foreach)
library(doParallel)

readQuery<-function(file) paste(readLines(file), collapse="\n") 
#sql_strings <- list("Select ZR from BL_ADV_DS.IPP_DATA",
#                 "Select ZR from BL_ADV_DS.MASTER_DATA")
portfolios <- "'000000017530201','000000072500201','000000078000201','000000120620201','000000176270201','000001510940201','000001535830201','000001696630201','000003658530201','000004777020201','000010111870201','000021308200201','000021866070201','000021895050201','000021944270201','000026011200201','000030052470201','000030096910201','000030100600201','000030177100202','000030202050201','000030217630202','000030230620201','000030234710201','000030255990201','000030264140201','000030267500201','000030278760201','000030322960201','000030375570201','000030376880201','000030380680201','000030386000201','000030422000201','000030446150201','000030467200201','000030470710201','000030489350201','000030503550201','000030556070201','000030640900201','000030659500201','000030768190201','000030794420201','000030886790201','000030897940201','000030958160201','000030982970201','000030989180201','000031002190201','000031008390201','000031020600201','000031027520201','000031028320201','000031103160201','000031165030201','000031182100201','000031197500202','000031237200201','000031250210201','000031277330201','000031277420201','000031288910201','000031299080201','000031309340201','000031325820201','000031341820201','000031352960201','000031353020201','000031354070201','000031369240201','000031401010201','000031404710201','000031411080201','000031471730201','000031483350201','000031490520201','000031494810201','000031495040201','000031495360201','000031546740201','000031550290201','000031562300201','000031589350201','000031607900201','000057300860201','000057303670201','000057303930201','000057304060201','000070625020201','000070625030201','000070626170201','000072642480201','000080093680201','000130002990201','000130003040201','000130003390201','000130006880201','000236944460201','000647126820201','000701315830203','000702067500201','000904285330201'"

sql_strings <- list(readQuery("D:\\Data\\data_processing\\SQL_Scripts\\Falcon\\portfolio_details.sql"),
                    readQuery("D:\\Data\\data_processing\\SQL_Scripts\\Falcon\\positions.sql"),
                    readQuery("D:\\Data\\data_processing\\SQL_Scripts\\Falcon\\distinct_bmo.sql"))

sql_strings <- gsub(":pf",portfolios, sql_strings)

cl<-makeCluster(2)
registerDoParallel(cl)
system.time({
oper <- foreach(sql = sql_strings, 
        .packages = "ROracle",
        .verbose = TRUE
       ) %dopar%  {
         # Create the connection string
         drv <- dbDriver("Oracle")
         host <- "PPC1"
         port <- 1702
         sid <- "PPC1"
         connect.string <- paste(
           "(DESCRIPTION=",
           "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
           "(CONNECT_DATA=(SID=", sid, ")))", sep = "")
         con <- dbConnect(drv, username = "BL_DM_ISG_CONS",
                          password = "wdck1876",dbname=connect.string)   
         
         res <- dbSendQuery(con,sql)
         
    data <- fetch(res)
    dbDisconnect(con)
    return(data)
  }

stopCluster(cl)
})


#system.time({
#  # Do something that takes time
#  data <- fetch(res_list[[2]])
#})

#res_list <- list()
#for (sql in sql_strings) {
#  res_list <- append(dbSendQuery(con,sql),res_list)
#}
#names(res_list) <- "res"
# test <- list(dbSendQuery(con,sql_strings[[1]]),dbSendQuery(con,sql_strings[[2]]))
# 
# system.time({ 
#   cat('MKL Threads=',getMKLthreads(),'\n')  
#   rxSetComputeContext("localpar") 
#   bla <- rxExec(fetch, elemArgs = list(res_list[1],res_list[2]))
#   #bla <- rxExec(fetch, res = rxElemArg(res_list[2]))
# })

























































