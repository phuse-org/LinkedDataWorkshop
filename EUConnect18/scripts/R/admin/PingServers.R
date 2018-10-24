###############################################################################
# FILE: PingServers.R
# DESC: Confirm workshop servers are up. 
# IN  : ClassInfo.xlsx - has IP address of each study for each attendee
#       ClassInfo.xlsx - Development version for testing
# OUT : R console 
# REQ : 
# NOTE: 
# TODO: 
###############################################################################
library(readxl)
library(pingr)

servers <- as.data.frame(read_excel("C:/_gitHub/LinkedDataWorkshop/EUConnect18/data/admin/CourseInfo.xlsx", sheet = "Servers", col_names=TRUE))

for (i in 1:nrow(servers)){
    if (!is.na(servers[i,"ip"]))
    {  
      ip <- gsub(" ", "",servers[i,"ip"])
      pingResult<-ping(ip, count = 1, timeout = 1)
      if (!is.na(pingResult)){
        status<-"Pass"
      }  else {
        status<-"!----FAIL----!"
      }
      summary <- paste("Server" , i, " ", servers[i,2], ": ", status )
      print(summary)
    }  
}
