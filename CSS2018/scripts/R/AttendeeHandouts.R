###############################################################################
# FILE: AttendeeHandouts.R
# DESC: Create customized handouts for each attendee, including Server IP
#       and the information needed for node creation during the exercises
# SRC : https://www.r-bloggers.com/mail-merge-with-rmarkdown/
# IN  : /data/ClassInfo.xlsx  - attendee names, server IP, etc. 
#       AttendeeHandouts.Rmd
# OUT : .PDF for each attendee, by Name.
# REQ : 
# SRC : 
# NOTE: 
# TODO: 
###############################################################################
library(knitr)
library(rmarkdown)
library(readxl)

setwd('C:/_gitHub/LinkedDataWorkshop/CSS2018')
servers<-read_excel("data/ClassInfo.xlsx", sheet = "Servers", col_names=TRUE)


# NOTES: Both sources have the same column name on which to merge: studyID
#         read.xlsx(file, sheetIndex, sheetName=NULL, rowIndex=NULL,
#                  startRow=NULL, endRow=NULL, colIndex=NULL,
#                  as.data.frame=TRUE, header=TRUE, colClasses=NA,
#                  keepFormulas=FALSE, encoding="unknown", ...)
###############################################################################
# intersect(names(phase), names(ind))),

sourceFile1  <- "C:/_github/r/CodeEg/filesAndIO/MergeTwoXLSX-Phase.xlsx"
sourceFile2  <- "C:/_sandbox/r/CodeEg/filesAndIO/MergeTwoXLSX-Ind.xlsx"

# Read in the files and add a source element to id the source dataframe.
phaseDf<-read.xlsx(sourceFile1,header=TRUE, sheetName="Sheet1")

 
## Data
personalized_info <- read.csv(file = "meeting_times.csv")
 
## Loop
for (i in 1:nrow(personalized_info)){
 rmarkdown::render(input = "mail_merge_handout.Rmd",
 output_format = "pdf_document",
 output_file = paste("handout_", i, ".pdf", sep=''),
 output_dir = "handouts/")
}
