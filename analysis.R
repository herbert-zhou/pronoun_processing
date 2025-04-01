library(tidyverse)
library(ggplot2)
library(lme4)

read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

# Read in results file
raw <- read.pcibex("results_prod.csv")

# Comprehension Accuracy
comprehension.questions.accuracy <- raw %>%
  filter(!id %in% c("xmzhu", "dfsdfsdfsdfsdf")) %>%
  filter(PennElementType == "Selector" & Label=="trials") %>%
  mutate(correct = ifelse((Value=="Yes" & correct_answer==1) | (Value=="No" & correct_answer ==0), 1, 0)) %>%
  group_by(id, version_id) %>%
  summarize(accuracy = mean(correct))

# Self-paced Results
self.paced.results <- raw %>%
  filter(!id %in% c("xmzhu", "dfsdfsdfsdfsdf")) %>%
  filter(PennElementType == "Controller-DashedSentence" & Label=="trials" & condition %in% c("NonTeleThe", "NonTeleEvery", "TeleEach", "NonTeleEach"))

# Plot