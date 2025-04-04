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
            
bad.people.id <- (comprehension.questions.accuracy %>%
  filter(accuracy<0.8))$id

# Self-paced Results
selfpaced <- raw %>%
  filter(!id %in% c("xmzhu", "dfsdfsdfsdfsdf")) %>%
  filter(!id %in% bad.people.id) %>%
  filter(PennElementType == "Controller-DashedSentence" & Label=="trials" & condition %in% c("NonTeleThe", "NonTeleEvery", "TeleEach", "NonTeleEach")) %>%
  filter(!(condition=="NonTeleEvery"&frame_id==18)) %>%
  mutate(
         Parameter=as.numeric(Parameter),
         Reading.time=as.numeric(Reading.time)) %>%
  mutate(region = case_when(condition %in% c("TeleEach", "NonTeleThe") & Parameter %in% c(1,2,3) ~ "PP_1",
                            condition %in% c("TeleEach", "NonTeleThe") & Parameter == 4 ~ "Quantifier",
                            condition %in% c("TeleEach", "NonTeleThe") & Parameter == 5 ~ "Antecedent",
                            condition %in% c("TeleEach", "NonTeleThe") & Parameter %in% c(6,7,8) ~ "VP",
                            condition %in% c("TeleEach", "NonTeleThe") & Parameter == 9 ~ "Pronoun",
                            condition %in% c("TeleEach", "NonTeleThe") & Parameter == 10 ~ "V_cont",
                            condition %in% c("TeleEach", "NonTeleThe") & Parameter %in% c(11,12) ~ "NP",
                            condition %in% c("TeleEach", "NonTeleThe") & Parameter %in% c(13,14,15) ~ "PP_2",
                            condition %in% c("NonTeleEvery", "NonTeleEach") & Parameter %in% c(1,2,3) ~ "PP_1",
                            condition %in% c("NonTeleEvery", "NonTeleEach") & Parameter == 4 ~ "Quantifier",
                            condition %in% c("NonTeleEvery", "NonTeleEach") & Parameter == 5 ~ "Antecedent",
                            condition %in% c("NonTeleEvery", "NonTeleEach") & Parameter %in% c(6,7,8) ~ "VP",
                            condition %in% c("NonTeleEvery", "NonTeleEach") & Parameter == 9 ~ "and",
                            condition %in% c("NonTeleEvery", "NonTeleEach") & Parameter == 10 ~ "Pronoun",
                            condition %in% c("NonTeleEvery", "NonTeleEach") & Parameter == 11 ~ "V_cont",
                            condition %in% c("NonTeleEvery", "NonTeleEach") & Parameter %in% c(12,13) ~ "NP",
                            condition %in% c("NonTeleEvery", "NonTeleEach") & Parameter %in% c(14,15,16) ~ "PP_2",
                            ))


# Results by Condition
nontelethe <- selfpaced %>% filter(condition=="NonTeleThe")
nonteleEvery <- selfpaced %>% filter(condition=="NonTeleEvery") 
teleeach <- selfpaced %>% filter(condition=="TeleEach")
nonteleeach <- selfpaced %>% filter(condition=="NonTeleEach")

nonteleEveryexamine <- nonteleEvery %>% group_by(MD5.hash.of.participant.s.IP.address) %>% summarize(length(MD5.hash.of.participant.s.IP.address))


plt.rt <- function (df) {
  plt <- df %>% group_by(Parameter) %>%
    summarize(mean_reading_time=mean(Reading.time)) %>%
    ggplot(aes(x=Parameter, y=mean_reading_time)) + 
    geom_line(color = "blue", linewidth=1) + 
    geom_point(color = "red") + labs(title="Average Reading Time per Word Position",
                                     x="position",
                                     y="Mean reading time") + theme_minimal()
  return (plt)
}
nontelethe.plt <- plt.rt(nontelethe)
nonteleEvery.plt <- plt.rt(nonteleEvery)
teleeach.plt <- plt.rt(teleeach)
nonteleeach.plt <- plt.rt(nonteleeach)

ggsave("nontelethe.pdf", nontelethe.plt, width=8, height=3)
ggsave("nonteleEvery.pdf", nonteleEvery.plt, width=8, height=3)
ggsave("teleeach.pdf", teleeach.plt, width=8, height=3)
ggsave("nonteleEach.pdf", nonteleeach.plt, width=8, height=3)


# Plot
teleeach_vs_nontelethe <- selfpaced %>% 
  filter(condition %in% c("TeleEach", "NonTeleThe")) %>%
  mutate(region=factor(region, levels=c("PP_1",
                                        "Quantifier",
                                        "Antecedent",
                                        "VP",
                                        "Pronoun",
                                        "V_cont",
                                        "NP",
                                        "PP_2"))) %>%
  group_by(region, condition) %>%
  summarize(mean_reading_time=mean(Reading.time)) %>%
  ggplot(aes(x=region, y=mean_reading_time, color=condition, group=condition)) + 
  geom_line( linewidth=1) + 
  geom_point() + labs(title="TeleEach vs. NonTeleThe",
                                   x="position",
                                   y="Mean reading time") +  theme_minimal() +  theme(legend.position = "bottom")
ggsave("teleeach_vs_nontelethe.pdf", teleeach_vs_nontelethe, width=8, height=3, dpi=300)

## Plot2
nonteleevery_vs_nonteleeach <- selfpaced %>% 
  filter(condition %in% c("NonTeleEvery", "NonTeleEach")) %>%
  mutate(region=factor(region, levels=c("PP_1",
                                        "Quantifier",
                                        "Antecedent",
                                        "VP",
                                        "and",
                                        "Pronoun",
                                        "V_cont",
                                        "NP",
                                        "PP_2"))) %>%
  group_by(region, condition) %>%
  summarize(mean_reading_time=mean(Reading.time)) %>%
  ggplot(aes(x=region, y=mean_reading_time, color=condition, group=condition)) + 
  geom_line( linewidth=1) + 
  geom_point() + labs(title="Each vs. Every",
                      x="position",
                      y="Mean reading time") +  theme_minimal() +  theme(legend.position = "bottom")
ggsave("nonteleevery_vs_nonteleeach.pdf", nonteleevery_vs_nonteleeach, width=8, height=3, dpi=300)

# Everything everywhere
everything <- selfpaced %>% 
  filter(region != "and") %>% 
  filter(condition != "NonTeleEvery") %>%
  mutate(region=factor(region, levels=c("PP_1",
                                        "Quantifier",
                                        "Antecedent",
                                        "VP",
                                        "Pronoun",
                                        "V_cont",
                                        "NP",
                                        "PP_2"))) %>%
  group_by(region, condition) %>%
  summarize(mean_reading_time=mean(Reading.time)) %>%
  ggplot(aes(x=region, y=mean_reading_time, color=condition, group=condition)) + 
  geom_line( linewidth=1) + 
  geom_point() + labs(title="Telescoping vs. Non-telescoping",
                      x="position",
                      y="Mean reading time") +  theme_minimal() +  theme(legend.position = "bottom")
ggsave("Telescoping vs. Non-telescoping.pdf", everything, width=8, height=3, dpi=300)
