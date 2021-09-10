library("readxl")
library("ggplot2")
library("dplyr")
library("reshape2")

raw_data <- read_excel("~/Downloads/Mikes Experiment - Middle Ucayali/Results/Results - V2.xlsx", sheet = 1)
slide_info <- read_excel("~/Downloads/Mikes Experiment - Middle Ucayali/Results/Results - V2.xlsx", sheet = 2)
targets <- read_excel("~/Downloads/Mikes Experiment - Middle Ucayali/Results/Results - V2.xlsx", sheet = 3)

df <- raw_data
df[,7:12] <- NA

for (row in 1:nrow(df)){
  for (col in 7:ncol(df)){
    slide <- names(df)[col]
    pres_order <- df$order[row]
    target <- targets$Code[targets$Order == pres_order & targets$Slide == slide]
    
    answer <- raw_data[row, col]
    
    if (answer == target){
      df[row, col] <- 1
    } else {
      df[row, col] <- 0
    }
  }
  
}

df$test <- (df$ojitas + df$maloca + df$nino + df$mototaxi)/4
df$control <- (df$sacha_vaca + df$mariposa)/2

test_df <- df[,c("age", "control","test")]
test_df2 <- melt(data = test_df, id.vars = "age", variable.name = "average")

ggplot(test_df2, aes(x = factor(age), colour = factor(average), y = value)) + geom_jitter(width = 0.1, height = 0.1) + xlab("Age (in years)") + ylab("Proportion correct") + labs(colour = "Trial Type") + theme_bw()

stats <- df %>% group_by(age) %>% summarise(test = mean(test), control = mean(control))
View(stats)

cor.test(df$age, df$test, "two.sided", "pearson")