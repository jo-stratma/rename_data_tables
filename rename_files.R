R
library(data.table)
library(dplyr)
library(plyr)
library(tidyverse)

devtools::source_url("file:///Users/Johannes/Desktop/Bioinformatics/scripts/core_commons.R")
rep = 'Rep1'
day = 'Day1'
sample='Sox9'
path = paste0('/Users/Johannes/Desktop/Allison_data/', rep,'/', day,'/',sample,'/')
setwd(path)


files <- list.files()
results <- vector(mode = "integer", length = length(files))
sphere_number <- vector(mode = "integer", length = length(files))

for(i in 1:length(files)) {
data <- read.csv(files[i])
files <- list.files()
data$sphere_number <- files[i]  %>% str_split(.,"_")  %>% as.data.frame() %>% .[8,1] %>%  str_split(.,"czi") %>% as.data.frame() %>% .[1,] %>% str_replace(., "Image", "sphere_")
results[i] <- data
results[[i]] <- data
}

combo <- results %>% ldply(., rbind)  %>% as_tibble
head(combo)

combo %>% rename_all( ~ paste(sample, ., sep = "_")) %>% head() 

rm(list=ls())
