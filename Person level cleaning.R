##Brian Redline 
# LML Person-level data management practice
# Starting w/ last full BL-SNi dataset (to be updated for starting from v beginning)

library(tidyverse)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(haven)


generate_version1(filename)
function for v1{
read a csv
process csv
write csv as rdata file
}
generate_version2(version1,filenamev2)
function for v2{
  adapter from v1 to v2
  read csv for v2
  process csv for v2
  merge v1 v2
  write csv as rdata file
}

raw_baseline <- read_csv()

filtered_baseline <- raw_baseline %>%
  filter(id != "abc") %>% # equivalent to keep in stata
  filter(id != "def") %>%
  select(abc = A,B,def = C) %>% # use = to rename on the fly, select keeps
  select(-B) %>% # equivalent to keep or drop for columns, but can also rename
  rename(xyz = abc) # difference between rename and select is taht rename keeps all vars

big <- read_csv(datadictionary.csv)
varlabels <- big$colname
names(filtered_baseline) <- varlabels
