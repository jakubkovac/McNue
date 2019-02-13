library(tidyverse)
library(stringr)
library(rvest)
library(lubridate)
library(pander)

#run web scraper
source("C:\\Users\\jakub.kovac\\Documents\\LEARNING\\MOJE\\menucka\\mcnue.R")
menu <- mcnue()
View(menu)

#remove special slovak characters
source("C:\\Users\\jakub.kovac\\Documents\\LEARNING\\MOJE\\menucka\\slovak_language_destroyer.R",encoding="utf-8")
menu <-
  menu %>%
  mutate_all(.fun = function(x) slovak_language_destroyer(x))

#destroy the nuances in data
source("C:\\Users\\jakub.kovac\\Documents\\LEARNING\\MOJE\\menucka\\benson_string_destroyer.R")
menu <-
  menu %>%
  mutate_all(.fun = function(x) benson_string_destroyer(x))
View(menu)

#save the menu in a text file as an ascii table
menu_ascii <- pandoc.table.return(menu, style = "grid", split.tables = Inf, split.cells = 35)
menu_ascii
write.table(menu_ascii,file = "menu.txt", row.names = F, col.names = F, quote = F)
beep <- readChar("beep_boop.txt",file.info("beep_boop.txt")$size)
write.table("\n \n",file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
write.table(beep,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
