library(tidyverse)
library(rvest)
library(lubridate)
library(pander)

#run web scraper
source("C:\\Users\\jakub.kovac\\Documents\\LEARNING\\MOJE\\menucka\\mcnue.R")
list_menu <- mcnue()
menu <- list_menu[[1]]
menu2 <- list_menu[[2]]
View(menu)

#remove special slovak characters
source("C:\\Users\\jakub.kovac\\Documents\\LEARNING\\MOJE\\menucka\\slovak_language_destroyer.R",encoding="utf-8")
menu <-
  menu %>%
  mutate_all(.fun = function(x) slovak_language_destroyer(x))

menu2 <-
  menu2 %>%
  mutate_all(.fun = function(x) slovak_language_destroyer(x))

#destroy the nuances in data
source("C:\\Users\\jakub.kovac\\Documents\\LEARNING\\MOJE\\menucka\\benson_string_destroyer.R")
menu <-
  menu %>%
  mutate_all(.fun = function(x) benson_string_destroyer(x))
View(menu)

#save the menu in a text file as an ascii table
menu_ascii <- pandoc.table.return(menu, style = "grid", split.tables = Inf, split.cells = 30)
menu_ascii
write.table(menu_ascii,file = "menu.txt", row.names = F, col.names = F, quote = F)

menu2_ascii <- pandoc.table.return(menu2, style = "grid", split.tables = Inf, split.cells = 30)
write.table(menu2_ascii,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
beep <- readChar("beep_boop.txt",file.info("beep_boop.txt")$size)
write.table(beep,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
