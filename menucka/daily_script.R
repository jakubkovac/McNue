library(tidyverse)
library(stringr)
library(rvest)
library(lubridate)
library(pander)
source("C:\\Users\\jakub.kovac\\Documents\\LEARNING\\MOJE\\menucka\\mcnue.R")
menu <- mcnue()
menu
menu_ascii <- pandoc.table.return(menu, style = "grid", split.tables = Inf, split.cells = 35)
menu_ascii
write.table(menu_ascii,file = "menu.txt", row.names = F, col.names = F, quote = F)
beep <- readChar("beep_boop.txt",file.info("beep_boop.txt")$size)
write.table("\n \n",file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
write.table(beep,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
