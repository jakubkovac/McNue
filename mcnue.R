library(tidyverse)
library(rvest)
library(lubridate)
library(pander)

#run web scraper
R.utils::sourceDirectory("Restaurants")

str_to_1up <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

str_remove_1gl <- function(x) {
  substr(x,1,1)[substr(x,1,1) == "l" | substr(x,1,1) == "g"] <- ""
  x
}

menu <- tibble(podnik = character(), polievka = character(), jedlo_1 = character(), jedlo_2 = character(), jedlo_3 = character(), jedlo_4 = character())
(menu[1,] <- bioland())
(menu[2,] <- bluebear())
(menu[3,] <- ceska())
(menu[4,] <- dilema())
(menu[5,] <- kasa(sme = TRUE))
(menu[6,] <- mestiansky())
(menu[7,] <- mnamka())
(menu[8,] <- veda())
(menu[9,] <- veglife())
(menu[10,] <- realstar())
(menu2 <- lenka())



#menu <- na.omit(menu)
#remove special slovak characters
source("slovak_language_destroyer.R",encoding="utf-8")
menu <-
  menu %>%
  mutate_all(.fun = slovak_language_destroyer)

menu2 <-
  menu2 %>%
  mutate_all(.fun = slovak_language_destroyer)

#destroy the nuances in data
source("benson_string_destroyer.R")
menu <-
  menu %>%
  mutate_all(.fun = benson_string_destroyer)
menu
menu[10,] <- menu[10,] %>% str_replace("  l","") %>% str_replace("  g","") %>% str_trim()

menu <-
  menu %>%
  mutate_all(.fun = str_remove_1gl)
#randomize order
#menu <- menu[sample(1:nrow(menu),nrow(menu)),]
menu <-
  menu %>%
  mutate_all(.fun =  str_to_1up)
menu <- menu %>% arrange(podnik)
menu2
#save the menu in a text file as an ascii table
menu_ascii <- pandoc.table.return(menu, style = "grid", split.tables = Inf, split.cells = 30)
menu_ascii
write.table(menu_ascii,file = "menu.txt", row.names = F, col.names = F, quote = F)

menu2_ascii <- pandoc.table.return(menu2, style = "grid", split.tables = Inf, split.cells = 30)
write.table(menu2_ascii,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
beep <- readChar("beep_boop.txt",file.info("beep_boop.txt")$size)
write.table(beep,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
