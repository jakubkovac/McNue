library(tidyverse)
library(rvest)
library(lubridate)
library(pander)
library(Zbot)

#run web scraper
R.utils::sourceDirectory("utility_functions",encoding = "UTF-8")
restaurant_fun_list <- map(list.files("Restaurants", full.names = TRUE), ~source(.x, encoding = "UTF-8"))
restaurant_fun_list <- map(restaurant_fun_list, ~.x[[1]])

menu <- map(restaurant_fun_list, ~tryCatch(.x(), error = function(e){NULL})) %>% compact()
menu <- map(menu, same_length)

menu <- Reduce(rbind, menu) %>% as.data.frame() %>% as_tibble()
colnames(menu) <- c("podnik", "polievka", "jedlo_1", "jedlo_2", "jedlo_3", "jedlo_4")

menu <- menu[-which(rowSums(is.na(menu)) == 5), ]

original_menu <- menu
menu <- menu %>% transmute_all(~replace_na(.,"")) # same as transmute_all(function(x) replace_na(x,""))


menu

menu <- 
  menu %>% mutate_all(.fun = remove_g_l)

menu <-
  menu %>%
  mutate_all(.fun = str_remove_1gl)

#remove special slovak characters
menu <-
  menu %>%
  mutate_all(.fun = slovak_language_destroyer)

#destroy the nuances in data
menu <-
  menu %>%
  mutate_all(.fun = benson_string_destroyer)

menu <-
  menu %>%
  mutate_all(.fun =  str_to_1up)

menu <- menu %>% arrange(podnik)
#save the menu in a text file as an ascii table and in csv
menu_ascii <- pandoc.table.return(menu, style = "grid", split.tables = Inf, split.cells = 30) %>% str_sub(3) # the last thing removes the first 2 /n
menu_ascii
write.table(menu_ascii,file = "menu.txt", row.names = F, col.names = F, quote = F)
# menu2_ascii <- pandoc.table.return(menu2, style = "grid", split.tables = Inf, split.cells = 30)
# write.table(menu2_ascii,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
beep <- readChar("beep_boop.txt",file.info("beep_boop.txt")$size)
write.table(beep,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
print(menu)
###############################
Zbot::send_teams_card(Zbot::teams_card_generator(title = "LunchBOT",
                                                 subtitle = lubridate::today(),
                                                 text = beep,
                                                 df = menu),
                      readLines("data/myhook.txt"))
menu$date <- lubridate::today()
menu <- select(menu, date, everything())
readr::write_csv(menu, "data/lunch_menu.csv", append = TRUE)

