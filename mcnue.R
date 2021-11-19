library(tidyverse)
library(rvest)
library(lubridate)
library(pander)
library(Zbot)
library(magick)
#require(pdftools)
#require(tesseract)

setwd("~/Moje/Mcnue")
#run web scraper
R.utils::sourceDirectory("utility_functions",encoding = "UTF-8")
restaurant_fun_list <- map(list.files("Restaurants", full.names = TRUE), ~source(.x, encoding = "UTF-8"))
restaurant_fun_list <- map(restaurant_fun_list, ~.x[[1]])

menu <- map(restaurant_fun_list, ~tryCatch(.x(), error = function(e){NULL}))

warning("Restaurants that failed: ","\n\n",
        paste0(str_sub(list.files("Restaurants"), end =-3)[map_lgl(menu, is.null)], collapse = "\n"))


menu <- map(compact(menu), same_length)
Sys.setlocale("LC_CTYPE", locale="Slovak")
menu <- Reduce(rbind, menu) %>% as.data.frame() %>% as_tibble()
colnames(menu) <- c("podnik", "polievka", "jedlo_1", "jedlo_2", "jedlo_3", "jedlo_4")

menu <- menu[-which(rowSums(is.na(menu)) == 5), ]

original_menu <- menu
menu <- menu %>% transmute_all(~replace_na(.,"")) # same as transmute_all(function(x) replace_na(x,""))

menu <- 
  menu %>% mutate_all(.fun = remove_g_l)

menu <-
  menu %>%
  mutate_all(.fun = str_remove_1gl)

#remove special slovak characters
# menu <-
#   menu %>%
#   mutate_all(.fun = slovak_language_destroyer)

#destroy the nuances in data
menu <-
  menu %>%
  mutate_all(.fun = benson_string_destroyer)

menu <-
  menu %>%
  mutate_all(.fun =  str_to_1up)

menu <- menu %>% arrange(podnik)
source("jedlo_dna.R")
purrr::walk(menu, ~{cat("---------------------------\n");print(.x)})

# {
#   p <- readline(prompt="Send lunch menu to Teams?[y/n]: " )
#   if(tolower(p) == "y"){
tt <- lubridate::today()

if(!(tt %in% unique(readr::read_csv("data/lunch_menu.csv", col_select = 1, show_col_types = FALSE)[[1]]))){
  hook <- readLines("data/myhook.txt")
  #hook <- readLines("data/testhook.txt")
  Zbot::send_teams_card(Zbot::teams_card_generator(title = "LunchBOT",
                                                   subtitle = paste(tt, format(tt, "%A")),
                                                   text = "",
                                                   df = menu),
                        hook)
  menu$date <- tt
  menu <- select(menu, date, everything())
  readr::write_csv(menu, "data/lunch_menu.csv", append = TRUE)
}

#   }
# }
###############################


