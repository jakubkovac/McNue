library(tidyverse)
library(rvest)
library(lubridate)
library(pander)

#run web scraper
R.utils::sourceDirectory("Restaurants",encoding = "UTF-8")
#lapply(list.files("Restaurants"),function(x) source(paste0("Restaurants/",x)))

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
(menu[5,] <- kasa(sme = T))
(menu[6,] <- mestiansky())
(menu[7,] <- mnamka())
(menu[8,] <- veda())
(menu[9,] <- veglife(sme = T))
(menu[10,] <- realstar())
(menu[11,] <- suvlaki())
(menu[12,] <- galileo())
#(menu[12,] <- lenka())
(menu[13,] <- svadbykari(sme = T)) # pondelok = F
(menu[14,] <- redcafe())
(menu[15,] <- hasic())
(menu[16,] <- alzbetka(sme = T))
(menu[17,] <- centralna_klubovna())
(menu[18,] <- prazsky_pub())
(menu[19,] <- rtvs())
(menu[20,] <- milton())


failed <- filter(menu,!complete.cases(menu)) %>% pull(podnik)
if(length(failed) > 0 | nrow(menu) < 21){
  message(paste("These restaurants failed:", paste(failed, collapse = ", ")))
} else message("All good.")
# 
# menu[11,2:6] <- c("Paradajková polievka",
#                   "Bravčový #GYROS s domácimi hranolkami, pita chlieb, #tzatziki dip",
#                   "Dusené bravčové mäso so sušenými paradajkami",
#                   "Zapečená Tortila plnená kuracím mäsom, červenou paprikou, jogurtom a syrom",
#                   "Vinný list plnený ryžou na grécky spôsob, #tzatziki dip")
# 
# menu[8,2:6] <- c("",
#                  "",
#                  "",
#                  "",
#                  ""
#               )

# menu[13,2:4] <- c("Japonská Miso polievka",
#                  "Thajské zelené VEGAN kari s cícerom, cuketou, mrkvou a zelenými fazuľkami, uhorkovo-hruškový šalát",
#                  "Mandľové voňavé kari s hovädzím mäsom, cviklou, brusnicami a zemiakmi, raita")

original_menu <- menu
menu <- menu %>% transmute_all(~replace_na(.,"")) # same as transmute_all(function(x) replace_na(x,""))


menu
remove_g_l <- function(x){
  x <- 
    x %>% 
    str_remove_all(paste0(c("120g","150g","200g", "240g", "250ml", "250g", "70g",
                            "300g","400g","0,33l","0.33l", "0.30 l", "0.33 l", "0.20 l", "0,25l",
                            "0,20 l", "140g", "320 g", "360 g", "400 g", "50ml", "50 ml", "350 g", "5 g", "0 g","50 g",
                            "0,30 l", "120 g", "250 g", "180 g", "150 g" , "300 g", "130 g", "5g", "0g"), collapse = "|")) %>%
    str_replace_all(" l ","") %>% 
    str_replace_all(" g "," ") %>% 
    str_replace_all("NA","") %>%
    str_replace_all(" ks","") %>%
    str_replace_all("  "," ") %>%
    str_replace_all(" ml "," ") %>%
    str_trim()
  y <- 
    x %>% 
    str_sub(end = 2) %>% 
    str_remove_all("g ") %>% 
    str_remove_all("l ")
  x <- paste0(y,str_sub(x,3))  
  return(x)
}
menu <- 
  menu %>% mutate_all(.fun = remove_g_l)

menu <-
  menu %>%
  mutate_all(.fun = str_remove_1gl)
#menu <- na.omit(menu)
#remove special slovak characters
source("slovak_language_destroyer.R",encoding="utf-8")
menu <-
  menu %>%
  mutate_all(.fun = slovak_language_destroyer)

# menu2 <-
#   menu2 %>%
#   mutate_all(.fun = slovak_language_destroyer)

#destroy the nuances in data
source("benson_string_destroyer.R")
menu <-
  menu %>%
  mutate_all(.fun = benson_string_destroyer)

# menu2 <-
#   menu2 %>%
#   mutate_all(.fun = benson_string_destroyer)
#randomize order
#menu <- menu[sample(1:nrow(menu),nrow(menu)),]
menu <-
  menu %>%
  mutate_all(.fun =  str_to_1up)
menu <- menu %>% arrange(podnik)
# menu2
#save the menu in a text file as an ascii table
menu_ascii <- pandoc.table.return(menu, style = "grid", split.tables = Inf, split.cells = 30) %>% str_sub(3) # the last thing removes the first 2 /n
menu_ascii
write.table(menu_ascii,file = "menu.txt", row.names = F, col.names = F, quote = F)

# menu2_ascii <- pandoc.table.return(menu2, style = "grid", split.tables = Inf, split.cells = 30)
# write.table(menu2_ascii,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
beep <- readChar("beep_boop.txt",file.info("beep_boop.txt")$size)
write.table(beep,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
write.table("\n",file = "menu.txt", append = T, col.names = F, row.names = F, quote = F)



tabulecka <- tibble(Podnik = c("Bioland", "Ceska pivnica", "Dilema", "Kasa", "Mestiansky pivovar",
                               "Bistro Mnamka", "Veda", "Suvlaki", "Jedla lenka", "Svadby a kari", "Red Cafe", "U Hasica",
                               "Alzbetka", "Centr. klub.","Prazsky pub", "RTVS", "Milton"),
                    Ulica = c("Mytna 23", "Radlinskeho 39","Sancova 70","Radlinskeho 11",
                              "Drevena 8","Vazovova 9", "Zilinska 2","Krizna 8","Cajkovskeho 14", "Americka 2",
                              "Racianske myto 1/A","Wilsonova 1","Mickiewiczova 1","Krizna 64", "Kominarska 1552/3A", "Mytna 1", "Soltesovej 14"), TR_karta = T, karta = T)
tabulecka[12,3] <- F
tabulecka[13,3] <- F
tabulecka <- arrange(tabulecka, Podnik)
tabulecka <- pandoc.table.return(tabulecka, style = "grid", split.tables = Inf, split.cells = 30) %>% str_sub(3)
write.table(tabulecka,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
