mcnue <- function(){
  library(tidyverse)
  library(stringr)
  library(rvest)
  library(lubridate)
  library(pander)
  url_kasa <- "https://restauracie.sme.sk/restauracia/kasa-3_9938-stare-mesto_2949/denne-menu"
  url_veglife <- "http://www.veglife.sk/sk/"
  url_bluebear <- "http://blue-bear.sk/denne-menu-blue-bear/"
  url_dilema <- "https://restauracie.sme.sk/restauracia/dilema-restaurant_731-stare-mesto_2949/denne-menu"
  url_mnamka <- "https://restauracie.sme.sk/restauracia/bistro-mnamka_9954-stare-mesto_2949/denne-menu"
  url_mestiansky <- "https://restauracie.sme.sk/restauracia/bratislavsky-mestiansky-pivovar-drevena_3951-stare-mesto_2949/denne-menu"
  #KASA3
  download.file(url_kasa, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo1 <- raw %>% 
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% 
    html_text()
  jedlo1 <- str_trim(jedlo1)
  menu <- tibble(podnik = character(), polievka = character(), jedlo_1 = character(), jedlo_2 = character(), jedlo_3 = character(), jedlo_4 = character())
  menu[1,] <- c("Kasa 3",jedlo1[1:3],"","")
  menu
  
  # VEGLIFE
  download.file(url_veglife, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo2 <-
    raw %>% 
    html_nodes(".pcs-content") %>% 
    html_children() %>% 
    html_text() %>%
    str_trim() %>%
    .[2:6]
  jedlo2 <- str_sub(jedlo2,3) %>% str_trim()
  jedlo2[1] <- str_sub(jedlo2[1],9)
  jedlo2
  menu[2,] <- c("Veglife",jedlo2)
  
  #BLUEBEAR
  Sys.Date()
  strftime(Sys.Date(), '%A')
  download.file(url_bluebear, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo3 <- raw %>%
    html_nodes(".menux > div:nth-child(1)") %>%
    html_children() %>%
    html_text()
  day_index <- c(1,7,13,19,25)
  day_menu <- jedlo3[day_index] %>% dmy
  today_i <- day_index[which(Sys.Date() == day_menu)]
  jedlo3 <- jedlo3[(today_i+1):(today_i + 5)]
  jedlo3 <- jedlo3[-2]
  jedlo3[2:4] <- 
    jedlo3[2:4] %>% 
    str_extract("g(.*)obsahuje") %>% 
    str_sub(start = 2, end = -9) %>%
    str_trim()
  jedlo3[1] <- 
    jedlo3[1] %>% 
    str_extract("ml(.*)obsahuje") %>%
    str_sub(start = 3, end = -9) %>%
    str_trim()
  menu[3,] <- c("BlueBear",jedlo3,"")
  
  #DILEMA
  download.file(url_dilema, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo4 <- raw %>% 
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% 
    html_text() %>% 
    str_trim()
  jedlo4 <- jedlo4[c(2,5:8)]
  menu[4,] <- c("Dilema",jedlo4)
  
  #BISTRO MNAMKA 
  download.file(url_mnamka, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo5 <- 
    raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>%
    html_text() %>% 
    str_trim() %>%
    .[1:5] #vip menu not included
  
  jedlo5[2:5] <- 
    jedlo5[2:5] %>% 
    str_extract("g(.*)") %>% 
    str_sub(start = 2) %>%
    str_trim()
  
  jedlo5[1] <- jedlo5[1] %>% str_sub(15) %>% str_trim()
  menu[5,] <- c("Bistro Mnamka", jedlo5)
  menu
  
  # BRATISLAVSKY MESTIANSKY PIVOVAR
  download.file(url_mestiansky, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo6 <- raw %>% 
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% 
    html_text()
  jedlo6 <- str_trim(jedlo6)
  jedlo6 <- jedlo6[c(2,4,5)]
  jedlo6[1] <- 
    jedlo6[1] %>% 
    str_extract("l(.*)") %>%
    str_sub(start = 3) %>%
    str_trim()
   jedlo6[2:3] <-
    jedlo6[2:3] %>%
    str_extract("g(.*)") %>%
    str_sub(start = 3) %>%
    str_trim()
  menu[6,] <- c("Mestiansky piv.",jedlo6,"","")
  #################################################################################
  
  spec_chrs <- read_table("special_chars_sk.txt",col_names = F)[[1]]
  spec_chrs <- c(spec_chrs,str_to_lower(spec_chrs))
  r_spec_chrs <- c("a","a","c","d","e","e","i","l","l","n","o","o","r","s","t","u","y","z")
  r_spec_chrs <- c(str_to_upper(r_spec_chrs),r_spec_chrs)
  names(r_spec_chrs) <- spec_chrs #this creates a named vector that represents the mapping of letters
  # menu$polievka <- str_replace_all(menu$polievka,r_spec_chrs) 
  # menu$jedlo_1 <- str_replace_all(menu$jedlo_1,r_spec_chrs) 
  # menu$jedlo_2 <- str_replace_all(menu$jedlo_2,r_spec_chrs) 
  # menu$jedlo_3 <- str_replace_all(menu$jedlo_3,r_spec_chrs) 
  # menu$jedlo_4 <- str_replace_all(menu$jedlo_4,r_spec_chrs) 
  menu <-
    menu %>%
    mutate_all(.fun = function(x) str_replace_all(x,r_spec_chrs))
  menu
  menu <- menu[sample(1:nrow(menu),nrow(menu)),]
  menu_ascii <- pandoc.table.return(menu, style = "grid", split.tables = Inf)
  menu_ascii <- str_trim(menu_ascii)
  menu_ascii
  write.table(menu_ascii,file = "menu.txt", row.names = F, col.names = F, quote = F)
  beep <- readChar("beep_boop.txt",file.info("beep_boop.txt")$size)
  write.table("\n \n",file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
  write.table(beep,file = "menu.txt",append = T, col.names = F, row.names = F, quote = F)
  return(menu)
}