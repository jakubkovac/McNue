mcnue <- function(){
  url_kasa <- "https://restauracie.sme.sk/restauracia/kasa-3_9938-stare-mesto_2949/denne-menu"
  url_veglife <- "http://www.veglife.sk/sk/"
  url_bluebear <- "http://blue-bear.sk/denne-menu-blue-bear/"
  url_dilema <- "https://restauracie.sme.sk/restauracia/dilema-restaurant_731-stare-mesto_2949/denne-menu"
  url_mnamka <- "https://restauracie.sme.sk/restauracia/bistro-mnamka_9954-stare-mesto_2949/denne-menu"
  url_mestiansky <- "https://restauracie.sme.sk/restauracia/bratislavsky-mestiansky-pivovar-drevena_3951-stare-mesto_2949/denne-menu"
  url_lenka <- "https://www.jedlalenka.sk/"
  url_bioland <- "https://www.bioland.sk/restauracia-bratislava/"
  url_veda <- "https://restauracie.sme.sk/restauracia/veda-vegetarian-vegan_10202-stare-mesto_2949/denne-menu"
  #KASA3
  download.file(url_kasa, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo1 <- raw %>% 
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% 
    html_text()
  jedlo1 <- str_trim(jedlo1)
  jedlo1 <- str_replace_all(jedlo1, "([\n\t])", "")
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
  jedlo2 <- str_replace_all(jedlo2, "([\n\t])", "")
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
  jedlo3 <- str_replace_all(jedlo3, "([\n\t])", "")
  menu[3,] <- c("BlueBear",jedlo3,"")
  
  #DILEMA
  download.file(url_dilema, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo4 <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>%
    html_text() %>%
    str_trim()
  jedlo4 <- jedlo4[c(2,5:8)]
  jedlo4 <- str_replace_all(jedlo4, "([\n\t])", "")
  jedlo4 <- str_replace_all(jedlo4, "-", "")
  jedlo4 <- str_replace_all(jedlo4, "\u00bd", "0.5")
  jedlo4[2:5] <- str_sub(jedlo4[2:5], start = 7)
  jedlo4 <- str_trim(jedlo4)
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
  
  jedlo5[1] <- jedlo5[1] %>% str_sub(start = 16) %>% str_trim()
  jedlo5 <- str_replace_all(jedlo5, "([\n\t])", "")
  menu[5,] <- c("Bistro Mnamka", jedlo5)
  menu
  
  # BRATISLAVSKY MESTIANSKY PIVOVAR
  download.file(url_mestiansky, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo6 <- raw %>% 
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% 
    html_text()
  jedlo6 <- str_trim(jedlo6)
  #jedlo6 <- jedlo6[c(2,4,5)]
  jedlo6[1] <- 
    jedlo6[1] %>% 
    str_extract("3l(.*)") %>%
    str_sub(start = 3) %>%
    str_trim()
   jedlo6[2:3] <-
    jedlo6[2:3] %>%
    str_extract("g(.*)") %>%
    str_sub(start = 3) %>%
    str_trim()
  jedlo6 <- str_replace_all(jedlo6, "([\n\t])", "")
  menu[6,] <- c("Mestiansky piv.",jedlo6,"","")
  
   # JEDLA LENKA
  download.file(url_lenka, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo7 <- raw %>% 
    html_nodes(".insert-page-30") %>%
    html_children() %>%
    html_text()
  jedlo7 <- jedlo7[2:9]
  a <- str_split(jedlo7[1]," ") %>% unlist()
  kde <- str_match(a,"^[[:upper:]]") %>% is.na() %>% `!` %>% which()
  jedlo7[1] <- paste(a[kde],collapse = "/")
  menu2 <- tibble(Jedla_lenka = jedlo7)
  
  #BIOLAND
  download.file(url_bioland, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo8 <- raw %>% 
    html_nodes(".td_content_body") %>% 
    html_children() %>%
    html_text()
  Sys.Date()
  strftime(Sys.Date(), '%A')
  jedlo8 <- jedlo8[13:56]
  jedlo8 <- jedlo8[str_length(jedlo8) >0]
  day_index <- seq(1,25,by = 5)
  day_menu <- jedlo8[day_index] %>% str_sub(start = -6)
  today <- paste0(format(Sys.Date(), "%d  %m") %>% str_replace("  ","."),".")
  today_i <- day_index[which(today == day_menu)]
  jedlo8 <- jedlo8[(today_i+1):(today_i + 4)]
  menu[7,] <- c("Bioland",jedlo8,"")
  
   # VEDA
  download.file(url_veda, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo9 <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  jedlo9 <- jedlo9[c(4,8,10,12,14)] %>% str_trim()
  jedlo9 <- jedlo9 %>% str_replace("VEGAN","") %>% str_replace("NOT","")
  jedlo9 <- jedlo9 %>% str_sub(start =4) %>% str_trim()
  jedlo9 <- jedlo9 %>% str_to_lower()
  menu[8,] <- c("Veda",jedlo9)
  #################################################################################
  menu <- menu[sample(1:nrow(menu),nrow(menu)),]
  return(list(menu,menu2))
}