svadbykari <- function(sme = FALSE){
  if(sme){
    url <- "https://restauracie.sme.sk/restauracia/svadby-a-kari-americka_10343-nove-mesto_2653/denne-menu"
    download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    raw <- read_html("scrapedpage.html")
    jedlo <- raw %>%
      html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
    # if(length(jedlo) > 5) jedlo <- jedlo[-1]
    #if(length(jedlo) > 5) jedlo <- jedlo[1:4]
    #if(length(jedlo) == 5) jedlo <- jedlo[-1]
    jedlo <- str_trim(jedlo)
    jedlo <- str_remove_all(jedlo,"[A][0-9]") %>% str_remove_all("Víťaz kari duelu na Instagrame")
    jedlo <- jedlo[c(3:1)]  
  }else{
    url <- "http://www.svadbykari.sk/denne-menu/"
    download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    raw <- read_html("scrapedpage.html")
    jedlo <- 
      raw %>%
      html_nodes(".cff-text") %>% 
      as.character() %>% 
      str_split("<br>") %>% 
      unlist()
    today <- format(Sys.Date(), "%A")
    today <- dplyr::case_when(today == "Monday" ~ "Pondelok",
                              today == "Tuesday" ~ "Utorok",
                              today == "Wednesday" ~ "Streda",
                              today == "Thursday" ~ "Štvrtok",
                              today == "Friday" ~ "Piatok",
                              TRUE ~ NA_character_)
    today <- paste0(today, "|", str_to_lower(today))
    jedlo <- str_trim(jedlo)
    jedlo <- jedlo[nchar(jedlo) != 0]
    day_index <- which(str_detect(jedlo,today))  
    # jedlo <- jedlo[!str_detect(jedlo, "^$")]
    # jedlo <- jedlo[!str_detect(str_to_lower(jedlo), "isic")]
    jedlo <- jedlo[(day_index + 1):(day_index + 3)] %>% str_trim()
    jedlo <- str_remove_all(jedlo,"[A][0-9]")
    jedlo <- jedlo[c(3:1)] 
    jedlo <- str_remove_all(jedlo,"See MoreSee Less")
  }
  return(c("Svadby a Kari",jedlo))
}
