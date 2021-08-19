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
    today <- unlist(str_split(format(Sys.Date(), "%d.%m.%Y", trim = T), "\\.")) %>% 
      as.numeric() %>% 
      paste0(collapse = ".")
    #today <- "1.7.2021" #they have wrong dates
    jedlo <- str_trim(jedlo)
    jedlo <- jedlo[nchar(jedlo) != 0]
    day_index <- which(str_detect(str_remove_all(jedlo, " "),today))  
    jedlo <- jedlo[(day_index + 1):(day_index + 3)] %>% str_trim()
    jedlo <- str_remove_all(jedlo,"[A][0-9]")
    jedlo <- str_remove_all(jedlo,"See MoreSee Less") %>% str_remove_all("\\<\\/span\\>")
  }
  return(c("Svadby a Kari",jedlo))
}
