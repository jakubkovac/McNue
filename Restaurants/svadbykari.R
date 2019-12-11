svadbykari <- function(sme = FALSE){
  if(sme){
    url <- "https://restauracie.sme.sk/restauracia/svadby-a-kari-americka_10343-nove-mesto_2653/denne-menu"
    download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    raw <- read_html("scrapedpage.html")
    jedlo <- raw %>%
      html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
    if(length(jedlo) > 5) jedlo <- jedlo[-1]
    jedlo <- str_trim(jedlo)
    jedlo <- jedlo %>% str_trim()
    jedlo <- str_remove_all(jedlo,"[A][0-9]")
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
    jedlo <- jedlo[-1] %>% str_remove("</span>")
    jedlo <- str_trim(jedlo)
    jedlo <- jedlo %>% str_trim()
    jedlo <- str_remove_all(jedlo,"[A][0-9]")
    jedlo <- jedlo[jedlo != ""]
    jedlo <- jedlo[c(3:1)] 
    jedlo <- str_remove_all(jedlo,"See MoreSee Less")
  }
  return(c("Svadby a Kari",jedlo,"",""))
}
