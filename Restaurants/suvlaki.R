suvlaki <- function(sme = TRUE){
  if(sme){
  url_suvlaki <- "https://restauracie.sme.sk/restauracia/bistro-suvlkai_7439-stare-mesto_2949/denne-menu"
  download.file(url_suvlaki, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text() %>% str_trim()
  jedlo <- jedlo[c(2,5:8)]
  jedlo <- 
    jedlo %>% str_trim()
  }else{
    # download.file("https://www.facebook.com/BistroSuvlaki/posts/?ref=page_internal", destfile = "scrapedpage.htm", quiet=TRUE)
    # raw <- read_html("scrapedpage.html")
    # raw <- raw %>% html_nodes("#id_5da42a939c5772247089010") %>% html_text()
    # polievka <- 
    #   raw %>%
    #   str_split("sme si pre Vás pripravili") %>%
    #   unlist() %>%
    #   str_split("\\.\\.\\.") %>%
    #   .[[2]] %>%
    #   .[1]
    # polievka <- unlist(str_split(polievka, "polievka"))[1] %>% str_remove("\\:") %>% str_trim() 
    # jedlo <- unlist(jedlo)[2:5]
    # jedlo <- c(paste(polievka, "polievka"), jedlo)
  }
  return(c("Suvlaki",jedlo))
}

