suvlaki <- function(){
  url_suvlaki <- "https://restauracie.sme.sk/restauracia/bistro-suvlkai_7439-stare-mesto_2949/denne-menu"
  download.file(url_suvlaki, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text() %>% str_trim()
  jedlo <- jedlo[c(2,seq(4,10,by = 2))]
  jedlo <- 
    jedlo %>% str_trim()
  return(c("Suvlaki",jedlo))
}

