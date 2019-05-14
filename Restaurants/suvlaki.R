suvlaki <- function(){
  url_suvlaki <- "https://restauracie.sme.sk/restauracia/bistro-suvlkai_7439-stare-mesto_2949/denne-menu"
  download.file(url_suvlaki, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  jedlo <- jedlo[c(1,4,6,8,10)]
  jedlo <- 
    jedlo %>% str_trim()
  return(c("Suvlaki",jedlo))
}

