alzbetka <- function(){
  url <- "https://restauracie.sme.sk/restauracia/mestsky-pivovar-alzbetka_9975-stare-mesto_2949/denne-menu"
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  jedlo <- jedlo %>% 
    str_trim() %>%
    str_remove_all("FIT: ") %>%
    str_remove_all("P1: ") %>%
    str_remove_all("P2: ") %>%
    str_remove_all("M1: ") %>%
    str_remove_all("M2: ") %>%
    str_trim()
  return(c("Alzbetka", jedlo))
}
