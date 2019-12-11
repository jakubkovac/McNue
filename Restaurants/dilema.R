dilema <- function(){
  url_dilema <- "https://restauracie.sme.sk/restauracia/dilema-restaurant_731-stare-mesto_2949/denne-menu"
  download.file(url_dilema, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>%
    html_text() %>%
    str_trim()
  jedlo <- jedlo[-2]
  jedlo <- str_replace_all(jedlo, "([\n\t])", "")
  jedlo <- str_replace_all(jedlo, "-", "")
  jedlo <- str_replace_all(jedlo, "\u00bd", "0.5")
  jedlo[2:5] <- str_sub(jedlo[2:5], start = 7)
  jedlo <- str_trim(jedlo)
  if(length(jedlo) == 6) jedlo <- jedlo[-5]
  if(length(jedlo) == 7) jedlo <- jedlo[-c(6,7)]
  return(c("Dilema",jedlo))
}
