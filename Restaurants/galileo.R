galileo <- function(){
  url <- "https://restauracie.sme.sk/restauracia/galileo-restaurant_902-stare-mesto_2949/denne-menu"
  #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html(url)
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>%
    html_text() %>%
    str_trim()
  jedlo <- jedlo[!str_detect(jedlo, "Polievka")]
  jedlo <- jedlo[!str_detect(jedlo, "Hlavné jedlo")]

  return(c("Galileo",jedlo))
}
