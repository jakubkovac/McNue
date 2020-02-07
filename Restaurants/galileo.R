galileo <- function(){
  url <- "https://restauracie.sme.sk/restauracia/galileo-restaurant_902-stare-mesto_2949/denne-menu"
  download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>%
    html_text() %>%
    str_trim()
  jedlo <- c(jedlo[2],str_subset(jedlo, "[0-9]\\."))[-5]
  return(c("Galileo",jedlo))
}
