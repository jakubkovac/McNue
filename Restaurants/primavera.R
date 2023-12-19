primavera <- function(sme = TRUE){
  if(sme){
    url <- "https://restauracie.sme.sk/restauracia/pizzeria-primavera_2302-stare-mesto_2949/denne-menu"
    raw <- read_html(url)
    jedlo <- raw %>%
      html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
    jedlo <- jedlo[-1]
    jedlo <- str_squish(jedlo)[1:5]
    return(c("Primavera", jedlo))
  }
}