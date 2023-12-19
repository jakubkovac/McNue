centralna_klubovna <- function(sme = T){
  if(sme){
    url <- "https://restauracie.sme.sk/restauracia/centralna-klubovna_7011-nove-mesto_2653/denne-menu"
    #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    raw <- read_html(url)
    jedlo <- raw %>%
      html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
    jedlo <-
      jedlo %>%
      str_squish()
    jedlo <- jedlo[which(str_detect(jedlo, "Polievka|Menu")) + 1]
    jedlo <- str_remove_all(jedlo, "obsahuje")
  }else{
    url <- "https://www.nasaklubovna.sk/sk/menu/centralna/tyzdenne-menu/"
    #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    raw <- read_html(url)
    jedlo <- 
      raw %>% 
      html_nodes(".daily-menu-wrapper > table:nth-child(1)") %>% 
      html_children() %>%
      html_text()
    days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
    dni <- c("Pondelok", "Utorok", "Streda", "Stvrtok", "Piatok", "Sobota", "Nedela")
    today <- format(Sys.Date(), "%A")

    jedlo <- slovak_language_destroyer(jedlo)
    match_day <- paste0(dni[which(today == days_of_the_week)])#,
    # " ",
    # unlist(str_split(format(Sys.Date(), "%D"),"/"))[2])
    dnes <- which(str_detect(jedlo,match_day))
    jedlo <- jedlo[(dnes+1):(dnes+4)]
    jedlo <- 
      jedlo %>%
      str_remove_all("Menu [a-zA-Z] s polievkou \\|") %>%
      str_remove_all("Polievka \\|") %>%
      str_remove_all("obsahuje") %>%
      str_remove_all("\t") %>% 
      str_remove_all("\n") %>% 
      str_remove_all("\r") %>% 
      str_trim() 
  }
  
  return(c("Centr. klub.",jedlo))
}
