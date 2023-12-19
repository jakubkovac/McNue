alzbetka <- function(sme = TRUE){
  if(sme){
  url <- "https://restauracie.sme.sk/restauracia/mestsky-pivovar-alzbetka_9975-stare-mesto_2949/denne-menu"
  #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html(url)
  jedlo <- raw %>%
    html_nodes(".dnesne_menu .jedlo_polozka .left") %>% html_text()
  jedlo <- jedlo %>% 
    str_trim() %>%
    str_remove_all("FIT") %>%
    str_remove_all("P1") %>%
    str_remove_all("P2") %>%
    str_remove_all("M1") %>%
    str_remove_all("M2") %>%
    str_trim()
  first_date <- tryCatch(dmy(jedlo[[1]]), error = function(e){FALSE}, finally = TRUE)
  if(!isFALSE(first_date)) jedlo <- jedlo[-1]
  }else{
    url <- "http://www.pivovaralzbetka.sk/denne-menu/"
    #download.file(url, destfile = "scrapedpage.html", quiet=TRUE)
    raw <- read_html(url)
    jedlo <-
      raw %>% html_nodes('//*[@id="main"]/div/div') %>% 
      html_children %>% html_children %>% 
      html_children %>% html_children %>% 
      html_children
    
    days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
    dni <- str_to_upper(c("pondelok", "utorok", "streda", "stvrtok", "piatok", "sobota", "nedela"))
    today <- format(Sys.Date(), "%A")
    search <- dni[which(today == days_of_the_week)]
    
    
    look_here <- map_lgl(jedlo, ~str_detect(slovak_language_destroyer(html_text(.x)), search)) |> 
      which() + 1
        
    jedlo <- jedlo[[look_here]] |> html_children() |> html_text()
    jedlo <- jedlo[!str_detect(jedlo,"Špeciálna")]

  }
  return(c("Alzbetka", jedlo))
}
