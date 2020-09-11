bioland <- function(){
  url_bioland <- "https://www.bioland.sk/restauracia-bratislava/"
  download.file(url_bioland, destfile = "scrapedpage.html", quiet=TRUE)
  raw <- read_html("scrapedpage.html")
  jedlo <-
    raw %>% 
    html_nodes(".td_content_body") %>% 
    html_children() %>%
    html_text()
  
  jedlo <- str_trim(jedlo) %>% .[. != ""]
  today <- format(Sys.Date(), "%A")
  today <- dplyr::case_when(today == "Monday" ~ "Pondelok",
                            today == "Tuesday" ~ "Utorok",
                            today == "Wednesday" ~ "Streda",
                            today == "Thursday" ~ "Štvrtok",
                            today == "Friday" ~ "Piatok",
                            TRUE ~ NA_character_)
  day_index <- which(str_detect(jedlo,today))
  # if(today == "Pondelok"){
  #   jedlo <- jedlo[day_index +1] %>% str_split("\\)") %>% unlist() %>% str_remove_all("Hl. jedlo")
  # }else{
  #   
  # }
  
  #jedlo <- jedlo[str_length(jedlo) >0] %>% str_remove_all("Hl. jedlo ")
  jedlo <- jedlo[(day_index+1):(day_index + 4)]
  return(c("Bioland",jedlo))
}
