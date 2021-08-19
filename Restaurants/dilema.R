dilema <- function(){
  url_dilema <- "http://restaurant-dilema.sk/Images/denneMenu.png"
  download.file(url_dilema, destfile = "dilema.png", quiet=TRUE, mode = "wb")
  size_str <- "640x520"
  position_start <- list("+190+450",
                         "+840+450",
                         "+190+970",
                         "+840+970",
                         "+190+1488")
  special <- "+840+1488"
  t <- format(lubridate::today(), "%A")
  days_of_the_week <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday","Sunday")
  t <- which(t == days_of_the_week)
  im <- magick::image_crop(magick::image_read("dilema.png"), paste0(size_str, position_start[[t]]))
  
  jedlo <- magick::image_ocr(im,
                             language = "slk")
  jedlo <- str_split(jedlo, "Polievky") %>% pluck(1,2)
  jedlo <- 
    str_split(jedlo, "Hlavné jedlá")
  polievka <- pluck(jedlo, 1, 1) %>% str_replace_all("\n", " ") %>% str_squish()
  
  jedlo <- pluck(jedlo, 1, 2)
  jedlo <- str_split(jedlo, "[1-3]:")[[1]]
  jedlo <- jedlo[-1] %>% str_replace_all("\n", " ") %>% str_squish() %>% str_remove_all("Menu")
  
  unlink("dilema.png")
  return(c("Dilema",polievka, jedlo))
}
