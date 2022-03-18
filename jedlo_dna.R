jedlo_dna <- menu[sample(1:nrow(menu),1),]
jedlo_dna <- jedlo_dna[1,unname(map_dbl(jedlo_dna, nchar) > 0)]
jedlo_dna
random_jedlo <- stringr::str_squish(jedlo_dna[[sample(3:length(jedlo_dna), 1)]])
#random_jedlo <- "Gril panenka v slaninkovom kabáte šťuchané zemiaky"
random_jedlo
random_jedlo <- random_jedlo %>%  enc2utf8() %>% str_trim() %>% str_squish()
insert_new_lines <- function(x, max_chars = 14){
  if(nchar(x)<= max_chars) return(x)
  whitespaces <- stringr::str_locate_all(x, " ")[[1]][,1]
  n <- max(whitespaces[which(whitespaces <= max_chars)])
  out <- stringr::str_sub(x, 1, n-1)
  out <- c(out, insert_new_lines(str_sub(x, n+1)))
  return(out)
}
n_lines <- length(insert_new_lines(random_jedlo, 18))
n_lines
jedlo_lines <- paste0(insert_new_lines(random_jedlo, 18), collapse="\n")
jedlo_size <- case_when(n_lines %in% c(6,7) ~ 40,
                        n_lines <=5 ~ 55,
                        TRUE~30)
lunch_folder <- "C:/Users/jakub.kovac/OneDrive - Zurich Insurance/Lunchbot/"
img <- image_read(paste0("C:/Users/jakub.kovac/OneDrive - Zurich Insurance/", "jedlo_dna_default.png"))
img <- image_annotate(img, jedlo_dna$podnik, size = 45, color = "black",
                      location = "+200+510", font = "consolas")
# img <- image_annotate(img, "NIE", size = 45, color = "black",
#               location = "+200+510", font = "consolas")
img <- image_annotate(img, jedlo_lines,
                      size = jedlo_size, color = "black", font = "consolas",
                      location = "+0+220", gravity = "North")
img <- image_annotate(img, lubridate::today(),
                      size = 45, color = "black", font = "consolas",
                      location = "+200+590")
img
f <- list.files(lunch_folder, full.names = TRUE)
f <- f[str_detect(f, "[0-9]\\.png$")]
file.remove(f)
image_write(img, paste0(lunch_folder,
                        "jedlo_dna",
                        stringr::str_remove_all(lubridate::today(),"-"),
                        ".png"))
