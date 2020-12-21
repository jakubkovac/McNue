library(tidyverse)
library(scales)

df <- read_csv("data/lunch_data.csv", col_types = "ccc__c_____________Dc")
colnames(df) <- c("subject", "body", "from", "to", "date", "size")
df <- mutate(df, n_users = map_dbl(str_split(df$to, ";"), length)) %>%
  filter(n_users > 1)
#lunch emails sent
nrow(df)

#sent to unique people
str_split(df$to, ";") %>% unlist %>% unique() %>% length()

# me vs daniel
df %>% count(from)

#first email from
min(df$date)

#subscribers
ggplot(df, aes(x = date, y = n_users)) +
  geom_line() +
  geom_point(aes(col = from)) +
  labs(x = "", y = "Number of receivers") + 
  scale_x_date(breaks = date_breaks("1 months"))

extract_list <- 
  df$body %>%
  str_split("\n") %>%
  map(~str_remove_all(.x, "\r")) %>%
  map(~.x[nchar(.x) > 1])

map(extract_list, ~sum(str_detect(.x, "I'm LunchBOT"))) %>% unlist()

extract_list <- map(extract_list, ~.x[1:(which(str_detect(.x, "I'm LunchBOT")) -1)]) %>%
  map(~.x[str_detect(.x, "\\|")])


map(extract_list[[1]], ~str_trim(unlist(str_split(.x, "\\|")))[-1])

outer_out <- vector(mode = "list", length(extract_list))

for (i in seq_along(extract_list)){
  x <- map(extract_list[[i]], ~str_trim(unlist(str_split(.x, "\\|")))[-1])
  new_rest <- which(map_lgl(x, ~nchar(.x[[1]]) > 0))
  ind <- map2(new_rest, lead(new_rest, default = length(x) +1) -1, ~seq(.x, .y))
  inner_out <- vector(mode = "list", length(ind))
  for (j in seq_along(ind)){
    inner_out[[j]] <- 
      map(x[ind[[j]]] %>% purrr::transpose(), ~unlist(.x)) %>%
      .[-length(.)] %>%
      map(~paste0(.x, collapse = " ")) %>%
      unlist() %>%
      str_trim()
  }
  outer_out[[i]] <- inner_out
}


outer_out <- map(outer_out, ~as_tibble(do.call(rbind, .x[-1]), .name_repair = "minimal")) %>%
  map(~setNames(.x, c("podnik", "polievka","jedlo_1","jedlo_2","jedlo_3","jedlo_4")))

df$df <- outer_out
