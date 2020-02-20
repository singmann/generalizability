
library("tidyverse")
library("readxl")

files <- list.files("data/", full.names = TRUE)
read_excel(files[6])

din <- map(files, read_excel)
map(din, colnames)

din2 <- din
cols <- c("Lead_Author", "Subject_Number", "Condition", "Lineup_Choice", 
          "Confidence")
for (i in seq_len(length(din))) {
  din2[[i]] <- tryCatch(din2[[i]][,cols], error = function(e) list(NULL))
  din2[[i]]$Condition <- as.character(din2[[i]]$Condition)
  din2[[i]]$Subject_Number <- as.character(din2[[i]]$Subject_Number)
}
map(din2, colnames)
d <- bind_rows(din2[map_lgl(din2, ~!is.null(.[[1]]))])
d$correct <- ifelse(d$Lineup_Choice == 6, 1, 0)

d2 <- d %>% 
  filter(Condition %in% c("Control", "Experimental"))

d %>% 
  count(Condition)

d %>% 
  group_by(Condition) %>% 
  summarise(acc = mean(correct, na.rm = TRUE))

str(d)

table(d$Condition, d$Lineup_Choice)
