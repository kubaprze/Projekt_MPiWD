library(rvest)
library(tidyverse)
library(httr)
library(curl)
library(anytime)



# action games

url <- "https://steamspy.com/genre/Action"

page <- read_html(url)

table1 <- page %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table1, "action_games.csv")
#_____________________________________________________________________________________


# tags
page2 <- read_html("https://steamspy.com/tag/")

table2 <- page2 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]

h1 <- table2$`Votes for this tag` |> 
  str_extract("^[0-9]+")
h2 <- table2$`Votes for this tag` |> 
  str_extract(",[0-9]+$") |> 
  str_remove("\\,") |> 
  replace_na("a")

table2$`Votes for this tag` <- paste0(h1,h2) |> 
  str_remove("a") |> 
  as.numeric()


head(table2)


write_csv2(table2, "tags2.csv")
#_____________________________________________________________________________________


# number of games each month
page3 <- read_html("https://steamspy.com/year/")

table3 <- page3 %>% 
  html_nodes("table#ataglance") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table3, "games_by_month.csv")
#_____________________________________________________________________________________


# steam devs and publishers

download.file("https://steamspy.com/dev/", destfile = "scrapedpage.html", quiet=TRUE)
page4 <- read_html("scrapedpage.html")

table4 <- page4 %>% 
  html_nodes("table#ataglance") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table4, "devs.csv")
#_____________________________________________________________________________________


# games with polish support

page5 <- read_html("https://steamspy.com/language/Polish")

table5 <- page5 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table5, "polish.csv")
#_____________________________________________________________________________________


# Strategy games
page6 <- read_html("https://steamspy.com/genre/Strategy")

table6 <- page6 %>% 
  html_nodes("table#gamesbygenre.table") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table6, "strategy_games.csv")
#_____________________________________________________________________________________



# RPG games
page7 <- read_html("https://steamspy.com/genre/RPG")

table7 <- page7 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table7, "rpg_games.csv")
#_____________________________________________________________________________________


# indie games
download.file("https://steamspy.com/genre/Indie", destfile = "scrapedpage2.html", quiet=TRUE)
page8 <- read_html("scrapedpage2.html")

table8 <- page8 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table8, "indie_games.csv")
#_____________________________________________________________________________________


# adventure games
page9 <- read_html("https://steamspy.com/genre/Adventure")

table9 <- page9 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table9, "adventure_games.csv")
#_____________________________________________________________________________________


# sports games
page10 <- read_html("https://steamspy.com/genre/Sports")

table10 <- page10 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table10, "sports_games.csv")
#_____________________________________________________________________________________


# Simulators
page11 <- read_html("https://steamspy.com/genre/Simulation")

table11 <- page11 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table11, "simulation_games.csv")
#_____________________________________________________________________________________

# MMO games
page12 <- read_html("https://steamspy.com/genre/Massively")

table12 <- page12 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]

write_csv2(table12, "mmo_games.csv")
#_____________________________________________________________________________________

#2K
page13 <- read_html("https://steamspy.com/dev/2K")

table13 <- page13 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table13$`Score rank(Userscore / Metascore)` <- table13$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table13$Price <- table13$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table13$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table13$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table13$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table13$`Release date` <- as.Date(anytime(table13$`Release date`))

table13 <- table13[-1]

names(table13) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table13, "dev_2k.csv")


#_____________________________________________________________________________________

page14 <- read_html("https://steamspy.com/dev/Activision")

table14<- page14 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table14$`Score rank(Userscore / Metascore)` <- table14$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table14$Price <- table14$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table14$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table14$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table14$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table14$`Release date` <- as.Date(anytime(table14$`Release date`))

table14 <- table14[-1]

names(table14) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table14, "dev_activision.csv")

#_____________________________________________________________________________________

# EA
page15 <- read_html("https://steamspy.com/dev/Electronic+Arts")

table15<- page15 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table15$`Score rank(Userscore / Metascore)` <- table15$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table15$Price <- table15$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table15$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table15$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table15$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table15$`Release date` <- as.Date(anytime(table15$`Release date`))

table15 <- table15[-1]

names(table15) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table15, "dev_ea.csv")


#_____________________________________________________________________________________

# bethesda
page16 <- read_html("https://steamspy.com/dev/Bethesda+Softworks")

table16<- page16 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table16$`Score rank(Userscore / Metascore)` <- table16$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table16$Price <- table16$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table16$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table16$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table16$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table16$`Release date` <- as.Date(anytime(table16$`Release date`))

table16 <- table16[-1]

names(table16) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table16, "dev_bethesda.csv")


#_____________________________________________________________________________________

# Klei
page17 <- read_html("https://steamspy.com/dev/Klei+Entertainment")

table17 <- page17 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table17$`Score rank(Userscore / Metascore)` <- table17$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table17$Price <- table17$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table17$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table17$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table17$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table17$`Release date` <- as.Date(anytime(table17$`Release date`))

table17 <- table17[-1]

names(table17) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table17, "dev_Klei.csv")


#_____________________________________________________________________________________

# Paradox
page18 <- read_html("https://steamspy.com/dev/Paradox+Interactive")

table18 <- page18 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table18$`Score rank(Userscore / Metascore)` <- table18$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table18$Price <- table18$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table18$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table18$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table18$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table18$`Release date` <- as.Date(anytime(table18$`Release date`))

table18 <- table18[-1]

names(table18) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table18, "dev_paradox.csv")


#_____________________________________________________________________________________

# Sega
page19 <- read_html("https://steamspy.com/dev/SEGA")

table19 <- page19 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table19$`Score rank(Userscore / Metascore)` <- table19$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table19$Price <- table19$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table19$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table19$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table19$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table19$`Release date` <- as.Date(anytime(table19$`Release date`))

table19 <- table19[-1]

names(table19) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table19, "dev_sega.csv")


#_____________________________________________________________________________________

# Ubisoft
page20 <- read_html("https://steamspy.com/dev/Ubisoft")

table20 <- page20 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table20$`Score rank(Userscore / Metascore)` <- table20$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table20$Price <- table20$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table20$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table20$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table20$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table20$`Release date` <- as.Date(anytime(table20$`Release date`))

table20 <- table20[-1]

names(table20) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table20, "dev_ubisoft.csv")


#_____________________________________________________________________________________

# Valve
page21 <- read_html("https://steamspy.com/dev/Valve")

table21 <- page21 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table21$`Score rank(Userscore / Metascore)` <- table21$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table21$Price <- table21$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table21$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table21$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table21$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table21$`Release date` <- as.Date(anytime(table21$`Release date`))

table21 <- table21[-1]

names(table21) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table21, "dev_valve.csv")


#_____________________________________________________________________________________

# Warner
page22 <- read_html("https://steamspy.com/dev/Warner+Bros.+Interactive+Entertainment")

table22 <- page22 %>% 
  html_nodes("table#gamesbygenre") %>% 
  html_table() %>% 
  .[[1]]


table22$`Score rank(Userscore / Metascore)` <- table22$`Score rank(Userscore / Metascore)` %>% 
  str_extract("[0-9]+%\\)") %>% 
  str_extract("[0-9]+") %>% 
  as.integer()

table22$Price <- table22$Price %>% 
  str_extract("[0-9]+.*([0-9]+)*") %>% 
  as.numeric()

min_ownersp <-  table22$Owners %>% 
  str_extract("^[0-9]+(,*[0-9]+)*(,*[0-9]+)*") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
max_ownersp <- table22$Owners %>% 
  str_extract("[0-9]+(,*[0-9]+)*(,*[0-9]+)*$") %>% 
  str_remove_all("\\,") %>% 
  as.integer()
table22$Owners <- rowMeans(cbind(min_ownersp, max_ownersp))

table22$`Release date` <- as.Date(anytime(table22$`Release date`))

table22 <- table22[-1]

names(table22) <- c("Game", "Release_date" ,"Price", "Metascore", "Owners" ,"Playtime")

write_csv2(table22, "dev_warner.csv")




