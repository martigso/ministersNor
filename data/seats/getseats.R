library(rvest)
### Add elections if more are neccessary ###
elections <- c("1945-49","1950-53", "1954-57", "1958-61", 
               paste0(seq(1961, 1993, 4), "-", seq(65, 97, 4)), 
               "1997-2001", "2001-2005", "2005-2009", "2009-2013", "2013-2017")



#### Getting seat information from stortinget.no ####

  lapply(elections, function(x)
    system(paste0("wget -O data/seats/", x, ".html ", "https://www.stortinget.no/no/",
                  "Representanter-og-komiteer/Partiene/Partioversikt/?pid=",
                  x)))


seats <- lapply(paste0(elections, ".html"), function(yeah) read_html(paste0("./data/seats/", yeah)))
seats <- lapply(seats, function(mhm) data.frame((mhm %>% html_node("table") %>% html_table()), stringsAsFactors = FALSE))
seats <- lapply(seats, function(ohh) data.frame(ohh, parl_size = ohh$Storting[nrow(ohh)], stringsAsFactors = FALSE))
seats <- lapply(seats, function(heyhey) heyhey[-(nrow(heyhey)), ])

names(seats) <- elections

seats$`2009-2013`$Odelsting <- NA
seats$`2013-2017`$Odelsting <- NA

seats$`2009-2013`$Lagting <- NA
seats$`2013-2017`$Lagting <- NA

seats <- do.call(rbind, seats)

seats$parl_period <- gsub("\\.[0-9]", "", rownames(seats))
rownames(seats) <- 1:nrow(seats)
colnames(seats) <- c("party_name", "seats", "seats_odelsting", "seats_lagting", "parl_size", "parl_period")

levels(factor(seats$party_name))
seats$party_name[which(seats$party_name == "Arbeiderpartiet")] <- "DNA"
seats$party_name[which(seats$party_name == "Høyre")] <- "H"
seats$party_name[which(seats$party_name == "Kristelig Folkeparti")] <- "KrF"
seats$party_name[which(seats$party_name == "Senterpartiet")] <- "Sp"
seats$party_name[which(grepl("Sosialistisk Folkeparti|Sosialistisk Valgforbund|Sosialistisk Venstreparti", seats$party_name))] <- "SV"
seats$party_name[which(seats$party_name == "Venstre")] <- "V"

seats$election_year <- gsub("\\-[0-9]*$", "", seats$parl_period)
seats$election_year <- gsub("1950", "1949", seats$election_year)
seats$election_year <- gsub("1954", "1953", seats$election_year)
seats$election_year <- gsub("1958", "1957", seats$election_year)

save(seats, file = "./data/seats/seats.rda")


