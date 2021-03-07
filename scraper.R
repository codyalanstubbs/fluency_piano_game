# Load the Library
library(RSelenium)
library(XML)
library(tidyverse)

get_song_data <- function(choose_port, site_url){
        eCaps <- list("chrome_binary" = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
        # start the server and browser(you can use other browsers here)
        rD <- rsDriver(browser=c("chrome"), chromever="87.0.4280.88", extraCapabilities = eCaps, port = choose_port)
        driver <- rD$client 
        
        # navigate to parcel view site
        driver$navigate(site_url)
        details_elements <- driver$findElements("css selector","span")
        details_class <- unlist(sapply(details_elements, function(x){x$getElementAttribute("class")}))
        details_data <- unlist(sapply(details_elements, function(x){x$getElementText()}))
        df <- data.frame(
                class = details_class,
                data = details_data) %>% 
                filter(class != "", data != "") %>% 
                mutate(id = rownames(.))
        df2 <- df  %>% filter(class == "s-part")
        section_df <- data.frame()
        for (i in 1:length(df2$data)) {
                
                this_section <- df2[i, 'id'] %>% as.numeric(.)
                before_next_section <- df2[i+1, 'id'] %>% as.numeric(.) - 1
                
                if (is.na(before_next_section)){
                        seq <- this_section:length(df$data)
                        
                } else{
                        seq <- this_section:before_next_section
                }
                data <- data.frame(id = seq) %>% mutate(section = df2[i, 'data']) %>% mutate(section_order = i)
                section_df <-  rbind(section_df, data)
                
                
                
        }
        
        song_data <- df %>% 
                left_join(section_df %>% mutate(id = as.character(.$id))) %>% 
                drop_na(.) %>%
                filter(!data %in% unique(df2$data)) %>%
                select(section_order, section, data) %>%
                mutate(chord_order = rownames(.)) %>%
                rename(section = section, chord = data) #%>%
                #select(section_order, section, chord_order, chord)


        song_data <<- song_data
        
        #close the driver
        driver$close()
        
        #close the server
        rD$server$stop()
}

site_url <- "https://www.amchords.com/piano/christina-perri/arms"
choose_port <- 46018L
get_song_data(choose_port = choose_port, site_url = site_url)

library(readr)
write_csv(song_data %>% mutate(song = site_url), "data/song_data.csv")

library(googlesheets4)
library(googledrive)

table <- "https://docs.google.com/spreadsheets/d/1V6R8VAWCpLNPeC5WWSui4tm1cTzi3LgQB1_-kA_cwuU/edit?usp=sharing"

#drive_auth(cache = ".secrets") #for the first time running the app in R to get the OAuth token
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

#sheet_write(song_data, table)
sheet_append(table, song_data)


read_sheet(table)
