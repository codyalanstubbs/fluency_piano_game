library(tidyverse)
library(googlesheets4)
library(googledrive)
source("scripts/visualizer_chords.R")
table <- "https://docs.google.com/spreadsheets/d/1V6R8VAWCpLNPeC5WWSui4tm1cTzi3LgQB1_-kA_cwuU/edit?usp=sharing"

#drive_auth(cache = ".secrets") #for the first time running the app in R to get the OAuth token
#gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

song_data <-read_sheet(table)
# song_data <- read_csv("data/song_data.csv")
vis_song_data <- 
        song_data %>%
        mutate(song = gsub("https://www.amchords.com/piano/", "", song)) %>% 
        mutate(chord = paste(chord_order, chord)) %>%
        mutate(section = paste(section_order, section)) %>%
        select(song, chord, section)

library(ggraph)
library(igraph)

library(tidygraph)
# Song visualization ----
i <- vis_song_data$song %>% unique(.)

song_section <- vis_song_data %>%
        filter(song == i) %>%
        mutate(visual = section) %>%
        select(song, section, visual)

colnames(song_section) <- c("from", "to", "visual")

section_chord <- vis_song_data %>% 
        filter(song == i) %>% 
        mutate(visual = section) %>% 
        select(section, chord, visual)

colnames(section_chord) <- c("from", "to", "visual")

song <- rbind(song_section, section_chord)

graph <- as_tbl_graph(song)

ggraph(graph, layout = 'dendrogram', circular = FALSE) + 
        geom_edge_elbow(aes(color = visual, yend = -1*to)) +
        # Song title 
        geom_node_text(aes( label=name, filter=name %in% unique(song_section$from), fill = NULL), angle=90 , hjust=0.5, nudge_y = -5, size = 10) +
        # Song sections
        geom_node_text(aes( label=name, filter=name %in% unique(song_section$to), fill = NULL), angle=0 , vjust = 0, hjust=0, nudge_y = 0 , size = 4) +
        # Song chords
        geom_node_text(aes(y= 1*(119:1), label=name, filter=name %in% unique(section_chord$to), fill = NULL), angle=45, hjust=0, nudge_x = 0, nudge_y = 119*1, size = 3) +
        #geom_node_text(aes( label=name, filter=leaf) , angle=270 , hjust=0.5, nudge_y = -0.4) +
        coord_flip() +  
        scale_x_reverse() + 
        scale_y_reverse() +
        #scale_edge_color_brewer(type = "qual") +
        theme(legend.position="none")

# Chords visualization ----
chords <- unique(song_data$chord)
# The following produces a series of combined figures for  a list of chords
plist = lapply(chords, function(x) {display_notes(chrd = x, octave_section = 5)})
library(gridExtra)
grid.arrange(grobs = plist, nrow = 1)