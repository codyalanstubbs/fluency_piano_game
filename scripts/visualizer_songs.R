library(tidyverse)
library(googlesheets4)
library(googledrive)

table <- "https://docs.google.com/spreadsheets/d/1V6R8VAWCpLNPeC5WWSui4tm1cTzi3LgQB1_-kA_cwuU/edit?usp=sharing"

#drive_auth(cache = ".secrets") #for the first time running the app in R to get the OAuth token
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)

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

chords <- unique(song_data$chord)
for (each in chords){
        octave_section = 5
        chrd = each
        chord_data <- read_csv("data/chords_01_cleaned.csv") %>% 
                filter(chord == "major") %>% 
                filter(key %in% chrd) %>% 
                select(key, notes) 
        k_data <- 
                read_csv("data/key-data-88 - keys.csv")  %>% 
                # Select a specific octave to focus
                filter(octave %in% octave_section) %>%  
                # Create different y-values to use for black and white keys
                mutate(viz.y = key.color) %>% 
                mutate(viz.y = gsub("white", "1", key.color)) %>%
                mutate(viz.y = gsub("black", "0.667", viz.y)) %>% 
                mutate(viz.y = as.numeric(viz.y))  %>% 
                # Separate the notes into rows to make it easier to filter 
                # through in if-else section 
                separate_rows(notes, sep = " ")
        
        b <- k_data %>% filter(key.color == "white") %>% select(key.number)
        b <- unique(b$key.number)
        
        # Create the baseline figure prior to if-else
        p <- k_data %>% 
                ggplot(aes(x = viz.x, y = viz.y, fill = key.color)) +
                geom_col(
                        data = k_data %>% filter(key.color == "white"),
                        position = position_dodge(), color = "black",
                        width = 1
                ) +
                geom_col(
                        data = k_data %>% filter(key.color == "black"),
                        position = position_dodge(), color = "white",
                        width = 0.75
                )  +
                scale_fill_manual(values = c("black", "white")) +
                scale_y_reverse(
                        limits = c(1,0), 
                        expand = c(0,0)
                ) +
                scale_x_continuous(position = "top") +
                theme_classic() +
                theme(axis.text.x = element_text(color = "black"),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      axis.title.y = element_blank(),
                      axis.line.y = element_blank(),
                      axis.title.x = element_blank(),
                      legend.position = "none")
        visual_data <- k_data %>% 
                filter(octave %in% octave_section) %>% 
                filter(notes %in% chord_data$notes)
        
        # The piano build ###
        p_w_chord <- p  + 
                geom_text(
                        data = k_data %>% filter(key.color == "white") %>% filter(notes %in% chord_data$notes),
                        color = "black",
                        aes(label = notes),
                        angle = 0, nudge_y = 0.1 ) + 
                geom_text(
                        data = k_data %>% filter(key.color == "black", str_detect(notes, "#$")) %>% filter(notes %in% chord_data$notes),
                        color = "white",
                        aes(label = notes),
                        angle = 0, nudge_y = 0.125
                ) +
                geom_text(
                        data = k_data %>% filter(key.color == "black", str_detect(notes, "b$")) %>% filter(notes %in% chord_data$notes),
                        color = "white",
                        aes(label = notes),
                        angle = 0, nudge_y = 0.075
                ) + 
                geom_col( 
                        data = visual_data %>% filter(key.color == "black"),
                        aes(x = viz.x, y = viz.y),
                        fill = "purple", color = "black",
                        width = 0.75 ) + 
                geom_col( 
                        data = visual_data %>% filter(key.color == "white"),
                        aes(x = viz.x, y = viz.y),
                        fill = "purple", color = "black",
                        width = 1 )
        
        # Rearrange the plot layers to optimize visibility
        p_w_chord$layers <- c(
                p_w_chord$layers[1], p_w_chord$layers[7], 
                p_w_chord$layers[2], p_w_chord$layers[6], 
                p_w_chord$layers[3], p_w_chord$layers[4], 
                p_w_chord$layers[5])
        
        # Print the figure
        print(p_w_chord)
        
}
