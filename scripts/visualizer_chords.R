require(tidyverse)
require(googlesheets4)
require(googledrive)

display_notes <- function(chrd = NULL, notes_lst, octave_section = 5){
        
        
        # Load data for piano figure ----
        if (exists("key_data")){ 
                
        } else {
                key_data <<- read_sheet("https://docs.google.com/spreadsheets/d/1V6R8VAWCpLNPeC5WWSui4tm1cTzi3LgQB1_-kA_cwuU/edit?usp=sharing", sheet = "key_data_88_keys")
        }        
        k_data <- 
                key_data  %>% 
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
        
        #----
        if (is.null(chrd)){
                visual_data <- k_data %>% 
                        filter(octave %in% octave_section) %>% 
                        filter(notes %in% notes_lst)
                
                p_w_chord <- # The piano build ####
                p + 
                        geom_text(
                                data = k_data %>% filter(key.color == "white") %>% filter(notes %in% notes_lst),
                                color = "black",
                                aes(label = notes),
                                angle = 0, nudge_y = 0.1 ) + 
                        geom_text(
                                data = k_data %>% filter(key.color == "black", str_detect(notes, "#$")) %>% filter(notes %in% notes_lst),
                                color = "white",
                                aes(label = notes),
                                angle = 0, nudge_y = 0.125
                        ) +
                        geom_text(
                                data = k_data %>% filter(key.color == "black", str_detect(notes, "b$")) %>% filter(notes %in% notes_lst),
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
                
                # Rearrange the plot layers to optimize visibility ####
                p_w_chord$layers <- c(p_w_chord$layers[1], p_w_chord$layers[7], p_w_chord$layers[2], p_w_chord$layers[6], p_w_chord$layers[3], p_w_chord$layers[4], p_w_chord$layers[5])
                
                # Print the figure
                p_w_chord
                
        } else{
                if (exists("chord_data_raw")){ 
                        
                } else {
                        chord_data_raw <<- read_sheet("https://docs.google.com/spreadsheets/d/1V6R8VAWCpLNPeC5WWSui4tm1cTzi3LgQB1_-kA_cwuU/edit?usp=sharing", sheet = "chord_data")
                }
                chord_data <-  chord_data_raw %>% filter(key_chord_short %in% chrd) %>% 
                        select(key_chord_short, notes) 
                
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
                p_w_chord
                
                
        }
}

# Test out the function
# notes_lst <- c("A", "C#", "E")
# chrd <- "Bm"
# display_notes(chrd = chrd, octave_section = 1:3)
# display_notes(notes_lst = notes_lst)
# # The following produces a series of combined figures for  a list of chords
# chrds <- c("Cm", "Bm", "G", "F")
# plist = lapply(chrds, function(x) {display_notes(chrd = x)})
# grid.arrange(grobs = plist)
