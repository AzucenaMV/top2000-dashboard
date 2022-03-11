#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggcorrplot)
library(shinyWidgets)
library(ggpubr)
library(RColorBrewer)
library(treemapify)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$info, {
    showModal(modalDialog(
      title = "Info Box",
      HTML("These filters are using a top-down approach. <br><br> To filter using the graphs click on the bars or dots.<br> To unselect use double click inside the area of the graph."),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  selected_occurrence <- reactiveVal()
  selected_decade <- reactiveVal()
  
  df_subset <- reactive({
    years <- paste(seq(input$years[1], input$years[2]))
    pos1 <- input$top[1]
    pos2 <- input$top[2]
      
    df_subset <- df[rowSums(is.na(df %>% select(all_of(years)))) < length(years),] %>%
      select(all_of(c(non_col_years, years)))
    
    df_subset <- df_subset[apply(df_subset %>% select(all_of(years)), 1, function(x) (any(x >= pos1 & x <= pos2, na.rm= TRUE))),]
    #apply(df_subset[col_years], 1, FUN = min, na.rm = TRUE)
    #apply(df[col_years], 1, function(x) +(any(x >= position[1] & x <= position[2])))
      
  })
  
  df_subset_bucket <- reactive({
    buckets <- selected_occurrence()
    #print(buckets)
    if(is.null(selected_occurrence())){
      df_ <- df_subset() 
    }else{
      df_ <- df_subset() %>%
        filter(song_occurance_bucket %in% buckets)
    }
  })
  
  df_selected <-reactive({
    
    decades <- selected_decade()
    
    if(is.null(selected_decade())){
      df_ <- df_subset_bucket() 
    }else{
      df_ <- df_subset_bucket() %>%
        filter(released_decade %in% decades)
    }
  })

  
  observeEvent(event_data("plotly_click", source = "O"),{
    row <- event_data("plotly_click", source = "O")$y
    #print(row)
    if(row %in% selected_occurrence()) return()
    new_rows <- c(row, selected_occurrence())
    selected_occurrence(new_rows)
  })
  
  observeEvent(event_data("plotly_click", source = "D"),{
    row <- event_data("plotly_click", source = "D")$x
    #print(row)
    if(row %in% selected_decade()) return()
    new_rows <- c(row, selected_decade())
    selected_decade(new_rows)
  })
  
  observeEvent(event_data("plotly_doubleclick", source = "O"), {
    selected_occurrence(NULL)
  })
  
  observeEvent(event_data("plotly_doubleclick", source = "D"), {
    selected_decade(NULL)
  })
  
  output$artist_ocurrance_plot <- renderPlotly({
    
    plot_data <- df_subset() %>%
      count(song_occurance_bucket) %>%
      mutate(song_occurance_bucket = fct_relevel(song_occurance_bucket,
                                                 "1","[2-5]","[6-15]","[16-22]","23"))
    
    if(is.null(selected_occurrence())){
      
      plot_data <- plot_data %>%
        mutate(current_color = "#7da7ca")
    }else{
      plot_data <- plot_data %>%
        mutate(current_color = if_else(song_occurance_bucket %in% selected_occurrence(),'#386890', "#7da7ca"))
    }
    
    plot_ly(
      plot_data, x = ~n, y = ~song_occurance_bucket, type = 'bar', source = "O", orientation = 'h', marker = list(color = ~current_color)
    ) %>%
      config(displayModeBar = FALSE) %>%
      layout(#title = list(text = "Songs by times of occurrance", y = 1.5), 
             title = NULL,
             xaxis = list(title = sprintf("<i>%s</i>", "Counts"), range = c(0,1600), titlefont = list(size = 15), tickfont = list(size = 11)), 
             yaxis = list(title = sprintf("<i>%s</i>", "Groups of occurrance"), titlefont = list(size = 15), tickfont = list(size = 11)))
  })
  

  
  
  #output$artist_occurrance_hover <- event_data("plotly_click")
  output$occurrance_click <- renderPrint({
    event_data("plotly_click", source = "O")
  })
  
  output$decade_click <- renderPrint({
    event_data("plotly_click", source = "D")
  })
  
  output$released_decade <- renderPlotly({
    
    if(is.null(selected_decade())){
      plot_data <- df_subset_bucket() %>%
        count(released_decade) %>%
        mutate(current_color = "#7da7ca")
    }
    else{
      plot_data <- df_subset_bucket() %>%
        count(released_decade) %>%
        mutate(current_color = ifelse(released_decade %in% selected_decade(),'#386890', "#7da7ca"))
    }

    #print(plot_data)  
    plot <- ggplot(data = plot_data, aes(x = released_decade, y = n)) +
      #geom_line(color = 'steelblue') +
      geom_point(color = "#7da7ca", size = 3) +
      geom_bar(stat = 'identity', fill = "steelblue", width = 0.5) +
      theme_minimal() +
      #ggtitle(label = "Songs by decade", subtitle = "") +
      ggtitle(NULL) +
      xlab("Decade") +
      scale_x_continuous(breaks = plot_data$released_decade) +
      ylab("Counts") +
      theme(
        #plot.title = element_text(color = "grey30", size = 14),
        #plot.title.position = "plot",
        axis.title.x = element_text(color = "grey40", size = 11, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 11, face = "italic"),
      )
    
      #scale_x_continuous("released_decade", labels = as.character(released_decade), breaks = released_decade)
    #theme_light()
    ggplotly(plot, source = "D") %>%
      config(displayModeBar = FALSE) %>%
      add_trace(x = plot_data$released_decade, y = plot_data$n ,mode = "markers", marker = list(color = plot_data$current_color, size = 13))
                
    #plot_ly(
    #  plot_data, x = ~song_occurance_bucket, y = ~n, type = 'bar' 
    #)
  })
  
  output$gender <- renderPlot({
    df_gender <- df_selected() %>%
      count(artist_pronoun) %>%
      replace_na(list(artist_pronoun = "na")) %>%
      mutate(artist_pronoun= gsub("/.*","",artist_pronoun)) %>%
      mutate(proportion = round(100*n / sum(n),0), row = 1) %>%
      mutate(artist_pronoun = fct_reorder(artist_pronoun, proportion)) 
    
    ggplot(df_gender, aes(x = row, y = proportion, fill = artist_pronoun)) +
      geom_col(width = 0.1) +
      geom_text(aes(label = paste0(artist_pronoun,"\n",proportion, "%")),
                position = position_stack(vjust = 0.5),
                size = 5) +
      scale_fill_brewer() +
      theme_minimal(base_size = 16)+
      theme(axis.ticks.y = element_blank(), 
            axis.text.y = element_blank(), 
            axis.title.y = element_blank(),
            panel.grid.minor.y=element_blank(),
            panel.grid.major.y=element_blank(),
            #plot.title = element_text(color = "grey30", size = 16, hjust = .07),
            axis.title.x = element_text(color = "grey40", size = 15, face = "italic"),
            #plot.title.position = "plot",
            legend.position="none") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      xlab(NULL) +
      coord_flip() +
      ggtitle(NULL)
      #ggtitle("Percentage of pronouns")
    
      #ggplotly(plot, source = "G")
  })
  
  output$audioplot <-renderPlot({
    #tempo_median <- round(median(df_selected()$tempo, na.rm = TRUE),1)
    shiny::validate(
      need(nrow(df_selected())>1, "You need more songs!")
    )

    p1 <-ggplot(df_selected(), aes(x= tempo)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      xlab("tempo (BPM)") +
      geom_vline(xintercept = median(df_selected()$tempo, na.rm = TRUE),size=1, linetype = "dashed", col = "#315b7d") +
      #geom_text(aes(label = tempo_median), )
      #annotate(x= tempo_median,y=400,label="Holaaa!",vjust=0,geom="label") +
      #geom_text(x= tempo_median, label= tempo_median, y = 400, color="#315b7d", hjust = -1, label.size = 1) +
      #geom_label(aes(tempo_median, 400), label = tempo_median) +
      #annotate(x=6,y=+Inf,label="Target Length",vjust=2,geom="label")
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
    
    p2 <-ggplot(df_selected(), aes(x= danceability)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      geom_vline(xintercept = median(df_selected()$danceability, na.rm = TRUE),size=1, linetype = "dashed", col = "#315b7d") +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
    
    p3 <-ggplot(df_selected(), aes(x= energy)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      geom_vline(xintercept = median(df_selected()$energy, na.rm = TRUE),size=1, linetype = "dashed", col = "#315b7d") +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
    
    p4 <-ggplot(df_selected(), aes(x= valence)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      geom_vline(xintercept = median(df_selected()$valence, na.rm = TRUE),size=1, linetype = "dashed", col = "#315b7d") +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
  
    
    p5 <-ggplot(df_selected(), aes(x= loudness)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      xlab("loudness (DB)") +
      geom_vline(xintercept = median(df_selected()$loudness, na.rm = TRUE),size=1, linetype = "dashed", col = "#315b7d") +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
    
    p6 <-ggplot(df_selected(), aes(x= instrumentalness)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      geom_vline(xintercept = median(df_selected()$instrumentalness, na.rm = TRUE),size=1, linetype = "dashed", col = "#315b7d") +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
    
    grid.arrange(p1,p2,p3,p4,p5,p6, ncol=3 ) #,
                 #top=textGrob("Histograms",gp=gpar(fontsize=16,font=3))
    #)
    
  })
  
  output$audiocorr <-renderPlot({
  
  shiny::validate(
    need(nrow(df_selected())>1, "You need more songs!")
  )
    
  corr <- cor(df_selected()[audio_features], use = "complete.obs")  
  ggcorrplot(corr, hc.order = TRUE, ggtheme = ggplot2::theme_minimal,
             type = "upper", outline.col = "white", lab = TRUE,
             colors = c("#6D9EC1", "white", "#E46726"))
  })
  
  output$country <-renderPlot({
    
  df_country <- df_selected() %>%
    replace_na(list(artist_country_name = "Unknown")) %>%
    count(artist_country_name, continent) %>%
    mutate(only_one = ifelse(n < 10, TRUE, FALSE)) %>%
    mutate(artist_country_name = ifelse(only_one,"Others",artist_country_name)) %>%
    group_by(artist_country_name, continent) %>%
    summarize(sum = sum(n)) %>%
    ungroup %>%
    mutate(key = paste(artist_country_name,continent, sep = "-")) %>%
    mutate(artist_country_name = ifelse(artist_country_name == "Others", paste0(artist_country_name,": ",continent),artist_country_name),
           continent = ifelse(continent == "other", "Unkown", continent))
  
  ggdotchart(df_country, x = "key", y = "sum",
             color = "continent",                                # Color by groups
             #palette = brewer.pal(n = 9, name = "Blues")[3:9], # Custom color palette
             palette = brewer.pal(n = 9, name = "YlGnBu")[3:9],
             sorting = "descending",                       # Sort value in descending order
             add = "segments",                             # Add segments from y = 0 to dots
             rotate = TRUE,                                # Rotate vertically
             ylab = "Counts", 
             xlab = "",
             #group = "continent",                                # Order by groups
             dot.size = 13,                                 # Large dot size
             label = "sum",                        # Add mpg values as dot labels
             font.label = list(color = "gray100", size = 13, 
                               vjust = 0.5),               # Adjust label parameters
             ggtheme = theme_minimal(),                        # ggplot2 theme
             facet_by = "continent"
  ) +
    scale_x_discrete(breaks = df_country$key,
                     labels = df_country$artist_country_name) +
    theme(
    axis.text=element_text(size=16),
    legend.position="right",
    #legend.box = "horizontal",
    legend.text=element_text(size=16),
    legend.title=element_blank(),
    axis.title.x = element_text(color = "grey40", size = 15, face = "italic"),
    plot.title = element_text(color = "grey30", size = 18, hjust = -.4, face = "bold"),
    ) +
    ggtitle(NULL)+
    #ggtitle("Songs by Country and Continent") +
    guides(color = guide_legend(#title.position = "top", 
                                # hjust = 0.5 centres the title horizontally
                                #hjust = -1,
                                label.position = "bottom")) 
  })
  
  output$genre <-renderPlot({
    df_genre <- df_selected() %>%
      count(genre_groups, name = 'Counts')
    
    ggplot(df_genre, aes(area = Counts, fill = Counts, label = paste(genre_groups, Counts, sep = "\n"))) +
      geom_treemap() +
      geom_treemap_text(colour = "white",
                        place = "centre",
                        size = 15) +
      theme(legend.position = "bottom",
            plot.title = element_text(color = "grey30", size = 18, hjust = 0),
            legend.title = element_text(size=15,  face = "italic", color = "grey40"), #change legend title font size
            legend.text = element_text(size=13, color = "grey40")
      ) +
      ggtitle(NULL) #+
      #guide_colourbar(title = "Counts")
      
  
    #scale_fill_continuos(name = "Counts")
    #guides(fill=guide_legend(title="Counts"))
    #ggtitle("Songs by Genre") 
    #scale_fill_brewer(palette = "Blues")
  

  })
  
  output$features <- renderUI({
    corr <- cor(df_selected()[audio_features], use = "complete.obs")
    diag(corr) <- 0
    corr <- abs(corr)
    feature <- which(corr == max(corr), arr.ind = TRUE) %>% rownames()
    #print(feature)
    tagList(
      renderText(
        paste("Relation between ", feature[1], " and ", feature[2])
      )
    )
  })
  
  output$corr_scatter <-renderPlot({
    shiny::validate(
      need(nrow(df_selected())>1, "You need more songs!")
    )
    # Getting the two more correlated
    corr <- cor(df_selected()[audio_features], use = "complete.obs")
    diag(corr) <- 0
    corr <- abs(corr)
    features <- which(corr == max(corr), arr.ind = TRUE) %>% rownames()
  
    ggplot(df_selected(), aes_string(x = features[1], y= features[2])) +
      geom_point(color = "SteelBlue", size = .5) +
      theme_minimal() +
      xlab(features[1]) +
      ylab(features[2]) +
      ggtitle(NULL) +
      #ggtitle(paste0("Correlation between ",features[1]," and ",features[2])) +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 11), 
        axis.text.x = element_text(size = 11), 
      )
    
  })
  
  
}

