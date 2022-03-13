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
library(shinyscreenshot)
library(ggpubr)

server <- function(input, output, session) {
  
  observeEvent(input$s, {
    screenshot()
  })
  
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
      
  })
  
  df_subset_bucket <- reactive({
    buckets <- selected_occurrence()

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

    if(row %in% selected_occurrence()) return()
    new_rows <- c(row, selected_occurrence())
    selected_occurrence(new_rows)
  })
  
  observeEvent(event_data("plotly_click", source = "D"),{
    row <- event_data("plotly_click", source = "D")$x

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
      plot_data, x = ~n, y = ~song_occurance_bucket, type = 'bar', source = "O", orientation = 'h', marker = list(color = ~current_color),
      hovertemplate = paste("Counts :", plot_data$n, "<extra></extra>") #text = ~n 
    ) %>%
      config(displayModeBar = FALSE) %>%
      layout(
             title = NULL,
             xaxis = list(title = sprintf("<i>%s</i>", "Counts"), range = c(0,1600), titlefont = list(size = 15), tickfont = list(size = 11)), 
             yaxis = list(title = sprintf("<i>%s</i>", "Groups of occurrance"), titlefont = list(size = 15), tickfont = list(size = 11))) 
  })
  

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

    plot <- ggplot(data = plot_data, aes(x = released_decade, y = n, text = paste("Counts :",n))) +
      geom_point(color = "#7da7ca", size = 3) +
      geom_bar(stat = 'identity', fill = "steelblue", width = 0.5) +
      theme_minimal() +
      ggtitle(NULL) +
      xlab("Decade") +
      scale_x_continuous(breaks = plot_data$released_decade) +
      ylab("Counts") +
      theme(
        axis.title.x = element_text(color = "grey40", size = 11, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 11, face = "italic"),
      )
    

    ggplotly(plot, source = "D", tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      add_trace(x = plot_data$released_decade, y = plot_data$n ,mode = "markers", marker = list(color = plot_data$current_color, size = 13), hoverinfo = "none")
                
  })
  
  output$gender <- renderPlot({
    shiny::validate(
      need(nrow(df_selected())>0, "Oops, no songs were found with those filters!")
    )
    
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
            axis.title.x = element_text(color = "grey40", size = 15, face = "italic"),
            legend.position="none") +
      ylab("Percentage") +
      scale_y_continuous(labels = scales::percent_format(scale = 1)) +
      xlab(NULL) +
      coord_flip() +
      ggtitle(NULL)

  })
  
  output$audioplot <-renderPlot({
    shiny::validate(
      need(nrow(df_selected())>1, "You need more songs!")
    )
    tempo_median <- round(median(df_selected()$tempo, na.rm = TRUE),1)
    p1 <-ggplot(df_selected(), aes(x= tempo)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      xlab("tempo (BPM)") +
      geom_vline(aes(xintercept = tempo_median, color = "Median"),size=1, linetype = "dashed") +
      scale_color_manual(name = "", values = c(Median = "#315b7d")) +
      geom_text(x= tempo_median, label= round(tempo_median,1), y = Inf, color="#315b7d", vjust = 2, hjust = -.4, size = 5) +
      theme(
        legend.title = element_text(size = 15, face = "bold"),
        legend.text=element_text(size=14),
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      ) 
    
    median_danceability <- median(df_selected()$danceability, na.rm = TRUE)
    p2 <-ggplot(df_selected(), aes(x= danceability)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      geom_vline(aes(xintercept = median_danceability),size=1, linetype = "dashed", color = "#315b7d") +
      geom_text(x= median_danceability, label= round(median_danceability,1), y = Inf, color="#315b7d", vjust = 2, hjust = -.4, size = 5) +
      theme(
        legend.title = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      ) 
    
    median_energy <- median(df_selected()$energy, na.rm = TRUE)
    p3 <-ggplot(df_selected(), aes(x= energy)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      geom_text(x= median_energy, label= round(median_energy,1), y = Inf, color="#315b7d", vjust = 2, hjust = -.4, size = 5) +
      theme_minimal() +
      ylab("Counts") +
      geom_vline(xintercept = median_energy,size=1, linetype = "dashed", col = "#315b7d") +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
    
    median_valence <- median(df_selected()$valence, na.rm = TRUE)
    p4 <-ggplot(df_selected(), aes(x= valence)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      geom_vline(xintercept = median_valence,size=1, linetype = "dashed", col = "#315b7d") +
      geom_text(x= median_valence, label= round(median_valence,1), y = Inf, color="#315b7d", vjust = 2, hjust = -.4, size = 5) +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
  
    median_loudness <- median(df_selected()$loudness, na.rm = TRUE)
    p5 <-ggplot(df_selected(), aes(x= loudness)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      xlab("loudness (DB)") +
      geom_vline(xintercept = median_loudness,size=1, linetype = "dashed", col = "#315b7d") +
      geom_text(x= median_loudness, label= round(median_loudness,1), y = Inf, color="#315b7d", vjust = 2, hjust = -.4, size = 5) +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
    
    median_instrumentalness <- median(df_selected()$instrumentalness, na.rm = TRUE)
    p6 <-ggplot(df_selected(), aes(x= instrumentalness)) +
      geom_histogram(aes(y=..count..),alpha=.5, fill = 'steelblue', color = '#7da7ca') +
      theme_minimal() +
      ylab("Counts") +
      geom_vline(xintercept = median_instrumentalness,size=1, linetype = "dashed", col = "#315b7d") +
      geom_text(x= median_instrumentalness, label= round(median_instrumentalness,1), y = Inf, color="#315b7d", vjust = 2, hjust = -.4, size = 5) +
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
      )
    
    p1_legend <- get_legend(p1)
    grid.arrange(arrangeGrob(p1 + theme(legend.position = "none"),p2,p3,p4,p5,p6, nrow = 2, ncol = 3), p1_legend, ncol = 2, widths=c(15, 1))

  })
  
  output$audiocorr <-renderPlot({
  
  shiny::validate(
    need(nrow(df_selected())>1, "You need more songs!")
  )
    
  corr <- cor(df_selected()[audio_features], use = "complete.obs") #[,audio_features]
  ggcorrplot(corr, ggtheme = ggplot2::theme_minimal,
             type = "upper", outline.col = "white", lab = TRUE, #show.diag = TRUE, #method = 'circle',
             colors = c("#6D9EC1", "white", "#E46726")) +
    theme(
    axis.text.x = element_text(color = "grey40", size = 13, face = "italic"),
    axis.text.y = element_text(color = "grey40", size = 13, face = "italic"),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    )
  })
  
  output$country <-renderPlot({
    
    shiny::validate(
      need(nrow(df_selected())>0, "Oops, no songs were found with those filters!")
    )
    
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
             palette = brewer.pal(n = 9, name = "YlGnBu")[3:9],
             sorting = "descending",                       # Sort value in descending order
             add = "segments",                             # Add segments from y = 0 to dots
             rotate = TRUE,                                # Rotate vertically
             ylab = "Counts", 
             xlab = "",
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
    legend.text=element_text(size=16),
    legend.title=element_blank(),
    axis.title.x = element_text(color = "grey40", size = 15, face = "italic"),
    plot.title = element_text(color = "grey30", size = 18, hjust = -.4, face = "bold"),
    ) +
    ggtitle(NULL)+
    guides(color = guide_legend(label.position = "bottom")) 
  })
  
  output$genre <-renderPlot({
    shiny::validate(
      need(nrow(df_selected())>0, "Oops, no songs were found with those filters!")
    )
    
    df_genre <- df %>%
      count(genre_groups, name = 'Counts') %>%
      mutate(proportion =  Counts/sum(Counts)) %>%
      mutate(only_one = ifelse(proportion < .008, TRUE, FALSE)) %>%
      mutate(genre_groups = ifelse(only_one,"Others",genre_groups)) %>%
      group_by(genre_groups) %>%
      summarize(Counts = sum(Counts)) 
    
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
      ggtitle(NULL) 

  })

  
  output$features <- renderUI({
    corr <- cor(df_selected()[audio_features], use = "complete.obs")
    diag(corr) <- 0
    corr <- abs(corr)
    feature <- which(corr == max(corr), arr.ind = TRUE) %>% rownames()

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
      theme(
        axis.title.x = element_text(color = "grey40", size = 14, face = "italic"),
        axis.title.y = element_text(color = "grey40", size = 14, face = "italic"),
        axis.text.y = element_text(size = 11), 
        axis.text.x = element_text(size = 11), 
      )
    
  })
  
  output$table = DT::renderDataTable({
    datatable(df_selected()[c("artist","title","released_date","genre_groups","artist_country_name")], 
              colnames=c("Artist", "Song", "Released date", "Genre", "Country"),
              options = list("pageLength" = 50, scrollY = "660px"))
  })
  
  
}

