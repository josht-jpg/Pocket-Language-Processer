library(shiny)
library(dplyr)
library(tidytext)
library(ggplot2)
library(purrr)
library(tidyr)
library(stringr)
library(igraph)
library(ggraph)
library(forcats)
library(readtext)
library(textdata)
library(DT)
library(topicmodels)
NRC <- get_sentiments("nrc")


ui <- (fluidPage(
    titlePanel(
        "Pocket Language Processer"
    ),
    h3("By Joshua Taylor"),
    sidebarLayout(
        sidebarPanel(
            fileInput("files", "Upload your text files", multiple = TRUE),
            selectInput("analysis", label = "Type of analysis", choices = c("Most common words" = "most_common", "Most common n-grams" = "common_ngrams", "NRC sentiment analysis" = "nrc", "tf-idf" = "td_idf", "tf-idf of n-grams" = "tf_idf_ngram", "Network of bigrams" = "bigrams_network", "Words appearing beside a particular word" = "beside"), selected = "Most Common Words"),
            uiOutput("drop"),
            br(),
            uiOutput("download_button")
        ),
        mainPanel(
            tabsetPanel(
                id = "tabid",
                type = "tabs",
                tabPanel("Plot", uiOutput("as_plot")),
                tabPanel("Table", uiOutput("as_print"))
            )
        )
    ),
    h5("This app is heavily inspired by the wonderful book "),
    uiOutput("guide"),
    br()
))

server <- (function(input, output, session) {
    output$download_button <- renderUI({
        if (input$tabid == "Plot") {
            tagList(
                selectInput("plot_save", "Save plot to file type: ", choices = c("jpeg", "png"), selected = isolate(input$plot_save) %||% "jpeg"),
                downloadButton("download_plot", "Download Plot"),
                br()
            )
        }
        else {
            tagList(
                downloadButton("download_table", "Download Table as CSV"),
                br()
            )
        }
    })
    
    output$as_print <- renderUI({
        if (input$analysis == "td_idf" || input$analysis == "tf_idf_ngram") {
            if (length(input$files[, 1]) == 1) {
                tagList("\nTwo or more files required")
            }
            else {
                tagList(dataTableOutput("table"))
            }
        }
        else {
            tagList(dataTableOutput("table"))
        }
    })
    
    output$as_plot <- renderUI({
        if (input$analysis == "td_idf" || input$analysis == "tf_idf_ngram") {
            if (length(input$files[, 1]) == 1) {
                tagList("\nTwo or more files required")
            }
            else {
                tagList(plotOutput("hist"))
            }
        }
        else {
            tagList(plotOutput("hist"))
        }
    })
    
    
    
    output$plot_print <- renderUI({
        tabsetPanel(
            id = "tabid",
            type = "tabs",
            tabPanel("Plot", uiOutput("as_plot")),
            if (input$analysis != "bigrams_network") tabPanel("Table", uiOutput("as_print"))
        )
    })
    
    output$guide <- renderUI({
        url <- a("Text Mining with R", href = "https://www.tidytextmining.com/", target = "_blank")
        tagList(url, " by Julia Silge and David Robinson")
    })
    
    load_files <- reactive({
        if (is.null(input$files)) {
            return(NULL)
        }
        
        input_text <- list()
        for (i in 1:length(input$files[, 1])) {
            input_text[i] <- readtext(input$files[[i, "datapath"]])[[2]]
        }
        textdf <- tibble(text = as.character(unlist(input_text)), name = input$files[, 1])
        
        return(textdf)
    })
    
    load_data <- reactive({
        if (is.null(input$files)) {
            return(NULL)
        }
        
        input_text <- list()
        for (i in 1:length(input$files[, 1])) {
            input_text[i] <- readtext(input$files[[i, "datapath"]])[[2]]
        }
        
        textdf <- tibble(line = seq_len(length(input$files[, 1])), text = input_text, name = input$files[, 1])
        
        if ((input$default_stops_common && input$analysis == "most_common")) {
            tidy_input <- textdf %>% unnest_tokens(word, text)
        }
        else if (input$remove_stops_lda && input$analysis == "lda") {
            tidy_input <- textdf %>% unnest_tokens(word, text)
        }
        else {
            tidy_input <- textdf %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words)
        }
        
        return(tidy_input)
    })
    
    load_data_stops <- reactive({
        if (is.null(input$files)) {
            return(NULL)
        }
        
        input_text <- list()
        for (i in 1:length(input$files[, 1])) {
            input_text[i] <- readtext(input$files[[i, "datapath"]])[[2]]
        }
        textdf <- tibble(line = seq_len(length(input$files[, 1])), text = input_text, name = input$files[, 1])
        
        tidy_input <- textdf %>% unnest_tokens(word, text)
        
        tidy_input
    })
    
    load_data_ngrams <- reactive({
        if (!is.null(load_files()) && !is.null(input$ngram) && !is.na(input$ngram) && input$ngram < 7 && input$ngram > 1) {
            if (input$analysis == "bigrams_network" || input$analysis == "beside") {
                ng <- 2
            } else {
                ng <- input$ngram
            }
            
            
            textdf <- load_files()
            
            tidy_input <- textdf %>% unnest_tokens(ngram, text, token = "ngrams", n = ng)
            
            words <- c()
            
            for (i in 1:ng) {
                words[i] <- paste0("word", i)
            }
            
            ngrams_separated <- tidy_input %>%
                separate(ngram, words, sep = " ")
            
            if ((input$default_stops_common_ngram && input$analysis == "common_ngrams")) {
                ngrams_filtered <- ngrams_separated
            }
            else if ((input$default_stops_network && input$analysis == "bigrams_network")) {
                ngrams_filtered <- ngrams_separated
            }
            else {
                if (ng == 2) {
                    if (input$analysis == "beside") {
                        if (is.null(input$remove_stops) || !input$remove_stops) {
                            ngrams_filtered <- ngrams_separated %>%
                                filter(!word1 %in% stop_words$word) %>%
                                filter(!word2 %in% stop_words$word)
                        }
                        else {
                            ngrams_filtered <- ngrams_separated
                        }
                    }
                    else {
                        ngrams_filtered <- ngrams_separated %>%
                            filter(!word1 %in% stop_words$word) %>%
                            filter(!word2 %in% stop_words$word)
                    }
                }
                
                if (ng == 3) {
                    ngrams_filtered <- ngrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word) %>%
                        filter(!word3 %in% stop_words$word)
                }
                
                if (ng == 4) {
                    ngrams_filtered <- ngrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word) %>%
                        filter(!word3 %in% stop_words$word) %>%
                        filter(!word4 %in% stop_words$word)
                }
                
                if (ng == 5) {
                    ngrams_filtered <- ngrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word) %>%
                        filter(!word3 %in% stop_words$word) %>%
                        filter(!word4 %in% stop_words$word) %>%
                        filter(!word5 %in% stop_words$word)
                }
                if (ng == 6) {
                    ngrams_filtered <- ngrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word) %>%
                        filter(!word3 %in% stop_words$word) %>%
                        filter(!word4 %in% stop_words$word) %>%
                        filter(!word5 %in% stop_words$word) %>%
                        filter(!word6 %in% stop_words$word)
                }
            }
            return(ngrams_filtered)
        }
    })
    
    load_data_ngrams_tf <- reactive({
        if (!is.null(load_files()) && !is.null(input$tf_ngram) && !is.na(input$tf_ngram)) {
            textdf <- load_files()
            
            tidy_input <- textdf %>% unnest_tokens(ngram, text, token = "ngrams", n = input$tf_ngram)
            
            words <- c()
            
            for (i in 1:input$tf_ngram) {
                words[i] <- paste0("word", i)
            }
            
            ngrams_separated <- tidy_input %>%
                separate(ngram, words, sep = " ")
            
            if (!input$default_stops_tf_ngram) {
                if (input$tf_ngram == 2) {
                    ngrams_filtered <- ngrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word)
                }
                
                if (input$tf_ngram == 3) {
                    ngrams_filtered <- ngrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word) %>%
                        filter(!word3 %in% stop_words$word)
                }
                
                if (input$tf_ngram == 4) {
                    ngrams_filtered <- ngrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word) %>%
                        filter(!word3 %in% stop_words$word) %>%
                        filter(!word4 %in% stop_words$word)
                }
                
                if (input$tf_ngram == 5) {
                    ngrams_filtered <- ngrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word) %>%
                        filter(!word3 %in% stop_words$word) %>%
                        filter(!word4 %in% stop_words$word) %>%
                        filter(!word5 %in% stop_words$word)
                }
                if (input$tf_ngram == 6) {
                    ngrams_filtered <- ngrams_separated %>%
                        filter(!word1 %in% stop_words$word) %>%
                        filter(!word2 %in% stop_words$word) %>%
                        filter(!word3 %in% stop_words$word) %>%
                        filter(!word4 %in% stop_words$word) %>%
                        filter(!word5 %in% stop_words$word) %>%
                        filter(!word6 %in% stop_words$word)
                }
            }
            else {
                ngrams_filtered <- ngrams_separated
            }
            return(ngrams_filtered)
        }
    })
    
    output$common_render <- renderUI({
        if (input$tabid == "Plot") numericInput("common_number", "Number of words dipslayed", value = isolate(input$common_number) %||% 5)
    })
    
    output$combine_render <- renderUI({
        if (input$tabid == "Plot") radioButtons("combine", "Combine files?", choiceNames = c("Across all files", "For each file"), choiceValues = c("all", "each"), selected = isolate(input$combine) %||% "all")
    })
    
    output$flip_common_render <- renderUI({
        if (input$tabid == "Plot") checkboxInput("flip_common", "Flip coordinates", value = isolate(input$flip_common) %||% FALSE)
    })
    
    
    output$common_number_ngram_render <- renderUI({
        if (input$tabid == "Plot") numericInput("common_number_ngram", "Number of n-grams dipslayed", value = isolate(input$common_number_ngram) %||% 5)
    })
    
    output$combine_common_ngram_render <- renderUI({
        if (input$tabid == "Plot") radioButtons("combine_common_ngram", "Combine files?", choiceNames = c("Across all files", "For each file"), choiceValues = c("all", "each"), selected = isolate(input$combine_common_ngram) %||% "all")
    })
    
    output$flip_common_ngrams_render <- renderUI({
        if (input$tabid == "Plot") checkboxInput("flip_common_ngrams", "Flip coordinates", value = isolate(input$flip_common_ngrams) %||% FALSE)
    })
    
    output$flip_tf_render <- renderUI({
        if (input$tabid == "Plot") checkboxInput("flip_tf", "Flip coordinates", value = isolate(input$flip_tf) %||% TRUE)
    })
    
    output$flip_tf_common_render <- renderUI({
        if (input$tabid == "Plot") checkboxInput("flip_tf_common", "Flip coordinates", value = isolate(input$flip_tf_common) %||% TRUE)
    })
    
    output$flip_beside_render <- renderUI({
        if (input$tabid == "Plot") checkboxInput("flip_beside", "Flip coordinates", value = isolate(input$flip_beside) %||% FALSE)
    })
    
    
    
    output$drop <- renderUI({
        parameter_tabs <- tagList(
            tags$style("#params { display:none; }"),
            tabsetPanel(
                id = "params",
                tabPanel(
                    "most_common",
                    uiOutput("common_render"),
                    textInput("stop_words_common", "Add your own stop words. Please separate each word with a space.", value = isolate(input$stop_words_common) %||% ""),
                    uiOutput("combine_render"),
                    checkboxInput("default_stops_common", "Include default stop words in computation (i.e. \"the\", \"and\", \"or\")", value = isolate(input$default_stops_common) %||% FALSE),
                    uiOutput("flip_common_render")
                ),
                tabPanel(
                    "common_ngrams",
                    numericInput("ngram", "n (maximum value is 6)", value = 2, min = 2, max = 6),
                    uiOutput("common_number_ngram_render"),
                    textInput("stop_words_ngram_common", "Add your own stop words. Please separate each word with a space.", value = isolate(input$stop_words_ngram_common) %||% ""),
                    uiOutput("combine_common_ngram_render"),
                    checkboxInput("default_stops_common_ngram", "Include default stop words in computation (i.e. \"the\", \"and\", \"or\")", value = isolate(input$default_stops_common_ngram) %||% FALSE),
                    uiOutput("flip_common_ngrams_render"),
                    uiOutput("link_common_ngrams")
                ),
                tabPanel(
                    "nrc",
                    selectInput("sentiment_type", "Select an emotion", choices = c("Joy", "Trust", "Surprise", "Anticipation", "Positive", "Negative", "Sadness", "Anger", "Disgust", "Fear"), selected = isolate(input$sentiment_type) %||% "Joy"),
                    # uiOutput("max_display"),
                    numericInput("nrc_number", "Number of words dipslayed", value = isolate(input$nrc_number) %||% 5, min = 1),
                    uiOutput("connected_stops"),
                    radioButtons("combine_nrc", "Combine files?", choiceNames = c("Across all files", "For each file"), choiceValues = c("all", "each"), selected = isolate(input$combine_nrc) %||% "all"),
                    checkboxInput("flip_nrc", "Flip coordinates", value = isolate(input$flip_nrc) %||% FALSE),
                    uiOutput("nrc_link"),
                    uiOutput("nrc_citation")
                ),
                tabPanel(
                    "cor",
                    selectInput("cor_file1", label = "Select file 1", choices = input$files[, 1]),
                    selectInput("cor_file2", label = "Select file 2", choices = input$files[, 1], selected = input$files[2, 1])
                ),
                tabPanel(
                    "td_idf",
                    textInput("stop_words", "Add your own stop words. Please separate each word with a space.", value = isolate(input$stop_words) %||% ""),
                    checkboxInput("default_stops_td", "Include default stop words in computation (i.e. \"the\", \"and\", \"or\")", value = isolate(input$default_stops_td) %||% FALSE),
                    uiOutput("flip_tf_render"),
                    uiOutput("link")
                ),
                tabPanel(
                    "tf_idf_ngram",
                    numericInput("tf_ngram", "n", value = 2, min = 2, max = 6),
                    textInput("stop_words_ngram", "Add your own stop words. Please separate each word with a space.", value = isolate(input$stop_words_ngram) %||% ""),
                    checkboxInput("default_stops_tf_ngram", "Include default stop words in computation (i.e. \"the\", \"and\", \"or\")", value = isolate(input$default_stops_tf_ngram) %||% FALSE),
                    uiOutput("flip_tf_common_render"),
                    uiOutput("link_ngram"),
                    uiOutput("tf_link_ngram")
                ),
                tabPanel(
                    "bigrams_network",
                    uiOutput("min_connections"),
                    textInput("stop_words_network", "Add your own stop words. Please separate each word with a space.", value = isolate(input$stop_words_network) %||% ""),
                    checkboxInput("default_stops_network", "Include default stop words in computation (i.e. \"the\", \"and\", \"or\")", value = isolate(input$default_stops_network) %||% FALSE),
                    uiOutput("link_network")
                ),
                tabPanel(
                    "beside",
                    textInput("chosen_word", "Specify a word within your text file(s)", value = isolate(input$chosen_word) %||% ""),
                    textInput("stop_words_beside", "Add your own stop words. Please separate each word with a space.", value = isolate(input$stop_words_beside) %||% ""),
                    checkboxInput("remove_stops", "Include default stop words in computation (i.e. \"the\", \"and\", \"or\")"),
                    # Click here for a list of stop words automatically used
                    radioButtons("before_after", "", choiceNames = c("Only compute words appearing after the specified word", "Only compute words appearing before the specified word", "Compute either before or after"), choiceValues = c("after", "before", "both"), selected = isolate(input$before_after) %||% "after"),
                    uiOutput("flip_beside_render")
                ),
                tabPanel(
                    "lda",
                    numericInput("topics", "Number of topics", value = isolate(input$topics) %||% 4, min = 1),
                    numericInput("lda_number", "Number of words dipslayed", value = isolate(input$lda_number) %||% 8, min = 1),
                    textInput("stop_words_lda", "Add your own stop words. Please separate each word with a space.", value = isolate(input$stop_words_lda) %||% ""),
                    checkboxInput("remove_stops_lda", "Include default stop words in computation (i.e. \"the\", \"and\", \"or\")", value = isolate(input$remove_stops_lda) %||% FALSE),
                ),
                tabPanel("tfd")
            )
        )
    })
    
    output$connected_stops <- renderUI({
        tagList(textInput("all_stops", "Add your own stop words. Please separate each word with a space.", value = isolate(input$all_stops) %||% ""))
    })
    
    output$nrc_link <- renderUI({
        url <- a("Learn more about the NRC lexicon", href = "http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm", target = "_blank")
        tagList(url)
    })
    
    output$link_common_ngrams <- renderUI({
        url <- a("What's an n-gram?", href = "https://en.wikipedia.org/wiki/N-gram", target = "_blank")
        tagList(url)
    })
    
    output$nrc_citation <- renderUI({
        url <- a("Citations for the NRC lexicon", href = "https://github.com/josht-jpg/The_Autodidact/blob/master/NRC_citations.pdf", target = "_blank")
        tagList(url)
    })
    
    output$tf_link_ngram <- renderUI({
        url <- a("What's an n-gram?", href = "https://en.wikipedia.org/wiki/N-gram", target = "_blank")
        tagList(url)
    })
    
    output$min_connections <- renderUI({
        if (!is.null(load_data_ngrams())) {
            count_bigrams <- load_data_ngrams() %>%
                count(word1, word2, sort = TRUE)
            if (nrow(count_bigrams) > 5) {
                sub <- 5
            } else {
                sub <- 0
            }
            
            tagList(numericInput("connections", "Number of connections", value = isolate(input$connections) %||% min(50, nrow(count_bigrams) - sub), min = 1, max = nrow(count_bigrams)))
        }
        else {
            tagList(numericInput("connections", "Number of connections", value = isolate(input$connections) %||% 50, min = 1))
        }
    })
    
    output$link_network <- renderUI({
        url <- a("What's a bigram network?", href = "https://www.tidytextmining.com/ngrams.html#visualizing-a-network-of-bigrams-with-ggraph", target = "_blank")
        tagList(url)
    })
    
    output$link <- renderUI({
        url <- a("What's tf-idf?", href = "https://monkeylearn.com/blog/what-is-tf-idf/")
        tagList(url)
    })
    
    output$link_ngram <- renderUI({
        url <- a("What's tf-idf?", href = "https://monkeylearn.com/blog/what-is-tf-idf/")
        tagList(url)
    })
    
    observeEvent(input$analysis, {
        updateTabsetPanel(session, "params", selected = input$analysis)
    })
    
    observeEvent(input$files, {
        updateTabsetPanel(session, "params", selected = input$analysis)
    })
    
    most_common_react <- reactive({
        if ("most_common" %in% input$analysis && !is.null(input$common_number) && !is.na(input$common_number)) {
            if (!is.null(load_data())) {
                if (!is.null(input$combine) && input$combine == "all" || input$tabid == "Table") {
                    top_words <- load_data() %>%
                        count(name, word, sort = TRUE)
                    
                    if (!input$stop_words_common == "") {
                        chosen_stop_words <- tibble(word = strsplit(input$stop_words_common, " ")[[1]])
                        
                        top_words <- anti_join(top_words, chosen_stop_words,
                                               by = "word"
                        )
                    }
                    
                    if (length(input$files[, 1]) == 1) {
                        title <- paste("Most common words in", input$files[1, 1])
                    }
                    else {
                        title <- "Most common words across "
                        
                        files <- 1:(length(input$files[, 1]) - 1)
                        
                        if (length(files) > 1) {
                            for (i in files) {
                                title <- paste0(title, input$files[i, 1], ", ")
                            }
                            title <- paste0(title, "and ", input$files[i + 1, 1])
                        }
                        else {
                            title <- paste0(title, input$files[1, 1], " and ", input$files[2, 1])
                        }
                    }
                    if (input$tabid == "Plot") {
                        add1 <- 0
                        if (input$default_stops_common) add1 <- add1 + 1
                        p <- plot(top_words %>%
                                      slice(1:input$common_number + 1) %>%
                                      mutate(word = reorder(word, n)) %>%
                                      ggplot(aes(word, n, fill = I("steelblue3"))) +
                                      geom_col() +
                                      xlab(NULL) +
                                      ylab("Number of apperences") +
                                      ggtitle(title) +
                                      theme(plot.title = element_text(hjust = 0.5)) +
                                      if (input$flip_common) coord_flip())
                        
                        return(p)
                    }
                    else {
                        top_words <- top_words %>%
                            mutate(file_name = name) %>%
                            select(-name)
                        
                        return(top_words)
                    }
                }
                
                
                if (!is.null(input$combine) && input$combine == "each") {
                    top_words <- load_data() %>%
                        count(name, word, sort = TRUE) %>%
                        group_by(name)
                    
                    
                    if (!input$stop_words_common == "") {
                        chosen_stop_words <- tibble(word = strsplit(input$stop_words_common, " ")[[1]])
                        
                        top_words <- anti_join(top_words, chosen_stop_words,
                                               by = "word"
                        )
                    }
                    
                    p <- plot(top_words %>%
                                  slice(1:input$common_number) %>%
                                  ggplot(aes(reorder(word, n), n, fill = name)) +
                                  geom_col(show.legend = FALSE) +
                                  labs(x = NULL, y = "Number of appearences") +
                                  facet_wrap(~name, ncol = 2, scales = "free") +
                                  if (input$flip_common) coord_flip())
                    
                    return(p)
                }
            }
        }
    })
    
    nrc_react <- reactive({
        if (!is.null(load_data()) && !is.null(input$nrc_number) && !is.na(input$nrc_number)) {
            print(input$combine_nrc)
            if (input$combine_nrc == "all" || input$tabid == "Table") {
                nrc <- NRC %>%
                    filter(sentiment == tolower(input$sentiment_type))
                
                nrc_data <- load_data() %>%
                    inner_join(nrc) %>%
                    count(name, word, sort = TRUE)
                
                if (!is.null(input$all_stops) && !(input$all_stops == "")) {
                    chosen_stop_words <- tibble(word = strsplit(input$all_stops, " ")[[1]])
                    
                    nrc_data <- anti_join(nrc_data, chosen_stop_words,
                                          by = "word"
                    )
                }
                
                
                top_words <- nrc_data %>% count(name, word, sort = TRUE)
                
                
                title <- paste("Most common words conveying", input$sentiment_type, "within ")
                
                files <- 1:(length(input$files[, 1]) - 1)
                
                if (length(input$files[, 1]) <= 1) {
                    title <- paste0(title, input$files[1, 1])
                } else if (length(files) > 1) {
                    for (i in files) {
                        title <- paste0(title, input$files[i, 1], ", ")
                    }
                    title <- paste0(title, "and ", input$files[i + 1, 1])
                }
                else {
                    title <- paste0(title, input$files[1, 1], " and ", input$files[2, 1])
                }
                
                if (input$tabid == "Plot") {
                    p <- plot(nrc_data %>%
                                  slice(1:input$nrc_number) %>%
                                  mutate(word = reorder(word, n)) %>%
                                  ggplot(aes(word, n, fill = I("steelblue3"))) +
                                  geom_col() +
                                  xlab(NULL) +
                                  ylab("Number of appearences") +
                                  ggtitle(title) +
                                  theme(plot.title = element_text(hjust = 0.5)) +
                                  if (input$flip_nrc) coord_flip())
                    return(p)
                }
                else {
                    top_words <- top_words %>%
                        mutate(file_name = name) %>%
                        select(-name)
                    
                    return(top_words)
                }
            }
            if (input$combine_nrc == "each") {
                nrc <- NRC %>%
                    filter(sentiment == tolower(input$sentiment_type))
                
                nrc_data <- load_data() %>%
                    group_by(name) %>%
                    inner_join(nrc) %>%
                    count(word, sort = TRUE)
                
                if (!input$all_stops == "") {
                    chosen_stop_words <- tibble(word = strsplit(input$all_stops, " ")[[1]])
                    
                    nrc_data <- anti_join(nrc_data, chosen_stop_words,
                                          by = "word"
                    )
                }
                
                title <- paste("Most common words conveying", input$sentiment_type)
                
                
                
                p <- plot(nrc_data %>%
                              slice(1:input$nrc_number) %>%
                              ggplot(aes(reorder(word, n), n, fill = name)) +
                              geom_col(show.legend = FALSE) +
                              labs(x = NULL, y = "Number of appearences") +
                              ggtitle(title) +
                              theme(plot.title = element_text(hjust = 0.5)) +
                              facet_wrap(~name, ncol = 2, scales = "free") +
                              if (input$flip_nrc) coord_flip())
                
                return(p)
            }
        }
    })
    
    frequency_react <- reactive({
        if (!is.null(load_data_stops())) {
            file_words <- load_data_stops() %>% count(name, word, sort = TRUE)
            
            total_words <- file_words %>%
                group_by(name) %>%
                summarize(total = sum(n))
            
            file_words <- left_join(file_words, total_words)
        }
    })
    
    frequency_react_tf <- reactive({
        if (!is.null(load_data())) {
            file_words <- load_data() %>% count(name, word, sort = TRUE)
            
            total_words <- file_words %>%
                group_by(name) %>%
                summarize(total = sum(n))
            
            file_words <- left_join(file_words, total_words)
        }
    })
    
    frequency_ngram_react <- reactive({
        if (!is.null(load_data_ngrams_tf())) {
            words <- c()
            for (i in 1:input$tf_ngram) {
                words[i] <- paste0("word", i)
            }
            
            ngrams_united <- load_data_ngrams_tf() %>%
                unite(ngram, words, sep = " ")
            
            file_words <- ngrams_united %>% count(name, ngram, sort = TRUE)
            
            total_words <- file_words %>%
                group_by(name) %>%
                summarize(total = sum(n))
            
            file_words <- left_join(file_words, total_words)
        }
    })
    
    term_frequency_react <- reactive({
        if (!is.null(load_data_stops())) {
            file_words <- frequency_react()
            
            plot(ggplot(file_words, aes(n / total, fill = name)) +
                     geom_histogram(show.legend = FALSE) +
                     xlim(NA, max(file_words$n / file_words$total)) +
                     facet_wrap(~name, ncol = 2, scales = "free_y"))
        }
    })
    
    tf_idf_react <- reactive({
        if (length(input$files[, 1]) >= 2) {
            if (!input$default_stops_td) {
                if (!is.null(load_data())) {
                    file_words <- load_data()
                    
                    
                    
                    file_words <- file_words %>%
                        count(name, word, sort = TRUE)
                    
                    if (!input$stop_words == "") {
                        chosen_stop_words <- tibble(word = strsplit(input$stop_words, " ")[[1]])
                        
                        file_words <- anti_join(file_words, chosen_stop_words,
                                                by = "word"
                        )
                    }
                    
                    
                    file_words <- file_words %>%
                        bind_tf_idf(word, name, n) %>%
                        mutate(word = fct_reorder(word, tf_idf))
                    if (!is.null(input$tabid) && input$tabid == "Plot" && !is.null(input$flip_tf)) {
                        p <- plot(file_words %>%
                                      group_by(name) %>%
                                      top_n(10, tf_idf) %>%
                                      ungroup() %>%
                                      mutate(word = reorder(word, tf_idf)) %>%
                                      ggplot(aes(word, tf_idf, fill = name)) +
                                      geom_col(show.legend = FALSE) +
                                      labs(x = NULL, y = "tf-idf") +
                                      facet_wrap(~name, ncol = 2, scales = "free") +
                                      if (input$flip_tf) coord_flip())
                        
                        return(p)
                    }
                    else {
                        file_words <- file_words %>%
                            mutate(file_name = name) %>%
                            select(-name)
                    }
                }
            }
            else {
                if (!is.null(load_data_stops())) {
                    file_words <- load_data_stops()
                    
                    
                    file_words <- file_words %>%
                        count(name, word, sort = TRUE)
                    
                    if (!input$stop_words == "") {
                        chosen_stop_words <- tibble(word = strsplit(input$stop_words, " ")[[1]])
                        
                        file_words <- anti_join(file_words, chosen_stop_words,
                                                by = "word"
                        )
                    }
                    
                    
                    file_words <- file_words %>%
                        bind_tf_idf(word, name, n) %>%
                        mutate(word = fct_reorder(word, tf_idf))
                    if (!is.null(input$tabid) && input$tabid == "Plot" && !is.null(input$flip_tf)) {
                        p <- plot(file_words %>%
                                      group_by(name) %>%
                                      top_n(10, tf_idf) %>%
                                      ungroup() %>%
                                      mutate(word = reorder(word, tf_idf)) %>%
                                      ggplot(aes(word, tf_idf, fill = name)) +
                                      geom_col(show.legend = FALSE) +
                                      labs(x = NULL, y = "tf-idf") +
                                      facet_wrap(~name, ncol = 2, scales = "free") +
                                      if (input$flip_tf) coord_flip())
                        
                        return(p)
                    }
                    else {
                        file_words <- file_words %>%
                            mutate(file_name = name) %>%
                            select(-name)
                    }
                }
            }
        }
        else {
            return("Two or more files required")
        }
    })
    
    cor_react <- reactive({
        if ("cor" %in% input$analysis) {
            if (!is.null(load_data())) {
                chosen_files <- load_data()[load_data()$name == input$cor_file1 || load_data()$name == input$cor_file2, ]
                
                frequency <- chosen_files %>%
                    mutate(word = str_extract(word, "[a-z']+")) %>%
                    count(name, word) %>%
                    group_by(name) %>%
                    mutate(proportion = n / sum(n)) %>%
                    select(-n) %>%
                    spread(name, proportion)
                
                cor.test(x = as.numeric(frequency[input$cor_file1]), y = as.numeric(frequency[input$cor_file2]))
            }
        }
    })
    
    common_n_gram_reactive <- reactive({
        if (!is.null(load_data_ngrams()) && !is.null(input$common_number_ngram) && !is.na(input$common_number_ngram)) {
            if (input$combine_common_ngram == "all" || input$tabid == "Table") {
                words <- c()
                for (i in 1:input$ngram) {
                    words[i] <- paste0("word", i)
                }
                
                file_words <- load_data_ngrams()
                
                if (!input$stop_words_ngram_common == "") {
                    for (i in 1:input$ngram) {
                        chosen_stop_words <- tibble(word = strsplit(input$stop_words_ngram_common, " ")[[1]])
                        
                        names(chosen_stop_words)[names(chosen_stop_words) == "word"] <- words[i]
                        
                        file_words <- anti_join(file_words, chosen_stop_words, by = words[i])
                    }
                }
                
                if (length(input$files[, 1]) == 1) {
                    title <- paste("Most common n-grams in", input$files[1, 1])
                }
                else {
                    title <- "Most common n-grams across "
                    
                    files <- 1:(length(input$files[, 1]) - 1)
                    
                    if (length(files) > 1) {
                        for (i in files) {
                            title <- paste0(title, input$files[i, 1], ", ")
                        }
                        title <- paste0(title, "and ", input$files[i + 1, 1])
                    }
                    else {
                        title <- paste0(title, input$files[1, 1], " and ", input$files[2, 1])
                    }
                }
                
                add1 <- 0
                if (!input$default_stops_common_ngram) add1 <- add1 + 1
                
                if (input$ngram == 2) {
                    ngram_counts <- file_words %>%
                        count(name, word1, word2, sort = TRUE)
                    
                    if (input$tabid == "Plot") {
                        p <- plot(ngram_counts[1:input$common_number_ngram + add1, ] %>%
                                      mutate(ngram = reorder(paste(word1, word2), n)) %>%
                                      ggplot(aes(ngram, n, fill = I("steelblue3"))) +
                                      geom_col() +
                                      xlab(NULL) +
                                      ggtitle(title) +
                                      theme(plot.title = element_text(hjust = 0.5)) +
                                      if (input$flip_common_ngrams) coord_flip())
                        
                        return(p)
                    }
                    else {
                        ngram_counts <- ngram_counts %>%
                            unite(bigram, c(word1, word2), sep = " ") %>%
                            mutate(file_name = name) %>%
                            select(-name)
                        
                        return(ngram_counts)
                    }
                }
                else if (input$ngram == 3) {
                    ngram_counts <- file_words %>%
                        count(name, word1, word2, word3, sort = TRUE)
                    if (input$tabid == "Plot") {
                        p <- plot(ngram_counts[1:input$common_number_ngram + add1, ] %>%
                                      mutate(ngram = reorder(paste(word1, word2, word3), n)) %>%
                                      ggplot(aes(ngram, n, fill = I("steelblue3"))) +
                                      geom_col() +
                                      xlab(NULL) +
                                      ggtitle(title) +
                                      theme(plot.title = element_text(hjust = 0.5)) +
                                      if (input$flip_common_ngrams) coord_flip())
                        
                        return(p)
                    }
                    else {
                        ngram_counts <- ngram_counts %>%
                            unite(trigram, c(word1, word2, word3), sep = " ") %>%
                            mutate(file_name = name) %>%
                            select(-name)
                        
                        return(ngram_counts)
                    }
                }
                else if (input$ngram == 4) {
                    ngram_counts <- file_words %>%
                        count(name, word1, word2, word3, word4, sort = TRUE)
                    if (input$tabid == "Plot") {
                        p <- plot(ngram_counts[1:input$common_number_ngram + add1, ] %>%
                                      mutate(ngram = reorder(paste(word1, word2, word3, word4), n)) %>%
                                      ggplot(aes(ngram, n, fill = I("steelblue3"))) +
                                      geom_col() +
                                      xlab(NULL) +
                                      ggtitle(title) +
                                      theme(plot.title = element_text(hjust = 0.5)) +
                                      if (input$flip_common_ngrams) coord_flip())
                        return(p)
                    }
                    else {
                        ngram_counts <- ngram_counts %>%
                            unite("4-gram", c(word1, word2, word3, word4), sep = " ") %>%
                            mutate(file_name = name) %>%
                            select(-name)
                        
                        return(ngram_counts)
                    }
                }
                else if (input$ngram == 5) {
                    ngram_counts <- file_words %>%
                        count(name, word1, word2, word3, word4, word5, sort = TRUE)
                    if (input$tabid == "Plot") {
                        p <- plot(ngram_counts[1:input$common_number_ngram + add1, ] %>%
                                      mutate(ngram = reorder(paste(word1, word2, word3, word4, word5), n)) %>%
                                      ggplot(aes(ngram, n, fill = I("steelblue3"))) +
                                      geom_col() +
                                      xlab(NULL) +
                                      ggtitle(title) +
                                      theme(plot.title = element_text(hjust = 0.5)) +
                                      if (input$flip_common_ngrams) coord_flip())
                        return(p)
                    }
                    else {
                        ngram_counts <- ngram_counts %>%
                            unite("5-gram", c(word1, word2, word3, word4, word5), sep = " ") %>%
                            mutate(file_name = name) %>%
                            select(-name)
                        
                        return(ngram_counts)
                    }
                }
                else if (input$ngram == 6) {
                    ngram_counts <- file_words %>%
                        count(name, word1, word2, word3, word4, word5, word6, sort = TRUE)
                    if (input$tabid == "Plot") {
                        p <- plot(ngram_counts[1:input$common_number_ngram + add1, ] %>%
                                      mutate(ngram = reorder(paste(word1, word2, word2, word3, word4, word5, word6), n)) %>%
                                      ggplot(aes(ngram, n, fill = I("steelblue3"))) +
                                      geom_col() +
                                      xlab(NULL) +
                                      ggtitle(title) +
                                      theme(plot.title = element_text(hjust = 0.5)) +
                                      if (input$flip_common_ngrams) coord_flip())
                        return(p)
                    }
                    else {
                        ngram_counts <- ngram_counts %>%
                            unite("6-gram", c(word1, word2, word3, word4, word5, word6), sep = " ") %>%
                            mutate(file_name = name) %>%
                            select(-name)
                        
                        return(ngram_counts)
                    }
                }
            }
            
            
            else if (input$combine_common_ngram == "each") {
                words <- c()
                for (i in 1:input$ngram) {
                    words[i] <- paste0("word", i)
                }
                
                file_words <- load_data_ngrams()
                
                if (!input$stop_words_ngram_common == "") {
                    for (i in 1:input$ngram) {
                        chosen_stop_words <- tibble(word = strsplit(input$stop_words_ngram_common, " ")[[1]])
                        
                        names(chosen_stop_words)[names(chosen_stop_words) == "word"] <- words[i]
                        
                        file_words <- anti_join(file_words, chosen_stop_words, by = words[i])
                    }
                }
                
                ngrams_united <- file_words %>%
                    group_by(name) %>%
                    unite(ngram, words, sep = " ")
                
                
                file_words <- ngrams_united %>% count(name, ngram, sort = TRUE)
                
                
                
                
                p <- plot(file_words %>%
                              slice(1:input$common_number_ngram) %>%
                              ggplot(aes(reorder(ngram, n), n, fill = name)) +
                              geom_col(show.legend = FALSE) +
                              labs(x = NULL, y = "Number of appearences") +
                              facet_wrap(~name, ncol = 2, scales = "free") +
                              if (input$flip_common_ngrams) coord_flip())
                return(p)
            }
        }
    })
    
    tf_idf_ngram_reactive <- reactive({
        if (!is.null(load_data_ngrams_tf())) {
            file_words <- load_data_ngrams_tf()
            
            words <- c()
            for (i in 1:input$tf_ngram) {
                words[i] <- paste0("word", i)
            }
            
            if (!input$stop_words_ngram == "") {
                for (i in 1:input$tf_ngram) {
                    chosen_stop_words <- tibble(word = strsplit(input$stop_words_ngram, " ")[[1]])
                    
                    names(chosen_stop_words)[names(chosen_stop_words) == "word"] <- words[i]
                    
                    file_words <- anti_join(file_words, chosen_stop_words, by = words[i])
                }
            }
            
            
            
            ngrams_united <- file_words %>%
                unite(ngram, words, sep = " ")
            
            file_words <- ngrams_united %>% count(name, ngram, sort = TRUE)
            
            
            
            
            
            file_words <- file_words %>%
                bind_tf_idf(ngram, name, n) %>%
                mutate(ngram = fct_reorder(ngram, tf_idf))
            
            if (input$tabid == "Plot" && !is.null(input$flip_tf_common)) {
                p <- plot(file_words %>%
                              group_by(name) %>%
                              slice(1:15, tf_idf) %>%
                              ungroup() %>%
                              mutate(ngram = reorder(ngram, tf_idf)) %>%
                              ggplot(aes(ngram, tf_idf, fill = name)) +
                              geom_col(show.legend = FALSE) +
                              labs(x = NULL, y = "tf-idf") +
                              facet_wrap(~name, ncol = 2, scales = "free") +
                              if (input$flip_tf_common) coord_flip())
                
                return(p)
            }
            else {
                file_words <- file_words %>%
                    mutate(file_name = name) %>%
                    select(-name)
            }
        }
    })
    
    
    bigrams_network_reactive <- reactive({
        if (!is.null(load_data_ngrams()) && !is.null(input$connections) && !is.na(input$connections)) {
            count_bigrams <- load_data_ngrams() %>%
                count(word1, word2, sort = TRUE)
            
            
            
            bigrams <- count_bigrams[1:input$connections, ]
            
            set.seed(2020)
            a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
            
            if (input$tabid == "Plot") {
                p <- bigrams %>%
                    graph_from_data_frame() %>%
                    ggraph(layout = "fr") +
                    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
                    geom_node_point(color = "lightblue", size = 5) +
                    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
                    theme_void()
                return(p)
            }
            else {
                count_bigrams <- load_data_ngrams() %>%
                    count(name, word1, word2, sort = TRUE)
                
                if (!input$stop_words_network == "") {
                    chosen_stop_words <- tibble(word1 = strsplit(input$stop_words_network, " ")[[1]])
                    
                    count_bigrams <- anti_join(count_bigrams, chosen_stop_words,
                                               by = "word1"
                    )
                    
                    chosen_stop_words <- tibble(word2 = strsplit(input$stop_words_network, " ")[[1]])
                    
                    count_bigrams <- anti_join(count_bigrams, chosen_stop_words,
                                               by = "word2"
                    )
                }
                
                bigrams <- count_bigrams[1:input$connections, ]
                
                bigrams <- bigrams %>%
                    unite(bigram, c(word1, word2), sep = " ") %>%
                    mutate(file_name = name) %>%
                    select(-name)
                
                
                return(bigrams)
            }
        }
    })
    
    
    beside_reactive <- reactive({
        if (!is.null(load_data_ngrams()) && input$chosen_word != "") {
            if (tolower(input$chosen_word) %in% load_data_ngrams()$word1 || tolower(input$chosen_word) %in% load_data_ngrams()$word2) {
                filtered <- load_data_ngrams()
                
                if (!(input$stop_words_beside == "")) {
                    chosen_stop_words <- tibble(word1 = strsplit(input$stop_words_beside, " ")[[1]])
                    
                    filtered <- anti_join(filtered, chosen_stop_words,
                                          by = "word1"
                    )
                    
                    chosen_stop_words <- tibble(word2 = strsplit(input$stop_words_beside, " ")[[1]])
                    
                    filtered <- anti_join(filtered, chosen_stop_words,
                                          by = "word2"
                    )
                }
                
                if (input$before_after == "after") {
                    filtered <- filtered %>%
                        filter(word1 == tolower(input$chosen_word)) %>%
                        count(name, word2, sort = TRUE)
                    
                    if (nrow(filtered) > 50) {
                        title <- paste0("Showing (50 / ", nrow(filtered), ") most common words appearing after \"", input$chosen_word, "\"")
                    }
                    title <- paste0("Words appearing after \"", input$chosen_word, "\"")
                    if (input$tabid == "Plot") {
                        p <- plot(filtered[1:min(nrow(filtered), 50), ] %>%
                                      mutate(word = reorder(word2, n)) %>%
                                      ggplot(aes(word, n, fill = I("steelblue3"))) +
                                      geom_col() +
                                      xlab(NULL) +
                                      ylab(paste0("Number of appearnces after \"", input$chosen_word, "\"")) +
                                      ggtitle(title) +
                                      theme(plot.title = element_text(hjust = 0.5)) +
                                      if (input$flip_beside) coord_flip())
                        return(p)
                    }
                    else {
                        filtered <- filtered %>%
                            mutate(file_name = name) %>%
                            select(-name)
                        return(filtered)
                    }
                }
                else if (input$before_after == "before") {
                    filtered <- filtered %>%
                        filter(word2 == tolower(input$chosen_word)) %>%
                        count(name, word1, sort = TRUE)
                    
                    if (input$tabid == "Plot") {
                        if (nrow(filtered) > 50) {
                            title <- paste0("Showing (50 / ", nrow(filtered), ") most common words appearing before \"", input$chosen_word, "\"")
                        }
                        
                        title <- paste0("Words appearing before \"", input$chosen_word, "\"")
                        
                        p <- plot(filtered[1:min(nrow(filtered), 50), ] %>%
                                      mutate(word = reorder(word1, n)) %>%
                                      ggplot(aes(word, n, fill = I("steelblue3"))) +
                                      geom_col() +
                                      xlab(NULL) +
                                      ylab(paste0("Number of appearnces before \"", input$chosen_word, "\"")) +
                                      ggtitle(title) +
                                      theme(plot.title = element_text(hjust = 0.5)) +
                                      if (input$flip_beside) coord_flip())
                    }
                    else {
                        filtered <- filtered %>%
                            mutate(file_name = name) %>%
                            select(-name)
                        return(filtered)
                    }
                }
                else {
                    filtered1 <- load_data_ngrams()
                    filtered2 <- load_data_ngrams()
                    
                    if (!input$stop_words_beside == "") {
                        chosen_stop_words1 <- tibble(word1 = strsplit(input$stop_words_beside, " ")[[1]])
                        
                        chosen_stop_words2 <- tibble(word2 = strsplit(input$stop_words_beside, " ")[[1]])
                        
                        filtered1 <- anti_join(filtered1, chosen_stop_words1,
                                               by = "word1"
                        )
                        
                        filtered1 <- anti_join(filtered1, chosen_stop_words2,
                                               by = "word2"
                        )
                        
                        filtered2 <- anti_join(filtered2, chosen_stop_words1,
                                               by = "word1"
                        )
                        
                        filtered2 <- anti_join(filtered2, chosen_stop_words2,
                                               by = "word2"
                        )
                    }
                    
                    filtered1 <- filtered1 %>%
                        filter(word2 == tolower(input$chosen_word)) %>%
                        count(name, word1, sort = TRUE)
                    
                    filtered2 <- filtered2 %>%
                        filter(word1 == tolower(input$chosen_word)) %>%
                        count(name, word2, sort = TRUE)
                    
                    
                    
                    colnames(filtered1)[2] <- "word"
                    colnames(filtered2)[2] <- "word"
                    
                    filtered <- rbind(filtered1, filtered2)
                    
                    filtered <- filtered %>% arrange(desc(n))
                    if (input$tabid == "Plot") {
                        if (nrow(filtered) > 50) {
                            title <- paste0("Showing (50 / ", nrow(filtered), ") most common words appearing either before or after \"", input$chosen_word, "\"")
                        }
                        else {
                            title <- paste0("Most common words appearing either before or after \"", input$chosen_word, "\"")
                        }
                        
                        p <- plot(filtered[1:min(nrow(filtered), 50), ] %>%
                                      mutate(words = reorder(word, n)) %>%
                                      ggplot(aes(words, n, fill = I("steelblue3"))) +
                                      geom_col() +
                                      xlab(NULL) +
                                      ylab(paste0("Number of appearnces either before or after \"", input$chosen_word, "\"")) +
                                      ggtitle(title) +
                                      theme(plot.title = element_text(hjust = 0.5)) +
                                      if (input$flip_beside) coord_flip())
                        return(p)
                    }
                    else {
                        filtered <- filtered %>%
                            mutate(file_name = name) %>%
                            select(-name)
                        return(filtered)
                    }
                }
            }
            return(p)
        }
    })
    
    lda_reactive <- reactive({
        if (!is.null(load_data())) {
            data_groups <- load_data()
            
            if (!input$stop_words_lda == "") {
                chosen_stop_words <- tibble(word = strsplit(input$stop_words_lda, " ")[[1]])
                
                data_groups <- anti_join(data_groups, chosen_stop_words,
                                         by = "word"
                )
            }
            
            data_groups <- data_groups %>%
                group_by(word) %>%
                mutate(word_total = n()) %>%
                ungroup() %>%
                filter(word_total > 10)
            
            
            data_dtm <- data_groups %>%
                count(name, word) %>%
                cast_dtm(name, word, n)
            
            
            data_lda <- LDA(data_dtm, k = input$topics, control = list(seed = 2020))
            
            if (input$tabid == "Plot") {
                p <- data_lda %>%
                    tidy() %>%
                    group_by(topic) %>%
                    top_n(8, beta) %>%
                    ungroup() %>%
                    mutate(term = reorder_within(term, beta, topic)) %>%
                    ggplot(aes(term, beta, fill = factor(topic))) +
                    geom_col(show.legend = FALSE) +
                    facet_wrap(~topic, scales = "free_y") +
                    coord_flip() +
                    scale_x_reordered()
                return(p)
            }
            else {
                return(data_lda %>%
                           tidy() %>%
                           group_by(topic) %>%
                           top_n(8, beta) %>%
                           ungroup() %>%
                           mutate(term = reorder_within(term, beta, topic)))
            }
        }
    })
    
    sample <- reactive({
        if (!is.null(input$analysis)) {
            switch(input$analysis,
                   most_common = most_common_react(),
                   nrc = nrc_react(),
                   tfd = term_frequency_react(),
                   td_idf = tf_idf_react(),
                   tf_idf_ngram = tf_idf_ngram_reactive(),
                   common_ngrams = common_n_gram_reactive(),
                   bigrams_network = bigrams_network_reactive(),
                   beside = beside_reactive(),
                   lda = lda_reactive()
            )
        }
    })
    
    output$hist <- renderPlot(sample())
    
    output$table <- renderDataTable(if (is.data.frame(sample())) sample())
    
    output$download_plot <- downloadHandler(
        filename = function() {
            return(paste0(input$analysis, ".", input$plot_save))
        },
        content = function(file) {
            if (input$plot_save == "jpeg") {
                jpeg(file)
            }
            else {
                png(file)
            }
            
            
            plot(sample())
            dev.off()
        }
    )
    
    output$download_table <- downloadHandler(
        filename = function() {
            return(paste(input$analysis, ".csv", sep = ""))
        },
        content = function(file) {
            write.csv(sample(), file, row.names = FALSE)
        }
    )
})

shinyApp(ui = ui, server = server)

