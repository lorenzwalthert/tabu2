#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(magrittr)
path_storage <- function(group_id) {
    fs::path_ext_set(group_id, "rds")
}
init_bin <- function(group_id) {
  saveRDS(tibble::tibble(word = character(), is_available = logical()), path_storage(group_id))
}

word_read <- function(group_id) {
  data <- readRDS(path_storage(group_id))
  available <- which(data$is_available)
  if (length(available) == 0) {
    return("Round over.")
  }
  picked_row <<- sample(available, 1)
  data$word[picked_row]
}
library(shiny)
ui <- fluidPage(
  textInput("group_id", "The key for your group", "", width = "500px", placeholder = NULL),
  actionButton("confirm_group", "Join or create group"),
  textInput("caption", "Words", "", width = "700px"),
  actionButton("confirmed", "Add a new word (initial phase)"),
  actionButton("ready", "I am ready to play the game"),
  actionButton("read_word", "Pick a word from the bin"),
  actionButton("solved", "I solved the word a word"),
  actionButton("next_round", "Put all items back in the bin"),
  actionButton("clear_bin", "Clear the bin"),

  verbatimTextOutput("value"), 
  verbatimTextOutput("words_left")
    
)
server <- function(input, output) {
  v <- reactiveValues(msg = NULL, words_left = 0)

  observeEvent(input$confirm_group, {
    if(!fs::file_exists(path_storage(input$group_id))) {
        init_bin(input$group_id)
        v$msg <- paste("successfully created new group", input$group_id)
    } else {
        v$msg <- "successfully joined existing group"
    }
      data <- readRDS(path_storage(input$group_id))
      v$words_left <- sum(as.integer(data$is_available))
      
  })
  
  observeEvent(input$confirmed, {
      data <- readRDS(path_storage(input$group_id))
      data <- data %>%
          dplyr::add_row(word = input$caption, is_available = TRUE)
      v$words_left <- sum(as.integer(data$is_available))
      saveRDS(data, path_storage(input$group_id))
      
      v$msg <- input$caption
  })

  observeEvent(input$ready, {
    data <- readRDS(path_storage(input$group_id))
    v$msg <- "Glad you are ready. Let's go."
    v$words_left <- sum(as.integer(data$is_available))
    
  })
  
  observeEvent(input$next_round, {
      data <- readRDS(path_storage(input$group_id))
      data$is_available <- TRUE
      saveRDS(data, path_storage(input$group_id))
      v$words_left <- sum(as.integer(data$is_available))
      v$msg <- "All items back in the bin. Let's go."
  })

  observeEvent(input$read_word, {
    v$msg <- word_read(input$group_id)
  })
  observeEvent(input$solved, {
    data <- readRDS(path_storage(input$group_id))
    data$is_available[picked_row] <- FALSE
    saveRDS(data, path_storage(input$group_id))
    v$words_left <- sum(as.integer(data$is_available))
    new_word <- word_read(input$group_id)
    v$msg <- paste0("Well Done. new word: ", new_word)
  })

  observeEvent(input$clear_bin, {
    init_bin(input$group_id)
    v$msg <- "Bin Cleared"
    v$words_left <- 0
    
  })


  output$value <- renderText({
    v$msg
  })
  output$words_left <- renderText({
      paste0(v$words_left, " words left")
  })
}
shinyApp(ui, server)
