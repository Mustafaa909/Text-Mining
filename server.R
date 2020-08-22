suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))
suppressWarnings(library(dplyr))
###############################################################################
#                                                                             #
#                              LOADING THE DATA                               #
#                                                                             #
###############################################################################
bi_words <-bi_words_fast
tri_words  <-tri_words_fast
quad_words <-quad_words_fast
bigram <- function(input_words){
    num <- length(input_words)
    filter(bi_words, 
           word1==input_words[num]) %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 2)) %>%
        as.character() -> out
    ifelse(out =="character(0)", "?", return(out))
}

trigram <- function(input_words){
    num <- length(input_words)
    filter(tri_words, 
           word1==input_words[num-1], 
           word2==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 3)) %>%
        as.character() -> out
    ifelse(out=="character(0)", bigram(input_words), return(out))
}

quadgram <- function(input_words){
    num <- length(input_words)
    filter(quad_words, 
           word1==input_words[num-2], 
           word2==input_words[num-1], 
           word3==input_words[num])  %>% 
        top_n(1, n) %>%
        filter(row_number() == 1L) %>%
        select(num_range("word", 4)) %>%
        as.character() -> out
    ifelse(out=="character(0)", trigram(input_words), return(out))
}
mesg <<- ""

###############################################################################
#                                                                             #
#                              CLEANING THE INPUT                             #
#                              PREDICTION METHOD                              #
#                                                                             #
###############################################################################

word_predict <- function(input){
    # Create a dataframe
    input <- data.frame(text = input)
    # Clean the Inpput
    replace_reg <- "[^[:alpha:][:space:]]*"
    input <- input %>%
        mutate(text = str_replace_all(text, replace_reg, ""))
    # Find word count, separate words, lower case
    input_count <- str_count(input, boundary("word"))
    input_words <- unlist(str_split(input, boundary("word")))
    input_words <- tolower(input_words)
    # Call the matching functions
    out <- ifelse(input_count == 0, "Please input a phrase",
                  ifelse(input_count == 3, quadgram(input_words),
                         ifelse(input_count == 2, trigram(input_words), bigram(input_words))))
    
    # Output
    return(out)
}
###############################################################################
#                                                                             #
#                              RESULTS ON THE SERVER                          #
#                                                                             #
###############################################################################

shinyServer(function(input, output) {
    output$prediction <- renderPrint({
        result <- word_predict(input$inputString)
        output$method <- renderText({mesg})
        result
    });
    
    output$answer <- renderText({
        input$inputString});
}
)
