#----------------------------
# Topic Modelling
#----------------------------

set.seed(4006)

theme_Publication <- function(base_size=14, base_family="HelveticaNeueLT Pro 55 Roman") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="bold"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}


#----------------------------
# Installing packages 
# ---------------------------
libraries <- c("topicmodels", "pdftools", "tm",
               "tidytext", "ggplot2", "dplyr", "gridExtra",
               "tidyverse", "extrafont", "textstem", "ggpubr",
               "text2vec", "slam" , "ldatuning", "stm", "quanteda",
               "LDAvis", "quanteda.textstats", "quanteda.textplots",
               "ggcorrplot", "ggdendro", "sna", "ca", "factoextra",
               "FactoMineR", "ggraph", "igraph")

install.packages(libraries)

for (lib in libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}


#--------------------------------------------
# Loading in the documents (preprocessed)
# Set the working directory to txt documents
#--------------------------------------------


all_files <-list.files(pattern = "pdf$")
all_documents <- lapply(all_files, pdf_text)

# Creating a corpus
documents <- Corpus(VectorSource(all_documents)) # Creating a corpus
documents


# Pre-processing the Corpus
documents <- tm_map(documents, content_transformer(tolower)) # Lowercase all words
documents <- tm_map(documents, removeNumbers)  # Remove numbers
documents <- tm_map(documents, removeWords, stopwords("english"))  # Remove English stop words
documents <- tm_map(documents, removePunctuation, preserve_intra_word_dashes = TRUE)  # Remove punctuation
documents <- tm_map(documents, stripWhitespace)  # Remove whitespace
documents <- tm_map(documents, content_transformer(function(x) trimws(x, "both"))) # Remove extra white space
documents <- tm_map(documents, lemmatize_strings)  # Lemmatize words
documents <- tm_map(documents, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub = ""))) # Removing Non-ASCII characters


# Creating Document Term Matrix (DTM)
DTM <- DocumentTermMatrix(documents)

#--------------------------------------------
# Creation of LDA Model
#--------------------------------------------

Model_LDA <- LDA(DTM, k = 15, control = list(seed = 4006))
Model_LDA


# Beta values of the model
beta_topics <- tidy(Model_LDA, matrix = "beta")
beta_topics

print(beta_topics, n = 100)

# Gamma values of the matrix

gamma_documents <- tidy(Model_LDA, matrix = "gamma")
gamma_documents

# Dataframe for gamma values
doc_gamma_df <- data.frame(gamma_documents)
doc_gamma_df$chapter <- rep(1:dim(DTM)[1], 15)


#---------------------------------------------------
# Visualisation (preliminary to check preprocessing)
#---------------------------------------------------
# Grouping terms by topic
beta_top_terms <- beta_topics %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  arrange(topic, -beta)


# Visualising the data
beta_top_terms %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered() + theme_Publication()

# Plotting gamma results
doc_gamma_df %>% 
ggplot(aes(x = chapter, y = gamma, group = factor(topic), colour = factor(topic))) +
  geom_line(linewidth = 1) + facet_wrap(~ factor(topic), ncol = 1) +
  theme_Publication()

#-----------------------------------------------------
# Using quanteda for LDA
#-----------------------------------------------------

# List all the PDF files in the working directory
document_files <- list.files(pattern = "\\.pdf$", full.names = TRUE)

# Read the text from the PDFs using the tm package
pdf_text <- lapply(document_files, pdftools::pdf_text)

# Create a corpus from the text data
my_corpus <- Corpus(VectorSource(pdf_text))

#Pre-processing the pdf text

my_corpus <- tm_map(my_corpus, content_transformer(gsub), pattern = "\\\\n", replacement = " ") # Remove the '\n' characters from the corpus
my_corpus <- tm_map(my_corpus, content_transformer(tolower)) # Lowercase all words
my_corpus <- tm_map(my_corpus, removeNumbers)  # Remove numbers
my_corpus <- tm_map(my_corpus, removeWords, stopwords("english"))  # Remove English stop words
my_corpus <- tm_map(my_corpus, removePunctuation, preserve_intra_word_dashes = TRUE)  # Remove punctuation
my_corpus <- tm_map(my_corpus, stripWhitespace)  # Remove whitespace
my_corpus <- tm_map(my_corpus, content_transformer(function(x) trimws(x, "both"))) # Remove extra white space
my_corpus <- tm_map(my_corpus, lemmatize_strings)  # Lemmatize words
my_corpus <- tm_map(my_corpus, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub = ""))) # Removing Non-ASCII characters


# Creation of DTM first
my_dtm <- DocumentTermMatrix(my_corpus)
inspect(my_dtm)

# Creating of a congruent DTM
my_dfm <- as.dfm(my_dtm)
my_dfm <- as.dfm(my_dfm)

set.seed(4006)

#-----------------------------------------------------
# Using LDA with hyper parameters
#-----------------------------------------------------

model <- LDA(my_dtm, method = "gibbs", k = 10, control = list(alpha = 0.5))
model

# Looking at terms
terms(model, 10)

#-----------------------------------------------------
# Some visualisation for Gibbs model
#-----------------------------------------------------

# Beta values of the model
beta_topics_gibbs <- tidy(model, matrix = "beta")
beta_topics_gibbs

print(beta_topics, n = 100)

# Gamma values of the matrix

gamma_documents_gibbs <- tidy(model, matrix = "gamma")
gamma_documents_gibbs

# Dataframe for gamma values
doc_gamma_df_gibbs <- data.frame(gamma_documents_gibbs)
doc_gamma_df_gibbs$chapter <- rep(1:dim(my_dtm)[1], 10)


# Grouping terms by topic
beta_top_terms_gibbs <- beta_topics_gibbs %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  arrange(topic, -beta)


# Visualising the data
beta_top_terms_gibbs %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered() + theme_Publication() +   labs(x = expression(beta), y = "Terms")

# Plotting gamma results
doc_gamma_df_gibbs %>% 
  ggplot(aes(x = chapter, y = gamma, group = factor(topic), colour = factor(topic))) +
  geom_line(linewidth = 1) + facet_wrap(~ factor(topic), ncol = 1) +
  theme_Publication()


# Visualisation using LDA Vis

dtm_vis <- my_dtm[slam::row_sums(my_dtm) > 0]
phi <- as.matrix(posterior(model)$terms)
theta <- as.matrix(posterior(model)$topics)
vocab <- colnames(phi)
doc.length <- slam::row_sums(my_dtm)
term.freq <- slam::col_sums(my_dtm)[match(vocab, colnames(my_dtm))]

json <- createJSON(phi = phi, theta = theta, vocab = vocab,
                   doc.length = doc.length, term.frequency = term.freq) 

serVis(json)


#--------------------------------------------------
# Network visualisation 
# Using (igraph)
#--------------------------------------------------


# Calculate coherence scores for different numbers of topics (adjust 'max_k' based on preference)
max_k <- 20

coherence_scores <- FindTopicsNumber(my_dtm, topics = seq(2, max_k), method = "Gibbs",
                                     metrics = c("Griffiths2004","CaoJuan2009", "Arun2010", "Deveaud2014"))

# Plot the coherence scores to find the optimal number of topics
coherence_scores[max(coherence_scores$Griffiths2004), ]

# Find the row with the maximum Griffiths2004 score
max_row <- coherence_scores %>%
  filter(Griffiths2004 == max(Griffiths2004))

# Extract the number of topics from the max_row
optimal_topics <- max_row$topics
max_row

# Print the result
cat("The optimal number of topics based on the maximum Griffiths2004 score is:", optimal_topics)


# Graphing the result
coherence_scores %>% 
  ggplot() +
  geom_line(aes(x = topics, y = Griffiths2004), size = 1, colour = "#0F6EB9") +
  theme_Publication() +
  labs(x = "Topics", y = "Coherence",
       caption = "Coherence scores for different numbers of topics in\ntopic modelling using UMass measure (Griffiths 2004)") +
  scale_x_continuous(breaks = seq(min(coherence_scores$topics), max(coherence_scores$topics), by = 1)) +
  scale_y_continuous(labels = scales::scientific_format()) +
  geom_point(data = coherence_scores[coherence_scores$topics == 8, ], 
             aes(x = topics, y = Griffiths2004), colour = "#E52934", size = 3) +
  ggplot2::annotate("text", x = 7, y = coherence_scores$Griffiths2004[coherence_scores$topics == 8] - 8000,
                    label = "Optimal coherence\nat 8 topics", hjust = -0.1, vjust = 0, color = "red", family = "HelveticaNeueLT Pro 55 Roman")


#--------------------------------------------------
# Second iteration of model with optimal topics
#--------------------------------------------------

# Building model
model1 <- LDA(my_dtm, method = "gibbs", k = 8, control = list(alpha = 0.5, seed = 4006))
model1

# Visualising the model

# Looking at terms
terms(model1, 10)

#-----------------------------------------------------
# Some visualisation for Gibbs model
#-----------------------------------------------------

# Beta values of the model
beta_topics_gibbs1 <- tidy(model1, matrix = "beta")
beta_topics_gibbs1

print(beta_topics_gibbs1, n = 100)

# Gamma values of the matrix

gamma_documents_gibbs1 <- tidy(model1, matrix = "gamma")
gamma_documents_gibbs1

# Dataframe for gamma values
doc_gamma_df_gibbs1 <- data.frame(gamma_documents_gibbs1)
doc_gamma_df_gibbs1$chapter <- rep(1:dim(my_dtm)[1], 8)


# Grouping terms by topic
beta_top_terms_gibbs1 <- beta_topics_gibbs1 %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  arrange(topic, -beta)


# Visualising the data with optimal number of topics
beta_top_terms_gibbs1 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free") +
  scale_y_reordered() + theme_Publication() + labs(x = expression(beta), y = "Terms")

# Plotting gamma results
doc_gamma_df_gibbs %>% 
  ggplot(aes(x = chapter, y = gamma, group = factor(topic), colour = factor(topic))) +
  geom_line(linewidth = 1) + facet_wrap(~ factor(topic), ncol = 1) +
  theme_Publication()


#--------------------------------------------
# Optimal parameterisation of alpha
#--------------------------------------------


# Setting up alpha values
alpha_values <- seq(0.1, 1.0, 0.1)

# Create a list to store the results
results <- list()

# Create a vector to store the results
results <- sapply(alpha_values, function(alpha) {
  # Build the model
  model <- LDA(my_dtm, method = "gibbs", k = 8, control = list(alpha = alpha, seed = 4000))
  # Evaluate the model
  return(logLik(model))
})


# Find the best model
best_alpha <- alpha_values[which.max(results)]
best_alpha # Most likely beta value at 0.8

#--------------------------------------------------
# 3rd iteration of model with optimal topics
#--------------------------------------------------

# Building model
model2 <- LDA(my_dtm, method = "gibbs", k = 8, control = list(alpha = 0.8, seed = 4000))
model2

# Visualising the model

# Looking at terms
terms(model2, 10)

#-----------------------------------------------------
# Some visualisation for Gibbs model
# The location of the topics may change based on the random allocation
# Thus, topics remain constant, however, visualisations may change
#-----------------------------------------------------
# Colour palette
lego_colours <- c("#FDBE02", "#0E2A47", "#00A844", "#0F6EB9",
                  "#2D3192", "#00B2EF", "#E52934", "#F58220")


# Beta values of the model
beta_topics_gibbs2 <- tidy(model2, matrix = "beta")
beta_topics_gibbs2

print(beta_topics_gibbs2, n = 10)

# Gamma values of the matrix

gamma_documents_gibbs2 <- tidy(model2, matrix = "gamma")
gamma_documents_gibbs2


# Dataframe for gamma values
doc_gamma_df_gibbs2 <- data.frame(gamma_documents_gibbs2)
doc_gamma_df_gibbs2$chapter <- rep(1:dim(my_dtm)[1], 8)

doc_gamma_df_gibbs2


# Specify the order of document names
document_names <- c("First Strategy (2015)",
                    "Implement Plan (2016)",
                    "First Progress Report (2017)",
                    "Animal Sector Plan (2018)",
                    "Final Progress Report (2021)",
                    "Strategy 2020 & Beyond (2020)",
                    "One Health Master Action Plan (2021)")

# Add the document_name column with the specified order
doc_gamma_df_gibbs2$document_name <- factor(doc_gamma_df_gibbs2$document, levels = 1:7, labels = document_names)

# Print the dataframe to check if the column has been added correctly
print(doc_gamma_df_gibbs2)

# Making a backup
doc_gamma_df_gibbs2$document_name_2 = doc_gamma_df_gibbs2$document_name

# G Sub

# Create line breaks in the factor labels
wrapped_labels <- gsub(" ", "\n", levels(doc_gamma_df_gibbs2$document_name))

# Assign wrapped labels back to the factor levels
levels(doc_gamma_df_gibbs2$document_name) <- wrapped_labels

# Grouping terms by topic
beta_top_terms_gibbs2 <- beta_topics_gibbs2 %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

beta_top_terms_gibbs2
view(beta_top_terms_gibbs2)
view(gamma_documents_gibbs2)
# Visualising the data with optimal number of topics

lego_colours <- c("#FDBE02", "#0E2A47", "#00A844", "#0F6EB9",
                  "#2D3192", "#00B2EF", "#E52934", "#F58220")

beta_top_terms_gibbs2 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free", ncol = 4) +
  scale_y_reordered() + theme_Publication() + labs(x = expression(beta), y = "Terms") +
  scale_fill_manual(values = lego_colours) +
  geom_text(aes(label = round(beta,3)), hjust = 1.1, family = "HelveticaNeueLT Pro 55 Roman",
            colour = "white") +
  theme(text = element_text(size = 18))


# Giving topic names
topic_names <- c(
  `1` = "Antimicrobial resistance\nresearch and surveillance",
  `2` = "Collaboration in\nantimicrobial resistance",
  `3` = "Antimicrobial resistance\nin agriculture",
  `4` = "Stewardship to prevent\nmisuse of antibiotics",
  `5` = "Antimicrobial use\nin food production",
  `6` = "One Health",
  `7` = "Healthcare system\ndevelopment",
  `8` = "International collaboration"
)

# With official labels
beta_top_terms_gibbs2 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free", ncol = 4, labeller = labeller(topic = topic_names)) +
  scale_y_reordered() + theme_Publication() + labs(x = expression(beta), y = "Terms") +
  scale_fill_manual(values = lego_colours) +
  geom_text(aes(label = round(beta,3)), hjust = 1.1, family = "HelveticaNeueLT Pro 55 Roman",
            colour = "white") +
  theme(text = element_text(size = 18))

# Gamma results in bar chart format

lego_colours <- c("#FDBE02", "#0E2A47", "#00A844", "#0F6EB9",
                 "#2D3192", "#00B2EF", "#E52934", "#F58220")

  

# Matched with terms(model2, 10)

doc_gamma_df_gibbs2 %>% 
  ggplot(aes(x = document_name, y = gamma, fill = factor(topic))) +
  geom_col(position = "dodge", width = 0.8, colour = "black") +
  facet_wrap(~ factor(topic), nrow = 3) +
  theme_Publication() +
  labs(x = "Document", y = expression(gamma), fill = "",
       caption = expression(paste(gamma, " values < 0.01 omitted"))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.05)) +
  scale_fill_manual(values = lego_colours) + 
  geom_text(aes(label = ifelse(gamma <= 0.01, "", round(gamma, 2))), vjust = -1,
            family = "HelveticaNeueLT Pro 55 Roman") +
  theme(axis.title.y = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.y = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.x = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        legend.position = "none", plot.caption = element_text(size = 12))

terms(model2, 10)

#----------------------
# With label
#----------------------
# Define a named vector with topic names
# This has to change with the randomisation of the LDA model
# Changes can be made accordingly to the set definitions

# Add the topic_name column to the dataframe
doc_gamma_df_gibbs2 <- doc_gamma_df_gibbs2 %>%
  mutate(topic_name = factor(topic, levels = names(topic_names), labels = topic_names))

# With topic numbers
doc_gamma_df_gibbs2 %>% 
  ggplot(aes(x = document_name, y = gamma, fill = factor(topic))) +
  geom_col(position = "dodge", width = 0.8, colour = "black") +
  facet_wrap(~ factor(topic), nrow = 3) +
  theme_Publication() +
  labs(x = "Document", y = expression(gamma), fill = "",
       caption = expression(paste(gamma, " values < 0.01 omitted"))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.05)) +
  scale_fill_manual(values = lego_colours) + 
  geom_text(aes(label = ifelse(gamma <= 0.01, "", round(gamma, 2))), vjust = -1,
            family = "HelveticaNeueLT Pro 55 Roman") +
  theme(axis.title.y = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.y = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.x = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        legend.position = "none", plot.caption = element_text(size = 12))

# With official labels
doc_gamma_df_gibbs2 %>% 
  ggplot(aes(x = document_name, y = gamma, fill = factor(topic))) +
  geom_col(position = "dodge", width = 0.8, colour = "black") +
  facet_wrap(~ topic, nrow = 3, labeller = labeller(topic = topic_names)) +
  theme_Publication() +
  labs(x = "Document", y = expression(gamma), fill = "",
       caption = expression(paste(gamma, " values < 0.001 omitted"))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.05)) +
  scale_fill_manual(values = lego_colours) + 
  geom_text(aes(label = ifelse(gamma <= 0.001, "", round(gamma, 3))), vjust = -1,
            family = "HelveticaNeueLT Pro 55 Roman") +
  theme(axis.title.y = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.y = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.x = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        legend.position = "none", plot.caption = element_text(size = 12), 
        text = element_text(size = 18))


# With official labels
doc_gamma_df_gibbs2 %>% 
  ggplot(aes(x = document_name, y = gamma, fill = factor(topic))) +
  geom_col(position = "dodge", width = 0.8, colour = "black") +
  facet_wrap(~ topic, nrow = 3, labeller = labeller(topic = topic_names)) +
  theme_Publication() +
  labs(x = "Document", y = expression(gamma), fill = "",
       caption = expression(paste(gamma, " values < 0.001 omitted"))) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1.05)) +
  scale_fill_manual(values = lego_colours) + 
  geom_text(aes(label = ifelse(gamma <= 0.001, "", round(gamma, 3))), vjust = -1,
            family = "HelveticaNeueLT Pro 55 Roman") +
  theme(axis.title.y = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.y = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.x = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        legend.position = "none", plot.caption = element_text(size = 12), 
        text = element_text(size = 18))


# By document
topic_labels <- c(
  `1` = "Antimicrobial\nresistance\nresearch\nand\nsurveillance",
  `2` = "Collaboration in\nantimicrobial\nresistance",
  `3` = "Antimicrobial\nresistance\nin\nagriculture",
  `4` = "Stewardship\nto\nprevent\nmisuse\nof\nantibiotics",
  `5` = "Antimicrobial\nuse\nin\nfood\nproduction",
  `6` = "One\nHealth",
  `7` = "Healthcare\nsystem\ndevelopment",
  `8` = "International\ncollaboration"
)

doc_gamma_df_gibbs2 %>% 
  ggplot(aes(x = factor(topic, levels = seq(1, 8)), y = gamma, fill = document_name_2)) +
  geom_col(position = "dodge", width = 0.8, colour = "black") +
  facet_wrap(~ document_name_2, nrow = 4) +
  theme_Publication() +
  labs(x = "Topic", y = expression(gamma), fill = "",
       caption = expression(paste(gamma, " values < 0.0001 omitted"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.05)) +
  scale_fill_manual(values = lego_colours) + 
  geom_text(aes(label = ifelse(gamma <= 0.0001, "", round(gamma, 3))), vjust = -1,
            family = "HelveticaNeueLT Pro 55 Roman") +
  scale_x_discrete(labels = topic_labels) +  
  theme(axis.title.y = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.y = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.x = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        legend.position = "none", plot.caption = element_text(size = 12), 
        text = element_text(size = 18))


#----------------------------
# Final LDA vis
#----------------------------

dtm_vis <- my_dtm[slam::row_sums(my_dtm) > 0]
phi <- as.matrix(posterior(model2)$terms)
theta <- as.matrix(posterior(model2)$topics)
vocab <- colnames(phi)
doc.length <- slam::row_sums(my_dtm)
term.freq <- slam::col_sums(my_dtm)[match(vocab, colnames(my_dtm))]

json <- createJSON(phi = phi, theta = theta, vocab = vocab,
                   doc.length = doc.length, term.frequency = term.freq) 

serVis(json)



#--------------------------------
# Cocurrence network 
#--------------------------------

# Getting the raw files
network_files <- list.files(pattern = "pdf$")

# Loading the PDF files
network_pdfs <- lapply(network_files, pdf_text)

# Creating the co-currence network corpus
network_documents <- Corpus(VectorSource(network_pdfs))

# Finding the collocations
sentences <- network_documents %>% 
  tolower() %>% 
  paste0(collapse = " ") %>% 
  stringr::str_split(fixed(".")) %>% 
  unlist() %>% 
  tm::removePunctuation() %>% 
  tm::removeNumbers() %>% 
  stringr::str_squish()

# Creating the token object
token_object <- tokens(sentences, remove_punct = T, 
                       remove_symbols = T, remove_url = T) %>% 
  tokens_remove(stopwords("english"))

# Looking at tokenised collocations

token_collocations <- textstat_collocations(token_object, size = 2,
                                            min_count = 20)

#-----------------------------------
# Creating document feature matrix
#-----------------------------------

sentence_dfm <- sentences %>% 
  quanteda::dfm(remove = stopwords("english"), remove_punct = T) %>% 
  quanteda::dfm_trim(min_termfreq = 10, verbose = T)

sentence_dfm

#--------------------------------------
# Cocurrence statistics
#--------------------------------------


# Function for cocurrence statistics calculator
source("https://slcladal.github.io/rscripts/calculateCoocStatistics.R")

# What term do we want to use the cocurrence statistics for?
coocterm1 <- "antimicrobial"

# Calculation of statistic
coocs1 <- calculateCoocStatistics(coocterm1, sentence_dfm,measure = "LOGIK")


#---------------------------------------------------
# Cleaning the terms due to non-sensical characters
# For example n = 413, -n = 67, nnnnfinal = 40
#----------------------------------------------------

coocs1 <- coocs1[-2] # removing n = 413 (non-sensical)


names(coocs1)[names(coocs1) %in% c("healthn", "-n", "ofn", "nnnnn", "nnnnnfinal", "stewardshipn",
                                   "inn", "industryn", "typen", "forn", "communityn", "nnnnnimplementation ",
                                   "resistancenn", "nnnn", "agriculturen", "humann", "nnobjective",
                                   "actionnn", "sectorsn", "developmentn", "researchn", "foodn",
                                   "usen", "willn", "nhealth", "actionn", "nthis", "completedn",
                                   "infectionn", "thatn", "aren", "asn", "departmentn", "infection",
                                   "othern")] <- c("health", "", "of", "", "final", "stewardship", "in",
                                                   "industry", "type", "for", "community", "implementation",
                                                   "resistance", " ", "agriculture", "human", "objective",
                                                   "action", "sectors", "development", "research", "food",
                                                   "use", "will", "health", "action", "this", "completed",
                                                   "infection", "that", "are", "as", "department", "infection",
                                                   "other")


coocs1

names(coocs1)[names(coocs1) == "healthn"] <- "health"
names(coocs1)[names(coocs1) == "-n"] <- ""
names(coocs1)[names(coocs1) == "ofn"] <- "of"
names(coocs1)[names(coocs1) == "nnnnn"] <- ""
names(coocs1)[names(coocs1) == "nnnnnfinal"] <- "final"
names(coocs1)[names(coocs1) == "stewardshipn"] <- "stewardship"
names(coocs1)[names(coocs1) == "inn"] <- "in"
names(coocs1)[names(coocs1) == "industryn"] <- "industry"
names(coocs1)[names(coocs1) == "typen"] <- "type"
names(coocs1)[names(coocs1) == "forn"] <- "for"
names(coocs1)[names(coocs1) == "communityn"] <- "community"
names(coocs1)[names(coocs1) == "nnnnnimplementation "] <- "implementation"
names(coocs1)[names(coocs1) == "resistancenn"] <- "resistance"
names(coocs1)[names(coocs1) == "nnnn"] <- ""
names(coocs1)[names(coocs1) == "agriculturen"] <- "agriculture"
names(coocs1)[names(coocs1) == "healthnn"] <- "health"
names(coocs1)[names(coocs1) == "nn"] <- ""
names(coocs1)[names(coocs1) == "humann"] <- "human"
names(coocs1)[names(coocs1) == "nnobjective"] <- "objective"
names(coocs1)[names(coocs1) == "actionnn"] <- "action"
names(coocs1)[names(coocs1) == "sectorsn"] <- "sectors"
names(coocs1)[names(coocs1) == "developmentn"] <- "development"
names(coocs1)[names(coocs1) == "researchn"] <- "research"
names(coocs1)[names(coocs1) == "foodn"] <- "foodn"
names(coocs1)[names(coocs1) == "usen"] <- "usen"
names(coocs1)[names(coocs1) == "willn"] <- "will"
names(coocs1)[names(coocs1) == "nhealth"] <- "health"
names(coocs1)[names(coocs1) == "actionn"] <- "action"
names(coocs1)[names(coocs1) == "nthis"] <- "this"
names(coocs1)[names(coocs1) == "completedn"] <- "completed"
names(coocs1)[names(coocs1) == "infectionn"] <- "infection"
names(coocs1)[names(coocs1) == "thatn"] <- "that"
names(coocs1)[names(coocs1) == "aren"] <- "are"
names(coocs1)[names(coocs1) == "asn"] <- "as"
names(coocs1)[names(coocs1) == "departmentn"] <- "department"
names(coocs1)[names(coocs1) == "infectionn"] <- "infection"
names(coocs1)[names(coocs1) == "nnnnnresponding"] <- "responding"
names(coocs1)[names(coocs1) == "environmentn"] <- "environment"
names(coocs1)[names(coocs1) == "australiann"] <- "australian"
names(coocs1)[names(coocs1) == "resourcesn"] <- "resources"
names(coocs1)[names(coocs1) == "nobjective"] <- "objective"
names(coocs1)[names(coocs1) == "antimicrobialn"] <- "antimicrobial"
names(coocs1)[names(coocs1) == "n"] <- ""
names(coocs1)[names(coocs1) == "-n"] <- ""
names(coocs1)[names(coocs1) == "andn"] <- "and"
names(coocs1)[names(coocs1) == "-"] <- ""
names(coocs1)[names(coocs1) == " -n"] <- ""
names(coocs1)[names(coocs1) == " "] <- ""
names(coocs1)[names(coocs1) == "©"] <- ""
names(coocs1)[names(coocs1) == "nnn"] <- ""
names(coocs1)[names(coocs1) == "nnnnnimplementation"] <- "implementation"
names(coocs1)[names(coocs1) == "nnthis"] <- "this"
names(coocs1)[names(coocs1) == "nnthe"] <- "the"
names(coocs1)[names(coocs1) == "sectorn"] <- "sector"
coocs1 <- coocs1[!grepl("–n", names(coocs1))]

coocs1 <- coocs1[names(coocs1) != ""] # Removing no name values

# Aggregate as some may have the same name
coocs1  <- tapply(coocs1, names(coocs1), sum)
coocs1 <- coocs1[order(coocs1, decreasing = TRUE)]
coocs1
#-----------------------------------------------
# Aggregating Australia variations 
# Terms Australia, Australian, Australia's have similar ideological value
# Removing "and" as well due to no ideological value
# Sector and sectors are the same ideologically
#-----------------------------------------------


# Replace names containing "australia" with "australia"
names(coocs1)[grepl("australia", names(coocs1))] <- "australia"

# Sectors and sector
names(coocs1)[grepl("sector", names(coocs1))] <- "sector"

# Sectors and sector
names(coocs1)[grepl("animal", names(coocs1))] <- "animal"

# Removing "and" due to it being a conjunction with no ideological value
coocs1 <- coocs1[!grepl("and", names(coocs1))]

# Aggregate by summing values for each name
coocs1 <- tapply(coocs1, names(coocs1), sum)

# Order by decreasing values
coocs1 <- coocs1[order(coocs1, decreasing = TRUE)]

coocs1[1:50]


# Reducing the document-feature matrix to contain the top 20 collates
reduced_dfm <- dfm_select(sentence_dfm, pattern = c(names(coocs1[1:50]), 
                                                    "antimicrobial"))

# Transforming the document feature matrix to feature co-currence matrix

tagged_fcm <- fcm(reduced_dfm)

#-------------------------------------------
# Visualising the network
#-------------------------------------------

p1 = textplot_network(tagged_fcm, min_freq = 30,
                 edge_alpha = 0.25,
                 edge_size = 3,
                 edge_color = "#68A9E0",
                 vertex_labelsize = 6,
                 vertex_labelfont = "HelveticaNeueLT Pro 55 Roman") +
  labs(caption = "Aggregate of all documents", title = "A)") +
  theme(title = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman", face = "bold"))
p1


#---------------------------
# Stratified analysis
#---------------------------

#--------------------------------------------
# Documents 1,2,4,6,7 are plans
# Documents 3 and 5 are progress reports
#--------------------------------------------

# Getting the raw files [Change working directory to plan documents]
plan_files <- list.files(pattern = "pdf$")

# Loading the PDF files
plan_pdfs <- lapply(plan_files, pdf_text)

# Creating the co-currence network corpus
plan_documents <- Corpus(VectorSource(plan_pdfs))

# Finding the collocations
plan_sentences <- plan_documents %>% 
  tolower() %>% 
  paste0(collapse = " ") %>% 
  stringr::str_split(fixed(".")) %>% 
  unlist() %>% 
  tm::removePunctuation() %>% 
  tm::removeNumbers() %>% 
  stringr::str_squish()


# Creating the token object
plan_token_object <- tokens(plan_sentences, remove_punct = T, 
                       remove_symbols = T, remove_url = T) %>% 
  tokens_remove(stopwords("english"))

# Looking at tokenised collocations

plan_token_collocations <- textstat_collocations(plan_token_object, size = 2,
                                            min_count = 20)

#-----------------------------------
# Creating document feature matrix
#-----------------------------------

plan_sentence_dfm <- plan_sentences %>% 
  quanteda::dfm(remove = stopwords("english"), remove_punct = T) %>% 
  quanteda::dfm_trim(min_termfreq = 10, verbose = T)

plan_sentence_dfm

#--------------------------------------
# Cocurrence statistics
#--------------------------------------


# Function for cocurrence statistics calculator
source("https://slcladal.github.io/rscripts/calculateCoocStatistics.R")

# What term do we want to use the cocurrence statistics for?
plan_coocterm1 <- "antimicrobial"

# Calculation of statistic
plan_coocs1 <- calculateCoocStatistics(plan_coocterm1, plan_sentence_dfm,measure = "LOGIK")


#---------------------------------------------------
# Cleaning non-nonsensical terms
#----------------------------------------------------

plan_coocs1 <- plan_coocs1[-2] # Nonsensical values of n


names(plan_coocs1)[names(plan_coocs1) %in% c("healthn", "-n", "ofn", "nnnnn", "nnnnnfinal", "stewardshipn",
                                   "inn", "industryn", "typen", "forn", "communityn", "nnnnnimplementation ",
                                   "resistancenn", "nnnn", "agriculturen", "humann", "nnobjective",
                                   "actionnn", "sectorsn", "developmentn", "researchn", "foodn",
                                   "usen", "willn", "nhealth", "actionn", "nthis", "completedn",
                                   "infectionn", "thatn", "aren", "asn", "departmentn", "infection",
                                   "othern")] <- c("health", "", "of", "", "final", "stewardship", "in",
                                                   "industry", "type", "for", "community", "implementation",
                                                   "resistance", " ", "agriculture", "human", "objective",
                                                   "action", "sectors", "development", "research", "food",
                                                   "use", "will", "health", "action", "this", "completed",
                                                   "infection", "that", "are", "as", "department", "infection",
                                                   "other")


plan_coocs1

names(plan_coocs1)[names(plan_coocs1) == "healthn"] <- "health"
names(plan_coocs1)[names(plan_coocs1) == "-n"] <- ""
names(plan_coocs1)[names(plan_coocs1) == "ofn"] <- "of"
names(plan_coocs1)[names(plan_coocs1) == "nnnnn"] <- ""
names(plan_coocs1)[names(plan_coocs1) == "nnnnnfinal"] <- "final"
names(plan_coocs1)[names(plan_coocs1) == "stewardshipn"] <- "stewardship"
names(plan_coocs1)[names(plan_coocs1) == "inn"] <- "in"
names(plan_coocs1)[names(plan_coocs1) == "industryn"] <- "industry"
names(plan_coocs1)[names(plan_coocs1) == "typen"] <- "type"
names(plan_coocs1)[names(plan_coocs1) == "forn"] <- "for"
names(plan_coocs1)[names(plan_coocs1) == "communityn"] <- "community"
names(plan_coocs1)[names(plan_coocs1) == "nnnnnimplementation "] <- "implementation"
names(plan_coocs1)[names(plan_coocs1) == "resistancenn"] <- "resistance"
names(plan_coocs1)[names(plan_coocs1) == "nnnn"] <- ""
names(plan_coocs1)[names(plan_coocs1) == "agriculturen"] <- "agriculture"
names(plan_coocs1)[names(plan_coocs1) == "healthnn"] <- "health"
names(plan_coocs1)[names(plan_coocs1) == "nn"] <- ""
names(plan_coocs1)[names(plan_coocs1) == "humann"] <- "human"
names(plan_coocs1)[names(plan_coocs1) == "nnobjective"] <- "objective"
names(plan_coocs1)[names(plan_coocs1) == "actionnn"] <- "action"
names(plan_coocs1)[names(plan_coocs1) == "sectorsn"] <- "sectors"
names(plan_coocs1)[names(plan_coocs1) == "developmentn"] <- "development"
names(plan_coocs1)[names(plan_coocs1) == "researchn"] <- "research"
names(plan_coocs1)[names(plan_coocs1) == "foodn"] <- "foodn"
names(plan_coocs1)[names(plan_coocs1) == "usen"] <- "usen"
names(plan_coocs1)[names(plan_coocs1) == "willn"] <- "will"
names(plan_coocs1)[names(plan_coocs1) == "nhealth"] <- "health"
names(plan_coocs1)[names(plan_coocs1) == "actionn"] <- "action"
names(plan_coocs1)[names(plan_coocs1) == "nthis"] <- "this"
names(plan_coocs1)[names(plan_coocs1) == "completedn"] <- "completed"
names(plan_coocs1)[names(plan_coocs1) == "infectionn"] <- "infection"
names(plan_coocs1)[names(plan_coocs1) == "thatn"] <- "that"
names(plan_coocs1)[names(plan_coocs1) == "aren"] <- "are"
names(plan_coocs1)[names(plan_coocs1) == "asn"] <- "as"
names(plan_coocs1)[names(plan_coocs1) == "departmentn"] <- "department"
names(plan_coocs1)[names(plan_coocs1) == "infectionn"] <- "infection"
names(plan_coocs1)[names(plan_coocs1) == "nnnnnresponding"] <- "responding"
names(plan_coocs1)[names(plan_coocs1) == "environmentn"] <- "environment"
names(plan_coocs1)[names(plan_coocs1) == "australiann"] <- "australian"
names(plan_coocs1)[names(plan_coocs1) == "resourcesn"] <- "resources"
names(plan_coocs1)[names(plan_coocs1) == "nobjective"] <- "objective"
names(plan_coocs1)[names(plan_coocs1) == "antimicrobialn"] <- "antimicrobial"
names(plan_coocs1)[names(plan_coocs1) == "n"] <- ""
names(plan_coocs1)[names(plan_coocs1) == "-n"] <- ""
names(plan_coocs1)[names(plan_coocs1) == "andn"] <- "and"
names(plan_coocs1)[names(plan_coocs1) == "-"] <- ""
names(plan_coocs1)[names(plan_coocs1) == " -n"] <- ""
names(plan_coocs1)[names(plan_coocs1) == " "] <- ""
names(plan_coocs1)[names(plan_coocs1) == "©"] <- ""
names(plan_coocs1)[names(plan_coocs1) == "nnn"] <- ""
names(plan_coocs1)[names(plan_coocs1) == "nnnnnimplementation"] <- "implementation"
names(plan_coocs1)[names(plan_coocs1) == "nnthis"] <- "this"
names(plan_coocs1)[names(plan_coocs1) == "nnthe"] <- "the"
names(plan_coocs1)[names(plan_coocs1) == "sectorn"] <- "sector"
plan_coocs1 <- plan_coocs1[!grepl("–n", names(plan_coocs1))]

plan_coocs1 <- plan_coocs1[names(plan_coocs1) != ""] # Removing no name values

# Aggregate as some may have the same name
plan_coocs1  <- tapply(plan_coocs1, names(plan_coocs1), sum)
plan_coocs1 <- plan_coocs1[order(plan_coocs1, decreasing = TRUE)]
plan_coocs1
#-----------------------------------------------
# Aggregating variations as per above
#-----------------------------------------------


# Replace names containing "australia" with "australia"
names(plan_coocs1)[grepl("australia", names(plan_coocs1))] <- "australia"

# Sectors and sector
names(plan_coocs1)[grepl("sector", names(plan_coocs1))] <- "sector"

# Sectors and sector
names(plan_coocs1)[grepl("animal", names(plan_coocs1))] <- "animal"

# Removing "and" due to it being a conjunction with no ideological value
plan_coocs1 <- plan_coocs1[!grepl("and", names(plan_coocs1))]

# Aggregate by summing values for each name
plan_coocs1 <- tapply(plan_coocs1, names(plan_coocs1), sum)

# Order by decreasing values
plan_coocs1 <- plan_coocs1[order(plan_coocs1, decreasing = TRUE)]


# Reducing the document-feature matrix to contain the top 20 collates
plan_reduced_dfm <- dfm_select(plan_sentence_dfm, pattern = c(names(plan_coocs1[1:50]), 
                                                    "antimicrobial"))

# Transforming the document feature matrix to feature co-currence matrix

plan_tagged_fcm <- fcm(plan_reduced_dfm)

#-------------------------------------------
# Visualising the network plan_network
#-------------------------------------------

p2 = textplot_network(plan_tagged_fcm, min_freq = 30,
                 edge_alpha = 0.25,
                 edge_size = 3,
                 edge_color = "#C5192D",
                 vertex_labelsize = 6,
                 vertex_labelfont = "HelveticaNeueLT Pro 55 Roman") +
  labs(caption = "Australian AMR plan documents", title = "B)") +
  theme(title = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman", face = "bold"))
p2

#------------------------------
# Progress report documents
#-----------------------------
# Getting the raw files [Change working directory to reports documents]
reports_files <- list.files(pattern = "pdf$")

# Loading the PDF files
reports_pdfs <- lapply(reports_files, pdf_text)

# Creating the co-currence network corpus
reports_documents <- Corpus(VectorSource(reports_pdfs))

# Finding the collocations
reports_sentences <- reports_documents %>% 
  tolower() %>% 
  paste0(collapse = " ") %>% 
  stringr::str_split(fixed(".")) %>% 
  unlist() %>% 
  tm::removePunctuation() %>% 
  tm::removeNumbers() %>% 
  stringr::str_squish()

# Creating the token object
reports_token_object <- tokens(reports_sentences, remove_punct = T, 
                            remove_symbols = T, remove_url = T) %>% 
  tokens_remove(stopwords("english"))

# Looking at tokenised collocations

reports_token_collocations <- textstat_collocations(reports_token_object, size = 2,
                                                 min_count = 20)

#-----------------------------------
# Creating document feature matrix
#-----------------------------------

reports_sentence_dfm <- reports_sentences %>% 
  quanteda::dfm(remove = stopwords("english"), remove_punct = T) %>% 
  quanteda::dfm_trim(min_termfreq = 10, verbose = T)

reports_sentence_dfm


#-----------------------
# Bigram creation
#-----------------------

# Creating the data.frame for the bigram
pdfbigram_df <- data.frame(text = sapply(my_corpus, as.character), stringsAsFactors = F)
pdfbigram_df


#Creating the bigram
corpus_bigram <- pdfbigram_df %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

corpus_bigram


# Bigram count
bigram_separated <- corpus_bigram %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigram_separated[] <-
  lapply(bigram_separated, function(x)
    gsub("australian", "australia", x)) #Change australian to australia (same meaning)

bigram_separated[] <-
  lapply(bigram_separated, function(x)
    gsub("animals", "animal", x)) # Change animals to animal (same meaning)

# Removing stop words that may still be here
bigram_filtered <- bigram_separated %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigram_filtered %>% 
  count(word1, word2, sort = T)

bigram_counts # Looking at the counts

bigram_graph <- bigram_counts %>% 
  filter(n > 20)  %>% # More than 10 to keep brevity 
graph_from_data_frame() 

bigram_graph

# Creating graph based on relationships
set.seed(4006)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(alpha = 0.25) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1, size = 5) +
  theme_Publication() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    text = element_text(size = 18)
  )

#--------------------------------------
# Cocurrence statistics
#--------------------------------------


# Function for cocurrence statistics calculator
source("https://slcladal.github.io/rscripts/calculateCoocStatistics.R")

# What term do we want to use the cocurrence statistics for?
reports_coocterm1 <- "antimicrobial"

# Calculation of statistic
reports_coocs1 <- calculateCoocStatistics(reports_coocterm1, reports_sentence_dfm,measure = "LOGIK")


#---------------------------------------------------
# Cleaning non-nonsensical terms
#----------------------------------------------------

reports_coocs1 <- reports_coocs1[-2] # Nonsensical values of n


names(reports_coocs1)[names(reports_coocs1) %in% c("healthn", "-n", "ofn", "nnnnn", "nnnnnfinal", "stewardshipn",
                                             "inn", "industryn", "typen", "forn", "communityn", "nnnnnimplementation ",
                                             "resistancenn", "nnnn", "agriculturen", "humann", "nnobjective",
                                             "actionnn", "sectorsn", "developmentn", "researchn", "foodn",
                                             "usen", "willn", "nhealth", "actionn", "nthis", "completedn",
                                             "infectionn", "thatn", "aren", "asn", "departmentn", "infection",
                                             "othern")] <- c("health", "", "of", "", "final", "stewardship", "in",
                                                             "industry", "type", "for", "community", "implementation",
                                                             "resistance", " ", "agriculture", "human", "objective",
                                                             "action", "sectors", "development", "research", "food",
                                                             "use", "will", "health", "action", "this", "completed",
                                                             "infection", "that", "are", "as", "department", "infection",
                                                             "other")


reports_coocs1

names(reports_coocs1)[names(reports_coocs1) == "healthn"] <- "health"
names(reports_coocs1)[names(reports_coocs1) == "-n"] <- ""
names(reports_coocs1)[names(reports_coocs1) == "ofn"] <- "of"
names(reports_coocs1)[names(reports_coocs1) == "nnnnn"] <- ""
names(reports_coocs1)[names(reports_coocs1) == "nnnnnfinal"] <- "final"
names(reports_coocs1)[names(reports_coocs1) == "stewardshipn"] <- "stewardship"
names(reports_coocs1)[names(reports_coocs1) == "inn"] <- "in"
names(reports_coocs1)[names(reports_coocs1) == "industryn"] <- "industry"
names(reports_coocs1)[names(reports_coocs1) == "typen"] <- "type"
names(reports_coocs1)[names(reports_coocs1) == "forn"] <- "for"
names(reports_coocs1)[names(reports_coocs1) == "communityn"] <- "community"
names(reports_coocs1)[names(reports_coocs1) == "nnnnnimplementation "] <- "implementation"
names(reports_coocs1)[names(reports_coocs1) == "resistancenn"] <- "resistance"
names(reports_coocs1)[names(reports_coocs1) == "nnnn"] <- ""
names(reports_coocs1)[names(reports_coocs1) == "agriculturen"] <- "agriculture"
names(reports_coocs1)[names(reports_coocs1) == "healthnn"] <- "health"
names(reports_coocs1)[names(reports_coocs1) == "nn"] <- ""
names(reports_coocs1)[names(reports_coocs1) == "humann"] <- "human"
names(reports_coocs1)[names(reports_coocs1) == "nnobjective"] <- "objective"
names(reports_coocs1)[names(reports_coocs1) == "actionnn"] <- "action"
names(reports_coocs1)[names(reports_coocs1) == "sectorsn"] <- "sectors"
names(reports_coocs1)[names(reports_coocs1) == "developmentn"] <- "development"
names(reports_coocs1)[names(reports_coocs1) == "researchn"] <- "research"
names(reports_coocs1)[names(reports_coocs1) == "foodn"] <- "foodn"
names(reports_coocs1)[names(reports_coocs1) == "usen"] <- "usen"
names(reports_coocs1)[names(reports_coocs1) == "willn"] <- "will"
names(reports_coocs1)[names(reports_coocs1) == "nhealth"] <- "health"
names(reports_coocs1)[names(reports_coocs1) == "actionn"] <- "action"
names(reports_coocs1)[names(reports_coocs1) == "nthis"] <- "this"
names(reports_coocs1)[names(reports_coocs1) == "completedn"] <- "completed"
names(reports_coocs1)[names(reports_coocs1) == "infectionn"] <- "infection"
names(reports_coocs1)[names(reports_coocs1) == "thatn"] <- "that"
names(reports_coocs1)[names(reports_coocs1) == "aren"] <- "are"
names(reports_coocs1)[names(reports_coocs1) == "asn"] <- "as"
names(reports_coocs1)[names(reports_coocs1) == "departmentn"] <- "department"
names(reports_coocs1)[names(reports_coocs1) == "infectionn"] <- "infection"
names(reports_coocs1)[names(reports_coocs1) == "nnnnnresponding"] <- "responding"
names(reports_coocs1)[names(reports_coocs1) == "environmentn"] <- "environment"
names(reports_coocs1)[names(reports_coocs1) == "australiann"] <- "australian"
names(reports_coocs1)[names(reports_coocs1) == "resourcesn"] <- "resources"
names(reports_coocs1)[names(reports_coocs1) == "nobjective"] <- "objective"
names(reports_coocs1)[names(reports_coocs1) == "antimicrobialn"] <- "antimicrobial"
names(reports_coocs1)[names(reports_coocs1) == "n"] <- ""
names(reports_coocs1)[names(reports_coocs1) == "-n"] <- ""
names(reports_coocs1)[names(reports_coocs1) == "andn"] <- "and"
names(reports_coocs1)[names(reports_coocs1) == "-"] <- ""
names(reports_coocs1)[names(reports_coocs1) == " -n"] <- ""
names(reports_coocs1)[names(reports_coocs1) == " "] <- ""
names(reports_coocs1)[names(reports_coocs1) == "©"] <- ""
names(reports_coocs1)[names(reports_coocs1) == "nnn"] <- ""
names(reports_coocs1)[names(reports_coocs1) == "nnnnnimplementation"] <- "implementation"
names(reports_coocs1)[names(reports_coocs1) == "nnthis"] <- "this"
names(reports_coocs1)[names(reports_coocs1) == "nnthe"] <- "the"
names(reports_coocs1)[names(reports_coocs1) == "sectorn"] <- "sector"
reports_coocs1 <- reports_coocs1[!grepl("–n", names(reports_coocs1))]

reports_coocs1 <- reports_coocs1[names(reports_coocs1) != ""] # Removing no name values

# Aggregate as some may have the same name
reports_coocs1  <- tapply(reports_coocs1, names(reports_coocs1), sum)
reports_coocs1 <- reports_coocs1[order(reports_coocs1, decreasing = TRUE)]
reports_coocs1
#-----------------------------------------------
# Aggregating variations as per above
#-----------------------------------------------

# Replace names containing "australia" with "australia"
names(reports_coocs1)[grepl("australia", names(reports_coocs1))] <- "australia"

# Sectors and sector
names(reports_coocs1)[grepl("sector", names(reports_coocs1))] <- "sector"

# Sectors and sector
names(reports_coocs1)[grepl("animal", names(reports_coocs1))] <- "animal"

# Removing "and" due to it being a conjunction with no ideological value
reports_coocs1 <- reports_coocs1[!grepl("and", names(reports_coocs1))]

# Aggregate by summing values for each name
reports_coocs1 <- tapply(reports_coocs1, names(reports_coocs1), sum)

# Order by decreasing values
reports_coocs1 <- reports_coocs1[order(reports_coocs1, decreasing = TRUE)]


# Reducing the document-feature matrix to contain the top 20 collates
reports_reduced_dfm <- dfm_select(reports_sentence_dfm, pattern = c(names(reports_coocs1[1:50]), 
                                                              "antimicrobial"))

# Transforming the document feature matrix to feature co-currence matrix

reports_tagged_fcm <- fcm(reports_reduced_dfm)

#-------------------------------------------
# Visualising the network reports_network
#-------------------------------------------

p3 = textplot_network(reports_tagged_fcm, min_freq = 30,
                 edge_alpha = 0.25,
                 edge_size = 3,
                 edge_color = "#007D4E",
                 vertex_labelsize = 6,
                 vertex_labelfont = "HelveticaNeueLT Pro 55 Roman") +
  labs(caption = "Australian progress report documents", title = "C)") +
  theme(title = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman", face = "bold"))
p3

plan_coocs1[1:50]
coocs1[1:50]
reports_coocs1[1:50]


#----------------------------------
# Combined plot
#----------------------------------

library(patchwork)

# Arrange plots in desired layout
combined_plot <- grid.arrange(
  arrangeGrob(p1, ncol = 1),
  arrangeGrob(p2, p3, ncol = 2, widths = c(1, 1)),
  ncol = 1,
  heights = c(1, 1)
)

# Display the combined plot
print(combined_plot)

#---------------------------------------------------------
# German NAPs
#---------------------------------------------------------


setwd("C:/Users/Ryzen/Documents/Journal Article Manuscripts/Topic Modelling of NAP/Documents for Analysis/Germany documents")


# Importing the files

german_files <-list.files(pattern = "pdf$")

german_pdfs <- lapply(german_files, pdf_text)

# Creating a corpus
german_documents <- Corpus(VectorSource(german_pdfs)) # Creating a corpus

# Applying transformations to remove numbers + special characters
german_documents <- tm_map(german_documents, content_transformer(tolower)) # Lowercase all words
german_documents <- tm_map(german_documents, removeNumbers)  # Remove numbers
german_documents <- tm_map(german_documents, removeWords, stopwords("english"))  # Remove English stop words
german_documents <- tm_map(german_documents, removePunctuation, preserve_intra_word_dashes = TRUE)  # Remove punctuation
german_documents <- tm_map(german_documents, stripWhitespace)  # Remove whitespace
german_documents <- tm_map(german_documents, content_transformer(function(x) trimws(x, "both"))) # Remove extra white space
german_documents <- tm_map(german_documents, lemmatize_strings)  # Lemmatize words
german_documents <- tm_map(german_documents, content_transformer(function(x) iconv(x, "latin1", "ASCII", sub = ""))) # Removing Non-ASCII characters
german_documents <- tm_map(german_documents, content_transformer(gsub), pattern = "\\\\n", replacement = " ") # Remove the '\n' characters from the corpus
german_documents <- tm_map(german_documents, content_transformer(gsub), pattern = "\\\n", replacement = " ")
german_documents <- tm_map(german_documents, content_transformer(gsub), pattern = "\\n", replacement = " ")
german_documents <- tm_map(german_documents, content_transformer(gsub), pattern = "nnnn", replacement = "")



# Creating Document Term Matrix (DTM)
German_DTM <- DocumentTermMatrix(german_documents)
German_DFM <- as.dfm(German_DTM)


# Testing initial model

# Calculate coherence scores for different numbers of topics (adjust 'max_k' based on preference)
max_k <- 20

coherence_scores <- FindTopicsNumber(German_DTM, topics = seq(2, max_k), method = "Gibbs",
                                     metrics = c("Griffiths2004","CaoJuan2009", "Arun2010", "Deveaud2014"),
                                     control = list(seed = 4006))

# Plot the coherence scores to find the optimal number of topics
coherence_scores[max(coherence_scores$Griffiths2004), ]

# Find the row with the maximum Griffiths2004 score
max_row <- coherence_scores %>%
  filter(Griffiths2004 == max(Griffiths2004))

# Extract the number of topics from the max_row
optimal_topics <- max_row$topics
max_row

# Print the result
cat("The optimal number of topics based on the maximum Griffiths2004 score is:", optimal_topics)


# Graphing the result
coherence_scores %>% 
  ggplot() +
  geom_line(aes(x = topics, y = Griffiths2004), size = 1, colour = "#0F6EB9") +
  theme_Publication() +
  labs(x = "Topics", y = "Coherence",
       caption = "Coherence scores for different numbers of topics in\ntopic modelling using UMass measure (Griffiths 2004)") +
  scale_x_continuous(breaks = seq(min(coherence_scores$topics), max(coherence_scores$topics), by = 1)) +
  scale_y_continuous(labels = scales::scientific_format()) +
  geom_point(data = coherence_scores[coherence_scores$topics == 12, ], 
             aes(x = topics, y = Griffiths2004), colour = "#E52934", size = 3) +
  ggplot2::annotate("text", x = 11, y = coherence_scores$Griffiths2004[coherence_scores$topics == 12] - 3000,
                    label = "Optimal coherence\nat 12 topics", hjust = 0, vjust = 0, color = "red", family = "HelveticaNeueLT Pro 55 Roman")


#--------------------------------------------------
# Iteration of model with optimal topics
#--------------------------------------------------

# Building model
German_Model1 <- LDA(German_DTM, method = "gibbs", k = 12, control = list(alpha = 0.5, seed = 4006))
German_Model1
# Visualising the model

# Looking at terms
terms(German_Model1, 10)



# Print the result
cat("The optimal number of topics based on the maximum Griffiths2004 score is:", optimal_topics)


#------------------------------------
# Finding the optimal alpha value
#------------------------------------

# Setting up alpha values
alpha_values <- seq(0.1, 1.0, 0.1)

# Create a list to store the results
results <- list()

# Create a vector to store the results
results <- sapply(alpha_values, function(alpha) {
  # Build the model
  model <- LDA(German_DTM, method = "gibbs", k = 12, control = list(alpha = alpha, seed = 4006))
  # Evaluate the model
  return(logLik(model))
})


# Find the best model
best_alpha <- alpha_values[which.max(results)]
best_alpha # Most likely beta value at 0.8

#--------------------------------------------------
# 3rd iteration of model with optimal topics
#--------------------------------------------------

# Building model
German_Model2 <- LDA(German_DTM, method = "gibbs", k = 12, control = list(alpha = 0.7, seed = 4006))
German_Model2

# Looking at terms
terms(German_Model2, 10)
#-------------------------------------------------------------------------
# Term finder function to replace spelling errors due to pdf formatting
#------------------------------------------------------------------------

# Define the term to search for
term_to_search <- "nlegal"

# Loop through the topics and find the index where the term appears
for (i in seq_along(German_Model2@terms)) {
  terms_for_topic <- German_Model2@terms[[i]]
  term_index <- which(terms_for_topic == term_to_search)
  
  # If the term is found in this topic, print the topic index and replace the term
  if (length(term_index) > 0) {
    cat("Found in topic", i, "at index", term_index, "\n")
    # You can choose to replace the term here
  }
}

# Replacing erroneous/misspelled values values due to PDF formatting
German_Model2@terms[3047] <- "interim"
German_Model2@terms[183] <- "antimicrobial"
German_Model2@terms[4734] <- "what"
German_Model2@terms[5817] <- "report"
German_Model2@terms[1630] <- "for"
German_Model2@terms[1771] <- "the"
German_Model2@terms[4731] <- "in"
German_Model2@terms[2479] <- "deutsche"
German_Model2@terms[3098] <- "report"
German_Model2@terms[3808] <- "sepsis"
German_Model2@terms[3808] <- "sepsis"
German_Model2@terms[4766] <- "spread"
German_Model2@terms[2048] <- "projects"
German_Model2@terms[1794] <- "veterinary"
German_Model2@terms[4757] <- "results"
German_Model2@terms[3780] <- "legal"


#-----------------------------
# Creating the matrices
#-----------------------------

# Beta values of the  german model
german_beta_topics_gibbs2 <- tidy(German_Model2, matrix = "beta")
german_beta_topics_gibbs2

print(german_beta_topics_gibbs2, n = 10)

# Gamma values of the matrix
german_gamma_documents_gibbs2 <- tidy(German_Model2, matrix = "gamma")
german_gamma_documents_gibbs2


# Dataframe for gamma values
german_doc_gamma_df_gibbs2 <- data.frame(german_gamma_documents_gibbs2)
german_doc_gamma_df_gibbs2$chapter <- rep(1:dim(German_DTM)[1], 12)

german_doc_gamma_df_gibbs2

#------------------------------
# Visualising
#------------------------------

# Colour palette
lego_colours <- c("#FDBE02", "#0E2A47", "#00A844", "#0F6EB9",
                  "#2D3192", "#00B2EF", "#E52934", "#F58220",
                  "#FFD100", "#FF6A00", "#FF0000", "#920A00")

# Beta Topics


# Grouping terms by topic
german_beta_top_terms_gibbs2 <- german_beta_topics_gibbs2 %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 10) %>% 
  ungroup() %>% 
  arrange(topic, -beta)


german_beta_top_terms_gibbs2 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free", ncol = 4) +
  scale_y_reordered() + theme_Publication() + labs(x = expression(beta), y = "Terms") +
  scale_fill_manual(values = lego_colours) +
  geom_text(aes(label = round(beta,3)), hjust = 1.1, family = "HelveticaNeueLT Pro 55 Roman",
            colour = "white") +
  theme(text = element_text(size = 18))

# Topic names with the labels

# By document
topic_labels <- c(
  `1` = "European\ncollaboration",
  `2` = "Veterinary\npractice",
  `3` = "Legislative\nmechanisms\nand\nregulations",
  `4` = "Collaborative\ncoordination",
  `5` = "Antibiotic\nresistance\nresearch",
  `6` = "Achievements",
  `7` = "Project\nreports\nand\naction",
  `8` = "International\ncollaboration",
  `9` = "Strategies\nbeyond\nresearch",
  `10` = "Reporting\nand\nevaluation",
  `11` = "Environmental\nhealth",
  `12` = "Medical\nprofessionals"
)


german_beta_top_terms_gibbs2 %>% 
  mutate(term = reorder_within(term, beta, topic)) %>% 
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = F) +
  facet_wrap(~topic, scales = "free", ncol = 4, labeller = labeller(topic = topic_labels)) +
  scale_y_reordered() + theme_Publication() + labs(x = expression(beta), y = "Terms") +
  scale_fill_manual(values = lego_colours) +
  geom_text(aes(label = round(beta,3)), hjust = 1.1, family = "HelveticaNeueLT Pro 55 Roman",
            colour = "white") +
  theme(text = element_text(size = 18))



# Specify the order of document names
german_document_names2 <- c("German\nreport (2015)",
                    "Interim\nreport (2016)",
                    "Interim\nreport (2017)",
                    "Interim\nreport (2018)",
                    "Interim\nreport (2019)")


# Specify the order of document names
german_document_facet <- c(`1` = "German\nreport (2015)",
                           `2` ="Interim\nreport (2016)",
                           `3` ="Interim\nreport (2017)",
                           `4` ="Interim\nreport (2018)",
                           `5` ="Interim\nreport (2019)")


# Gamma

german_doc_gamma_df_gibbs2

# Add the topic_name column to the dataframe
german_doc_gamma_df_gibbs2 <- german_doc_gamma_df_gibbs2 %>%
  mutate(topic_name = factor(topic, levels = names(topic_labels), labels = topic_labels))


# Visualisation
german_doc_gamma_df_gibbs2 %>% 
  ggplot(aes(x = factor(document, levels = seq(1, 5)), y = gamma, fill = topic_name)) +
  geom_col(position = "dodge", width = 0.8, colour = "black") +
  facet_wrap(~ topic_name, nrow = 4) +
  theme_Publication() +
  labs(x = "Topic", y = expression(gamma), fill = "",
       caption = expression(paste(gamma, " values < 0.0001 omitted"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.05)) +
  scale_fill_manual(values = lego_colours) + 
  geom_text(aes(label = ifelse(gamma <= 0.0001, "", round(gamma, 3))), vjust = -1,
            family = "HelveticaNeueLT Pro 55 Roman") +
  scale_x_discrete(labels = german_document_names2) +  
  theme(axis.title.y = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.y = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.x = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        legend.position = "none", plot.caption = element_text(size = 12), 
        text = element_text(size = 18))



# Visualisation by topic
german_doc_gamma_df_gibbs2 %>% 
  ggplot(aes(x = factor(topic_name), y = gamma, fill = document)) +
  geom_col(position = "dodge", width = 0.8, colour = "black") +
  facet_wrap(~ document, nrow = 4,
             labeller = labeller(document = german_document_facet)) +
  theme_Publication() +
  labs(x = "Topic", y = expression(gamma), fill = "",
       caption = expression(paste(gamma, " values < 0.0001 omitted"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.05)) +
  scale_fill_manual(values = lego_colours) + 
  geom_text(aes(label = ifelse(gamma <= 0.0001, "", round(gamma, 3))), vjust = -1,
            family = "HelveticaNeueLT Pro 55 Roman") +
  theme(axis.title.y = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.y = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        axis.text.x = element_text(size = 12, family = "HelveticaNeueLT Pro 55 Roman"),
        legend.position = "none", plot.caption = element_text(size = 12), 
        text = element_text(size = 18))



# Stratified document analysis

# Function for cocurrence statistics calculator
source("https://slcladal.github.io/rscripts/calculateCoocStatistics.R")

# What term do we want to use the cocurrence statistics for?
plan_coocterm1 <- "antimicrobial"

# Calculation of statistic
german_plan_coocs <- calculateCoocStatistics(plan_coocterm1, German_DFM,measure = "LOGIK")

german_plan_coocs


# Aggregate as some may have the same name
german_plan_coocs  <- tapply(german_plan_coocs, names(german_plan_coocs), sum)
german_plan_coocs <- german_plan_coocs[order(german_plan_coocs, decreasing = TRUE)]
german_plan_coocs
#-----------------------------------------------
# Aggregating variations as per above
#----------------------------------------------


# Reducing the document-feature matrix to contain the top 20 collates
german_plan_reduced_dfm <- dfm_select(German_DFM, pattern = c(names(german_plan_coocs[1:50]), 
                                                              "antimicrobial"))

# Transforming the document feature matrix to feature co-currence matrix

german_plan_tagged_fcm <- fcm(german_plan_reduced_dfm)

#-------------------------------------------
# Visualising the network plan_network
#-------------------------------------------

p4 = textplot_network(german_plan_tagged_fcm, min_freq = 60,
                      edge_alpha = 0.25,
                      edge_size = 3,
                      edge_color = "#C5192D",
                      vertex_labelsize = 6,
                      vertex_labelfont = "HelveticaNeueLT Pro 55 Roman") +
  labs(caption = "German AMR documents") +
  theme(title = element_text(size = 16, family = "HelveticaNeueLT Pro 55 Roman", face = "bold"))
p4
