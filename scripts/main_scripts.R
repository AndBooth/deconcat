
library(tidyverse)
library(profvis)

#### Test script

# Load dictionary data

deconcat::load_default_dict()

# string to translate
s <- paste(sample(default_dict$word, size = 50), collapse = "")
s1 <- "thisstringisshort"
# Calculate word costs

max_length <- max(map_int(default_dict$word, ~ str_length(.)))

profvis({
  deconcat_dev(s, word_costs = default_dict, max_word = max_length)
})


microbenchmark::microbenchmark(
  deconcat(s1, word_costs = default_dict, max_word = max_length),
  deconcat_dev(s1, word_costs = default_dict, max_word = max_length)
)


test_strings <- sapply(1:1000, function(x){paste(sample($word, sample(2:10, 1, replace = T)), collapse = "")})

all.equal(sapply(test_strings, function(x){
  deconcat(x, word_costs = words_by_freq, max_word = max_length)
}),sapply(test_strings, function(x) {
  deconcat_dev(x, word_costs = words_by_freq, max_word = max_length)}))

microbenchmark::microbenchmark(sapply(test_strings, function(x){
                                deconcat(x, word_costs = words_by_freq, max_word = max_length)
                                }),
                               sapply(test_strings, function(x) {
                                 deconcat_dev(x, word_costs = words_by_freq, max_word = max_length)})
                                 ,
                               times = 5)

