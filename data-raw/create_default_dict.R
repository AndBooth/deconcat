
default_dict <- read.table("./data-raw/sourcedata/words-by-frequency.txt", header = TRUE, stringsAsFactors = FALSE)
N <- nrow(default_dict)
default_dict$cost <- log(1:nrow(default_dict) * log(N))
