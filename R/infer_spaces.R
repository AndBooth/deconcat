
#' @export

deconcat <- function(s, word_costs, max_word) {

  s_length <- stringr::str_length(s)
  cost <- vector(mode = "double", length = s_length+1)

  for (i in 2:(s_length+1)) {

    candidates <- rev(cost[max(c(1, i - max_word - 1)):(i-1)])
    k <- 1:length(candidates)

    curr_ss <- stringr::str_sub(s, (i-k), i - 1 )
    curr_cost <- word_costs$cost[fastmatch::fmatch(curr_ss, word_costs$word)]
    curr_res <- candidates + curr_cost

    cost[i] <- curr_res[which.min(curr_res)]

  }



  ii <- s_length + 1

  cost_new <- vector()
  found_words <- vector()

  idx <- 1

  while(ii > 1) {

    candidates <- rev(cost[max(c(1, ii - max_word - 1)):(ii-1)])
    kk <- 1:length(candidates)
    # candidates <- cost[max(c(1, i - max_word)):i, , drop = FALSE]

    curr_ss <- stringr::str_sub(s, (ii-kk), ii - 1 )
    curr_cost <- word_costs$cost[fastmatch::fmatch(curr_ss, word_costs$word)]
    curr_res <- candidates + curr_cost
    names(curr_res) <- kk

    cost_new[idx] <- curr_res[which.min(curr_res)]
    names(cost_new)[idx] <- names(curr_res[which.min(curr_res)])
    #print(cost_new)
    found_words[idx] <- stringr::str_sub(s, ii-as.numeric(names(curr_res[which.min(curr_res)])), ii-1)
    ii <- ii - as.numeric(names(curr_res[which.min(curr_res)]))
    idx <- idx + 1
  }

    paste(rev(found_words), collapse = " ")

}




#' @export
deconcat_dev <- function(s, word_costs, max_word) {

  if (any(!c("word", "cost") %in% colnames(word_costs))) {
    stop("Dictionary columns misnamed - they must be 'word' and 'cost'")
  }

  s_length <- stringr::str_length(s)
  cost <- vector(mode = "double", length = s_length + 1)

  for (i in 2:(s_length+1)) {

    candidates <- rev(cost[max(c(1, i - max_word - 1)):(i-1)])
    k <- 1:length(candidates)

    curr_ss <- stringr::str_sub(s, (i-k), i - 1 )
    curr_cost <- word_costs$cost[fastmatch::fmatch(curr_ss, word_costs$word)]
    curr_res <- candidates + curr_cost
    cost[i] <- curr_res[which.min(curr_res)]

  }

  ii <- s_length + 1
  idx <- 1

  cost_new <- vector()
  found_words <- vector()

  while(ii > 1) {

    candidates <- rev(cost[max(c(1, ii - max_word - 1)):(ii-1)])
    kk <- 1:length(candidates)

    curr_ss <- stringr::str_sub(s, (ii-kk), ii - 1 )
    curr_cost <- word_costs$cost[fastmatch::fmatch(curr_ss, word_costs$word)]
    curr_res <- candidates + curr_cost

    names(curr_res) <- 1:length(candidates)

    cost_new[idx] <- curr_res[which.min(curr_res)]
    names(cost_new)[idx] <- names(curr_res[which.min(curr_res)])
    found_words[idx] <- stringr::str_sub(s, ii-as.numeric(names(curr_res[which.min(curr_res)])), ii-1)
    ii <- ii - as.numeric(names(curr_res[which.min(curr_res)]))
    idx <- idx + 1
  }
  paste(rev(found_words), collapse = " ")
}



#' @export
load_default_dict <- function(){

  load("./data/default_dict.rda", envir = .GlobalEnv)  # Loads the default dictionary to global environment
  message("Default dictionary loaded to Global Environment")

}


