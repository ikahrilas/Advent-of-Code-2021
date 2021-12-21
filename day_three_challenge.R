##-- part one
# use stringr and tidyverse packages
library(stringr)
library(tidyverse)

# read in the data
dat <- readLines("day_three_dat.txt") |>
  str_split(patter = "", simplify = TRUE) |>
  as.data.frame() |>
  mutate(across(.cols = everything(), .fns = ~ as.numeric(.x)))

# define mode function
Mode <- function(x, max_or_min = "max"){
  if(max_or_min == "max") {
  names(which.max(table(x)))
  } else if(max_or_min == "min") {
    names(which.min(table(x)))
  } else {
    stop("'max_or_min' must be 'max' or 'min'")
  }
  }

# find gamma and epsilon rates
modes <- map_chr(dat, ~ Mode(.x)) |> paste0(collapse = "")

gamma_rate <- strtoi(modes, base = 2)

inv_modes <- map_chr(dat, ~ Mode(.x, "min")) |> paste0(collapse = "")

epsilon_rate <- strtoi(inv_modes, base = 2)

# derive answer
gamma_rate * epsilon_rate

##-- part two
# derive oxygen generator rating
o2_gen_rating <- dat

for(j in 1:ncol(o2_gen_rating)) {
  o2_gen_rating <- o2_gen_rating[o2_gen_rating[j] == Mode(o2_gen_rating[j]),]
}

o2_gen_rating <- o2_gen_rating |> paste0(collapse = "") |> strtoi(base = 2)

# derive CO2 scrubber rating
co2_gen_rating <- dat
co2_gen_rating["keep"] <- TRUE

for(j in 1:ncol(co2_gen_rating)) {
  for(i in 1:nrow(co2_gen_rating)){
    if(co2_gen_rating[i, j] != Mode(co2_gen_rating[j])){
      co2_gen_rating[i, "keep"] <- FALSE
    } else {
      NULL
    }
  }
}

co2_gen_rating["keep"] |> table()

co2_gen_rating <- co2_gen_rating |> paste0(collapse = "") |> strtoi(base = 2)

# derive answer
o2_gen_rating * co2_gen_rating
