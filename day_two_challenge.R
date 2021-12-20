##-- part one
# use the tidyverse for this one
library(tidyverse)
library(stringi)

# read in data from text file
position_dat <- readLines("day_two_dat.txt")

position_dat <- data.frame(direction = position_dat) |> # create data frame
  separate(col = direction,
           into = c("direction", "magnitude"), # separate the column into direction
           sep  = " ") |>                      # and magnitude
  mutate(magnitude = as.integer(magnitude)) |> # make magnitude integer type
  group_by(direction) |>                       # group by direction
  summarize(total = sum(magnitude))            # calculate totals by direction

# calculate the depth
depth <- position_dat[1, 2] - position_dat[3,  2]

# multiple by horizontal position
depth * position_dat[2, 2]

##-- part two
position_dat <- read.table("day_two_dat.txt",
                           sep = " ",
                           colClasses = c("character", "integer"),
                           col.names = c("direction", "magnitude"))

depth <- 0
horiz <- 0
aim <- 0

for(i in 1:nrow(position_dat)) {
  if(position_dat[i, 1] == "forward") {
    horiz <- horiz + position_dat[i, 2]
    depth <- depth + (aim * position_dat[i, 2])
  } else if(position_dat[i, 1] == "down") {
    aim <- aim + position_dat[i, 2]
  } else {
    aim <- aim - position_dat[i, 2]
  }
}

# derive answer by multiplying horizontal position by depth
depth * horiz
