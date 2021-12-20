# read in data from text file
sonar_scans <- readLines("day_one_dat.txt") |> as.integer()

# initialize object with integer zero
inc_count <- 0

# iterate through all sonar scan measurements
for(i in 1:length(sonar_scans) - 1) {
  ifelse(sonar_scans[i + 1] > sonar_scans[i], # if a measurement is greater than the preceding
         inc_count <- inc_count + 1,          # element, add one to the inc_count.
         inc_count <- inc_count)              # otherwise, do not add to the count
}

# check out the final count
inc_count
