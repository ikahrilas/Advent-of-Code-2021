##-- Part one, just find the number of increases from the previous measurement
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

# even simpler answer -- in a single line of code!
sum(diff(sonar_scans) > 0)

##-- Part two: find the number of increases for the sliding 3 measurement window
sum(diff(rowSums(embed(sonar_scans, 3))) > 0)



