# this has become way more convoluted than it needs to be...

GetSocrata <- function(url){
  dt <- data.table(read.socrata(url = url, app_token="sjgsoeUq8DRLDUvmLsc8BpEkY"))
}

GetOccupationData <- function(){
  data <-PrepareOccupationData()
  nodes_and_links <- GetNodesAndLinks(data)
  
  return(append(x = nodes_and_links, list(data = data)))
}

PrepareOccupationData <- function(){
  occupation <- GetSocrata(url = "https://data.edmonton.ca/resource/b6h3-ft29.json")
  occupation_long <- data.table(gather(occupation, key = "job_type", value = "number", -neighbourhood_name, -neighbourhood_number,-ward))
  
  occupation_long <- occupation_long[!job_type %in% c("no_response", "other")]
  occupation_long[, number := as.numeric(number)]
  
  occupation_long[, fraction := number / sum(number), by = neighbourhood_name]
  
  # remove neighbourhoods without results
  occupation_long <- occupation_long[neighbourhood_name %in% occupation_long[fraction > 0]$neighbourhood_name]
  
  occupation_wide <- spread(occupation_long[!job_type %in% c("no_response", "other"), list(neighbourhood_name, ward, job_type, fraction)], key = job_type, fraction)
  return(data.table(occupation_wide)) 
}

PrepareLengthOfResidenceData <- function(){
  length_of_residence <- GetSocrata(url = "https://data.edmonton.ca/resource/wief-svjr.json")
  length_of_residence[, ward := toupper(ward)]
  length_of_residence[ownership != "No Response"]
  setnames(length_of_residence, 
           old = c("X_1_year_to_less_than_3_years","X_3_years_to_less_than_5_years","X_5_years_or_more","child_less_than_1_year","less_than_1_year"),
           new = c("years_1_3","years_3_5","year_gt_5","year_lt_1_child","year_lt_1"))
  length_of_residence_long <- data.table(gather(length_of_residence, key = "residence_period", value = "number", -neighbourhood_name, -neighbourhood_number, -ward, -ownership))
  length_of_residence_long <- length_of_residence_long[!ownership %in% c("no_response", "other")]
  
  length_of_residence_long[, number := as.numeric(number)]
  length_of_residence_long[, fraction := number / sum(number), by = neighbourhood_name]
  
  length_of_residence_long <- length_of_residence_long[neighbourhood_name %in% length_of_residence_long[fraction > 0]$neighbourhood_name]
  
  length_of_residence_long[, own_period_residence := paste(ownership,residence_period, sep="_")]
  
  length_of_residence_wide <- spread(length_of_residence_long[!residence_period %in% c("No Response", "Other"), list(neighbourhood_name, ward, own_period_residence, fraction)], key = own_period_residence, fraction)
  return(length_of_residence_wide)
}

PrepareGenderAgeData <- function(){
  population_age_gender <- GetSocrata("https://data.edmonton.ca/resource/adgc-xwyt.json")
  population_age_gender[, ward := toupper(ward)]
  population_age_gender <- population_age_gender[age != "No Response"]
  #population_age_gender
  population_age_gender_long <- data.table(gather(population_age_gender[, list(neighbourhood_name, ward, age, male, female)], key = "gender", value = "number", -neighbourhood_name,-ward, -age))
  
  l_ply(seq(0,70, 10), function(x){
    ages <- seq(x, x+9, 1)
    population_age_gender_long[age %in% ages, age_range := paste("age", x, x+9, sep = "_")]
  })
  population_age_gender_long[age %in% c("80","81","82","83","84","85+"), age_range := "age_80+"]
  population_age_gender_long[, gender_age := paste(gender, age_range, sep = "_")]
  
  population_age_gender_long[, number := as.numeric(number)]
  population_age_gender_long <- population_age_gender_long[, list(number = sum(number)), by = list(neighbourhood_name, ward, gender_age)]
  population_age_gender_long[, fraction := number / sum(number), by = neighbourhood_name]
  
  population_age_gender_long <- population_age_gender_long[neighbourhood_name %in% population_age_gender_long[fraction > 0]$neighbourhood_name]
  
  population_age_gender_wide <- spread(population_age_gender_long[, list(neighbourhood_name, ward, gender_age, fraction)], key = gender_age, fraction)
}

GetNodesAndLinks <- function(wide_table, category_name = "neighbourhood_name"){
  var_names <- setdiff(names(wide_table),category_name)
  nodes <- data.table(name = unique(wide_table[[category_name]]))
  nodes <- data.table(mutate(nodes, NUMBER = as.numeric(row.names(nodes)), name = as.character(name)))
  wide_table_df <- data.frame(wide_table)
  row.names(wide_table_df) <- wide_table_df[[category_name]]
  distance_matrix <- as.matrix(dist(wide_table_df))
  links <- melt(distance_matrix)
  names(links) <- c("name1", "name2", "DISTANCE")
  
  # I prefer working with data tables
  links <- data.table(links)
  links[, name1 := as.character(name1)]
  links[, name2 := as.character(name2)]
  links <- links[(DISTANCE > 0)]

  GetNumber <- function(x){
    nodes[name == x, NUMBER[1]] - 1
  }
  
  l_ply(nodes$name, function(x) {
    links[name1 == x, SOURCE := GetNumber(x)]
    links[name2 == x, TARGET := GetNumber(x)]
  })
  
  setkey(links, DISTANCE)
  links[, ORDER := as.numeric(row.names(.SD)), by = SOURCE]
  
  output <- list(nodes = copy(nodes),
                 links = copy(links))
  return(output)
}
