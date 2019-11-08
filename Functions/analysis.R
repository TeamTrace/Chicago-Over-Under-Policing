
# Load data
arrest_dist <- read_csv("Output/summary_tables/share_arrest_police_district.csv", guess_max = 8000000)
op_up <- read_csv("Output/summary_tables/arrest_rates_police_district.csv", guess_max = 8000000)

crime_stats <- read_csv("Output/summary_tables/crime_stats.csv", guess_max = 8000000)

districts <- read_csv("Output/districts_setup_edit.csv", guess_max = 8000000)

crime <- read_csv("Output/crime_setup_edit.csv", guess_max = 8000000)
# make shooting extract
shot <- crime %>% filter(!is.na(shot_cat))

staff <- read_csv("Output/summary_tables/staff_summary_invisible_institute.csv", guess_max = 2000000)
quarter <- read_csv("Output/staff_quarter_invisible_institute.csv", guess_max = 2000000)
sworn_join <- read_csv("Output/staff_join_invisible_institute.csv", guess_max = 2000000)


