
national_production_data_2016_path = "~/Google Drive/Skola/SRC/Thesis/Code/Scripts/output/national_production_data_2016.csv"
national_production_data_2016 = read_csv(national_production_data_2016_path)


national_production_data_2016 %>% 
  filter(flex)
