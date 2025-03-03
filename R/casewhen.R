zonal_df <- zonal_df %>% 
  mutate(color = case_when(
  service == 'Nature Access' ~ '#A57C00',
  service == 'Nitrogen Export' ~ '#2c944c',
  service == 'Pollination' ~ '#dd1c77',
  service == 'Sediment Export' ~ '#08306b',
  TRUE ~ color
))


