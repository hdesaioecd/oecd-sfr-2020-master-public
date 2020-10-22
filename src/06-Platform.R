#script for all updates to the SoF platform

#first,take care of the snail
agg <- aggr.pca_adj %>%
  arrange(Fragility) %>% 
  select(iso3c,'2019' = Fragility) 
fwrite(agg,"./platform/agg.csv")

#calculate each of the dimensional orderings as well, keeping in mind that they should be ordered according to their fragility score within a given cluster
clusters_platform <- clusters_export_for_designer %>%
  pivot_longer(names_to = "dimension",
               values_to = "value",-c(iso3c,country)) %>% 
  mutate(value = gsub("Darkest Shade","1",value),
         value = gsub("Second-darkest shade","2",value),
         value = gsub("Third-darkest shade","3",value),
         value = gsub("Fourth-darkest shade","4",value),
         value = gsub("Lightest shade","5",value))

dim_scores <- raw.data %>% 
  filter(year == 2019) %>% 
  select(iso3c,ends_with(".PC1")) %>% 
  pivot_longer(names_to = "dimension",
               values_to = "frag.score",-iso3c) %>% 
  mutate(dimension = gsub(".PC1","",dimension))

clusters_platform_exp <- clusters_platform %>% 
  left_join(dim_scores,c("iso3c","dimension")) %>% 
  group_by(dimension,value) %>% 
  arrange(frag.score) %>% 
  split(.$dimension) %>% 
  map(function(x) x %>% 
        ungroup() %>% 
        select(iso3c,value,frag.score) %>% 
        mutate(id = row_number(),
               `2019` = paste0(value,".0",id)) %>% 
        select(iso3c,`2019`))
write.xlsx(clusters_platform_exp,"./platform/clusters.xlsx")

clusters_overall_scores <- clusters_platform %>% 
  split(.$dimension) %>% 
  map(function(x) x %>% 
        select(iso3c,value) %>% 
        rename(`2019` = value))
write.xlsx(clusters_overall_scores,"./platform/clusters (unordered).xlsx")
#indicator exports - create quantiles after filtering, and be sure to reverse order of values so that 1 = severe, 5 = minor. 
indicators <- sfr.time.series %>%
  filter(year == 2019) %>%
  select(iso3c,year,variablename,dimension,value) %>% 
  split(.$variablename) %>% 
  map(function(x) x %>% 
        mutate(`2019` = ntile(-value,5)) %>% 
        select(iso3c,`2019`,dimension) %>% 
        filter(iso3c %in% unique(aggr.pca_adj$iso3c)))
write.xlsx(indicators,"./platform/indicators.xlsx")