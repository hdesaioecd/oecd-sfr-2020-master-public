#this script analyses changes in the fragility framework in line with SFR 2018.
#HD updated this script for SFR 2020 starting Aug. 27, 2019, adding sections on analyses
#with descriptive stats and tests of variables.
output_folders <- c("./data_out2020/")
#extract scaled dataset of all variables
sfr.time.series_export <- sfr.time.series %>%
  select(iso3c, year, variablename, dimension, value) %>%
  split(.$dimension) %>%
  map(
    function(x)
      x %>%
      group_by(variablename) %>%
      pivot_wider(names_from = "variablename",
                  values_from = "value") %>%
      select(-dimension) %>%
      left_join(aggr.pca_adj %>% select(iso3c, fragility.level), "iso3c")
  )
write.xlsx(
  sfr.time.series_export,
  paste0(
    output_folders,
    "export of indicators (with imputed values).xlsx"
  )
)

`%!in%` = Negate(`%in%`)


# Country Breakdown by Groups ---------------------------------------------

#import list of population from UNDESA, multiply result by 1000 as source data is represented in thousands
population_filtered <-
  read_excel("./data/population data/world_pop_2018_timeseries.xlsx",
             "ESTIMATES",
             skip = 16) %>% clean_names() %>%
  dplyr::filter(type == "Country") %>%
  dplyr::select(-c(index, variant, notes, country_code, type, parent_code)) %>%
  dplyr::rename(country = region_subregion_country_or_area) %>%
  pivot_longer(names_to = "year", values_to = "value", -country) %>%
  dplyr::mutate(
    value = as.numeric(value) * 1000,
    year = gsub("x", "", year),
    year = as.numeric(year),
    iso3c = countrycode(
      .$country,
      "country.name.en",
      "iso3c",
      custom_match = c("Eswatini" = "SWZ",
                       "Romania" = "ROM")
    )
  ) %>%
  dplyr::rename(population = value)

##import list of ODA eligible recipients - should amount to 143
ODA_recipients <-
  read_xls("./data/additional data/DAC-CRS-CODES.xls", sheet = "Recipient") %>%
  clean_names() %>% remove_empty("cols") %>% dplyr::select(-x13) %>%
  dplyr::mutate(iso3c = countrycode(
    recipient_name_en,
    "country.name.en",
    "iso3c",
    custom_match = c(
      "Eswatini" = "SWZ",
      "Kosovo" = "XKX",
      "Micronesia" = "FSM",
      "Netherlands Antilles" = "SXM"
    )
  )) %>%
  dplyr::filter(complete.cases(iso3c),
                dac_income_group != "MADCTs") %>%
  dplyr::select(iso3c, country = recipient_name_en, region) %>%
  dplyr::mutate(category = "ODA eligible")

#use oecd classifications of country groupings
oecd_classifications <-
  read_xls("./data/additional data/DAC-CRS-CODES.xls",
           sheet = "Recipient") %>%
  clean_names() %>%
  filter(dac_income_group != "Part I unallocated by income") %>%
  dplyr::mutate(iso3c = countrycode(
    recipient_name_en,
    "country.name",
    "iso3c",
    custom_match = c(
      "Eswatini" = "SWZ",
      "Kosovo" = "XKX",
      "Micronesia" = "FSM",
      "Netherlands Antilles" = "ANT"
    )
  )) %>%
  dplyr::select(iso3c, dac_income_group, region) %>%
  dplyr::filter(complete.cases(iso3c))

#income classification
oecd_class_income <-
  oecd_classifications %>% dplyr::select(iso3c, group = dac_income_group)

#region classifications, with income classifications r-bind
oecd_class <-
  oecd_classifications %>% dplyr::select(iso3c, group = region) %>%
  rbind(oecd_class_income) %>%
  dplyr::mutate(group = paste0("OECD Classes - ", group))

#create lists limited to the ODA eligible recipients
oecd_nonfragile <- ODA_recipients %>%
  dplyr::filter(iso3c %!in% unique(aggr.pca_adj$iso3c)) %>%
  dplyr::mutate(group = "Non-fragile, ODA-eligible") %>%
  dplyr::select(iso3c, group)

#create list separating other and extremely fragile contexts
oecd_fragile <- aggr.pca_adj %>%
  dplyr::select(iso3c, group = fragility.level) %>%
  dplyr::mutate(
    group = gsub("Fragile", "Other Fragile", group),
    group = gsub("Extremely Other Fragile", "Extremely Fragile", group)
  )

#create list for all fragile contexts
oecd_fragile_all <- aggr.pca_adj %>%
  select(iso3c, group = fragility.level) %>%
  mutate(group = gsub("Extremely Fragile", "Fragile", group))

#import WB 2020 June classifications, and use Groups spreadsheet
wb_classifications <-
  read_xls("./data/additional data/WB_2020_classifications.xls",
           sheet = "Groups") %>%
  dplyr::mutate(iso3c = countrycode(
    CountryName,
    "country.name",
    "iso3c",
    custom_match = c(
      "Eswatini" = "SWZ",
      "Kosovo" = "XKX",
      "Romania" = "ROM"
    )
  )) %>%
  dplyr::select(iso3c, GroupName)

wb_class <- wb_classifications %>%
  dplyr::select(iso3c, group = GroupName) %>%
  dplyr::mutate(group = paste0("WB Classes - ", group))

#import custom list of sub-regions, developed for SoF 2020
subregions <-
  read_excel("./data/additional data/List of sub-regions.xlsx") %>%
  dplyr::mutate(iso3c = countrycode(countries, "country.name", "iso3c"))

##import custom list of resource-dependent economies from UNCTAD 2019
commodities_list <-
  read_excel("./data/additional data/cddc.xlsx") %>%
  mutate(iso3c = countrycode(
    country,
    "country.name",
    "iso3c",
    custom_match = c("Micronesia FSM" = "FSM")
  ))

commodities_all <-
  commodities_list %>% mutate(group = "Commodity dependence") %>% select(-country)

commodities_disaggregated <-
  commodities_list %>% select(iso3c, group)

#import custom list of conflict countries from UCDP
conflict_list <-
  fread("./data/additional data/UCDP Conflict Intensity.csv")

conflict_list_all <-
  conflict_list %>% dplyr::mutate(group = "In conflict") %>%
  rbind(conflict_list)

#import custom list of climate change countries, from ND-GAIN, using thresholds proposed in Krampe 2019 peacebuilding and climate change brief
climate_change <-
  fread("./data/additional data/exposure to climate change.csv")

climate_change_frag <- climate_change %>%
  filter(iso3c %in% unique(aggr.pca_adj$iso3c))

conflict_and_climate <-
  conflict_list_all %>% filter(group == "In conflict") %>%
  filter(iso3c %in% unique(climate_change_frag$iso3c)) %>%
  mutate(group = "Conflict-affected most exposed to climate change")

#eiu
eiu_raw <- fread("./data/additional data/governance_index.csv") %>%
  dplyr::mutate(iso3c = countrycode(
    country,
    "country.name",
    "iso3c",
    custom_match = c("eSwatini" = "SWZ")
  )) %>%
  select(iso3c, group = type)

#rbind entire list
iso_bygroup <-
  rbind(
    oecd_class,
    wb_class,
    oecd_fragile,
    oecd_fragile_all,
    oecd_nonfragile,
    subregions %>% dplyr::select(-c(countries, source)),
    ODA_recipients %>% dplyr::select(iso3c, group = category),
    commodities_all,
    commodities_disaggregated,
    conflict_list_all,
    climate_change,
    conflict_and_climate,
    eiu_raw
  )

#summarise the number of countries per group
iso_counts <- iso_bygroup %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(count_total = n())

#subset the isogroup list to only include fragile contexts
iso_counts_fragile <- iso_bygroup %>%
  dplyr::left_join(aggr.pca_adj[, c("iso3c", "fragility.level")], by = "iso3c") %>%
  dplyr::filter(complete.cases(.))

#compute count of number of fragile states per group, then join the previous list of total countries per group to determine proportion of group that is fragile
iso_counts_fragile_bygroup <- iso_counts_fragile %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(count_fragile = n()) %>%
  dplyr::left_join(iso_counts, by = "group") %>%
  dplyr::mutate(proportion = count_fragile / count_total * 100)

#export counts of countries, population-weighted (using 2020 population figures, which is fine as framework is time invariant, and there isn't a big difference b/t 2019 and 2020 for your estimates of the proportion)
iso_counts_wpop <- iso_bygroup %>%
  dplyr::left_join(
    population_filtered %>%
      dplyr::filter(year == 2020) %>%
      dplyr::select(iso3c, population),
    by = "iso3c"
  ) %>%
  dplyr::filter(complete.cases(.))

iso_counts_wpop_bygroup <- iso_counts_wpop %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(population_all = sum(population) / 1000000)

#confirm that the unique(iso3c) is 57 in the subset below
iso_counts_wpop_fragile <- iso_counts_wpop %>%
  dplyr::left_join(aggr.pca_adj, by = "iso3c") %>%
  dplyr::filter(complete.cases(fragility.level))

pop_fragile <- iso_counts_wpop_fragile %>%
  filter(group == "Fragile") %>%
  summarise(total_frag_pop = sum(population, na.rm = T) / 1000000)

iso_counts_wpop_fragile_bygroup <- iso_counts_wpop_fragile %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(population_fragile = sum(population) / 1000000) %>%
  dplyr::left_join(iso_counts_wpop_bygroup, by = "group") %>%
  dplyr::mutate(proportion = population_fragile / population_all * 100) %>%
  dplyr::mutate(
    total_frag_pop = pop_fragile$total_frag_pop,
    proportion_of_frag = population_fragile / total_frag_pop *
      100
  )

iso_counts_export <-
  list(
    "Countries" = iso_counts_fragile_bygroup %>% dplyr::select(
      Groups = group,
      `Fragile Contexts` = count_fragile,
      `Total Contexts` = count_total,
      Proportion = proportion
    ),
    "Population" = iso_counts_wpop_fragile_bygroup %>%
      dplyr::select(
        Groups = group,
        `Population in Fragile Contexts in 2020` = population_fragile,
        `Total Population in 2020` = population_all,
        Proportion = proportion,
        `Proportion of Fragile Contexts` = proportion_of_frag
      )
  )
write.xlsx(
  iso_counts_export,
  paste0(output_folders, "Dataset - Country Representation by Group.xlsx")
)

# Clusters ----------------------------------------------------------------

#export clusters
#incorporate fragile.levels, the qual decisions on the clusters
fragile.levels <-
  import("./data/additional data/dimensional fragility.xlsx")
clusters <-
  clusters %>% dplyr::mutate(country = countrycode(
    iso3c,
    "iso3c",
    "un.name.en",
    custom_match = c(
      "PSE" = "Palestine",
      "ROM" = "Romania",
      "SWZ" = "Eswatini"
    )
  ))
colours <-
  mean.plot %>% dplyr::rename(color = colour) %>% dplyr::select(cluster, color) %>% distinct()
clusters_adj <-
  clusters %>% dplyr::left_join(colours, by = "cluster") %>% dplyr::select(dimension, iso3c, country, color) %>%
  arrange(dimension, color, iso3c) %>%
  left_join(aggr.pca_adj, "iso3c") %>% select(-c(fragility.level, Fragility, location))
clusters_split <- clusters_adj %>% split(.$dimension)
write.xlsx(clusters_split,
           paste0(output_folders, "clusters/clusters.xlsx"))

# Viz - PCA Analysis ------------------------------------------------------

#extract and visualise latest year (in this case, 2019)
all.pca.2019 <- lapply(all.pca, function(i) {
  i[["2019"]]
})

#extract variables from PCA analysis
pca.var.dimension <- lapply(all.pca.2019, function(i) {
  get_pca_var(i)
})

#extract individuals from PCA analysis
pca.ind.dimension <- lapply(all.pca.2019, function(i) {
  get_pca_ind(i)
})

##CONTRIBUTION - BAR PLOTS
#visualise contributions to the first two dimensions (PCAs), using bar plots.
#contributions is a measure of which variables are the most important in explaining variability in a dataset
#the ones correlated with the first two PCAs are the most important in explaining that
#variability.

contrib.bar.viz <-
  lapply(all.pca.2019, function(i) {
    fviz_contrib(i, choice = "var",
                 axes = 1:2) +
      ggtitle(paste("Contributions to PC1 and PC2 in the", i$dimension, "Dimension"))
  })

lapply(names(contrib.bar.viz),
       function(x)
         ggsave(
           filename = paste0("./graphs/contributions/PC1and2/", x, ".jpg", sep = ""),
           plot = contrib.bar.viz[[x]],
           width = 10,
           height = 10
         ))

contrib.bar.viz.Dim1 <-
  lapply(all.pca.2019, function(i) {
    fviz_contrib(i, choice = "var",
                 axes = 1) +
      ggtitle(paste("Contributions to PC1 in the", i$dimension, "Dimension"))
  })

lapply(names(contrib.bar.viz.Dim1),
       function(x)
         ggsave(
           filename = paste0("./graphs/contributions/PC1/", x, ".jpg", sep = ""),
           plot = contrib.bar.viz.Dim1[[x]],
           width = 12,
           height = 12
         ))

contrib.bar.viz.Dim2 <-
  lapply(all.pca.2019, function(i) {
    fviz_contrib(i, choice = "var",
                 axes = 2) +
      ggtitle(paste("Contributions to PC2 in the", i$dimension, "Dimension"))
  })

lapply(names(contrib.bar.viz.Dim2),
       function(x)
         ggsave(
           filename = paste0("./graphs/contributions/PC2/", x, ".jpg", sep = ""),
           plot = contrib.bar.viz.Dim2[[x]],
           width = 12,
           height = 12
         ))

# Aggregate Changes -------------------------------------------------------

agg = lapply(all.pca$Aggregate, function(x) {
  x = data.frame(iso3c = x$iso3c, value = x$x[, 1])
  return(x)
})
agg = bind_rows(agg, .id = "year")

# Dimensional and Variable Changes -----------------------------------------------------------------
### Dimensional-level Deltas -------------------------------------------------------------
dimensional_changes_disag_all <-
  raw.data %>% dplyr::mutate(year = as.numeric(year)) %>% arrange(-year) %>%
  gather(variablename, value,-c(iso3c, year)) %>% dplyr::filter(grepl("PC1", variablename))

deltas_all <-
  hpc.change(dimensional_changes_disag_all) %>% dplyr::filter(from == 2016,
                                                              to == 2019) %>%
  dplyr::arrange(absolute.diff) %>%
  dplyr::select(-c(prop.growth, annual.prop.growth))
deltas_all$change = ifelse(deltas_all$absolute.diff < 0, "Deterioration", "Improvement")
deltas_all$location <- country.code.name(deltas_all$iso3c)
deltas_all <-
  deltas_all %>% dplyr::left_join(aggr.pca_adj[, c("iso3c", "fragility.level")], by = "iso3c") %>%
  dplyr::select(iso3c, location, fragility.level, everything()) %>%
  dplyr::mutate(
    fragility.level = gsub("Fragile", "Other Fragile", fragility.level),
    fragility.level = gsub("Extremely Other Fragile", "Extremely Fragile", fragility.level),
    fragility.level = replace_na(fragility.level, "Non-fragile")
  )

#export deteriorations and improvements
##first, create subset of deltas dataset with only the countries that have experienced deteriorations in this time period in the aggregate
deltas_fragile_agg_deterioration <-
  deltas_all %>% dplyr::filter(
    variablename == "Aggregate.PC1",
    change == "Deterioration",
    fragility.level %in% c("Other Fragile",
                           "Extremely Fragile")
  )
##prepare a subset of all other dimensions, using the previous subsets - you will rbind these, and then split them into a list for ease of export
deltas_fragile_other_deterioration <-
  deltas_all %>% dplyr::filter(
    variablename != "Aggregate.PC1",
    iso3c %in% unique(deltas_fragile_agg_deterioration$iso3c)
  )

deltas_fragile_deterioration <-
  rbind(deltas_fragile_agg_deterioration,
        deltas_fragile_other_deterioration) %>% arrange(iso3c) %>% split(.$iso3c)
write.xlsx(
  deltas_fragile_deterioration,
  paste0(
    output_folders,
    "changes/Deterioration within Each Dimension - by Context.xlsx"
  )
)

##second, create subset of deltas dataset with only the countries that have experienced improvements in this time period in the aggregate
deltas_fragile_agg_improvement <-
  deltas_all %>% dplyr::filter(
    variablename == "Aggregate.PC1",
    change == "Improvement",
    fragility.level %in% c("Other Fragile",
                           "Extremely Fragile")
  )
##prepare a subset of all other dimensions, using the previous subsets - you will rbind these, and then split them into a list for ease of export
deltas_fragile_other_improvement <-
  deltas_all %>% dplyr::filter(
    variablename != "Aggregate.PC1",
    iso3c %in% unique(deltas_fragile_agg_improvement$iso3c)
  )

deltas_fragile_improvement <- rbind(deltas_fragile_agg_improvement,
                                    deltas_fragile_other_improvement) %>% arrange(iso3c) %>% split(.$iso3c)
write.xlsx(
  deltas_fragile_improvement,
  paste0(
    output_folders,
    "changes/Improvement within Each Dimension - by Context.xlsx"
  )
)

### Variable-level Deltas ----------------------------------------------------------------

variable_changes_disag <- sfr.time.series %>%
  dplyr::filter(year %in% 2012:2019) %>%
  dplyr::mutate(
    location = countrycode(
      iso3c,
      "iso3c",
      "un.name.en",
      custom_match = c(
        "PSE" = "West Bank and Gaza Strip",
        "ROM" = "Romania",
        "SWZ" = "Eswatini"
      )
    ),
    variablename = gsub("\\(C\\)", "", variablename),
    variablename = gsub("\\(R\\)", "", variablename),
    variablename = trimws(variablename)
  )

variable_changes_disag_subset <- variable_changes_disag %>%
  dplyr::select(year, iso3c, variablename, value)

deltas_byvar_final <- hpc.change(variable_changes_disag_subset) %>%
  dplyr::filter(from == 2016, to == 2019) %>%
  arrange(absolute.diff) %>%
  dplyr::select(-c(prop.growth, annual.prop.growth)) %>%
  dplyr::left_join(
    variable_changes_disag %>% distinct(variablename, dimension, doesmoreincreasefragility),
    "variablename"
  )

#need to be careful about the interpretation, because it is based on the directionality of the raw variable
deltas_byvar_bydirectionality <-
  deltas_byvar_final %>% split(.$doesmoreincreasefragility)
deltas_byvar_one <-
  deltas_byvar_bydirectionality$`1` %>% dplyr::select(-doesmoreincreasefragility) %>%
  dplyr::distinct()
deltas_byvar_zero <-
  deltas_byvar_bydirectionality$`0` %>% dplyr::select(-doesmoreincreasefragility) %>%
  dplyr::distinct()

deltas_byvar_one$change <-
  ifelse(
    deltas_byvar_one$absolute.diff < 0,
    "Improvement",
    ifelse(
      deltas_byvar_one$absolute.diff == 0,
      "No Change",
      "Deterioration"
    )
  )
deltas_byvar_zero$change <-
  ifelse(
    deltas_byvar_zero$absolute.diff < 0,
    "Improvement",
    ifelse(
      deltas_byvar_zero$absolute.diff == 0,
      "No Change",
      "Deterioration"
    )
  )
deltas_by_var_final_exp <- rbind(deltas_byvar_one, deltas_byvar_zero)
deltas_list <- list("Dimension" = deltas_all,
                    "Variables" = deltas_by_var_final_exp)
write.xlsx(
  deltas_list,
  paste0(
    output_folders,
    "changes/Changes - All Contexts, by Variable and Dimension.xlsx"
  )
)

# Groupings Analysis ------------------------------------------------------

#all contexts first

group_scores_raw <- raw.data %>%
  dplyr::select(iso3c, year, ends_with("PC1")) %>%
  dplyr::filter(year == 2019) %>%
  dplyr::select(-year) %>%
  dplyr::filter(iso3c %in% unique(ODA_recipients$iso3c)) %>%
  dplyr::mutate_at(vars(-iso3c), function(x)
    rescale(x, c(100, 0))) %>%
  dplyr::left_join(
    population_filtered %>%
      dplyr::filter(year == 2019) %>%
      dplyr::select(iso3c, population),
    "iso3c"
  ) %>%
  dplyr::left_join(iso_bygroup, "iso3c")

group_scores_aggregated_raw <- group_scores_raw %>%
  pivot_longer(names_to = "dimension",
               values_to = "value",-c("iso3c", "group", "population")) %>%
  dplyr::group_by(dimension, group) %>%
  dplyr::summarise(raw_value = weighted.mean(value, population, na.rm = T))
write.xlsx(
  group_scores_aggregated_raw,
  paste0(output_folders, "group scores (single year).xlsx")
)

dat <- group_scores_raw %>%
  pivot_longer(names_to = "dimension",
               values_to = "value",
               -c(iso3c, group, population)) %>%
  mutate(
    dimension = gsub(".PC1", "", dimension),
    group = gsub("WB Classes - ", "", group)
  ) %>%
  filter(
    group %in% c(
      "Sub-Saharan Africa",
      "East Asia & Pacific",
      "Europe & Central Asia",
      "South Asia",
      "Middle East & North Africa",
      "Latin America & Caribbean"
    )
  ) %>%
  split(.$dimension)

for (i in names(dat)) {
  ggplot(
    dat[[i]] %>%
      group_by(group) %>%
      summarise(
        ymin = min(value),
        ymax = max(value),
        y25 = quantile(value, 0.25),
        y50 = quantile(value, 0.50),
        y75 = quantile(value, 0.75),
        wgtmean = weighted.mean(value, population)
      ),
    aes(x = reorder(group, wgtmean))
  ) +
    geom_boxplot(
      aes(
        ymin = ymin,
        lower = y25,
        middle = y50,
        upper = y75,
        ymax = ymax
      ),
      stat = "identity",
      fatten = NULL,
      outlier.shape = NA
    ) +
    geom_point(
      aes(x = group, y = wgtmean),
      shape = 18,
      size = 3,
      fill = "white",
      inherit.aes = FALSE
    ) +
    theme_classic() +
    coord_flip() +
    labs(x = "", y = "Fragility score")
  ggsave(paste0(
    "./graphs/dimensional_fragility/",
    i,
    "_Bar_Boxplot_Scaled.pdf"
  ),
  width = 10)
  
  dat[[i]] %>%
    group_by(group) %>%
    summarise(
      ymin = min(value),
      ymax = max(value),
      y25 = quantile(value, 0.25),
      y50 = quantile(value, 0.50),
      y75 = quantile(value, 0.75),
      wgtmean = weighted.mean(value, population)
    ) %>%
    mutate(dimension = i) %>%
    fwrite(., paste0("./graphs/dimensional_fragility/", i, ".csv"))
}

group_overtime_scores_raw_unweighted <- raw.data %>%
  filter(iso3c %in% unique(ODA_recipients$iso3c)) %>%
  select(iso3c, year, ends_with("PC1")) %>%
  pivot_longer(names_to = "dimension", values_to = "value", -c(year, iso3c)) %>%
  group_by(dimension) %>%
  mutate(value = rescale(value, c(100, 0))) %>%
  ungroup() %>%
  dplyr::left_join(iso_bygroup, "iso3c") %>%
  group_by(year, dimension, group) %>%
  summarise(value = mean(value, na.rm = T)) %>%
  ungroup() %>%
  split(.$dimension) %>%
  map(
    function(x)
      x %>% pivot_wider(names_from = "year", values_from = "value") %>%
      rowwise() %>%
      mutate(rate_of_change = (`2019` / `2012` - 1) * 100)
  )
write.xlsx(
  group_overtime_scores_raw_unweighted,
  paste0(
    output_folders,
    "Scaled group scores - overtime (no weighting).xlsx"
  )
)

group_overtime_scores_raw <- raw.data %>%
  filter(iso3c %in% unique(ODA_recipients$iso3c)) %>%
  select(iso3c, year, ends_with("PC1")) %>%
  pivot_longer(names_to = "dimension", values_to = "value", -c(year, iso3c)) %>%
  group_by(dimension) %>%
  mutate(value = rescale(value, c(100, 0))) %>%
  ungroup() %>%
  dplyr::left_join(
    population_filtered %>% mutate(year = as.character(year)) %>%
      dplyr::select(year, iso3c, population),
    c("year", "iso3c")
  ) %>%
  dplyr::left_join(iso_bygroup, "iso3c") %>%
  group_by(year, dimension, group) %>%
  summarise(value = weighted.mean(value, population, na.rm = T)) %>%
  ungroup() %>%
  split(.$dimension) %>%
  map(
    function(x)
      x %>% pivot_wider(names_from = "year", values_from = "value") %>%
      rowwise() %>%
      mutate(rate_of_change = (`2019` / `2012` - 1) * 100)
  )
write.xlsx(
  group_overtime_scores_raw,
  paste0(
    output_folders,
    "Scaled group scores - overtime (weighting).xlsx"
  )
)


#global analysis
dat_global_forviz <- group_overtime_scores_raw_unweighted %>%
  map(
    function(x)
      x %>%
      select(-rate_of_change) %>%
      pivot_longer(
        names_to = "year",
        values_to = "value",
        -c(dimension, group)
      ) %>%
      filter(
        year != "2019",
        group %in% c(
          "Extremely Fragile",
          "Other Fragile",
          "Non-fragile, ODA-eligible"
        )
      ) %>%
      mutate(dimension = gsub(".PC1", "", dimension))
  )

names(dat_global_forviz) <-
  c("Aggregate",
    "Economic",
    "Environmental",
    "Political",
    "Security",
    "Societal")

dat_global_foranalysis <- dat_global_forviz %>%
  map(
    function(x)
      x %>%
      select(-dimension) %>%
      pivot_wider(names_from = "group", values_from = "value") %>%
      clean_names() %>%
      rowwise() %>%
      mutate(
        diff_ext_nonfrag = extremely_fragile - non_fragile_oda_eligible,
        diff_ext_otherfrag = extremely_fragile - other_fragile
      )
  )

devtools::install_github("hrbrmstr/ggalt")
library(ggplot2)
library(ggalt)

dat_global_foranalysis_combined <-
  bind_rows(dat_global_foranalysis, .id = "dimension") %>%
  select(dimension, year, diff_ext_nonfrag, diff_ext_otherfrag) %>%
  clean_names() %>%
  pivot_longer(names_to = "type",
               values_to = "value",
               -c(dimension, year)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(
    type = gsub("diff_ext_nonfrag", "Extremely vs. non-Fragile Contexts", type),
    type = gsub(
      "diff_ext_otherfrag",
      "Extremely vs. Other Fragile Contexts",
      type
    )
  )

ggplot(data = dat_global_foranalysis_combined,
       mapping = aes(
         x = reorder(year, -year),
         y = ifelse(
           test = type == "Extremely vs. non-Fragile Contexts",
           yes = -value,
           no = value
         ),
         fill = type
       )) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  facet_grid(dimension ~ .) +
  scale_y_continuous(labels = abs, limits = c(-60, 60)) +
  labs(y = "Fragility gap", title = "The Growing Gap of Fragility, 2012-18",
       x = "") +
  theme_classic()
ggsave("./graphs/growing_gap.pdf")

dat_global_foranalysis_combined_export <-
  dat_global_foranalysis_combined %>%
  pivot_wider(names_from = "type", values_from = "value")
write.xlsx(dat_global_foranalysis_combined_export,
           "./graphs/growing_gap_data.xlsx")

#Subregional analysis
dat_subregion <- group_overtime_scores_raw %>%
  list_modify("Aggregate.PC1" = NULL) %>%
  map(function(x)
    x %>%
      mutate(dimension = gsub(".PC1", "", dimension)) %>%
      filter(
        group %in% c("African Great Lakes",
                     "Lake Chad Basin",
                     "Horn of Africa",
                     "Sahel")
      ))

dat_subregion_forviz <- dat_subregion %>%
  map(
    function(x)
      x %>%
      select(-c(rate_of_change, `2019`)) %>%
      pivot_longer(
        names_to = "year",
        values_to = "value",
        -c(group, dimension)
      ) %>%
      mutate(year = as.numeric(year))
  )
names(dat_subregion_forviz) <- c("Economic",
                                 "Environmental",
                                 "Political",
                                 "Security",
                                 "Societal")

dat_subregion_viz <- imap(
  dat_subregion_forviz,
  ~ ggplot(
    data = .x,
    mapping = aes(x = year,
                  y = value,
                  group = group)
  ) +
    geom_line(aes(colour = group)) +
    theme_classic() +
    labs(
      colour = "Subregion",
      x = "",
      y = "Fragility level",
      title = paste0(.y, " fragility in select subregions, over time")
    ) +
    ylim(0, 100)
)
ggsave(
  "./graphs/dimensional_fragility/subregion_graphs.pdf",
  marrangeGrob(grobs = dat_subregion_viz,
               nrow = 1, ncol = 1)
)
write.xlsx(dat_subregion_forviz,
           "./graphs/dimensional_fragility/subregion_dataset.xlsx")

# Gender Inequality ------------------------------------------------------
sfr_gender_ineq <- sfr.time.series %>%
  filter(
    variablename == "Gender inequality (R)",
    year == 2018,
    iso3c %in% unique(ODA_recipients$iso3c)
  ) %>%
  select(iso3c, gender_ineq = imputed) %>%
  left_join(raw.data %>%
              filter(year == 2019) %>%
              select(ends_with(".PC1"), iso3c),
            "iso3c") %>%
  mutate_at(vars(-c(iso3c, gender_ineq)), function(x)
    - x) %>%
  pivot_longer(names_to = "variable",
               values_to = "value", -iso3c) %>%
  mutate(
    variable = gsub("gender_ineq", "Gender Inequality", variable),
    variable = gsub(".PC1", "", variable)
  ) %>%
  pivot_wider(names_from = "variable",
              values_from = "value") %>%
  select(-Aggregate)

library(ggpmisc)
ggplot(data = sfr_gender_ineq,
       aes(x = `Gender Inequality`)) +
  geom_smooth(
    aes(y = Economic,
        colour = "Economic"),
    method = "lm",
    formula = y ~ x,
    se = F
  ) +
  geom_smooth(
    aes(y = Environmental,
        colour = "Environmental"),
    method = "lm",
    formula = y ~ x,
    se = F
  ) +
  geom_smooth(
    aes(y = Security,
        colour = "Security"),
    method = "lm",
    formula = y ~ x,
    se = F
  ) +
  theme_classic() +
  labs(y = "Fragility score by dimension",
       x = "Gender inequality index",
       colour = "Dimension")
ggsave("./graphs/dimensional_fragility/Gender Inequality.pdf")



# Political and peace agreements snapshot ---------------------------------

peace_gender <-
  fread("./data/snapshot data/pax_wgg_all_agreements_data.csv") %>%
  clean_names()

peace_gender_total <- peace_gender %>%
  filter(loc1iso %in% unique(aggr.pca_adj$iso3c) |
           loc2iso %in% unique(aggr.pca_adj$iso3c)) %>%
  select(agt_id, starts_with("wgg"), stage) %>%
  pivot_longer(names_to = "type",
               values_to = "value", -c(stage, agt_id)) %>%
  filter(value == 1) %>%
  group_by(type) %>%
  summarise(n = n())

peace_gender_sub <- peace_gender_total %>%
  filter(
    type %in% c(
      "wgg_par",
      "wgg_grp",
      "wgg_eq",
      "wgg_int_law",
      "wgg_new_inst",
      "wgg_vio",
      "wgg_tra_jus",
      "wgg_inst_ref",
      "wgg_dev",
      "wgg_impl",
      "wgg_oth"
    )
  ) %>%
  mutate(
    type = gsub("wgg_dev", "Development", type),
    type = gsub("wgg_grp", "Particular groups of women", type),
    type = gsub("wgg_impl", "Implementation", type),
    type = gsub("wgg_new_inst", "New institutions", type),
    type = gsub("wgg_inst_ref", "Institutional reform", type),
    type = gsub("wgg_int_law", "International law", type),
    type = gsub("wgg_oth", "Other", type),
    type = gsub("wgg_par", "Participation", type),
    type = gsub("wgg_eq", "Equality", type),
    type = gsub("wgg_tra_jus", "Transitional justice", type),
    type = gsub("wgg_vio", "Violence against women", type)
  )

ggplot(peace_gender_sub,
       aes(x = reorder(type, -n),
           y = n)) +
  geom_col() +
  theme_classic() +
  labs(x = "Gender provisions in peace agreements",
       y = "Number of agreements") +
  theme(axis.text.x = element_text(
    angle = 45,
    vjust = 1,
    hjust = 1
  ))
ggsave("./graphs/gender_provisions.pdf", width = 9)
write.xlsx(peace_gender_sub, "./graphs/Gender peace agreements.xlsx")
