Data Visualization - Mini-Project 2
================
Nico Seddio `nseddio0283@floridapoly.edu`

- [Setup](#setup)
- [Summarize](#summarize)
- [Visualizations](#visualizations)
- [Redesigning a Bad Chart
  (Before/After)](#redesigning-a-bad-chart-beforeafter)
- [Report](#report)

## Setup

``` r
library(tidyverse)
# install.packages("sf")
library(sf)
# install.packages("plotly")
library(plotly)
lakes <- st_read("../data/Florida_Lakes/Florida_Lakes.shp")
```

    ## Reading layer `Florida_Lakes' from data source `/Users/nicoseddio/Downloads/FPU_CAP5735_final_project/data/Florida_Lakes/Florida_Lakes.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 4243 features and 6 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -87.42774 ymin: 25.02625 xmax: -80.03097 ymax: 31.00254
    ## Geodetic CRS:  WGS 84

``` r
counties <- st_read("../data/Florida_Counties/Florida_Counties.shp")
```

    ## Reading layer `Florida_Counties' from data source `/Users/nicoseddio/Downloads/FPU_CAP5735_final_project/data/Florida_Counties/Florida_Counties.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 67 features and 7 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -87.62601 ymin: 24.54522 xmax: -80.03095 ymax: 30.99702
    ## Geodetic CRS:  WGS 84

``` r
schools <- st_read("../data/Public_Private_Schools_in_Florida-2017/gc_schools_sep17.shp")
```

    ## Reading layer `gc_schools_sep17' from data source `/Users/nicoseddio/Downloads/FPU_CAP5735_final_project/data/Public_Private_Schools_in_Florida-2017/gc_schools_sep17.shp' using driver `ESRI Shapefile'
    ## Simple feature collection with 8552 features and 51 fields
    ## Geometry type: POINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -87.47647 ymin: 24.55048 xmax: -80.04066 ymax: 30.97757
    ## Geodetic CRS:  WGS 84

``` r
# Some functions on lakes return an error about a bad vertex:
# Error in wk_handle.wk_wkb(wkb, s2_geography_writer(oriented = oriented, :
# Loop 0 is not valid: Edge 382 has duplicate vertex with edge 385
# https://stackoverflow.com/questions/79756123/what-issue-is-st-make-valid-fixing-when-applied-to-osm-data
lakes <- st_make_valid(lakes)
```

## Summarize

Across the three datasets, the `COUNTY` name is our best join key. The
`Florida_Counties` dataset calls this `COUNTYNAME`, and counties `DADE`,
`ST. JOHNS`, and `ST. LUCIE` are mismatched in spelling between the
datasets. We fix these issues before working with the data and do a
quick check to make sure no other mismatches in county name exist (an
empty result is good).

``` r
# Rename COUNTY -> COUNTY_ID and COUNTYNAME -> COUNTY so COUNTY is consistent across data
counties <- counties %>% 
  mutate(COUNTY_ID = COUNTY, COUNTY = COUNTYNAME) %>% 
  select(-COUNTYNAME)
# Fix mismatched spellings to match Florida Counties source of truth
schools <- schools %>% 
  mutate(COUNTY = recode_values(COUNTY,
    "ST JOHNS" ~ "ST. JOHNS",
    "ST LUCIE" ~ "ST. LUCIE",
    "MIAMI-DADE" ~ "DADE",
    default = COUNTY))
lakes <- lakes %>% 
  mutate(COUNTY = recode_values(COUNTY,
    "ST. JOHN" ~ "ST. JOHNS",
    "MIAMI-DADE" ~ "DADE",
    default = COUNTY))
# Review for mismatches, empty is good
counties %>% 
  distinct(county = COUNTY) %>% 
  arrange(county) %>% 
  full_join(
    schools %>% distinct(school_county = COUNTY),
    by = join_by(county == school_county),
    keep = TRUE) %>% 
  full_join(
    lakes %>% distinct(lake_county = COUNTY),
    by = join_by(county == lake_county),
    keep = TRUE) %>% 
  filter(is.na(county) | is.na(school_county) | is.na(lake_county))
```

    ## [1] county        school_county lake_county  
    ## <0 rows> (or 0-length row.names)

#### Group lakes into counties

``` r
lakes_by_county <- lakes %>% 
  group_by(COUNTY) %>% 
  summarise(lake_count = n(),
            total_lake_area = sum(SHAPEAREA))
lakes_by_county %>% head(5)
```

    ## Simple feature collection with 5 features and 3 fields
    ## Geometry type: MULTIPOLYGON
    ## Dimension:     XY
    ## Bounding box:  xmin: -86.00468 ymin: 27.95441 xmax: -80.57845 ymax: 30.55578
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 5 × 4
    ##   COUNTY   lake_count total_lake_area                                                                                geometry
    ##   <chr>         <int>           <dbl>                                                                      <MULTIPOLYGON [°]>
    ## 1 ALACHUA          68      139982755. (((-82.40637 29.62032, -82.40656 29.62039, -82.40679 29.62038, -82.40718 29.62038, -...
    ## 2 BAKER             3        7301007. (((-82.28798 30.55528, -82.28801 30.55538, -82.28808 30.55544, -82.28833 30.55549, -...
    ## 3 BAY              46       29948288. (((-85.71071 30.40863, -85.7106 30.40868, -85.71053 30.40889, -85.7105 30.40889, -85...
    ## 4 BRADFORD         11       15876511. (((-82.05423 29.79646, -82.05382 29.79645, -82.05347 29.7965, -82.05317 29.79665, -8...
    ## 5 BREVARD          35       53529147. (((-80.86448 28.63067, -80.86458 28.63085, -80.86466 28.63094, -80.86475 28.63116, -...

#### Group schools into counties

``` r
schools_by_county <- schools %>% 
  group_by(COUNTY) %>% 
  summarise(school_count = n(),
            total_enrollment = sum(ENROLLMENT, na.rm = TRUE))
schools_by_county %>% head(5)
```

    ## Simple feature collection with 5 features and 3 fields
    ## Geometry type: MULTIPOINT
    ## Dimension:     XY
    ## Bounding box:  xmin: -85.85879 ymin: 27.90883 xmax: -80.55548 ymax: 30.39436
    ## Geodetic CRS:  WGS 84
    ## # A tibble: 5 × 4
    ##   COUNTY   school_count total_enrollment                                                                                  geometry
    ##   <chr>           <int>            <dbl>                                                                          <MULTIPOINT [°]>
    ## 1 ALACHUA           124            33566 ((-82.16952 29.79194), (-82.09225 29.58973), (-82.09092 29.59384), (-82.18792 29.64269...
    ## 2 BAKER              12             4980 ((-82.13119 30.29802), (-82.11622 30.27716), (-82.12333 30.28557), (-82.12761 30.29), ...
    ## 3 BAY                75            28862 ((-85.44081 30.36637), (-85.59207 30.25767), (-85.56238 30.24211), (-85.56434 30.24158...
    ## 4 BRADFORD           18             3838 ((-82.16688 30.04016), (-82.07495 30.04767), (-82.07198 30.04632), (-82.09421 29.99128...
    ## 5 BREVARD           249            83712 ((-80.81837 28.3615), (-80.78083 28.34936), (-80.76889 28.35501), (-80.74217 28.30523)...

## Visualizations

#### Total enrollment in county with school count

The horizontal bar chart below shows the top 15 counties by total
enrollment, with Dade county at the top with the most number of schools
and the most number of enrollments. The interactivity allows users to
review raw numbers of both enrollment and school count without these
values cluttering the chart.

``` r
enrollment_bar <- schools_by_county %>% 
  slice_max(total_enrollment, n=15) %>% 
  ggplot(aes(
    total_enrollment,
    fct_reorder(COUNTY, total_enrollment), 
    fill = school_count)) +
  geom_col() +
  scale_fill_viridis_c() +
  labs(
    title = "County Enrollment Size and School Count", 
    fill = "Schools", 
    x = "Total Enrollment", 
    y = "County") + 
  scale_x_continuous(labels = scales::comma) +
  theme_minimal()
#enrollment_bar
#ggplotly(enrollment_bar)
#save self-contained
#htmlwidgets::saveWidget(ggplotly(enrollment_bar),"interactive-enrollment-bar.html", selfcontained = TRUE)
if (knitr::pandoc_to(c("gfm", "markdown_github", "commonmark"))) {
  print(enrollment_bar)
} else {
  htmlwidgets::saveWidget(ggplotly(enrollment_bar),"interactive-enrollment-bar.html", selfcontained = TRUE)
  ggplotly(enrollment_bar)
}
```

<img src="project-02_files/figure-gfm/bar_fifteen-1.png" alt="Interactive horizontal bar chart showing top 15 counties by total enrollment with Dade county at the top with the most number of schools and the most number of enrollments."  />

(In Github / markdown, this renders as the static version. [Click to
view the interactive version](interactive-enrollment-bar.html))

#### Five largest counties with operating enrollment

Column chart showing top 5 largest counties by total enrollment, each
column as a county showing percentage division by school type (public,
private, charter, etc.). The interactivity here allows presenting the
data in a visually interesting way as percentages of a whole while still
allowing the user to drill down and get actual values like total
enrollment and school operating type.

``` r
enrollment_col <- schools %>% 
  group_by(COUNTY,OPERATING) %>% 
  summarise(total_enrollment = sum(ENROLLMENT, na.rm = TRUE)) %>% 
  filter(total_enrollment != 0) %>% 
  distinct(COUNTY,OPERATING,total_enrollment) %>% 
  inner_join(
    schools_by_county %>% 
      slice_max(total_enrollment, n=5) %>% 
      distinct(COUNTY)) %>% 
  ggplot() +
  geom_col(aes(x = COUNTY, y = total_enrollment, fill = OPERATING), position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Largest Counties by Enrollment with Type", 
    x = "", 
    y = "Enrollment", fill = "School Type") +
  scale_fill_viridis_d() +
  theme_minimal()
#enrollment_col
#ggplotly(enrollment_col)
#htmlwidgets::saveWidget(ggplotly(enrollment_col),"interactive-enrollment-col.html", selfcontained = TRUE)
if (knitr::pandoc_to(c("gfm", "markdown_github", "commonmark"))) {
  print(enrollment_col)
} else {
  htmlwidgets::saveWidget(ggplotly(enrollment_col),"interactive-enrollment-col.html", selfcontained = TRUE)
  ggplotly(enrollment_col)
}
```

<img src="project-02_files/figure-gfm/col_five-1.png" alt="Column chart showing top 5 largest counties by total enrollment, each column as a county showing percentage division by school type (public, private, charter, etc.)."  />

(In Github / markdown, this renders as the static version. [Click to
view the interactive version](interactive-enrollment-col.html))

#### Lakes and Schools of Central Florida

Plotting all the counties of Florida together on one map would be
difficult to look at. So let’s plot just the data around Florida
Polytechnic University.

``` r
florida_poly <- st_sfc(st_point(c(-81.9498, 28.1499)), crs = st_crs(counties))
central_florida <- counties %>% ggplot() +
  geom_sf(color = "white") +
  geom_sf(data = lakes, fill = "lightblue", color = NA) +
  geom_sf(data = schools, color = "black", alpha = 0.1) +
  geom_sf_label(aes(label = COUNTY), size = 2.5, color = "blue", alpha = .3) +
  geom_sf(data = florida_poly, color = "red", size = 4, shape = 18) +
  coord_sf(xlim = c(-81,-83), ylim = c(27.5,29)) +
  labs(title = "Central Florida Lakes and Schools", x = "", y = "") +
  annotate("text", x = -81.9, y = 28.25, label = "Florida Poly", color = "red") +
  theme_minimal()
central_florida
```

    ## Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not give correct results for longitude/latitude data

<img src="project-02_files/figure-gfm/map_florida-1.png" alt="Map of central Florida from Pinellas to Osceola, Volusia to Hardee, with distribution of schools and lakes. Florida Poly is pointed out towards the center of the map."  />

``` r
ggsave("../figures/project-02-central-florida.png", width = 8, height = 5, dpi = 150)
```

    ## Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not give correct results for longitude/latitude data

Interactive version of the above chart. Adding interaction here is neat
because it allows us to start the visualization centered on where we
want users to look to tell the story, but it also allows them to zoom
out and scroll around the data to see how our story lines up with the
rest of the data. Not all the features of the original map are supported
by default though, so this demonstrates the extra work required to make
sure the graph is compatible with interactive tools.

``` r
#ggplotly(central_florida)
#htmlwidgets::saveWidget(ggplotly(central_florida),"interactive-central-florida.html", selfcontained = TRUE)
if (knitr::pandoc_to(c("gfm", "markdown_github", "commonmark"))) {
  print(central_florida)
} else {
  htmlwidgets::saveWidget(ggplotly(central_florida),"interactive_central_florida.html", selfcontained = TRUE)
  ggplotly(central_florida)
}
```

    ## Warning in st_point_on_surface.sfc(sf::st_zm(x)): st_point_on_surface may not give correct results for longitude/latitude data

<img src="project-02_files/figure-gfm/map_florida_interactive-1.png" alt="Interactive version of the map of central Florida from Pinellas to Osceola, Volusia to Hardee, with distribution of schools and lakes. Florida Poly is pointed out towards the center of the map."  />

(In Github / markdown, this renders as the static version again. [Click
to view the interactive version](interactive_central_florida.html))

#### Does lake area have an impact on total enrollment?

Plotting the relationship.

``` r
county_model_data <- counties %>% 
  distinct(COUNTY) %>% 
  inner_join(
    lakes_by_county %>% 
      distinct(COUNTY,total_lake_area,lake_count),
    by = join_by(COUNTY)) %>% 
  inner_join(
    schools_by_county %>% 
      distinct(COUNTY,total_enrollment,school_count),
    by = join_by(COUNTY))
county_model_data %>% 
  ggplot(aes(total_lake_area/1e6,total_enrollment/1000)) +
  geom_point() + 
  geom_smooth(method="lm") +
  geom_text(
    aes(label = if_else(total_enrollment > 150000, COUNTY, "")), 
    nudge_y = 15, 
    size = 3) +
  labs(
    title = "County Lake Area by Total Enrollment", 
    x = "Total Lake Area (degrees² × 10⁻⁶)", 
    y = "Total Enrollment (thousands)") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

<img src="project-02_files/figure-gfm/scatter_enrollment-1.png" alt="Scatter plot of county total lake area (x) against total school enrollment in thousands (y) with a linear trend line; the relationship is weak and pulled by a few high-enrollment counties such as Dade and Broward, which are labeled."  />

Creating a model.

``` r
model <- lm(
  total_enrollment ~ total_lake_area, 
  data = county_model_data)
broom::tidy(model, conf.int = TRUE) %>% 
  filter(term != "(Intercept)") %>% 
  ggplot() +
  geom_pointrange(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "County Lake Area Effect on Enrollment") +
  theme_minimal()
```

<img src="project-02_files/figure-gfm/coefficient_enrollment-1.png" alt="Coefficient plot of the estimated effect of total lake area on enrollment with its 95% confidence interval, sitting just above the dashed zero line — a weak, marginally positive effect."  />

Clustering the plot with k-means.

``` r
mini_county_model_data <- county_model_data %>% 
  select(total_lake_area,total_enrollment)
kclusts <- tibble(k = 1:9) %>% 
  mutate(
    kclust = map(k, ~kmeans(mini_county_model_data, .x)),
    tidied = map(kclust, broom::tidy),
    glanced = map(kclust, broom::glance),
    augmented = map(kclust, broom::augment, mini_county_model_data)
  )
kclusts %>% 
  unnest(cols = c(augmented)) %>% 
  ggplot(aes(total_lake_area/1e6,total_enrollment/1000)) +
  geom_point(aes(color = .cluster), alpha = .8) +
  facet_wrap(~ k) +
  geom_point(data = kclusts %>% unnest(cols = c(tidied)), size = 5, shape = "x") +
  labs(
    title = "County Clustering by Lake Area and Enrollment", 
    x = "Total Lake Area (degrees² × 10⁻⁶)", 
    y = "Total Enrollment (thousands)") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  scale_color_viridis_d() +
  theme_minimal()
```

<img src="project-02_files/figure-gfm/kmeans_clustering-1.png" alt="Grid of nine scatter plots (k = 1 to 9) coloring counties by k-means cluster on lake area versus enrollment. Miami-Dade splits off early and the rest stay tightly grouped, so higher k adds little structure."  />

## Redesigning a Bad Chart (Before/After)

A common way to mislead with an otherwise honest dataset is a truncated
axis. Here is the top-five-county enrollment shown badly, then
corrected.

``` r
top5 <- schools_by_county %>%
  st_drop_geometry() %>%
  slice_max(total_enrollment, n = 5) %>%
  mutate(COUNTY = fct_reorder(COUNTY, total_enrollment))
```

**Before — truncated axis and distracting fill:**

``` r
top5 %>% 
  ggplot(aes(COUNTY, total_enrollment, fill = COUNTY)) +
  geom_col() +
  coord_cartesian(ylim = c(220000, 460000)) +   # truncated window
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Enrollment by County", x = NULL, y = "Enrollment") +
  theme_minimal() +
  theme(legend.position = "none")
```

<img src="project-02_files/figure-gfm/bad_chart-1.png" alt="A column chart of the five highest-enrollment counties whose y-axis starts at 220,000 instead of zero. The truncation makes Broward and Hillsborough look like tiny fractions of Dade even though their enrollments are much closer than that. Each bar is a different color."  />

**What’s wrong with it:** the y-axis starts at 220,000, so bar *heights*
no longer encode the actual values. Broward looks like a sliver of Dade
when its enrollment is actually more than half. The colors of the bars
differentiate them, but also suggest a value hierarchy where there is
none.

**After — zero baseline, single accessible hue:**

``` r
top5 %>% 
  ggplot(aes(COUNTY, total_enrollment)) +
  geom_col(fill = "grey") +
  geom_text(
    aes(label = scales::comma(total_enrollment)), 
    vjust = -0.4, 
    size = 3) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Enrollment by County (Top 5)", x = NULL, y = "Enrollment") +
  theme_minimal()
```

<img src="project-02_files/figure-gfm/good_chart-1.png" alt="A column chart of the same five counties with the y-axis starting at zero and exact enrollment labeled above each bar. Now the bars are proportional: Dade is clearly the largest but Broward and Hillsborough are visibly comparable, not negligible. All bars share one color."  />

**How the redesign fixes it:** anchoring the y-axis at zero restores
honest, proportional bar heights; value labels make the numbers exact;
and dropping the color fill for a single colorblind-safe grey removes
redundant, distracting color and any perceived bias.

## Report

#### Data & Process

I joined three Florida spatial datasets — county boundaries, lakes, and
public/private schools — on county name. The join surfaced three name
mismatches (`ST JOHNS`/`ST LUCIE` missing periods, `MIAMI-DADE`
vs. `DADE`), which I found by `full_join`-ing all three and filtering
for `NA` matches, then normalized with `recode_values()`. One invalid
lake geometry was repaired with `st_make_valid()`. I started without a
fixed question and let the lakes-vs-schools comparison drive it.

#### What the Charts Show

- **Enrollment.** Dade leads on both enrollment and school count by a
  wide margin, and among the top five counties, district-run public
  schools account for nearly all enrollment. The interactive bars let
  the reader hover for exact counts and the school-count fill — values a
  static chart would force them to estimate.

- **Map + model.** Schools and lakes both concentrate in the same
  central areas, but the linear model finds only a weak, barely-positive
  lake-area effect, and k-means isolates Dade as its own cluster while
  every other county stays tightly grouped. The likeliest explanation is
  a shared driver — population and county size — not a real
  lake-to-school link; coastal water, which the data ignores, probably
  matters more than inland lakes. I read the map pattern as a population
  proxy, not evidence of a relationship.

#### Redesign & Accessibility

The before/after redesign takes the top-five enrollment bars, shows them
with a truncated axis and distracting fill, then fixes both: a zero
baseline restores proportional heights and a single colorblind-safe hue
removes the false hierarchy that per-bar color implied. Accessibility
runs through the whole report — every figure uses a `viridis` palette
and carries `fig.alt` text, and color is never the only cue (the
redesign labels values directly).

#### Design Decisions & Next Steps

Plots share `theme_minimal()` and use color purposefully: a continuous
`viridis` scale encodes school count in the enrollment bars, and the map
orients the reader through position and one labeled reference point
(Florida Poly) rather than color. With more time I’d add county
population and land area to separate “lakes” from “size,” and measure
school-to-water distance directly instead of aggregating to the county.
