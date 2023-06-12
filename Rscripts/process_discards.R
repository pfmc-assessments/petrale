# extract GEMM estimates for catch by sector
gemm <- nwfscSurvey::pull_gemm("petrale sole")

# code to get total catch by sector for 2011+
gemm %>% 
  dplyr::filter(year >= 2011) %>%
  dplyr::group_by(sector) %>%
  dplyr::summarize(sum = sum(total_discard_and_landings_mt)) %>%
  dplyr::arrange(desc(sum))
# # A tibble: 31 x 2
#    sector                             sum
#    <chr>                            <dbl>
#  1 CS - Bottom Trawl              21763.
#  2 CS EM - Bottom Trawl            1595.
#  3 Tribal Shoreside                1576.
#  4 CS - Bottom and Midwater Trawl   812.
#  5 Research                         137.
#  6 Oregon Recreational               23.9
#  7 California Recreational           23.0
#  8 Incidental                        22.1
#  9 OA CA Halibut                     15.3
# 10 LE Sablefish - Hook & Line        13.0
# # ... with 21 more rows
# # i Use `print(n = ...)` to see more rows