# pull catch for petrale and canary
catch_tri <- nwfscSurvey::pull_catch(
  common = "petrale sole",
  survey = "Triennial",
  dir = "data-raw/nwfscSurvey"
)
catch_tri_canary <- nwfscSurvey::pull_catch(
  common = "canary rockfish",
  survey = "Triennial",
  dir = "data-raw/nwfscSurvey"
)

# make plots
depth_vs_lat <- function(dat, title = NULL) {
  ggplot(dat, aes(x=-Depth_m, y=Latitude_dd, 
    size = cpue_kg_per_ha_der, color = cpue_kg_per_ha_der > 0)) +
    geom_point(alpha=0.3) + 
    scale_size(range = c(.1, 15), name="CPUE (kg/ha)") +
    labs(color = "Species observed",
      title = title) + 
    geom_hline(yintercept = 36+48/60, 
      linetype=3) + 
    geom_vline(xintercept = -366, 
      linetype=3)
}
depth_vs_lat(catch_tri_canary, title = "Canary in the triennial")
ggsave("figures/depth_vs_lat_tri_canary.png")
depth_vs_lat(catch_tri, title = "Petrale in the triennial")
ggsave("figures/depth_vs_lat_tri_petrale.png")
