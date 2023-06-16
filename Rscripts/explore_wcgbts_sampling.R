load("data-raw/nwfscSurvey/Catch__NWFSC.Combo_2023-03-06.rda")
out2 = Out %>% dplyr::filter(total_catch_numbers > 0)

p = ggplot(out2, aes(x = total_catch_numbers, y = Subsample_count / total_catch_numbers)) + 
  geom_point(alpha = 0.5) + scale_x_continuous(trans='log', breaks = 2^(1:13))
p + geom_hline(yintercept=0.05, linetype="dashed", color = "red") +
  labs(title = "Petrale sole sampling ratios") + 
  xlab("total catch in numbers") + 
  ylab("subsample count / total catch in numbers")


out2 %>% dplyr::filter(Subsample_count / total_catch_numbers < 0.05) %>% 
  dplyr::select(4:18)
ggsave("figures/sampling_ratios.png")
