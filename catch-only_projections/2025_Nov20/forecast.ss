#C file created using an r4ss function
#C file write time: 2025-11-20  11:50:39
#
1 #_benchmarks
2 #_MSY
0.3 #_SPRtarget
0.25 #_Btarget
#_Bmark_years: beg_bio, end_bio, beg_selex, end_selex, beg_relF, end_relF,  beg_recr_dist, end_recr_dist, beg_SRparm, end_SRparm (enter actual year, or values of 0 or -integer to be rel. endyr)
-1 -1 -1 -1 -1 -1 1876 2022 1876 2022
2 #_Bmark_relF_Basis
1 #_Forecast
14 #_Nforecastyrs
1 #_F_scalar
#_Fcast_years:  beg_selex, end_selex, beg_relF, end_relF, beg_recruits, end_recruits (enter actual year, or values of 0 or -integer to be rel. endyr)
0 0 -4 0 -999 0
0 #_Fcast_selex
3 #_ControlRuleMethod
0.25 #_BforconstantF
0.05 #_BfornoF
-1 #_Flimitfraction
 #_year fraction
   2023   1.0000
   2024   1.0000
   2025   1.0000
   2026   0.8556
   2027   1.0000
   2028   1.0000
   2029   0.9170
   2030   0.9130
   2031   0.9090
   2032   0.9040
   2033   0.9000
   2034   0.8960
   2035   0.8920
   2036   0.8870
-9999 0
3 #_N_forecast_loops
3 #_First_forecast_loop_with_stochastic_recruitment
0 #_fcast_rec_option
1 #_fcast_rec_val
0 #_HCR_anchor
2025 #_FirstYear_for_caps_and_allocations
0 #_stddev_of_log_catch_ratio
1 #_Do_West_Coast_gfish_rebuilder_output
2013 #_Ydecl
2015 #_Yinit
1 #_fleet_relative_F
# Note that fleet allocation is used directly as average F if Do_Forecast=4 
2 #_basis_for_fcast_catch_tuning
# enter list of fleet number and max for fleets with max annual catch; terminate with fleet=-9999
-9999 -1
# enter list of area ID and max annual catch; terminate with area=-9999
-9999 -1
# enter list of fleet number and allocation group assignment, if any; terminate with fleet=-9999
-9999 -1
2 #_InputBasis
 #_year seas fleet catch_or_F
   2023    1     1  2018.1000
   2023    1     2   884.2000
   2024    1     1  2246.0000
   2024    1     2   681.4000
   2025    1     1  1588.6000
   2025    1     2   568.6000
   2027    1     1  1825.3236
   2027    1     2   663.6764
   2028    1     1  1825.3236
   2028    1     2   663.6764
-9999 0 0 0
#
999 # verify end of input 
