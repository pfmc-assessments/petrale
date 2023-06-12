---
geometry: margin=1in
month: "June"
year: "2023"
preamble: |
output:
  sa4ss::techreport_pdf:
    default
  bookdown::pdf_document2:
    keep_tex: true
lang: en
papersize: letter
csl: canadian-journal-of-fisheries-and-aquatic-sciences.csl
---



<!--chapter:end:00a.Rmd-->

---
author:
  - name: Ian G. Taylor
    code: 1
    first: I
    middle: G
    family: Taylor
  - name: Vladlena Gertseva
    code: 1
    first: V
    middle: ''
    family: Gertseva
author_list: Taylor, I.G., V. Gertseva
affiliation:
  - code: 1
    address: Northwest Fisheries Science Center, U.S. Department of Commerce, National
      Oceanic and Atmospheric Administration, National Marine Fisheries Service, 2725
      Montlake Boulevard East, Seattle, Washington 98112
address: ^1^Northwest Fisheries Science Center, U.S. Department of Commerce, National
  Oceanic and Atmospheric Administration, National Marine Fisheries Service, 2725
  Montlake Boulevard East, Seattle, Washington 98112
---

<!--chapter:end:00authors.Rmd-->

---
bibliography:
  - sa4ss.bib
---

<!--chapter:end:00bibliography.Rmd-->

---
title: |
  Status of petrale sole (_Eopsetta jordani_) \
  along the US West coast in 2023
---

<!--chapter:end:00title.Rmd-->

\pagebreak
\pagenumbering{roman}
\setcounter{page}{1}

\renewcommand{\thetable}{\roman{table}}
\renewcommand{\thefigure}{\roman{figure}}


\setlength\parskip{0.5em plus 0.1em minus 0.2em}

<!--chapter:end:01a.Rmd-->



# Executive summary{-}

## Stock{-}

This assessment reports the status of petrale sole (_Eopsetta jordani_) off the US West coast using data through 2022.

## Catches{-}

Adding text here for example.
trends and current levels.
Include Table for last 10 years.
Include Figure with long-term estimates.

\clearpage

\input{tex_tables/a_Catches_ES.tex}


![Landings by fleet used in the base model where catches in metric tons by fleet are stacked.\label{fig:es-catch}](../models/2023.a024.018_min_sample_retuned/plots/catch2 landings stacked.png){width=100% height=100% alt="."}

\clearpage

## Data and assessment{-}

This assessment uses the stock assessment framework
Stock Synthesis 

```
[1] "3.30.21.00"
```
(SS3).

Replace text with
date of last assessment,
type of assessment model,
data available,
new information, and
information lacking.

## Stock biomass and dynamics{-}

Replace text with
trends and current levels relative to virgin or historic levels and
description of uncertainty.
Include Table for last 10 years.
Include Figure with long-term estimates.

\input{tex_tables/b_SSB_ES.tex}


![Estimated time series of spawning output for the base model.\label{fig:es-ssb}](../models/2023.a024.018_min_sample_retuned/plots/ts7_Spawning_output.png){width=100% height=100% alt="."}


![Estimated time series of fraction of unfished spawning output for the base model.\label{fig:es-depl}](../models/2023.a024.018_min_sample_retuned/plots/ts9_Relative_spawning_output.png){width=100% height=100% alt="."}

\clearpage

## Recruitment{-}

Replace text with
trends and current levels relative to virgin or historic levels and
description of uncertainty.
Include Table for last 10 years.
Include Figure with long-term estimates.


\input{tex_tables/c_Recr_ES.tex}


![Estimated time series of age-0 recruits (1000s) for the base model.\label{fig:es-recruits}](../models/2023.a024.018_min_sample_retuned/plots/ts11_Age-0_recruits_(1000s).png){width=100% height=100% alt="."}


![Estimated time series of recruitment deviations.\label{fig:es-rec-devs}](../models/2023.a024.018_min_sample_retuned/plots/recdevs1_points.png){width=100% height=100% alt="."}

\clearpage
## Exploitation status{-}

Replace text with
total catch divided by exploitable biomass or SPR harvest rate.
Include Table for last 10 years.
Include Figure with trend in f relative to target vs. trend in biomass relative to the target.

\input{tex_tables/d_SPR_ES.tex}


![Estimated 1 - relative spawning ratio (SPR) by year for the base model. The management target is plotted as a red horizontal line and values above this reflect harvest in excess of the proxy harvest rate.\label{fig:es-1-spr}](../models/2023.a024.018_min_sample_retuned/plots/SPR2_minusSPRseries.png){width=100% height=100% alt="."}

\clearpage

## Ecosystem considerations{-}

Replace text with
a summary of reviewed environmental and ecosystem factors that appear to be correlated with stock dynamics.
These may include variability in they physical environment, habitat, competitors, prey, or predators that directly or indirectly affects the stock's status, vital rates (growth, survival, productivity/recruitment) or range and distribution.
Note which, if any, ecosystem factors are used in the assessment and how (e.g., as background information, in data preparations, as data inputs, in decisions about model structure).

## Reference points{-}

Replace text with
management targets and definition of overfishing, including the harvest rate that brings the stock to equilibrium at $B_{40\%}$, i.e., the $B_{MSY}$ proxy and the equilibrium stock size that results from fishing at the default harvest rate, i.e., the $F_{MSY}$ proxy.
Include Table of estimated reference points for ssb, SPR, exploitation rate, and yield based on SSB proxy for MSY, SPR proxy for MSY, and estimated MSY values.


![Phase plot of estimated 1-SPR versus fraction unfished for the base model.\label{fig:es-phase}](../models/2023.a024.018_min_sample_retuned/plots/SPR4_phase.png){width=100% height=100% alt="."}


![Equilibrium yield curve for the base case model. Values are based on the 2020
fishery selectivities and with steepness fixed at 0.80.\label{fig:es-yield}](../models/2023.a024.018_min_sample_retuned/plots/yield2_yield_curve_with_refpoints.png){width=100% height=100% alt="."}

\input{tex_tables/e_ReferencePoints_ES.tex}

\clearpage

## Management performance{-}

Include Table of most recent 10 years of
catches in comparison with OFL, ABC, HG, and OY/ACL values,
overfishing levels,
actual catch and discard.
Include OFL (encountered), OFL (retained), and OFL (dead) if different due to discard and discard mortality.

## Unresolved problems and major uncertainties{-}

Replace text with
any special issues that complicate scientific assessment, questions about the best model scenario, etc.

## Decision table and projections{-}

Replace text with
projected yields (OFL, ABC, and ACL), spawning biomass, and stock depletion levels for each year.
OFL calculations should be based on the assumption that future catches equal ABCs and not OFLs.

## Scientific uncertainty{-}

Replace text with
the sigma value and the basis for its calculation.

## Research and data needs{-}

Replace text with
information gaps that seriously impede the stock assessment.

<!--chapter:end:01executive.Rmd-->

\pagebreak
\setlength{\parskip}{5mm plus1mm minus1mm}
\pagenumbering{arabic}
\setcounter{page}{1}
\renewcommand{\thefigure}{\arabic{figure}}
\renewcommand{\thetable}{\arabic{table}}
\setcounter{table}{0}
\setcounter{figure}{0}

<!--chapter:end:10a.Rmd-->

# Introduction

This updated assessment does not attempt to reiterate all background information for petrale sole presented in the 2013 assessment document. Instead, only a few key assumptions are restated, along with a detailed description of changes made during the course of the update. Those interested in a more complete description of petrale sole life-history and the details of previous assessments should refer to the 2013 assessment [@haltuch_status_2013].
  
## Basic Information
Petrale Sole (\emph{Eopsetta jordani}) is a right-eyed flounder in the family Pleuronectidae ranging from the western Gulf of Alaska to the Coronado Islands, northern Baja California [@kramer_guide_1995; @love_milton_resource_2005] with a preference for soft substrates at depths ranging from 0-550 m [@love_milton_resource_2005]. Common names include brill, California sole, Jordan's flounder, cape sole, round nose sole, English sole, soglia, petorau, nameta, and tsubame garei [@smith_report_1937; @gates_designated_1974; @love_milton_probably_1996; @eschmeyer_field_1983]. In northern and central California petrale sole are dominant on the middle and outer continental shelf. PacFIN fishery logbook data show that adults are caught in depths from 18 to 1,280 m off the U.S. West Coast with a majority of the catches of petrale sole being taken between 70-220 m during March through October, and between 290-440 m during November through February.

There is little information regarding the stock structure of petrale sole off the U.S. Pacific coast. No genetic research has been undertaken for petrale sole and there is no other published research indicating separate stocks of petrale sole within U.S. waters. Tagging studies show adult petrale sole can move up to 350 - 390 miles, having the ability to be highly migratory with the possibility for homing ability [@alverson_results_1957]. Juveniles show little coast-wide or bathymetric movement while studies suggest that adults generally move inshore and northward onto the continental shelf during the spring and summer to feeding grounds and offshore and southward during the fall and winter to deep water spawning grounds [@horton_species_1989; @love_milton_probably_1996]. Adult petrale sole can tolerate a wide range of bottom temperatures [@perry_environmental_1994].

Tagging studies indicate some mixing of adults between different spawning groups. DiDonato and Pasquale [-@didonato_migration_1970] reported that five fish tagged on the Willapa Deep grounds during the spawning season were recaptured during subsequent spawning seasons at other deepwater spawning grounds, as far south as Eureka (northern California) and the Umpqua River (southern Oregon). However, Pedersen [-@pedersen_movements_1975] reported that most of the fish (97%) recaptured from spawning grounds in winter were originally caught and tagged on those same grounds.

Mixing of fish from multiple deep water spawning grounds likely occurs during the spring and summer when petrale sole are feeding on the continental shelf. Fish that were captured, tagged, and released off the northwest Coast of Washington during May and September were subsequently recaptured during winter from spawning grounds off Vancouver Island (British Columbia, 1 fish), Heceta Bank (central Oregon, 2 fish), Eureka (northern California, 2 fish), and Halfmoon Bay (central California, 2 fish) (Pederson, 1975). Fish tagged south of Fort Bragg (central California) during July 1964 were later recaptured off Oregon (11 fish), Washington (6 fish), and Swiftsure Bank (southwestern tip of Vancouver Island, 1 fish) (D. Thomas, California Department of Fish and Game, Menlo Park, CA, cited by Sampson and Lee [-@sampson_assessment_1999]).

The highest densities of spawning adults off of British Columbia, as well as of eggs, larvae and juveniles, are found in the waters around Vancouver Island. Adults may utilize nearshore areas as summer feeding grounds and non-migrating adults may stay there during winter [@starr_petrale_2004].

Past assessments completed by Demory [-@demory_progress_1984,], Turnock et al. [-@turnock_status_1993], and Sampson and Lee [-@sampson_assessment_1999] considered petrale sole in the Columbia and U.S.-Vancouver INPFC areas a single stock. Sampson and Lee (1999) assumed that petrale sole in the Eureka and Monterey INPFC areas represented two additional distinct socks. The 2005 petrale sole assessment assumed two stocks, northern (U.S.-Vancouver and Columbia INPFC areas) and southern (Eureka, Monterey and Conception INPFC areas), to maintain continuity with previous assessments. Three stocks (West Coast Vancouver Island, Queen Charlotte Sound, and Heceta Strait) are considered for petrale sole in the waters off British Columbia, Canada [@starr_petrale_2004]. The 2009, 2011, 2013, and 2015 assessments integrate the previously separate north-south assessments to provide a coast-wide status evaluation. The decision to conduct a single-area assessment is based on strong evidence of a mixed stock from tagging studies, a lack of genetic studies on stock structure, and a lack of evidence for differences in growth between the 2005 northern and southern assessment areas and from examination of the fishery size-at-age data, as well as confounding differences in data collection between Washington, Oregon, and California. This 2019 update assessment provides a coast-wide status evaluation for petrale sole using data through 2018.

Fishing fleets are separated both geographically and seasonally to account for spatial and seasonal patterns in catch given the coast-wide assessment area. The petrale sole fisheries possess a distinct seasonality, with catches peaking during the winter months, so the fisheries are divided into winter (November-February) and summer (March-October) fisheries. Note that the "fishing year" for this assessment (November 1 to October 31) differs from the standard calendar year. The U.S.-Canadian border is the northern boundary for the assessed stock, although the basis for this choice is due to political and current management needs rather than the population dynamics. Given the lack of clear information regarding the status of distinct biological populations, this assessment treats the U.S. Petrale Sole resource from the Mexican border to the Canadian border as a single coast-wide stock.


## Life History
Petrale Sole spawn during the winter at several discrete deepwater sites (270-460 m) off the U.S. West Coast, from November to April, with peak spawning taking place from December to February [@harry_analysis_1956; @best_petrale_1960; @gregory_validity_1976; @castillo_g.c._environmental_1993; @reilly_recreational_1994; @castillo_latitudinal_1995; @love_milton_probably_1996]. Females spawn once each year and fecundity varies with fish size, with one large female laying as many as 1.5 million eggs [@porter_notes_1964]. Petrale Sole eggs are planktonic, ranging in size from 1.2 to 1.3 mm, and are found in deep water habitats at water temperatures of 4-10 degrees C and salinities of 25-30 ppt [@best_petrale_1960; @ketchen_population_1966; @alderdice_effects_1971; @gregory_validity_1976]. The duration of the egg stage can range from approximately 6 to 14 days [@alderdice_effects_1971; @love_milton_probably_1996]. The most favorable conditions for egg incubation and larval growth are 6-7 degrees C and 27.5-29.5 ppt [@ketchen_population_1966; @alderdice_effects_1971]. 

Petrale Sole spawn during the winter at several discrete deepwater sites (270-460 m) off the U.S. West Coast, from November to April, with peak spawning taking place from December to February [@harry_time_1959; @best_petrale_1960; @gregory_validity_1976; @castillo_g.c._environmental_1993; @reilly_recreational_1994; @love_milton_probably_1996]. Females spawn once each year and fecundity varies with fish size, with one large female laying as many as 1.5 million eggs [@porter_notes_1964]. Petrale Sole eggs are planktonic, ranging in size from 1.2 to 1.3 mm, and are found in deep water habitats at water temperatures of 4-10 degrees C and salinities of 25-30 ppt [@best_petrale_1960; @ketchen_population_1966;  @alderdice_effects_1971; @gregory_validity_1976]. The duration of the egg stage can range from approximately 6 to 14 days [@alderdice_effects_1971; @love_milton_probably_1996]. The most favorable conditions for egg incubation and larval growth are 6-7 degrees C and 27.5-29.5 ppt [@ketchen_population_1966; @alderdice_effects_1971; @castillo_latitudinal_1995].

Adult petrale sole achieve a maximum size of around 50 cm and 63 cm for males and females, respectively [@best_e.a._movements_1963; @pedersen_movements_1975]. The maximum length reported for petrale sole is 70 cm [@eschmeyer_field_1983; @love_milton_resource_2005] while the maximum observed break-and-burn age is 31 years [@haltuch_status_2013].

## Ecosystem Considerations
Petrale Sole juveniles are carnivorous, foraging on annelid worms, clams, brittle star, mysids, sculpin, amphipods, and other juvenile flatfish [@casilla_essential_1998; @pearsall_diet_2007]. Predators on juvenile petrale sole include adult petrale sole as well as other larger fish [@casilla_essential_1998] while adults are preyed upon by marine mammals, sharks, and larger fishes [@trumble_abundance_1995; @love_milton_probably_1996; @casilla_essential_1998].

One of the ambushing flatfishes, adult petrale sole have diverse diets that become more piscivorous at larger sizes [@allen_ecology_2006]. Adult petrale sole are found on sandy and sand-mud bottoms [@eschmeyer_field_1983] foraging for a variety of invertebrates including, crab, octopi, squid, euphausiids, and shrimp, as well as anchovies. hake, herring, sand lance, and other smaller rockfish and flatfish [@kravitz_food_1977; @birtwell_fish_1984; @reilly_recreational_1994; @love_milton_probably_1996; @pearsall_diet_2007]. In Canadian waters evidence suggests that petrale sole tend to prefer herring [@pearsall_diet_2007]. On the continental shelf petrale sole generally co-occur with English sole, rex sole, Pacific sanddab, and rock sole [@kravitz_food_1977].

Ecosystem factors have not been explicitly modeled in this assessment, but there are several aspects of the California current ecosystem that may impact petrale sole population dynamics and warrant further research. Castillo [-@castillo_g.c._fluctuations_1992] and Castillo et al. [-@castillo_latitudinal_1995] suggest that density-independent survival of early life stages is low and show that offshore Ekman transportation of eggs and larvae may be an important source of variation in year-class strength in the Columbia INPFC area. The effects of the Pacific Decadal Oscillation (PDO) on California current temperature and productivity [@mantua_pacific_1997] may also contribute to non-stationary recruitment dynamics for petrale sole. The prevalence of a strong late 1990s year-class for many West Coast groundfish species suggests that environmentally driven recruitment variation may be correlated among species with relatively diverse life history strategies. Although current research efforts along these lines are limited, a more explicit exploration of ecosystem processes may be possible in future petrale sole stock assessments.

## Historical and Current Fishery Information
Petrale Sole have been caught in the flatfish fishery off the U.S. Pacific coast since the late 19th century. The fishery first developed off of California where, prior to 1876, fishing in San Francisco Bay was by hand or set lines and beach seining [@scofield_trawling_1948]. By 1880 two San Francisco based trawler companies were running a total of six boats, extending the fishing grounds beyond the Golden Gate Bridge northward to Point Reyes [@scofield_trawling_1948]. Steam trawlers entered the fishery during 1888 and 1889, and four steam tugs based out of San Francisco were sufficient to flood market with flatfish [@scofield_trawling_1948]. By 1915 San Francisco and Santa Cruz trawlers were operating at depths of about 45-100 m with catches averaging 10,000 lbs per tow or 3,000 lbs per hour [@scofield_trawling_1948]. Flatfish comprised approximately 90% of the catch with 20-25% being discarded as unmarketable [@scofield_trawling_1948]. During 1915 laws were enacted that prohibited dragging in California waters and making it illegal to possess a trawl net from Santa Barbara County southward [@scofield_trawling_1948]. By 1934 twenty 56-72 foot diesel engine trawlers operated out of San Francisco fishing between about 55 and 185 m [@scofield_trawling_1948]. From 1944-1947 the number of California trawlers fluctuated between 16 and 46 boats [@scofield_trawling_1948]. Although the flatfish fishery in California was well developed by the 1950s and 1960s, catch statistics were not reported until 1970 [@heimann_pacific_1970]. In this early California report petrale sole landings during 1916 to 1930 were not separated from the total flatfish landings. 

The earliest trawl fishing off Oregon began during 1884-1885, and the fishery was solidly established by 1937, with the fishery increasing rapidly during WWII (Harry and Morgan, 1961). Initially trawlers stayed close to the fishing grounds adjacent to Newport and Astoria, operating at about 35-90 m between Stonewall Bank and Depoe Bay. Fishing operations gradually extended into deep water. For example, Newport-based trawlers were commonly fishing at about 185 m in 1949, at about 185-365 m by 1952, and at about 550 m by 1953.

Alverson and Chatwin [-@alverson_results_1957] describe the history of the petrale sole fishery off of Washington and British Columbia with fishing grounds ranging from Cape Flattery to Destruction Island. Petrale Sole catches off of Washington were small until the late 1930s with the fishery extending to about 365 m following the development of deepwater rockfish fisheries during the 1950s.

By the 1950s the petrale sole fishery was showing signs of depletion with reports suggesting that petrale sole abundance had declined by at least 50% from 1942 to 1947 [@harry_analysis_1956]. Sampson and Lee [-@sampson_assessment_1999] reported that three fishery regulations were implemented during 1957-67: 1) a winter closure off Oregon, Washington and British Columbia, 2) a 3,000 lb per trip limit, and 3) no more than two trips per month during 1957. With the 1977 enactment of the Magnuson Fishery Conservation and Management Act (MFCMA) the large foreign-dominated fishery that had developed since the late 1960s was replaced by the domestic fishery that continues today. Petrale Sole are harvested almost exclusively by bottom trawls in the U.S. West Coast groundfish fishery. Recent petrale sole catches exhibit marked seasonal variation, with substantial portions of the annual harvest taken from the spawning grounds during December and January. Evidence suggests that the winter fishery on the deepwater spawning grounds developed sporadically during the 1950s and 1960s as fishers discovered new locations (e.g., @alverson_results_1957; @ketchen_population_1966). Both historical and current petrale sole fisheries have primarily relied upon trawl fleets. Fishery removals were divided among 4 fleets: 1) winter North trawl, 2) summer North trawl, 3) winter South trawl, and 4) summer South trawl. Landings for the North fleet are defined as fish landed in Washington and Oregon ports. Landings for the South fleet are defined as fish landed in California ports.

<!-- commenting out section that references tables and values not yet available -->
<!-- Historical landings reconstructions show peak catches from the summer fishery occurred during the 1940s and 1950s and subsequently declined, during which time the fleet moved to fishing in deeper waters during the winter. After the period of peak landings during the 1940s and 1950s, total landings were somewhat stable until about the late 1970s, and then generally declined until the mid-2000s. -->

## Summary of Management History and Performance
Beginning in 1983 the Pacific Fishery Management Council (PFMC) established coast-wide annual catch limits (ACLs) for the annual harvests of petrale sole in the waters off the U.S. West Coast. The first assessment of West Coast petrale sole occurred in 1984 [@demory_progress_1984]. Based on the 1999 assessment a coast-wide ACL of 2,762 mt was specified and remained unchanged between 2001 and 2006.

The 2005 assessment of petrale sole stock assessment split the stock into two areas, the northern area that included U.S.-Vancouver and Columbia INPFC areas and the southern area that included the Eureka, Monterey and Conception INPFC areas [@lai_stock_2005]. While petrale sole stock structure is not well understood, CPUE and geographical differences between states were used to support the use of two separate assessment areas. In 2005 petrale sole were estimated to be at 34 and 29% of unfished spawning stock biomass in the northern and southern areas, respectively. In spite of different models and data, the biomass trends were qualitatively similar in both areas, providing support for a coast-wide stock. This assessment estimated that petrale sole had historically been below the Pacific Council's minimum stock size threshold of 25% of unfished biomass from the mid-1970s until just prior to the completion of the assessment, with estimated harvest rates in excess of the target fishing mortality rate implemented for petrale sole at that time (F40%). However, the 2005 stock assessment determined that the stock was in the precautionary zone and was not overfished (i.e., the spawning stock biomass was not below 25% of the unfished spawning stock biomass). Based on the 2005 stock assessment results, ACLs were set at 3,025 mt and 2,919 mt for 2007 and 2008, respectively, with an ACT of 2,499 mt for both years. 

In comparison to the 1999 assessment of petrale sole, the 2005 assessment represented a significant change in the perception of petrale sole stock status. The stock assessment conducted in 1999 (Washington-Oregon only) estimated the spawning stock biomass in 1998 at 39% of unfished stock biomass. Although the estimates of 1998 spawning-stock biomass were little changed between the 1999 and 2005 (Northern area) assessments, the estimated depletion in the 2005 assessment was much lower. The change in status between the 1999 and 2005 analyses was due to the introduction of a reconstructed catch history in 2005, which spanned the entire period of removals. The 1999 stock assessment used a catch history that started in 1977, after the bulk of the removals from the fishery had already taken place. Thus the 1999 stock assessment produced a more optimistic view of the petrale stock's level of depletion. The stock's estimated decline in status between the 2005 and 2009 assessments was driven primarily by a significant decline in the trawl-survey index over that period. The 2011 assessment concluded that the stock status continued to be below the target of 25% of unfished biomass.

The 2009 coast-wide stock assessment estimated that the petrale sole stock had declined from its 2005 high to 11.6% of the unfished spawning stock biomass [@haltuch_status_2009]. The petrale sole was declared overfished based on newly adopted management targets (e.g., target spawning biomass for flatfish stocks defined as 25% and overfished threshold of 12.5% of unfished spawning stock biomass) resulting in a rebuilding plan and catch restrictions for petrale sole. The stock was declared rebuilt based on the results of the 2015 update stock assessment which estimated the coastwide biomass at 30.7% of unfished spawning stock output with ACLs of 3,136 and 3,013 in 2017 and 2018 respectively [@stawitz_stock_2015]. 

For additional information on changes in the petrale sole fishery please see the 2013 stock assessment [@haltuch_status_2013].

## Fisheries off Canada and Alaska
The Canadian fishery developed rapidly during the late 1940s to mid-1950s following the discovery of petrale sole spawning aggregations off the West Coast of Vancouver Island [@anon_fish_2001]. Annual landings of petrale sole in British Columbia peaked at 4,800 mt in 1948 but declined significantly after the mid-1960s [@anon_fish_2001]. By the 1970s, analysis conducted by Pederson [-@pedersen_movements_1975] suggested that petrale sole abundance was low and abundance remained low into the 1990s. In the early 1990s vessel trip quotas were established to try to halt the decline in petrale sole abundance [@anon_fish_2001]. Winter quarter landings of petrale sole were limited to 44,000 lb per trip during 1985-91; to 10,000 lb per trip during 1991-95; and to 2,000 lb per trip in 1996. Biological data collected during 1980-1996 showed a prolonged decline in the proportion of young fish entering the population [@anon_fish_2001]. Therefore, no directed fishing for petrale sole has been permitted in Canada since 1996 due to a continuing decline in long term abundance [@fargo_j.j._flatfish_1997; @anon_fish_2001]. As of 2005 petrale sole off of British Columbia were treated as three "stocks" and were still considered to be at low levels. The recent assessments for the Canadian stocks have been based on catch histories and limited biological data. 

In Alaska petrale sole are not targeted in the Bering Sea/Aleutian Island fisheries and are managed as a minor species in the "Other Flatfish" stock complex.

<!--chapter:end:11introduction.Rmd-->

# Data

Data comprise the foundational components of stock assessment models.
The decision to include or exclude particular data sources in an assessment model depends on many factors.
These factors often include, but are not limited to,
the way in which data were collected (e.g., measurement method and consistency);
the spatial and temporal coverage of the data;
the quantity of data available per desired sampling unit;
the representativeness of the data to inform the modeled processes of importance;
timing of when the data were provided;
limitations imposed by the Terms of Reference; and
the presence of an avenue for the inclusion of the data in the assessment model.
Attributes associated with a data source can change through time,
as can the applicability of the data source when different modeling approaches are explored (e.g., stock structure or time-varying processes).
Therefore, the specific data sources included or excluded from this assessment should not necessarily constrain the selection of data sources applicable to future stock assessments for petrale sole.
Even if a data source is not directly used in the stock assessment they can provide valuable insights into biology, fishery behavior, or localized dynamics.

Data from a wide range of programs were available for possible inclusion in the current assessment model.
Descriptions of each data source included in the model (Figure \@ref(fig:data-plot)) and sources that were explored but not included in the base model are provided below.
Data that were excluded from the base model were explicitly explored during the development of this stock assessment or have not changed since their past exploration in a previous petrale sole stock assessment.
In some cases, the inclusion of excluded data sources were explored through sensitivity analyses (see Section \@ref(assessment-model)).

<!--chapter:end:20data.Rmd-->

## Fishery-Dependent Data
Fishery removals were divided among two fleets: 1) North and 2) South. Landings for the North fleet are defined as fish landed in Washington and Oregon ports. Landings for the South feet are defined as fish landed in California ports. The landings of Petrale sole are made primarily by groundfish bottom trawl gear; landings by gear types other than bottom trawl have been inconsequential, averaging less than 2.5% of the coast-wide landings. The non-trawl landings (that consist of only a small fraction of the total landings) are included along with the trawl landings in a single gears fleets.

<!--chapter:end:21f-.Rmd-->

## Fishery-Independent Data

<!--chapter:end:21s-.Rmd-->

### \acrlong{s-aslope}

The \gls{s-aslope} operated during the months of October to November aboard the R/V _Miller Freeman_.
Partial survey coverage of the US west coast occurred during the years 1988-1996 and complete coverage (north of 34\textdegree 30\textquotesingle S) during the years 1997 and 1999-2001.
Typically, only these four years that are seen as complete surveys are included in assessments.

<!--chapter:end:21s-aslope.Rmd-->

### \acrlong{s-ccfrp}

Since 2007, the \gls{s-ccfrp} has monitored several areas in California to evaluate the performance of \glspl{mpa}
and understand nearshore fish populations
[@Wendt2009; @Starr2015].
In 2017, the survey expanded beyond the four \Gls{mpa}s in central California
(A&ntilde;o Nuevo, Point Lobos, Point Buchon, and Piedras Blancas)
to include the entire California coast.
Fish are collected by volunteer anglers aboard \glspl{cpfv} guided by one of the following academic institutions based on proximity to fishing location:
Humboldt State University;
Bodega Marine Laboratories;
Moss Landing Marine Laboratories;
Cal Poly San Luis Obispo;
University of California, Santa Barbara; and
Scripps Institution of Oceanography.

Surveys consist of fishing with hook-and-line gear for 30-45 minutes within randomly chosen 500 by 500 m grid cells within and outside \glspl{mpa}.
Prior to 2017, all fish were measured for length and release or descended to depth;
since then, some were sampled for otoliths and fin clips.


<!--chapter:end:21s-ccfrp.Rmd-->

### \acrlong{s-tri}

The \gls{s-tri} was first conducted by the \gls{afsc} in 1977, and the survey continued until 2004 [@weinberg_2001_2002].
Its basic design was a series of equally-spaced east-to-west transects across the continential shelf from which searches for tows in a specific depth range were initiated.
The survey design changed slightly over time.
In general, all of the surveys were conducted in the mid summer through early fall.
The 1977 survey was conducted from early July through late September.
The surveys from 1980 through 1989 were conducted from mid-July to late September.
The 1992 survey was conducted from mid July through early October.
The 1995 survey was conducted from early June through late August.
The 1998 survey was conducted from early June through early August.
Finally, the 2001 and 2004 surveys were conducted from May to July.

Haul depths ranged from 91-457 m during the 1977 survey with no hauls shallower than 91 m.
Due to haul performance issues and truncated sampling with respect to depth, the data from 1977 were omitted from this analysis.
The surveys in 1980, 1983, and 1986 covered the US West Coast south to 36.8\textdegree N latitude and a depth range of 55-366 m.
The surveys in 1989 and 1992 covered the same depth range but extended the southern range to 34.5\textdegree N (near Point Conception).
From 1995 through 2004, the surveys covered the depth range 55-500 m and surveyed south to 34.5\textdegree N.
In 2004, the final year of the \gls{s-tri} series, the \gls{nwfsc} \gls{fram} conducted the survey following similar protocols to earlier years.

<!--chapter:end:21s-tri.RMd-->

### \acrlong{s-wcgbt}

The \Gls{s-wcgbt} is based on a random-grid design;
covering the coastal waters from a depth of 55-1,280 m [@bradburn_2003_2011].
This design generally uses four industry-chartered vessels per year assigned to a roughly equal number of randomly selected grid cells and divided into two 'passes' of the coast.
Two vessels fish from north to south during each pass between late May to early October.
This design therefore incorporates both vessel-to-vessel differences in catchability,
as well as variance associated with selecting a relatively small number (approximately 700) of possible cells from a very large set of possible cells spread from the Mexican to the Canadian borders.

<!--chapter:end:21s-wcgbts.Rmd-->

## Biological Data

### Natural Mortality


### Maturation and Fecundity


### Sex Ratio


### Length-Weight Relationship {#sec-biological-data-Length-Weight-relationship}


The weight-length relationship for petrale sole was estimated outside of the 
assessment model by fitting biological data to the standard power function, 
$W = aL^b$ using the R function `PacFIN.Utilities::getWLpars()` 
(where $W$ is weight in kilograms and $L$ is fork length in centimeters). 
The function estimates the relationship on a log-log scale and then uses the 
estimated standard deviation of the observed weights around the expected value 
to calculate the median weight at each length from the resulting lognormal 
distribution.

The parameters were estimated using data from the \gls{s-wcgbt}, where 21,704 
fish collected between 2003 and 2022 had both weight and length available 
of which 57.6\% were female, 42.3\% were male, and 0.1\% were unsexed. 
The resulting relationships were
$W=0.000002035 * L^{3.478}$ for females and
$W=0.000003043 * L^{3.359}$ for males. 
These relationships are very similar to those used for the previous assessment 
[@wetzel_status_2019].

### Growth (Length-at-Age)


### Ageing Precision and Bias

<!--chapter:end:22biology.Rmd-->

## Environmental and Ecosystem Data

<!--chapter:end:23enviro.Rmd-->

# Assessment Model


<!--chapter:end:30model.Rmd-->

## Summary of Previous Assessments and Reviews


### History of Modeling Approaches (not required for an update assessment)


### Most Recent STAR Panel and SSC Recommendations (not required for an update assessment)


### Response to Groundfish Subcommittee Requests (not required in draft)

<!--chapter:end:31summary.Rmd-->

## Model Structure and Assumptions


### Model Changes from the Last Assessment (not required for an update assessment)


### Modeling Platform and Structure
General model specifications (e.g., executable version, model structure, definition of fleets and areas)


### Model Parameters
Describe estimated vs. fixed parameters, priors

### Key Assumptions and Structural Choices

<!--chapter:end:32structure.Rmd-->

## Base Model Results


### Parameter Estimates
<!-- Parameters values are shown in Tables \ref{tab:table-pars-base-1}-\ref{tab:table-pars-base-16}. -->

### Fits to the Data

### Population Trajectory

### Reference Points



<!--chapter:end:33results.Rmd-->

## Model Diagnostics
Describe all diagnostics

### Convergence

### Sensitivity Analyses

### Retrospective Analysis

### Likelihood Profiles

### Unresolved Problems and Major Uncertainties

<!--chapter:end:34diagnostics.Rmd-->

# Management 

## Reference Points

## Unresolved Problems and Major Uncertainties

## Harvest Projections and Decision Tables

## Evaluation of Scientific Uncertainty

## Research and Data Needs

<!--chapter:end:40management.Rmd-->

# Acknowledgments
Here are all the mad props!

<!--chapter:end:41acknowledgments.Rmd-->

\clearpage

# References
<!-- If you want to references to appear somewhere before the end, add: -->
<div id="refs"></div>
<!-- where you want it to appear -->
<!-- The following sets the appropriate indentation for the references
  but it cannot be used with bookdown and the make file because it leads
  to a bad pdf.
\noindent
\vspace{-2em}
\setlength{\parindent}{-0.2in}
\setlength{\leftskip}{0.2in}
\setlength{\parskip}{8pt}
 -->

<!--chapter:end:49bibliography.Rmd-->

\clearpage
# Tables


<!-- ====================================================================== -->
<!-- model tables -->
<!-- ====================================================================== -->
\begin{landscape}\end{landscape}

<!--chapter:end:52tables.Rmd-->

\clearpage
# Figures

## Data 
<!-- ====================================================================== --> 
<!-- *******************    Assessment Map      *************************** --> 
<!-- ====================================================================== --> 



![Data presence by year for each fleet, where circle area is
relative within a data type. Circles are proportional to
total catch for catches; to precision for indices, discards, and
mean body weight observations; and to total sample size for
compositions.\label{fig:data-plot}](../models/2023.a024.018_min_sample_retuned/plots/data_plot2.png){width=100% height=100% alt="."}

<!-- ====================================================================== -->  
<!-- ****************** Catches Used in the Model ************************* --> 
<!-- ====================================================================== -->  


![Landings (mt) by fleet used in the base model.\label{fig:catch-figures-r4ss}](../models/2023.a024.018_min_sample_retuned/plots/catch2 landings stacked.png){width=100% height=100% alt="."}

![Landings plus dead discards (mt) by fleet as estimated in the base model.\label{fig:catchdiscard-figures-r4ss}](../models/2023.a024.018_min_sample_retuned/plots/catch16 landings + dead discards.png){width=100% height=100% alt="."}

<!-- ====================================================================== --> 
<!-- ******************* Data Used in the Model *************************** --> 
<!-- ====================================================================== --> 


## Biology


## Model Results

### Bridging


### Model Structure


### Estimated Biology


### Selectivity


### Recruitment


### Fits to Data


### Time-series


### Sensitivity Analyses and Retrospectives


### Likelihood Profiles


### Reference Points and Forecasts

<!--chapter:end:53figures.Rmd-->

