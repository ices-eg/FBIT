Data is prepared by Karin van der Reijden based on publicly available outputs.

The original data sources are:

ICES 2022. HELCOM request 2022 for spatial data layers on effort, fishing intensity and fishing footprint for the years 2016-2021 (data downloaded in 2024)

ICES. 2021. Data for OSPAR request on the production of spatial data layers of fishing intensity/pressure. Data Outputs. https://doi.org/10.17895/ices.data.8294 (data downloaded in 2024)


# ----------------
Both datasets are clipped to ICES Ecoregions, based on the midpoint of an 0.05 degree c-square. The HELCOM data is clipped to the Baltic Sea Ecoregion, the OSPAR data is clipped to the Greater North Sea, Celtic Seas, and Bay of Biscay and the Iberian Coast ecoregions.

For both datasets, the landing weights are (partly) categorized. In the Baltic Sea data, some c-squares mention the actual total weight, while other c-squares only mention the category. In the Atlantic are, all landing data is categorized.

For each c-square with no actual total weight mentioned, the mean landings weight was estimated as: (lower + upper value)/2.

The OT_MIX métier includes information from OT_MIX_DMF_BEN. Similarly, the OT_CRU includes information from OT_MIX_CRU_DMF. This is done by aggregating their annual SAR and landings per c-square (following the approach from WGFBIT).

The ICES data for HELCOM provided information per quarter. Here, I first determined mean landings (from upper and lower limit of the category), then summed up to annual sar and landings, and then combined the métiers.