# FBIT
Fisheries Benthic Impact Tools


R-script for a regional assessment of different ICES marine ecoregions from WKFBIT. Output follows ICES demonstration product (2017) in “EU request on indicators of the pressure and impact of bottom-contacting fishing gear on the seabed, and of trade-offs in the catch and the value of landings “. Documentation on fishing and environmental input parameters, longevity predictions and state/impact predictions is found in WKFBIT report (http://doi.org/10.17895/ices.pub.5955). Some data sources are provided here:

### Fisheries and MSFD habitats
•	MSFD habitat categorization: taken from EMODNET as downloaded in 2019 http://www.emodnet.eu/

•	Bottom trawl fishing activities are obtained from the ICES datacentre using the icesVMS R package; current output is based on ICES VMS 2019 data call  

### Longevity prediction North Sea
•	Parameterization of the biomass longevity composition of the benthic community is taken from Rijnsdorp et al. (2018) Estimating sensitivity of seabed habitats to disturbance by bottom trawling based on the longevity of benthic fauna Ecological Applications 

•	For Kattegat, longevity,slope,intercept were taken from van Denderen et al. 2020 ICES J (see Baltic Sea), which included data in the Kattegat, whereas benthic data from Rijnsdorp et al were mainly from Southern part of the North Sea.

### Longevity prediction Baltic Sea
•	Parameterization of the biomass longevity composition of the benthic community is taken from van Denderen et al. (2020) Evaluating impacts of bottom trawling and hypoxia on benthic communities at the local, habitat, and regional scale using a modelling approach ICES journal of Marine Science

### Impact/state prediction
•	The population dynamic model is first described in Pitcher et al. (2017) Estimating the sustainability of towed fishing‐gear impacts on seabed habitats: a simple quantitative risk assessment method applicable to data‐limited fisheries." Methods in Ecology and Evolution 8.4 (2017): 472-480.

•	The parameterization of the trawl-specific depletion rates is taken from Hiddink et al. (2017) Global analysis of depletion and recovery of seabed biota after bottom trawling disturbance. Proceedings of the National Academy of Sciences 201618858.

•	The parameterization of the longevity-specific recovery rates is taken from Hiddink et al. (2018) Assessing bottom‐trawling impacts based on the longevity of benthic invertebrates Journal of Applied Ecology 


