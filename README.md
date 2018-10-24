# FBIT
Fisheries Benthic Impact Tools


R-script for a regional assessment of the North and Celtic Sea. Output follows ICES demonstration product (2017) in “EU request on indicators of the pressure and impact of bottom-contacting fishing gear on the seabed, and of trade-offs in the catch and the value of landings “. A (short) documentation on fishing and environmental input parameters, longevity predictions and state/impact predictions is found below.

### Environmental data North Sea/Celtic Sea
•	Seabed depth: taken from BENTHIS, original source DBSEABED – Chris Jenkins http://instaar.colorado.edu/~jenkinsc/dbseabed/

•	Sediment fractions (gravel, sand, mud): taken from Wilson et al. (2018) A synthetic map of the north-west European Shelf sedimentary environment for applications in marine science Earth Syst. Sci. Data, 10, 109-130 https://www.earth-syst-sci-data.net/10/109/2018/

•	Tidal-bed shear stress: estimated using a 2-dimensional hydrographic model. This model predicts shear stress (the force per unit area exerted on the seabed by the tidal currents: N m−2) per sampled station on a 1/8° longitude by 1/12° latitude spatial scale. The shear stress calculations are explained in more detail in Hiddink et al. (2006). Contact person: John Aldridge / Jan Hiddink

•	EUNIS habitat categorization: taken from EMODNET EUSEAMAP as downloaded on 7 March 2018 http://www.emodnet.eu/

•	Bottom trawl fishing disturbance: taken from OSPAR request on the production of spatial data layers of fishing intensity/pressure (2017) http://www.ices.dk/sites/pub/Publication%20Reports/Advice/2017/Special_requests/OSPAR.2017.17.pdf  

### Longevity prediction North Sea/Celtic Sea
•	Parameterization of the biomass longevity composition of the benthic community is taken from Rijnsdorp et al. (2018) Estimating sensitivity of seabed habitats to disturbance by bottom trawling based on the longevity of benthic fauna Ecological Applications 

### Impact/state prediction
•	The population dynamic model is first described in Pitcher et al. (2017) Estimating the sustainability of towed fishing‐gear impacts on seabed habitats: a simple quantitative risk assessment method applicable to data‐limited fisheries." Methods in Ecology and Evolution 8.4 (2017): 472-480.

•	The parameterization of the trawl-specific depletion rates is taken from Hiddink et al. (2017) Global analysis of depletion and recovery of seabed biota after bottom trawling disturbance. Proceedings of the National Academy of Sciences 201618858.

•	The parameterization of the longevity-specific recovery rates is taken from Hiddink et al. (2018) Assessing bottom‐trawling impacts based on the longevity of benthic invertebrates Journal of Applied Ecology 


