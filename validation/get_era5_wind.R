

# download from
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=form

require(tidync)
require(ncmeta)

url = "c:\\git_checkouts\\sealevelmonitor_nieuwestijl\\data\\copernicus\\era5\\adaptor.mars.internal-1719383727.0796268-6723-3-bd385cf1-ecd9-4668-9858-a62a44a878fd.nc"

con = tidync(url)

tidync::hyper_tibble(con) %>%View()

ncmeta::nc_vars(url)

ncmeta::nc_atts(url)
ncmeta::nc_att(url, "v10", "units") %>% View()





