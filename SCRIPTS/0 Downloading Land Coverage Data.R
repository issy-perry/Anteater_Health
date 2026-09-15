# 0 Downloading Land Coverage Data
# This script includes the following:
#     1. Download rasters for entire country of Brazil
#     2. Extract extents of telemetry data for cropping the rasters
#     3. Crop rasters
#     4. Load HFI raster

# download RSFs from Mapbiomas
land_cover_2018 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2018.tif")
land_cover_2017 <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2017.tif")


# load tel data
load("./DATA/Wild_raised_tel_data/Data_telemetry.rda") 


# get extents from tel data to use for cropping the rasters
# convert back to a dataframe
DATA_wild <- do.call(rbind.data.frame, DATA_wild)
# convert to a sf object to get extents of tel data
DATA_wild_sf <- st_as_sf(DATA_wild, coords = c("longitude", "latitude"), 
                         crs = crs(land_cover_2017))
# get extent barrier of tel data
ew <- ext(DATA_wild_sf)
# convert to a polygon 
POLY <- as.polygons(DATA_wild_ext, crs = "EPSG:4326") 
# create a 4000 m buffer around the polygon (buffer ensures all points and UD boundaries are within the raster's boundary)
POLY_buff <- buffer(POLY, width = 4000)
# create new extent that includes buffer
ew <- ext(POLY_buff)
# print to check
print(ew)


# crop rasters
cover_2018 <- crop(land_cover_2018, ew)
cover_2017 <- crop(land_cover_2017, ew)


# free environment space
rm(land_cover_2018, land_cover_2017, ew)
gc()


# load HFI raster
HFI <- terra::rast("./DATA/hfp_2021_100m_v1-2_cog.tif")

