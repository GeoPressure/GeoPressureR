#!/usr/bin/env python3
"""Download small ERA5/ERA5-Land extracts for the Gibraltar transect."""

from pathlib import Path
from urllib.request import urlretrieve

import cdsapi
import xarray as xr


output_dir = Path("data/era5_gibraltar")
output_dir.mkdir(parents=True, exist_ok=True)

year = "2020"
month = "07"
day = "15"
time = "12:00"

# CDS area order: North, West, South, East.
area = [36.4, -5.8, 35.5, -5.0]

dynamic_request = {
    "product_type": ["reanalysis"],
    "variable": ["2m_temperature", "surface_pressure"],
    "year": [year],
    "month": [month],
    "day": [day],
    "time": [time],
    "area": area,
    "data_format": "netcdf",
    "download_format": "unarchived",
}

era5_file = output_dir / "era5_gibraltar.nc"
land_dynamic_file = output_dir / "era5_land_dynamic_gibraltar.nc"
if not era5_file.exists() or not land_dynamic_file.exists():
    client = cdsapi.Client()
    if not era5_file.exists():
        client.retrieve(
            "reanalysis-era5-single-levels",
            {**dynamic_request, "variable": ["2m_temperature", "surface_pressure", "geopotential"]},
            era5_file,
        )
    if not land_dynamic_file.exists():
        client.retrieve(
            "reanalysis-era5-land",
            dynamic_request,
            land_dynamic_file,
        )

# ERA5-Land geopotential is an invariant auxiliary field, already on the
# 0.1-degree ERA5-Land grid. The URL is the NetCDF4 link in the official
# ERA5-Land documentation.
land_geopotential_global = output_dir / "era5_land_geopotential_global.nc"
if not land_geopotential_global.exists():
    urlretrieve(
        "https://confluence.ecmwf.int/download/attachments/140385202/geo_1279l4_0.1x0.1.grib2_v4_unpack.nc?api=v2&modificationDate=1591983422003&version=1",
        land_geopotential_global,
    )

land_dynamic = xr.open_dataset(land_dynamic_file)
land_geopotential = xr.open_dataset(land_geopotential_global)

# Normalize the CDS hourly coordinate name before merging with the invariant
# field. The invariant file may contain a metadata-only time dimension.
era5 = xr.open_dataset(era5_file)
if "valid_time" in era5.dims:
    era5 = era5.rename({"valid_time": "time"})
    era5.to_netcdf(era5_file.with_suffix(".tmp.nc"))
    era5.close()
    era5_file.with_suffix(".tmp.nc").replace(era5_file)

land_latitude = "latitude" if "latitude" in land_dynamic.coords else "lat"
land_longitude = "longitude" if "longitude" in land_dynamic.coords else "lon"
static_latitude = "latitude" if "latitude" in land_geopotential.coords else "lat"
static_longitude = "longitude" if "longitude" in land_geopotential.coords else "lon"

land_geopotential = land_geopotential.rename(
    {static_latitude: land_latitude, static_longitude: land_longitude}
)
if land_geopotential[land_longitude].max() > 180:
    land_geopotential = land_geopotential.assign_coords(
        {land_longitude: ((land_geopotential[land_longitude] + 180) % 360) - 180}
    )
land_geopotential = land_geopotential.sortby(land_latitude).sortby(land_longitude)

latitude_slice = slice(area[0], area[2])
if land_geopotential[land_latitude].isel({land_latitude: 0}) < land_geopotential[land_latitude].isel({land_latitude: -1}):
    latitude_slice = slice(area[2], area[0])

land_geopotential = land_geopotential.sel(
    {
        land_latitude: latitude_slice,
        land_longitude: slice(area[1], area[3]),
    }
)

# Match the invariant field exactly to the coordinates of the hourly extract.
land_geopotential = land_geopotential.reindex(
    {
        land_latitude: land_dynamic[land_latitude],
        land_longitude: land_dynamic[land_longitude],
    },
    method="nearest",
)
if "time" in land_geopotential.dims:
    land_geopotential = land_geopotential.isel(time=0, drop=True)

land = xr.merge([land_dynamic, land_geopotential], join="exact")
if "valid_time" in land.dims:
    land = land.rename({"valid_time": "time"})
land.to_netcdf(output_dir / "era5_land_gibraltar.nc")

land_dynamic.close()
land_geopotential.close()
land.close()

print(f"Wrote files to {output_dir}")
