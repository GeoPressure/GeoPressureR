#!/usr/bin/env python3
"""Compare ERA5 and ERA5-Land hydrostatic altitude estimates.

The two NetCDF files must contain surface pressure, 2 m temperature, and
either geopotential (m2 s-2) or surface elevation (m).  They should be small
extracts downloaded for the same location and UTC time.
"""

import argparse
from pathlib import Path

import numpy as np
import xarray as xr


R_D = 287.05
G = 9.80665
L = 0.0065


def find_variable(dataset, names):
    for name in names:
        if name in dataset:
            return dataset[name]
    raise KeyError(f"None of {names} found in {list(dataset.data_vars)}")


def extract(dataset, latitude, longitude, timestamp):
    pressure = find_variable(dataset, ("sp", "surface_pressure"))
    temperature = find_variable(dataset, ("t2m", "2m_temperature", "temperature_2m"))
    elevation_name = next(
        name for name in ("z", "geopotential", "orography", "altitude") if name in dataset
    )
    elevation = dataset[elevation_name]

    selection = {"latitude": latitude, "longitude": longitude}
    if "lat" in dataset.coords:
        selection = {"lat": latitude, "lon": longitude}
    if "time" in dataset.coords:
        selection["time"] = np.datetime64(timestamp)

    values = xr.Dataset(
        {
            "pressure": pressure,
            "temperature": temperature,
            "elevation": elevation,
        }
    ).sel(selection, method="nearest")

    pressure_pa = float(values.pressure)
    temperature_k = float(values.temperature)
    elevation_value = float(values.elevation)

    # ERA5 geopotential is in m2 s-2; altitude/orography fields may already be m.
    if elevation_name in ("z", "geopotential"):
        elevation_m = elevation_value / G
    else:
        elevation_m = elevation_value

    return {
        "pressure_hpa": pressure_pa / 100,
        "temperature_k": temperature_k,
        "elevation_m": elevation_m,
    }


def estimate_altitude(surface, logger_pressure_hpa):
    pressure_ratio = (logger_pressure_hpa * 100) / (surface["pressure_hpa"] * 100)
    altitude_offset = surface["temperature_k"] / L * (
        pressure_ratio ** (-R_D * L / G) - 1
    )
    return surface["elevation_m"] + altitude_offset


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--era5", type=Path, required=True)
    parser.add_argument("--era5-land", type=Path, required=True)
    parser.add_argument("--latitude", type=float, required=True)
    parser.add_argument("--longitude", type=float, required=True)
    parser.add_argument("--time", required=True, help="UTC time, e.g. 2020-07-15T12:00:00")
    parser.add_argument("--logger-pressure", type=float, required=True, help="hPa")
    args = parser.parse_args()

    with xr.open_dataset(args.era5) as era5, xr.open_dataset(args.era5_land) as era5_land:
        values = {
            "ERA5": extract(era5, args.latitude, args.longitude, args.time),
            "ERA5-Land": extract(era5_land, args.latitude, args.longitude, args.time),
        }

    for name, surface in values.items():
        surface["logger_altitude_m"] = estimate_altitude(surface, args.logger_pressure)

    era5_values = values["ERA5"]
    land_values = values["ERA5-Land"]
    delta_elevation = land_values["elevation_m"] - era5_values["elevation_m"]
    mean_temperature = np.mean([era5_values["temperature_k"], land_values["temperature_k"]])
    expected_pressure_difference = (
        -era5_values["pressure_hpa"] * G * delta_elevation / (R_D * mean_temperature)
    )

    print(f"Location: {args.latitude:.6f}, {args.longitude:.6f}")
    print(f"Time UTC: {args.time}")
    print(f"Logger pressure: {args.logger_pressure:.3f} hPa\n")
    print("dataset       surface_z_m   surface_p_hPa   T2m_K   inferred_z_m")
    for name, surface in values.items():
        print(
            f"{name:10s}  {surface['elevation_m']:12.3f}"
            f"  {surface['pressure_hpa']:14.3f}"
            f"  {surface['temperature_k']:6.3f}"
            f"  {surface['logger_altitude_m']:13.3f}"
        )
    print("\nERA5-Land minus ERA5")
    print(f"surface elevation: {delta_elevation:.3f} m")
    print(f"surface pressure:  {land_values['pressure_hpa'] - era5_values['pressure_hpa']:.3f} hPa")
    print(f"inferred altitude: {land_values['logger_altitude_m'] - era5_values['logger_altitude_m']:.3f} m")
    print(f"hydrostatic pressure estimate for terrain difference: {expected_pressure_difference:.3f} hPa")


if __name__ == "__main__":
    main()
