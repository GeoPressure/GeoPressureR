# altitude with ERA5-Land is deprecated

    Code
      era5_dataset_deprecate_altitude(dataset, TRUE)
    Condition
      Warning:
      Computing `"altitude"` with `era5_dataset = "land"` was deprecated in GeoPressureR 3.7.0.
      i Please use `era5_dataset = "single-levels"` instead.
      * ERA5-Land surface pressure is not hydrostatically consistent with ERA5-Land orography, so the orography term does not cancel from the barometric relation.
      * Against station barometers the mean absolute error is 55 m, against 9 m for 'single-levels', reaching several hundred metres in steep terrain.
      i 'land' and 'both' remain supported for variables other than 'altitude', and for geopressure_map().

