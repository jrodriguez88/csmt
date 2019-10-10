# csmt
## Crop Simulation Model Tools
### *Rodriguez-Espinoza J.*
#### International Center for Tropical Agriculture
#### Contact: [Email](mailto:j.r.espinosa@cgiar.org) - [LinkedIn](https://www.linkedin.com/in/jeferson-rodriguez-espinoza-24749625/)
#### Repository: [GitHub](https://github.com/jrodriguez88/csmt/)

Crop Simulation Models Tools (csmt) is an R package of functions for downloading and creating models input files (weather * .WTH, soil * .SOL), estimating parameters (crop, climate and soil), importing and ploting outputs of different crops of crop simulation models (CSM) such as [ORYZA V3.0](https://sites.google.com/a/irri.org/oryza2000/about-oryza-version-3), [DSSAT](https://dssat.net/) and [AquaCrop](http://www.fao.org/aquacrop/).



## make_wth_* 
Function to create weather files for specific model. *(oryza, dssat, aquacrop)

### Usage
```
make_wth_oryza(data, path, local, lat, lon, alt, stn=1)
make_wth_dssat(data, path, local, lat, lon, alt, stn=1)
make_wth_aquacrop(data, path, local, lat, lon, alt, stn=1)
```

### Arguments

* `data:` csv file name or data.frame of daily values. [View Weather Template](https://raw.githubusercontent.com/jrodriguez88/ORYZA_Model_RTOOLS/master/weather_input.csv)

| Var_Name | Description  |  Class| Unit |
| --- | --- | --- | --- | 
| DATE |  Date in "mm/dd/yyyy" | Date | MM/DD/YYYY |
| TMAX |  Maximum temperature | num  | (oC) |
| TMIN |  Minimun temperature | num  | (oC) |
| RAIN |  Rain or Precipitation | num  | (mm) |
| SRAD |  Solar Radiation | num  | (MJ) |
| RHUM |  Relative humidity (opcional) | num  | (%)  |
| WVEL |  Wind Velocity (opcional) | num  | (m/s)  |

* `path:`      path folder or working directory
* `local:`     4 letters string of locality name. "AIHU"--> Aipe, Huila
* `lat:`       latitud (decimal degrees)
* `lon:`       longitud (decimal degrees)
* `alt:`       altitude (meters above sea level)
* `stn:`       Station number. default=1

## make_soil_*
Function to create soil files for specific model. *(oryza, dssat, aquacrop)

### Usage
```
make_soil_oryza(data, path, ZRTMS = 0.50, WL0I = 0, WCLI='FC' , RIWCLI = 'NO', SATAV=20)
make_soil_aquacrop(data, path, ZRTMS = 0.50, WL0I = 0, WCLI='FC' , RIWCLI = 'NO', SATAV=20)
make_soil_oryza(data, path, ZRTMS = 0.50, WL0I = 0, WCLI='FC' , RIWCLI = 'NO', SATAV=20)
```

### Arguments

* `data:` csv file name or data.frame. [View Soil Template](https://raw.githubusercontent.com/jrodriguez88/ORYZA_Model_RTOOLS/master/soil_input.csv)

* `path:`   path folder or working directory
* `ZRTMS:`  Maximum rooting depth in the soil (m), 
* `WL0I:`   Initial ponded water depth at start of simulation (mm)"
* `WCLI:`   can take 3 values: Field Capacity ('FC'), 50% of Soil Saturation ('ST50'), Fraction of water content ('0.0'- '1.0') 
* `RIWCLI:` Re-initialize switch RIWCLI is YES or NO
* `SATAV:`  Soil annual average temperature of the first layers
