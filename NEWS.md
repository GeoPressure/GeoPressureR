# GeoPressureR v3.5.1

## Tests and Coverage

- [Add dedicated tests for tag_assert()](https://github.com/Rafnuss/GeoPressureR/commit/4c771240)
- [Expand tag_label test coverage](https://github.com/Rafnuss/GeoPressureR/commit/f388a3b8)
- [Add magnetic data test coverage](https://github.com/Rafnuss/GeoPressureR/commit/de47d840)
- [Add BAS/light-file test and related tag_create fixes](https://github.com/Rafnuss/GeoPressureR/commit/322db6b4)
- [Exclude _add_wind helpers from coverage](https://github.com/Rafnuss/GeoPressureR/commit/81dce788)
- [Coverage/test config updates and fixes](https://github.com/Rafnuss/GeoPressureR/commit/3c5f6d81), [fix tests](https://github.com/Rafnuss/GeoPressureR/commit/a91fb41b), [misc cleanup](https://github.com/Rafnuss/GeoPressureR/commit/ee680169)
- [Add shiny startup tests](https://github.com/Rafnuss/GeoPressureR/commit/2573e648)

## Tag Creation / Tabular input

- [Replace legacy dataframe/datapackage modes with unified tabular mode](https://github.com/Rafnuss/GeoPressureR/commit/7f41a71d)
- [Remove obsolete tag_create_datapackage.R](https://github.com/Rafnuss/GeoPressureR/commit/fc0d1836)
- [Validate crop range (crop_start < crop_end) and error when crop removes all data](https://github.com/Rafnuss/GeoPressureR/commit/5ebabc49)
- [Fix temperature variable naming issue in tag creation flow](https://github.com/Rafnuss/GeoPressureR/commit/c4621c97)

## Minor

- Labeling / Quiet behavior: [Fix missing quiet checks in labeling paths](https://github.com/Rafnuss/GeoPressureR/commit/9ca92c81)
- [Remove experimental lifecycle badge from trainset](https://github.com/Rafnuss/GeoPressureR/commit/55a3aaab)

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.5.0...v3.5.1>

# GeoPressureR v3.5.0

## New functions related to light

- [Add tag_stap_daily() to build stap from twilight midpoints](https://github.com/Rafnuss/GeoPressureR/commit/3b6fec5f) and [add read_stap() to validate/read stap files](https://github.com/Rafnuss/GeoPressureR/commit/7c1cecff)
- [Add map_add_mask_water() for water masking](https://github.com/Rafnuss/GeoPressureR/commit/db9fb2ba) and [mask water pixels in plot.map() filtering](https://github.com/Rafnuss/GeoPressureR/commit/af1727c4)
- [Create geolight_fit_location()](https://github.com/Rafnuss/GeoPressureR/commit/3f81ae68), [refactor geolight mapping pipeline](https://github.com/Rafnuss/GeoPressureR/commit/9478b430), and [modularize geolight map flow](https://github.com/Rafnuss/GeoPressureR/commit/58cae1de)
- [Create twilight calibration plotting (plot_twl_calib)](https://github.com/Rafnuss/GeoPressureR/commit/9df93da4)

## Major Update

- [Add era5_dataset parameter to geopressure map functions](https://github.com/Rafnuss/GeoPressureR/commit/8d028bc5)
- [Fix missing variable parameter to graph_add_wind()](https://github.com/Rafnuss/GeoPressureR/commit/36d4da14)
- [Remove geosphere dependency and refactor distance/bearing computation](https://github.com/Rafnuss/GeoPressureR/commit/d6c47fa1)
- [Refactor geotiff downloads with future.apply + progressr](https://github.com/Rafnuss/GeoPressureR/commit/d610f3ac)

## Trainset

- [Major update of Trainset app](https://github.com/Rafnuss/GeoPressureR/commit/f79fb60a)
- [Add experimental lifecycle badge to trainset](https://github.com/Rafnuss/GeoPressureR/commit/b11a1ab6)
- [Handle acceleration-only input (no pressure)](https://github.com/Rafnuss/GeoPressureR/commit/78b397c5)
- [Add modal checks/UI fixes](https://github.com/Rafnuss/GeoPressureR/commit/4833f8c5), [missing UI fix](https://github.com/Rafnuss/GeoPressureR/commit/2343a990), and [auto label_dir / auto-labeling fixes](https://github.com/Rafnuss/GeoPressureR/commit/4a95e313)

## GeoLightViz

- [Add GeoLightViz app + modules](https://github.com/Rafnuss/GeoPressureR/commit/ed7e3813)
- [Improve Shiny process handling so apps can co-exist](https://github.com/Rafnuss/GeoPressureR/commit/5b94a4de) and [refactor app launcher utilities](https://github.com/Rafnuss/GeoPressureR/commit/7e80b10a)

## GeoPressureViz

- [Refactor GeoPressureViz map handling](https://github.com/Rafnuss/GeoPressureR/commit/c730a4a9) and [map type/palette handling](https://github.com/Rafnuss/GeoPressureR/commit/7cbbf5a5)
- [Refactor async pressure query logic](https://github.com/Rafnuss/GeoPressureR/commit/a1d23c74)
- [Fix twilight grouping by stap in aggregation](https://github.com/Rafnuss/GeoPressureR/commit/7882a4e7) and [fix zoom relayout triggering](https://github.com/Rafnuss/GeoPressureR/commit/4714c66b)

## Minor fixes

- [Make stap filtering robust to missing columns](https://github.com/Rafnuss/GeoPressureR/commit/f94bd5b8)
- [Fix missing known lat/lon edge case](https://github.com/Rafnuss/GeoPressureR/commit/0b0b2daa)
- [Restrict merged path columns in pressurepath creation](https://github.com/Rafnuss/GeoPressureR/commit/e8bd622f)
- [Move ecmwfr and ncdf4 to Suggests with runtime checks](https://github.com/Rafnuss/GeoPressureR/commit/168aab34)
- [Use vapply and memory-focused cleanups](https://github.com/Rafnuss/GeoPressureR/commit/13e346db), [opt pass 1](https://github.com/Rafnuss/GeoPressureR/commit/fe9b4a71), [opt pass 2](https://github.com/Rafnuss/GeoPressureR/commit/6033a1cd)
- [Refine messaging / remove multiline cli text](https://github.com/Rafnuss/GeoPressureR/commit/97e83947)
- [Switch formatting/lint workflow to air+jarl (follow-up fixes)](https://github.com/Rafnuss/GeoPressureR/commit/eb36dcd0)
- [Refactor plot.tag() dispatch/modularity](https://github.com/Rafnuss/GeoPressureR/commit/9ddcd0f3)
- [Refactor plot.map() args/path plotting](https://github.com/Rafnuss/GeoPressureR/commit/170dc05c)
- [Refactor plot_path() customization/static mode](https://github.com/Rafnuss/GeoPressureR/commit/591460b9)
- [Improve plot_graph_movement() options and annotations](https://github.com/Rafnuss/GeoPressureR/commit/02f5927c) / [visual polish](https://github.com/Rafnuss/GeoPressureR/commit/af38b4f5)

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.4.5...v3.5.0>

# GeoPressureR v3.4.5

## Main

- [Add ability to not read a sensor file with `file= ""`](https://github.com/Rafnuss/GeoPressureR/commit/4fb57dd3cb88841d343a964204e8887a153deae5)
- [Improve the computation of the decimal of stap_id during flight in `find_stap()`](https://github.com/Rafnuss/GeoPressureR/commit/2316008611d88649f0febff94be8b3a4e2e8bacb)
- [Remove bearing value of 0 when no movement](https://github.com/Rafnuss/GeoPressureR/commit/2ac00c9726090e0658a7fef3d2cee114072ffab7) and [Normalize bearing to 0-360 degrees in path2edge](https://github.com/Rafnuss/GeoPressureR/commit/cd479c6aa2813b83a8b1f041548c51d97c5a4fb4)
- [Add twl_time_tolerance parameter to time series functions](https://github.com/Rafnuss/GeoPressureR/commit/c732716eef31f20c8fe10f25ee1b345d4c883589)
- Improving tag_create: [fix issue with multiple col](https://github.com/Rafnuss/GeoPressureR/commit/a1db215bef3c07daded3a165321bbc43e2ca3b10) and [Simplify the approach avoiding memory error](https://github.com/Rafnuss/GeoPressureR/commit/48df064e637b47cb091ecc8c17745da45f122b82)
- Switch from lintr+styler to [air](https://github.com/posit-dev/air)+[jarl](https://jarl.etiennebacher.com/) [link](https://github.com/Rafnuss/GeoPressureR/pull/143/commits/27025c6b79d7f4d606f03392ecc771f18459f853)

## Minor fixes

- [Remove unused 'directory' argument from tag_create_dataframe](https://github.com/Rafnuss/GeoPressureR/commit/1759d08a0dbccb0d4f91c5d6d1738e073e1a7ae3)
- [Refactor trainset_read to use trainset_read_raw helper](https://github.com/Rafnuss/GeoPressureR/commit/4cfe53a04a8d6bbac9170d9e6c1a76313de5b944)
- Remove `timeout` in pressurepath_create, geopressure_timeseries() and path2elevation()
- [Add fix for migratech "invalid" keyword in file](https://github.com/Rafnuss/GeoPressureR/commit/ca9e2195e9b9a7f764b7d1201d106188c153cd31)
- [Fix issue when migratech deg file has no pressure](https://github.com/Rafnuss/GeoPressureR/commit/ef7f1941f4c3011cdff6dd66ffdd5ca51fd99c06)
- [Refine twilight threshold calculation logic](https://github.com/Rafnuss/GeoPressureR/commit/6fdaddf387a05362ba85a57f5ffb0f7d872629b5)
- [Swap point colors in plot_tag_twilight function](https://github.com/Rafnuss/GeoPressureR/commit/5d0ca440486610e3ef855ecbf7df7fe1d938b9b9)
- [Refactor test setup to use temporary extdata directory](https://github.com/Rafnuss/GeoPressureR/pull/146/commits/4c111897ee0dd280bbf7011a36b06e0943ccfb4b)
- Improve messaging [1](https://github.com/Rafnuss/GeoPressureR/commit/b2ae8221db5ad91acdd3dcfe97e6553abe0ed28d) [2](https://github.com/Rafnuss/GeoPressureR/commit/7aaf5132554477897bc4e642a9f0cb48b53bcacd), [3](https://github.com/Rafnuss/GeoPressureR/commit/458a128700c15962882bde4ca6458d0f536b1fc2)

## GeoPressureViz

- [refactor prev/next flight message](https://github.com/Rafnuss/GeoPressureR/commit/8b9b190359fafeb7ee5641053912ac4224f0b807)
- [Make geopressureviz as background process](https://github.com/Rafnuss/GeoPressureR/commit/1d6866d2f09fbdb9b6f3fe7783309367872a6a2b)
- [Refactor to use shiny options instead of global varia…](https://github.com/Rafnuss/GeoPressureR/commit/1075d7ed19e6ea6fae2410ffc231944faea86060)
- [refine plotly icon](https://github.com/Rafnuss/GeoPressureR/commit/581efb77f7df30440a2646fd4d72e13f5e8d35e9)
- [GeoPressureViz--Add async support for pressure time series queries](https://github.com/Rafnuss/GeoPressureR/commit/880184c0aff46b5d52c089c7ed2ad59a1d4bff29)
- [Fix layer order using mapPane](https://github.com/Rafnuss/GeoPressureR/commit/7edd101c37839f358252f71a642dcf5ca672b567)

# GeoPressureR v3.4.4

## What's Changed

- Refactored wind `file` path argument to accept both `stap_id` and `tag_id`.[[1]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-de80963ddc9729133208365a472110b4b22a64c70e7c8a8f0cfacbb4937cda7cL62-R71) [[2]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-de80963ddc9729133208365a472110b4b22a64c70e7c8a8f0cfacbb4937cda7cL174-R176) [[3]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-de80963ddc9729133208365a472110b4b22a64c70e7c8a8f0cfacbb4937cda7cL186-R188) [[4]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-de80963ddc9729133208365a472110b4b22a64c70e7c8a8f0cfacbb4937cda7cL450-R458) [[5]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-de80963ddc9729133208365a472110b4b22a64c70e7c8a8f0cfacbb4937cda7cL479-R486) [[6]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-de80963ddc9729133208365a472110b4b22a64c70e7c8a8f0cfacbb4937cda7cL503-R516) [[7]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-de80963ddc9729133208365a472110b4b22a64c70e7c8a8f0cfacbb4937cda7cL524-R530) [[8]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-de80963ddc9729133208365a472110b4b22a64c70e7c8a8f0cfacbb4937cda7cL537-R543)
- Enhanced the `print.graph` and `print.tag` [[1]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-794439155a0f41c1cc5aaa40853b59f4703ea99993c7cd6514fbd150227c5515L33-R45) [[2]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-794439155a0f41c1cc5aaa40853b59f4703ea99993c7cd6514fbd150227c5515L84-R81) [[3]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-b11208ddd2d8b01010b8f8f46684271153b2f9228c76d5da35450c5065509138L22-R31)
Improved CLI section headers to include the relevant id in several workflow functions, providing better context for the user. [[1]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-28fb565b269914409f8b993d7d6f06a698135ce43833eea427c0d38a1a139315L35-R35) [[2]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-6003298481533c53dd0e551d42f705cd451013ed30797168d7af73118e31431eL18-R18) [[3]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-ab4f0442eb06bea9f3fa4ee6afc18ded3b9c921f28be69b2a3d80d048b865aadL27-L35)
- Added an `assert_tag` parameter to `geopressuretemplate_config` [[1]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-1bfd397fb34e77518ba47485b9d410dd7b36c6b7868fa861150a884f3b56c436R6) [[2]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-1bfd397fb34e77518ba47485b9d410dd7b36c6b7868fa861150a884f3b56c436R21-R29) [[3]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-cf4e6dd771977c8c5dac1a96439ed7322f9e391fe8527a1fac220531239094b6R70-R71) [[4]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-ab4f0442eb06bea9f3fa4ee6afc18ded3b9c921f28be69b2a3d80d048b865aadL9-R11)
Added an assertion for scientific_name type early in bird_create to ensure input validity, and removed a redundant assertion later in the function. [[1]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-b47dd25dfce3df695d2734f1fb01822f2404070612b9f69aeb7c4c935c6b3016R46) [[2]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-b47dd25dfce3df695d2734f1fb01822f2404070612b9f69aeb7c4c935c6b3016L106)

- Updated progress messages and clarified output in several places. [[1]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-36c0f44c40be2ba090e398b65e14e027182593b255d32813c8f8e943227bb61aL132-R132) [[2]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-747951350c4b8dce58eb055b7309f61d718eef2acf1490fd50516f91c679a30fL156-R159)
Improved handling of return values in error cases for geopressuretemplate_graph. [[1]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-28fb565b269914409f8b993d7d6f06a698135ce43833eea427c0d38a1a139315L85-R86) [[2]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-28fb565b269914409f8b993d7d6f06a698135ce43833eea427c0d38a1a139315L137-L147)
Cleaned up and clarified documentation and comments, including removal of outdated references and minor formatting fixes. [[1]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-cf4e6dd771977c8c5dac1a96439ed7322f9e391fe8527a1fac220531239094b6L49-L50) [[2]](https://github.com/Rafnuss/GeoPressureR/pull/141/files#diff-de80963ddc9729133208365a472110b4b22a64c70e7c8a8f0cfacbb4937cda7cL29-R30)
- Fixed a logical error in plot_path_leaflet when checking the interp field.
- Ensured likelihood maps are retrieved and displayed more clearly in graph_create.
- Added a Year: 2022 field to the DESCRIPTION file.

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.4.3...v3.4.4>

# GeoPressureR v3.4.3

## Moderate

### Memory Optimization on Graph Creation

- Replaced the use of `geosphere` functions with custom memory-efficient implementations for distance and bearing calculations.
- Improved graph creation by introducing distance filtering before distance calculation and adding progress messages for better user feedback.
- Added cleanup steps (`rm` and `gc`) in the `edge_add_wind` function to free memory by removing unused variables and closing netCDF files.

## Minor

- [Use an interp=1 by default in geopressureviz](https://github.com/Rafnuss/GeoPressureR/commit/670096e5ec3b332b3d7152f25f5476ba5a2cfbb2)
- [Temporarily set default era5_dataset to land](https://github.com/Rafnuss/GeoPressureR/commit/d2aaa894dcf486f94e10996f7655e1f569db9425)
- Removed redundant retry logic in `httr2` requests across multiple functions (`geopressure_map_mismatch`, `geopressure_timeseries`) to simplify the code ([f227a90](https://github.com/Rafnuss/GeoPressureR/commit/f227a90e7ba19a09d7603e85a40ad864ab005b7e)).

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.4.2...v3.4.3>

# GeoPressureR v3.4.2

## Moderate

- [Normalization of forward/backward mapping in model to prevent underflow.](https://github.com/Rafnuss/GeoPressureR/commit/af53338f795345f67430357496e644ca860fa3ea)
- [Add actogram plot](https://github.com/Rafnuss/GeoPressureR/commit/478b1fdfe194aa6afe7f554f3e819052db5371ab)

## Minor

- [Fix of geopressuretemplate_pressurepath](https://github.com/Rafnuss/GeoPressureR/commit/4f9e17f538d430b9be3b60c44486c444dc7a76e5)

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.4.1...v3.4.2>

# GeoPressureR v3.4.1

## Moderate

- [Fixing error which inverted the variable accelerations and magnetic in SOI sensor](https://github.com/Rafnuss/GeoPressureR/commit/b1099c961ec4190647d8ba8a344f1b2ac7cc732f)

## Minor

- [Refactoring of light for future development of geopressureviz()](https://github.com/Rafnuss/GeoPressureR/commit/e1de16fa85c071572fdcdb1782daa773c275263d)
- [Add path_geopressureviz from csv to interim folder](https://github.com/Rafnuss/GeoPressureR/commit/e6ddf1a863a6a7a488f750933da93af272b57894)
- [improvement in the computation efficiency of light2mat](https://github.com/Rafnuss/GeoPressureR/commit/2ef158da9fecc7f03075132f1171fc72fa4b248c)
- [split tag_create by manufacturer](https://github.com/Rafnuss/GeoPressureR/commit/081d60b310596e81741d391a8982ab69c6730b61)

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.4.0...v3.4.1>

# GeoPressureR v3.4.0

## Moderate

- [Fix major issue of most_likely_path with computation of probability r…](https://github.com/Rafnuss/GeoPressureR/pull/131/commits/ad486b13b7f4a76f45355a6ee04b13f3ea457d32)

## Minor

- [Update path$ind in geopressureviz as path changes](https://github.com/Rafnuss/GeoPressureR/pull/131/commits/da566ae7d9da6f252d5b4b9b67c61d6a1d33acb0)
- [Add pruning of the graph after filtering graph by airspeed.](https://github.com/Rafnuss/GeoPressureR/pull/131/commits/13b27ae7119f0827c8a7eff436b29430089cc2b6)

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.3.4...v3.4.0>

# GeoPressureR v3.3.4

## Moderate

- [Make `geolight_map()` work for non-regular light data](https://github.com/Rafnuss/GeoPressureR/pull/130/commits/54d24b192252e759875ce0d608a9df25b7c0af9c)

## Minor

- [Add reading of BAS tags](https://github.com/Rafnuss/GeoPressureR/pull/130/commits/b019b255707c0351440e8aab7aa2d196611121ea) and [PRESTAG tag](https://github.com/Rafnuss/GeoPressureR/pull/130/commits/6dd337840b727749cd47984a17e56ec52e8987aa)
- [deprecate cds api token as parameter](https://github.com/Rafnuss/GeoPressureR/pull/130/commits/6404dfe2f54169b583283b060be43c3b58955940)
- [deprecate *_update()](https://github.com/Rafnuss/GeoPressureR/pull/130/commits/e61b4b6c5c1c9e709d6e349c1b81a5019a375a99)
- [Increase timeout for downloading wind](https://github.com/Rafnuss/GeoPressureR/pull/130/commits/85ed9d5c0498b83cc8d39f6af0083d292cb7a778)

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.3.3...v3.3.4>

# GeoPressureR v3.3.3

## Minor

- Minor fixes of plot_path
- Minor fix on plot_twilight
- Minor edits on docs and website

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.3.2...v3.3.3>

# GeoPressureR v3.3.2

## Major update

- Change the computation of distance of edges in the graph by removing the fix that added 1 resolution to the distance to account for large grid square and short flight distance. Instead, add warning message in case there might be such an issue (flight distance < grid resolution) [ddbd07d](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/ddbd07db195440005f7f1ff96dd134dd43bdd8ae).
- Add `zero_speed_threshold` parameter that allow to encourage bird to stay at the same location. This is typically the case for short flight that don't seems to affect the position. Quite similar to a `stap_elev` [2060790](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/2060790f8508c447a00877c2fc1d86c9d52bfe2e)
- Add other type of pressurepath in interim [8d7f1d1](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/8d7f1d12665608d37f5eafa53fb251554e7f5cdd)

## Minor

- Plenty of small fixes and minor improvements [834155e](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/834155eeffcf04b33effce8d9b9d193c5554cbdf), [152c6e7](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/152c6e7a2091970b444a47a3ceb0a367a42a9c2a), [54aa995](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/54aa99589f8e709089a08ae02c5aace821ab7cea),  [3a5ff95](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/3a5ff95774b353c853a32ed63327dad8df82fbbe), [6eedd83](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/6eedd83802f9377db43cba06c2e4b684b2154452), [8797533](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/8797533b49c9cc76a3d81a90824a8ceaabd48692), [1e68260](https://github.com/Rafnuss/GeoPressureR/pull/126/commits/1e68260c39bc49816c95095d0f3c3541986a50df)

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.3.1...v3.3.2>

# GeoPressureR v3.3.1

## Major update

- [Update of param structure to `function_name$param_name`](https://github.com/Rafnuss/GeoPressureR/commit/2235e9ec0bceef3b49d8e1887a309b2048353552). The structure of the param has been reorganised: this named list stored inside of tag and graph stores parameters used during the building of tag and graph. We standardized this structure as param${function_name}${argument_name}. [See the migration instructions in the GeoPressureR wiki](https://github.com/Rafnuss/GeoPressureR/wiki/Migration-v3.x-%E2%80%90--v3.3). This will mean you'll need to update your config.yml structure - sorry for that.
- [Add geopressuretemplate() functions](https://github.com/Rafnuss/GeoPressureR/commit/b000764ff3c2179eefb48bbf9178c5323c89aa7d). The main improvement is related to the use of a single function to run the entire workflow: geopressuretemplate. Read more about this in the [corresponding chapter of the GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html).

## Minor

- [replace species_name by scientific_name](https://github.com/Rafnuss/GeoPressureR/commit/5e1b15fd025f355107a90229779969ad2030d7c7)
- [crop date UTC](https://github.com/Rafnuss/GeoPressureR/commit/0707ae6041383bff704a4c78511ab2dbd16305ce)
- [fix new variable name in netcdf](https://github.com/Rafnuss/GeoPressureR/commit/6c12362128c569c05ae49bb0ed40b8a31adc5980)

## New Contributors

- @PabloCapilla made their first contribution in [#125](https://github.com/Rafnuss/GeoPressureR/pull/125)

**Full Changelog**: <https://github.com/Rafnuss/GeoPressureR/compare/v3.3.0...v3.3.1>

# GeoPressureR v3.3

## Major

- Read all sensors type and allow reading sensor without pressure `assert_pressure = FALSE` ([d11f8cc](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/d11f8cc4774b4c91c27318c43d438823b681066e), [85ffe94](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/85ffe940ec46af8cd56592c2d641f25d19712129))
- [Update to ecmwfr v2. Change to cds_token](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/4253e04f9ed16e3b06d45edfda0b2a0900d31d0c). We use the Climate Data Store to download the wind data during the flight. They have recently [updated their infrastructure](https://confluence.ecmwf.int/display/CKB/Please+read%3A+CDS+and+ADS+migrating+to+new+infrastructure%3A+Common+Data+Store+%28CDS%29+Engine) and their login procedure has changed. You’ll need an ECMWF login with an Access Token. See updated procedure in the chapter [Trajectory with wind of the GeoPressureManual](https://raphaelnussbaumer.com/GeoPressureManual/trajectory-with-wind.html#download-wind-data).
- [Improvement of tag_label_auto() with post-processing step](https://github.com/Rafnuss/GeoPressureR/commit/69c26adf559fc1bc7c2690346de41f6732f9eda5). Based on a simple classification of prolonged high activity, migratory flight classification was often not very performant, e.g. when a bird was gliding during the flight. I have now added a post-processing step in the automatic classification to fix this. Read more in [the detail section of tag_label_auto().](https://raphaelnussbaumer.com/GeoPressureR/reference/tag_label_auto.html#details).
- [Create path2twilight.R](https://github.com/Rafnuss/GeoPressureR/commit/eff97315c5eca3dff03736e7d40efad30b209819) and [Add twilight_line in plot_twilight](https://github.com/Rafnuss/GeoPressureR/commit/b21aa06ceff257ec4473a3af530a8ba7cef5e225). You can now compute the theoretical twilight of a path, or more interestingly, of a pressurepath. It's also used in [pressurepath_create()](https://raphaelnussbaumer.com/GeoPressureR/reference/pressurepath_create.html), returning a column with sunrise and sunset. Its original purpose was to be able to check the twilight labeling by comparing it to a path generated, e.g., with GeoPressureViz. See the [last section of the light map chapter](https://raphaelnussbaumer.com/GeoPressureManual/light-map.html#check-light-label) for more info.

## Minor

- [Fix issue with tag_plot_twilight() when twilight was not yet computed](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/6de3af2a97e27763c5a70a0e570c91921b699f01)
- [Update documentation of windsupport/drift](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/93ec8a579e12fb39fe034d108f8afa89150e700d)
- [Make twilight works with NA in light](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/3a704fd054127cbc72e303091a4755bcbd31eaf0)
- [Add type to path as attribute](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/e900e518d39a0e1b44a98f98b81aa1ffef17c760)
- [Accept known as list and convert it if so](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/22307d41014da8c2ddfbd10861078f89b7426451)
- [Fix bug in compute_known in geolight_map()](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/e4b02796a8932f014aaa0fc212631d1dd24c8a52)
- Improve progress_bar, remove extra `\f`, improve `print`
- [Change default map height](https://github.com/Rafnuss/GeoPressureR/pull/123/commits/7fc311be6cacafef9c3954700f1deb68f97c77b2)
- [fix plot_twilight() for twl_offset](https://github.com/Rafnuss/GeoPressureR/commit/2734537e7997555200f05eb30b451520e1c1cfb7)
- [update all actions](https://github.com/Rafnuss/GeoPressureR/commit/076b3568d191e60d96a3a4703b7709a947dae66c)

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v3.2.0...v3.3.0](https://github.com/Rafnuss/GeoPressureR/compare/v3.2.0...v3.3.0)

# GeoPressureR v3.2

## Major

- Use the new GeoPressureAPI pressurepath entry point for `geopressurepath_create()`
- Update to GeoPressureAPI v2 for `geopressure_timeseries()` <https://github.com/Rafnuss/GeoPressureR/pull/103/commits/732d1a02cc241dc7d2dde3401c8747fa860650c6>
- Fix major bug <https://github.com/Rafnuss/GeoPressureR/pull/103/commits/05c3203ef1588bbc1f769050377cadf5f1aadcbd>
- Migrate from `httr`to `httr2`
- Fix major bug with saving environment variable in param [9bcbf790](https://github.com/Rafnuss/GeoPressureR/commit/9bcbf790a7de133448c738db272bf136dc831f8f)
- Add functions `speed2bearing()` and `windsupport()` [fe244f60](https://github.com/Rafnuss/GeoPressureR/commit/fe244f6057db14b6a286c0a77aaaef4c5ec0152c)
- Use interpolated `stap_id` for flight instead of `0` [d7491c2a](https://github.com/Rafnuss/GeoPressureR/commit/d7491c2a5580c9eeafe598935933727489590a75)
- Create `edge_add_wind()` [36412dc8](https://github.com/Rafnuss/GeoPressureR/commit/36412dc8f0fd061798a701ca632766dbe6f069c8)
- Create `path2elevation()` using GeoPressureAPI to compute ground elevation from a path

## Minor

- Add `workers` argument in `graph_create()` [e1ce4588](https://github.com/Rafnuss/GeoPressureR/commit/e1ce45882809e1fd3da0e8feb2ff80ac70f2bf8b)
- Add `codemeta.json` <https://github.com/Rafnuss/GeoPressureR/pull/105/commits/4f7f7bce8875b4af59db3fc1ce403d41d6317469>
- Add project status badge <https://github.com/Rafnuss/GeoPressureR/pull/105/commits/ecd8f61ec49dcd376748e54d19dfb2000675d302>
- Fix leaflet tile provider with Stadia change <https://github.com/Rafnuss/GeoPressureR/pull/106/commits/8d9bd159874deb87d61907ea14911eca12877038>
- Add `WORDLIST` for `spelling` package.
- Remove the use of `ind` in path [f7b38e1c](https://github.com/Rafnuss/GeoPressureR/commit/f7b38e1c1b06666f590df394395d7db387f2565a)
- Read temperature sensor [17524658](https://github.com/Rafnuss/GeoPressureR/commit/17524658a5f49466a211ea5bfbfc34c523b09a47)
- Only download wind data for non-existing file by default (instead of all flights) [b6a2c414](https://github.com/Rafnuss/GeoPressureR/commit/b6a2c41420bef2354a9ace640adffcb4e79e1aa1)
- Remove `pressurepath2altitude()` now computed in `pressurepath_create()`

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v3.1.0...v3.2.0](https://github.com/Rafnuss/GeoPressureR/compare/v3.1.0...v3.2.0)

# GeoPressureR v3.1

## Major

- Update to GeoPressureAPI v2, using `thr_mask` in `geopressure_map_mismatch()` and splitting `keep_mse_mask`.
- Adjust computation of ground speed to account for grid resolution.

## Minor

- Use negative indexing for `known`
- remove trailing `/` to default directories.
- documentations and minor fixes.

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v3.0.0...v3.1.0](https://github.com/Rafnuss/GeoPressureR/compare/v3.0.0...v3.1.0)

# GeoPressureR v3.0

## Guiding principles of v3

This new version consists of a significant revamp of the entire code centred around these themes:

- Name more general than SOI sensors (e.g., use `tag` instead of `pam`)
- Focus the workflow on pressure sensor (but still allows for acceleration or light data)
- Update the notion of graph into State-Space Model notations (e.g. probability -> likelihood)
- More memory efficient (store minimum graph info) while minimizing computational expense of the "slow" functions
- Shorter workflow [#69](https://github.com/Rafnuss/GeoPressureR/issues/69)
- Ease of labelling [#67](https://github.com/Rafnuss/GeoPressureR/issues/67)
- Reproducibility and long-term storage with `param`.
- Use of S3 class object with print and plot generic function.
- Compatible with pipe `|>` or `%>%`
- Use of [cli](https://cli.r-lib.org/index.html) for message and progress bar.
- Be able to update `tag` and `pressurepath` without re-computing everything.
- See [#55](https://github.com/Rafnuss/GeoPressureR/issues/55) for details on the functions named change
- See the [migration wiki](https://github.com/Rafnuss/GeoPressureR/wiki/Migration-v2-%E2%80%90--v3) for a small guide to transition from v2.

## Major

- Use of GeoPressureR object: `tag`, `graph`, `param`, `bird`
- Many new plot functions including update of `geopressureviz()`
- Transition from `raster` to `terra` [#59](https://github.com/Rafnuss/GeoPressureR/issues/59)
- New label scheme with test and messaging for troubleshooting [#67](https://github.com/Rafnuss/GeoPressureR/issues/67) [#73](https://github.com/Rafnuss/GeoPressureR/issues/73) [#83](https://github.com/Rafnuss/GeoPressureR/issues/83)
- Create `tag_update()` and `pressurepath_update()`
- Review the structure of a path and edges.

## Minor

- Formulate graph as a HMM [#68](https://github.com/Rafnuss/GeoPressureR/issues/68)
- Simplified workflow [#69](https://github.com/Rafnuss/GeoPressureR/issues/69)
- Use of `cli` for message.
- Create `graph_shortestpath` [b69c2a21](https://github.com/Rafnuss/GeoPressureR/commit/b69c2a21b784f598b03822e940c02c216114e9f9)
- Review all tests and example
- Review all functions names and parameters

# GeoPressureR v2.7-beta

## Major

- Major fix in the computation of the marginal map [bd1103fd](https://github.com/Rafnuss/GeoPressureR/commit/bd1103fda0c5b4e3c0f218ee7bcf3fbc69dc6123)

## Minor

- Improve `graph_download_wind()` [#54](https://github.com/Rafnuss/GeoPressureR/issues/54)
- GeoPressureViz function in [#52](https://github.com/Rafnuss/GeoPressureR/issues/52)
- Replace `isoutliar` with `isoutlier` in [#43](https://github.com/Rafnuss/GeoPressureR/issues/43)
- Use `assertthat` in [#46](https://github.com/Rafnuss/GeoPressureR/issues/46) and [#47](https://github.com/Rafnuss/GeoPressureR/issues/47)
- Typo of equipment and retrieval in [#48](https://github.com/Rafnuss/GeoPressureR/issues/48)
- Various minor fixes

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v2.6-beta...v2.7-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.6-beta...v2.7-beta)

# GeoPressureR v2.6-beta

## Major

- add windspeed download function `graph_download_wind()`

## Minor

- fixes for reading pam data
- various fixes (see [#42](https://github.com/Rafnuss/GeoPressureR/issues/42))

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v2.5-beta...v2.6-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.5-beta...v2.6-beta)

# GeoPressureR v2.5-beta

## Major

- Migration of all the vignette and data used for the vignette in GeoPressureManual [bda0f789](https://github.com/Rafnuss/GeoPressureR/commit/bda0f7898dd9e6b8d9d786ce56ae3e5ec422c935)
- Read Migrate Technology data (should not be breaking change, but some significant changes) [#23](https://github.com/Rafnuss/GeoPressureR/issues/23)
- Add `logis` function in `flight_prob()` [6e1a8f0e](https://github.com/Rafnuss/GeoPressureR/commit/6e1a8f0e93d82ec2a9bccce404cdb59fcc218277)

## Minor

- Read Avonet data as package data [c5c8d807](https://github.com/Rafnuss/GeoPressureR/commit/c5c8d807f9a7e13a49e3d1565a7b3beffb58022f)
- Update of `r-lib/actions` to v2 [3382fb9b](https://github.com/Rafnuss/GeoPressureR/commit/3382fb9b7b9970f1c102cf9aabf3a6b06b5d505e)
- [8720b6e6](https://github.com/Rafnuss/GeoPressureR/commit/8720b6e6032f910f0c702e649a907dcf10bc2258)
- Improvement of GeoPressureViz [97be49de](https://github.com/Rafnuss/GeoPressureR/commit/97be49de4ed6c309b16e23fbedde1d618ae0a04c) [964b5589](https://github.com/Rafnuss/GeoPressureR/commit/964b558913de7f7b6ef9915fc9cc41fc0b3dd0d3)
- Add checks and warning in functions
- Preparation of the code for CRAN

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v2.4-beta...v2.5-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.4-beta...v2.5-beta)

# GeoPressureR v2.4-beta

## Major

- Accept request over water and display warning message. See [#15](https://github.com/Rafnuss/GeoPressureR/issues/15)
- Add logging of error and return JSON file of the request in case of error for troubleshooting
- Change downloading and reading of geotiff file to work on windows. See [#16](https://github.com/Rafnuss/GeoPressureR/issues/16)
- Remove the artificial increase of flight duration at the creation of graph [696566e8](https://github.com/Rafnuss/GeoPressureR/commit/696566e8041e90d04e3e01d7d84ef299660bab6e)
- Compute groundspeed in parallel in graph creation [b1466c73](https://github.com/Rafnuss/GeoPressureR/commit/b1466c737a66c740e2f6a35bcdbc19d9f5aebfd1)

## Minor

- minor fixes for `sta_id=0` or `NA`
- minor fixes in `geopressureviz()`
- add dummy graph test to improve coverage.
- compute windspeed for short flight happening during the same hour
- typos, code readability and `stlyer`

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v2.3-beta...v2.4-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.3-beta...v2.4-beta)

# GeoPressureR v2.3-beta

## Major

- [Major fix of wind computation bearing to angle and m/s -> km/h](https://github.com/Rafnuss/GeoPressureR/commit/0eee443944e0b7ecf86c64901b45cd0f659d3d19)
- Major fix of twilight uncertainty using kernel density. The gamma fitting was very wrong [5acfb136](https://github.com/Rafnuss/GeoPressureR/commit/5acfb136b8cac49d3cfd9633ce9a0a81ccc9b252)
- Major update in the data location to avoid being loaded when using the package. Move all data to `inst/extdata` to avoid having them loaded with [65c8f806](https://github.com/Rafnuss/GeoPressureR/commit/65c8f8062cf07fb1471c9f15f6f08757d00951df)
- Add more information on various dataset to be able to load in GeoPressureViz
- Change to the graph [4aeed9ab](https://github.com/Rafnuss/GeoPressureR/commit/4aeed9ab77c8efe15b2da591247700d0ebb0cb5f)

## Minor

- Multiple test file and add `covr`
- [Optimize `sta_pam()`](https://github.com/Rafnuss/GeoPressureR/commit/eb398697ce600d229f14c50141808ab671c1309d)
- [Re-write `find_twilights`](https://github.com/Rafnuss/GeoPressureR/commit/d52e14e62c4b212a54f31ca78baa5342d372b4c7)
- [Create function graph_path2edge](https://github.com/Rafnuss/GeoPressureR/commit/db73fcfea317f0db795d6a629bec9d42b9f073fd)
- [Add energy figure](https://github.com/Rafnuss/GeoPressureR/commit/8b4c4efbce7c029e1a0f0628985bf53616c829a0)
- Multiple improvements on GeoPressureViz
- Add citation and contribution file
- [use 100 character width](https://github.com/Rafnuss/GeoPressureR/commit/ae97874788658b8684bb3e3fa539c063cc0046ab)
- Add link to [GeoPressureTemplate](https://github.com/Rafnuss/GeoPressureTemplate)

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v2.2-beta...v2.3-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.2-beta...v2.3-beta)

# GeoPressureR v2.2-beta

## Major

- New function `geopressure_map2path` with return of index of lat-lon option
- New function `geopressure_ts_path` to compute multiple `geopressure_ts` function on a full path
- Update GeoPressureViz ([demo app](https://rafnuss.shinyapps.io/GeoPressureViz/)) to accept `geopressure_ts_path` output

## Minor

- fix flight and avonet database [#10](https://github.com/Rafnuss/GeoPressureR/issues/10)
- fix [#9](https://github.com/Rafnuss/GeoPressureR/issues/9)

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v2.1-beta...v2.2-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.1-beta...v2.2-beta)

# GeoPressureR v2.1-beta

## Major

- Graph Addition of wind: <https://raphaelnussbaumer.com/GeoPressureR/articles/wind-graph.html>
- Movement model function: converting airspeed/groundspeed to probability.

## Minor

- Minor correction of existing code
- cleaning of name, variable and file saved for more consistency
- Update to GeoPressureAPI v2.1

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v2.0-beta...v2.1-beta](https://github.com/Rafnuss/GeoPressureR/compare/v2.0-beta...v2.1-beta)

# GeoPressureR v2.0-beta

## What's Changed

- Add vignette and code for light geopositioning in [#4](https://github.com/Rafnuss/GeoPressureR/issues/4)
- minor language changes by @jsocolar in [#7](https://github.com/Rafnuss/GeoPressureR/issues/7)

## New Contributors

- @jsocolar made their first contribution in [#7](https://github.com/Rafnuss/GeoPressureR/issues/7)

## Full Changelog

[https://github.com/Rafnuss/GeoPressureR/compare/v1.1-beta...v2.0-beta](https://github.com/Rafnuss/GeoPressureR/compare/v1.1-beta...v2.0-beta)

# GeoPressureR v1.1-beta

## Full Changelog

<https://github.com/Rafnuss/GeoPressureR/commits/v1.1-beta>
