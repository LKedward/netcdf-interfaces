# netcdf-interfaces
__Netcdf4 interface module reproduced from [netcdf-fortran](https://github.com/Unidata/netcdf-fortran) as an [fpm](https://github.com/fortran-lang/fpm) package.__

![License: BSD-3-Clause AND Apache-2.0](https://img.shields.io/badge/License-BSD--3--Clause%20AND%20Apache--2.0-blue)


| [![fpm test](https://github.com/LKedward/netcdf-interfaces/actions/workflows/test.yml/badge.svg)](https://github.com/LKedward/netcdf-interfaces/actions) | `ubuntu-latest` | `ifort 2021.2.0` | `gfortran 10.2.0` |
|---|---|---|---|

## Usage

__Prerequisites:__
The following should already be installed on your system, see each package for their respective installation instructions.
- [netcdf-c](https://github.com/Unidata/netcdf-c)
- [netcdf-fortran](https://github.com/Unidata/netcdf-fortran)

To use within your *fpm* project, add the following to your package manifest file (`fpm.toml`):

```toml
[dependencies]
netcdf-interfaces = { git = "https://github.com/LKedward/netcdf-interfaces.git" }
```

__NOTE:__ when building with `gfortran-10` you need to use `--flag -fallow-argument-mismatch` in order to compile correctly.

_e.g_

```shell
$ fpm build --profile debug --flag -fallow-argument-mismatch
$ fpm run --profile debug --flag -fallow-argument-mismatch
```
