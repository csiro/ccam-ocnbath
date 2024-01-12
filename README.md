# Ocnbath (Bathymetry and river routing for CCAM)

Ocnbath is used to create bathymetry and river routing maps for the Conformal
Cubic Atmospheric Model (CCAM).

## Website

For documentation, see our website at

[https://confluence.csiro.au/display/CCAM/CCAM]

## Dependencies

Ocnbath requires the NetCDF C library.

## Building ocnbath

To build ocnbath with intel, gnu and cray fortran compiler use

```
make
make GFORTRAN=yes
make CRAY=yes
```

Debugging is also enabled with

```
make TEST=yes
```
