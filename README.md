# Required tools

Latest commit built with Windows 11, Python 3.12.8, R 4.4.2.

Should be compatible with Windows 10 or 11, Python == 3.12, R>=4.4.0.

Python 3.13 not compatible at this time, until which time dependencies can be resolved properly.

## Steps to reproduce

1. Download and install conda or mamba. Assumption is that mamba is available. Alternatively, a regular python 3.12 installation with pip can be used.
2. Build python environment from `environment.yml`. This particular environment file is supplied by the python package `goes2go` and gets necessary tech for that. `mamba env create -f environment.yml`. For maximal compatibility, use one of the next options; this environment file does not lock many package versions and may not be compatible far into the future.
3. Alternatively, the `environment_spec.yml` is a little more specific with package versions, but also might not be portable to different systems. `mamba env create -f environment_spec.yml`
4. Alternatively, the `pipreqs.txt` file contains a minimal set of packages from pip to compose all the content. `pip install -r pipreqs.txt`
5. Download and install R version 4.4.0 or higher. This can be downloaded from [the website](https://cran.r-project.org/bin/windows/base/), or done through conda or something like chocolatey. Make sure the Rscript.exe is on your path variable.
6. Run `setup.R` to install requisite packages.
7. Install `ffmpeg`>=7.0.0; this can be done through conda or something like chocolatey.
8. Run all the steps in `procedure.sh`.
