# ExpressionAtlasInternal
Expression Atlas internal R package for internal atlas processing tbat is used by atlas-prod for data processing and creating summary objects
### Note
Should there be a version bump to this package (with any improvements) made to the DESCRIPTION file 
For example `Version: 1.x` then the same version number `1.x` should be used to create a git tag number (1.x) to the latest commit. The AtlasProd code looks for the desired version that matches git tags to install the package while its uses DESCRIPTION file version internally to match desired version numbers.
