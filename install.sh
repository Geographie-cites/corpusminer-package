cd ..
# create the archive
tar -czf corpusminer.tgz corpusminer-package

# install the package
# must specify local lib (or run from the console after a fresh R install)
R -e 'install.packages("corpusminer.tgz",repos=NULL,source=TRUE,lib="~/R/x86_64-pc-linux-gnu-library/3.6")'
# test
R -e 'library(corpusminer)'

# optional : copy app code ?
# only if unchanged - not needed for now

# remove archive
rm -f corpusminer.tgz
