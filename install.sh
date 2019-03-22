cd ..
# create the archive
tar -czf corpusminer.tgz corpusminer-package

# install the package
R -e 'install.packages("corpusminer.tgz",repos=NULL,source=TRUE)'
# test
R -e 'library(corpusminer)'

# optional : copy app code ?
# only if unchanged - not needed for now

# remove archive
rm -f corpusminer.tgz
