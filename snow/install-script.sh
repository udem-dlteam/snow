export SNOW_DEBUG=
export SNOW_TEST=
export SNOW_PATH=
export SNOW_USER_ROOT=
export SNOW_SITE_ROOT=
export SNOW_USER_DIR=
export SNOW_SITE_DIR=
export SNOW_PATH_GLOBAL=
export SNOW_URL=

cd ..
chmod +x configure
chmod +x install-sh
chmod +x mkidirs
chmod +x test.scm
./configure
make install
