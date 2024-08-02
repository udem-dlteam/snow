# Makefile for Scheme Now! framework.

#==============================================================================

# File: "makefile", Time-stamp: <2007-04-04 17:34:42 feeley>

# Copyright (c) 2006-2007 by Marc Feeley, All Rights Reserved.

#==============================================================================

herefromroot = .
rootfromhere = .

snow_version = v1.1.2
snow_site_root = /Users/feeley/snow-site
snow_site_dir = $(snow_site_root)/$(snow_version)
snow_site_current_dir = $(snow_site_root)/current
all_snow_hosts =  bigloo chicken gambit kawa larceny mzscheme petite scheme48 scm scsh sisc stklos

PROGRAM_bigloo = /Users/feeley/bigloo/bin/bigloo
PROGRAM_chez = chez-scheme
PROGRAM_chicken = /Users/feeley/chicken/bin/csi
PROGRAM_chicken_comp = /Users/feeley/chicken/bin/csc
PROGRAM_gambit = /usr/bin/gsi
PROGRAM_gambit_comp = /usr/bin/gsc
PROGRAM_gauche = /Users/feeley/gauche/bin/gosh
PROGRAM_guile = guile-1.6
PROGRAM_kawa = /Users/feeley/kawa/bin/kawa
PROGRAM_larceny = /Users/feeley/larceny-0.94-bin-native-ia32-macosx/larceny
PROGRAM_mit = mit-scheme
PROGRAM_mzscheme = /Users/feeley/MzScheme-v352/bin/mzscheme
PROGRAM_petite = /Users/feeley/petite/bin/petite
PROGRAM_rscheme = rs
PROGRAM_scheme48 = /Users/feeley/scheme48/bin/scheme48
PROGRAM_scm = /Users/feeley/scm5e3/scm
PROGRAM_scsh = /Users/feeley/scsh/bin/scsh
PROGRAM_sisc = /Users/feeley/sisc-1.16.6/sisc
PROGRAM_stalin = stalin
PROGRAM_stklos = /Users/feeley/stklos/bin/stklos

bin_dir = $(snow_site_dir)/bin
base_dir = $(snow_site_dir)/base
pack_dir = $(snow_site_dir)/pack

MKIDIRS = $(rootfromhere)/mkidirs
INSTALL = $(rootfromhere)/install-sh -c
INSTALL_DATA = $(rootfromhere)/install-sh -c -m 644
INSTALL_SOURCE = $(rootfromhere)/install-sh -c -m 644
INSTALL_PROGRAM = $(rootfromhere)/install-sh -c -m 755

BASE_FILES = snow.scm compat-chez.scm compat-chicken.scm compat-kawa.scm \
compat-larceny.scm compat-mit.scm compat-mzscheme.scm compat-scheme48.scm \
compat-scm.scm compat-scsh.scm compat-sisc.scm

CORE_PACKAGES = aes base64 bignum binio cert cgi cryptio \
digest extio filesys fixnum genport \
homovector hostos http list mime random rfc1423 rsa \
snowfort-app snowlib snowman-app sort string \
tar tcpip time ttyui vector zlib

all:
	@echo "To install type: \"make install\""

clean:
	find . -name "*~*" -exec rm "{}" ";"
	find . -name "*.sig" -exec rm "{}" ";"
	rm -f makefile bin/snow bin/snowman bin/snowfort base/snow.scm bin/index.cgi package-list
	rm -rf host*

install:
	$(MKIDIRS) $(bin_dir)
	$(INSTALL_PROGRAM) bin/snow $(bin_dir)/snow
	$(INSTALL_PROGRAM) bin/snowman $(bin_dir)/snowman
	$(INSTALL_PROGRAM) bin/snowfort $(bin_dir)/snowfort
	$(MKIDIRS) $(base_dir)
	@for file in $(BASE_FILES); do \
	  echo $(INSTALL_SOURCE) base/$$file $(base_dir)/$$file; \
	  $(INSTALL_SOURCE) base/$$file $(base_dir)/$$file; \
	done
	$(MKIDIRS) $(pack_dir)
	@for pkg in $(CORE_PACKAGES); do \
	  VERSIONS=`ls pack/$$pkg`; \
	  $(MKIDIRS) $(pack_dir)/$$pkg; \
	  for ver in $$VERSIONS; do \
	    SUBDIRS=`ls pack/$$pkg/$$ver`; \
	    $(MKIDIRS) $(pack_dir)/$$pkg/$$ver; \
	    for sub in $$SUBDIRS; do \
	      FILES=`ls pack/$$pkg/$$ver/$$sub`; \
	      $(MKIDIRS) $(pack_dir)/$$pkg/$$ver/$$sub; \
	      for file in $$FILES; do \
	        echo $(INSTALL_SOURCE) pack/$$pkg/$$ver/$$sub/$$file $(pack_dir)/$$pkg/$$ver/$$sub/$$file; \
	        $(INSTALL_SOURCE) pack/$$pkg/$$ver/$$sub/$$file $(pack_dir)/$$pkg/$$ver/$$sub/$$file; \
	      done; \
	    done; \
	  done; \
	done
	@for snow_host in $(all_snow_hosts); do \
	  echo "Setting up for SNOW_HOST=$$snow_host"; \
	  for pkg in $(CORE_PACKAGES); do \
	    VERSIONS=`ls $(pack_dir)/$$pkg`; \
	    for ver in $$VERSIONS; do \
	      echo "  compiling $$pkg/$$ver"; \
	      (cd $(pack_dir)/$$pkg/$$ver/snow && \
	      SNOW_HOST="$$snow_host" "$(bin_dir)/snow" --compile "$$pkg.scm" \
	      ) || exit 1; \
	    done \
	  done \
	done
	rm -f $(snow_site_current_dir)
	ln -s $(snow_site_dir) $(snow_site_current_dir)
	@echo "Don't forget to add $(snow_site_current_dir)/bin to your PATH"

uninstall:
