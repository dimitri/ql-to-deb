# ql-to-deb build tool
APP_NAME   = ql-to-deb

# use either sbcl or ccl
CL	   = sbcl

LISP_SRC   = $(wildcard src/*lisp) ql-to-deb.asd

BUILDDIR   = build
LIBS       = $(BUILDDIR)/libs.stamp
QLDIR      = $(BUILDDIR)/quicklisp
QUICKLISP  = $(BUILDDIR)/quicklisp.lisp
MANIFEST   = $(BUILDDIR)/manifest.ql
QL_TO_DEB  = $(BUILDDIR)/bin/$(APP_NAME)

BUILDAPP_CCL  = $(BUILDDIR)/bin/buildapp.ccl
BUILDAPP_SBCL = $(BUILDDIR)/bin/buildapp.sbcl

ifeq ($(CL),sbcl)
BUILDAPP   = $(BUILDAPP_SBCL)
CL_OPTS    = --no-sysinit --no-userinit
else
BUILDAPP   = $(BUILDAPP_CCL)
CL_OPTS    = --no-init
endif

COMPRESS_CORE ?= yes

ifeq ($(CL),sbcl)
ifeq ($(COMPRESS_CORE),yes)
COMPRESS_CORE_OPT = --compress-core
else
COMPRESS_CORE_OPT = 
endif
endif

# ifeq ($(CL),sbcl)
# BUILDAPP_OPTS =          --require sb-posix
# endif

DEBUILD_ROOT = /tmp/ql-to-deb/debian

all: $(QL_TO_DEB)

clean:
	rm -rf $(LIBS) $(QUICKLISP) $(QLDIR) $(MANIFEST) $(BUILDAPP) $(QL_TO_DEB)

$(QLDIR)/setup.lisp:
	mkdir -p $(BUILDDIR)
	curl -o $(QUICKLISP) http://beta.quicklisp.org/quicklisp.lisp
	$(CL) $(CL_OPTS) --load $(QUICKLISP) \
             --eval '(quicklisp-quickstart:install :path "$(BUILDDIR)/quicklisp")' \
             --eval '(quit)'

quicklisp: $(QLDIR)/setup.lisp ;

$(LIBS): $(QLDIR)/setup.lisp
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp                 \
             --eval '(require :asdf)'                               \
             --eval '(asdf:load-system :asdf)'                      \
             --eval '(push "$(PWD)/" asdf:*central-registry*)'      \
             --eval '(ql:quickload "ql-to-deb")'                    \
             --eval '(quit)' > /dev/null 2>&1
	touch $@

libs: $(LIBS) ;

$(MANIFEST): $(LIBS)
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp                \
             --eval '(ql:write-asdf-manifest-file "$(MANIFEST)")'  \
             --eval '(quit)'

manifest: $(MANIFEST) ;

$(BUILDAPP_CCL): $(QLDIR)/setup.lisp
	mkdir -p $(BUILDDIR)/bin
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
             --eval '(ql:quickload "buildapp")'                   \
             --eval '(buildapp:build-buildapp "$@")'              \
             --eval '(quit)'

$(BUILDAPP_SBCL): $(QLDIR)/setup.lisp
	mkdir -p $(BUILDDIR)/bin
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
             --eval '(ql:quickload "buildapp")'                   \
             --eval '(buildapp:build-buildapp "$@")'              \
             --eval '(quit)'

buildapp: $(BUILDAPP) ;

$(QL_TO_DEB): $(MANIFEST) $(BUILDAPP) $(LISP_SRC)
	mkdir -p $(BUILDDIR)/bin
	$(BUILDAPP)      --logfile /tmp/build.log                \
                         $(BUILDAPP_OPTS)                        \
                         --sbcl $(CL)                            \
                         --require asdf                          \
                         --load-system asdf                      \
                         --asdf-tree $(QLDIR)/dists              \
                         --asdf-path .                           \
                         --load-system $(APP_NAME)               \
                         --entry ql-to-deb:main                  \
                         --dynamic-space-size 4096               \
                         $(COMPRESS_CORE_OPT)                    \
                         --output $@

ql-to-deb: $(QL_TO_DEB) ;

deb:
	# intended for use on a debian system
	mkdir -p $(DEBUILD_ROOT) && rm -rf $(DEBUILD_ROOT)/*
	rsync -Ca --exclude=build/* ./ $(DEBUILD_ROOT)/
	cd $(DEBUILD_ROOT) && make -f debian/rules orig
	cd $(DEBUILD_ROOT) && debuild -us -uc -sa
	cp -a /tmp/ql-to-deb/debian/ql-to-deb_* build/
