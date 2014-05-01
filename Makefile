# ql-to-deb build tool
APP_NAME   = ql-to-deb

# use either sbcl or ccl
CL	   = sbcl

ifeq ($(CL),sbcl)
CL_OPTS    = --no-sysinit --no-userinit
else
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

ifeq ($(CL),sbcl)
BUILDAPP_OPTS =          --require sb-posix                      \
                         --require sb-bsd-sockets                \
                         --require sb-rotate-byte
endif


BUILDDIR   = build
LIBS       = $(BUILDDIR)/libs.stamp
BUILDAPP_  = $(BUILDDIR)/buildapp.stamp
BUILDAPP   = $(BUILDDIR)/bin/buildapp
MANIFEST   = $(BUILDDIR)/manifest.ql
QL_TO_DEB  = $(BUILDDIR)/bin/$(APP_NAME)
QLDIR      = $(BUILDDIR)/quicklisp

all: $(QL_TO_DEB)

$(QLDIR)/setup.lisp:
	mkdir -p $(BUILDDIR)
	curl -o $(BUILDDIR)/quicklisp.lisp http://beta.quicklisp.org/quicklisp.lisp
	$(CL) $(CL_OPTS) --load $(BUILDDIR)/quicklisp.lisp                         \
             --eval '(quicklisp-quickstart:install :path "$(BUILDDIR)/quicklisp")' \
             --eval '(quit)'

quicklisp: $(QLDIR)/setup.lisp ;

$(LIBS): quicklisp
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp                 \
             --eval '(require :asdf)'                               \
             --eval '(asdf:load-system :asdf)'                      \
             --eval '(ql:quickload "ql-to-deb")'                    \
             --eval '(quit)' > /dev/null 2>&1
	touch $@

libs: $(LIBS) ;

$(BUILDAPP): quicklisp
	mkdir -p $(BUILDDIR)/bin
	$(CL) $(CL_OPTS) --load $(QLDIR)/setup.lisp               \
             --eval '(ql:quickload "buildapp")'                   \
             --eval '(buildapp:build-buildapp "$(BUILDAPP)")'     \
             --eval '(quit)'

$(BUILDAPP_): $(BUILDAPP)
	touch $@

buildapp: $(BUILDAPP_) ;

$(QL_TO_DEB): buildapp
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
