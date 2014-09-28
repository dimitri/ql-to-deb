# Quicklisp to debian

This projects updates Common Lisp libraries debian packages from the current
release found at Quicklisp, if an update is needed.

## Quicklisp

ql-to-deb works with a directory of debian packaging as input, and then
fetches informations from the quicklisp web site.

The quicklisp URLs used are:

  - http://beta.quicklisp.org/dist/quicklisp.txt
  - http://beta.quicklisp.org/dist/quicklisp/2014-04-25/releases.txt
  
The first URL contains as its `release-index-url` the second URL, which
contains the list of currently most recent available releases in Quicklisp.

## Debian

It's only possible to `ql-to-deb` on packages you've prepared before hand.
Add the packaging in the `debian/` subdirectory of the project. A typical
workflow would be:

    $ ql-to-deb status [ package ]
    $ ql-to-deb build [ package ]
    $ ql-to-deb lint
    $ ql-to-deb sign
    $ ql-to-deb upload

The `build` command stores the name of the debian *changes* files in
`~/.ql-to-deb.changes` and will reuse that information in the other steps,
so that it's possible to use the `lint`, `sign` and `upload` commands
without any arguments meaning *work on what I just built please*.

The application will parse the `debian/control` file to figure out the name
of the debian source package, it is adviced to name the packaging directory
the same as the source file for easier maintenance.

### Origin tarball

The tarball given by Quicklisp is considered to be the `.orig.tar.gz` file
for debian packaging purposes. `ql-to-deb` just symlinks it in the format
expected by debian, and at the right place.

### Version numbers

Basically Quicklisp wins. When quicklisp release has a version number that
is both not the same as the current debian one, nor `gt` to it (in terms of
`dpkg --compare-versions`), then an *epoch* is added to the debian version
number.

For example, the `cffi` package debian version currently is *20100219* where
in Quicklisp the release version string is *0.12.0*. In that case
`ql-to-deb` will prepare a new version of the package with the debian
version string `1:0.12.0-1`.

## INSTALL

Currently lacking a debian package, because of a classic chicken and eggs
problem: `ql-to-deb` itself has some dependencies that are not to be found
in debian yet (drakma, md5 and command-line-arguments).

    $ sudo apt-get install sbcl cl-ppcre cl-split-sequence cl-alexandria
    $ sudo apt-get build-dep cl-asdf
    $ sudo apt-get build-dep buildapp
    $ dpkg -i cl-asdf...

Note that we need `cl-asdf` version 3.0.3 or more recent, and `buildapp`
version 1.5 or more recent, both as found in *sid* and an `apt-get
build-dep` command away to being backported into *squeeze*.

Now that you have the build requirements sorted:

    $ make

## Usage

And then:

    $ ./build/bin/ql-to-deb --version
    ql-to-deb version "0.7.0"
    compiled with SBCL 1.2.3.debian

    $ ./build/bin/ql-to-deb [ option ... ] command [ package ...]
    
    Options:
      --help -h                       boolean  Show usage and exit. 
      --version -V                    boolean  Displays version and exit. 
      --verbose -v                    boolean  Be verbose. 
      --fix-bugs -F                   boolean  Fix packaging bugs. 
      --config -c                     string   configuration file.  (default: #P"/vagrant/ql-to-deb.ini")
      --dir -D                        string   where to build packages.  (default: "/tmp/ql-to-deb/")
      --logs -L                       string   where to write detailed logs.  (default: "/tmp/ql-to-deb/logs/")
      --quicklisp -Q                  string   URL to use to fetch quicklisp.txt main file  (default: "http://beta.quicklisp.org/dist/quicklisp.txt")
    
    Commands:
      status    Compare versions (debian sid, local, Quicklisp)
      check     List packages that need building
      update    Build either packages or all that need a build
      build     Build either packages or all that need a build
      lint      Run lintian on just built packages
      sign      Run debsin on just built packages
      upload    Run dput on just built packages
