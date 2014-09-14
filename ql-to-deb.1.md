# ql-to-deb(1) -- PostgreSQL data loader

## SYNOPSIS

`ql-to-deb` [<options>] [<quicklisp system> ...]

## DESCRIPTION

ql-to-deb aims to simplify maintaining debian packages for Common Lisp
librairies, by automating their updates from Quicklisp. When using
ql-to-deb, debian upstream actually is Quicklisp.

## OPTIONS

  * `-h`, `--help`:
    Show command usage summary and exit.

  * `-V`, `--version`:
    Show ql-to-deb version string and exit.

  * `-v`, `--verbose`:
    Be verbose.

  * `-c`, `--config`:
    Which filename to use as the main ql-to-deb configuration.

  * `--status`:
    Display a list of known packages and their status in the debian sid
    distribution. The status is ok only when the Quicklisp release and the
    current sid package both have the same version string, discarding the
    debian epoch and debian release parts of the version number.

  * `-F`, `--fix-bugs`:
    Rebuild given package(s) from the same Quicklisp release as the current
    most recent debian/changelog entry, allowing for debian packaging bug
    fixes.

  * `-D`, `--dir`:
    Where to build the packages.
    
  * `-L`, `--logs`:
    Where to store the build log files. A separate log file is produced for
    each package that's being built.

  * `-Q`, `--quicklisp`:
    Quicklisp URL to use to fetch Quicklisp distribution.

## Quicklisp Releases and Debian packages

The *quicklisp systems* on the command line should be names after their
Quicklisp name, which is used as the directory name where the debian
packaging is maintained. When usin the ql-to-deb sources, that's in
`./packages`, and when using the debian package version of ql-to-deb, the
packages are located in `/usr/share/ql-to-deb/packages`.

## AUTHOR

Dimitri Fontaine <dimitri@2ndQuadrant.fr>

## SEE ALSO

The ql-to-deb source code and all documentation may be downloaded from
<https://github.com/dimitri/ql-to-deb/>.
