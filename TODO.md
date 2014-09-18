# TODO list for ql-to-deb

  - add support for running lintian
  - add support for running debsign
  - add support for running dput

## Local Distribution

Add a target directory where to host the produced files. Maybe make it so
that it's possible to have that as a sources.list entry.

Produce a Package.gz file?

## Commands

Add a command based CLI rather than the --status thing we have now.

  - status
  
  - update
  
    Download QL release then rebuild all packages that needs a rebuild
    
    Prints the list of systems that just got updated
    
  - lintian
  
    By default, run lintian on just previously generated packages
    
    We will need a cache system ala debi/debc so that the tool knows what to do
