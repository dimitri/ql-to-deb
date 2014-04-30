# Quicklisp to debian

This projects updates Common Lisp libraries debian packages from the current
release found at Quicklisp, if an update is needed.

## Inputs

ql-to-deb works with a directory of debian packaging as input, and then
fetches informations from the quicklisp web site.

The quicklisp URLs used are:

  - http://beta.quicklisp.org/dist/quicklisp.txt
  - http://beta.quicklisp.org/dist/quicklisp/2014-04-25/releases.txt
  
The first URL contains as its `release-index-url` the second URL, which
contains the list of currently most recent available releases in Quicklisp.
