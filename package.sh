#!/bin/sh

mkdir xml-emitter-1.0.2
cp xml-emitter.asd package.sh package.lisp README xml.lisp rss2.lisp mailbox.lisp xml-emitter-1.0.2/

rm -f xml-emitter-latest.tar.gz xml-emitter-latest.tar.gz.asc

tar -czvf xml-emitter-1.0.2.tar.gz xml-emitter-1.0.2/
ln -s ~/.sbcl/site/xml-emitter-1.0.1/xml-emitter-1.0.2.tar.gz ~/.sbcl/site/xml-emitter-1.0.1/xml-emitter-latest.tar.gz
gpg -b -a ~/.sbcl/site/xml-emitter-1.0.1/xml-emitter-1.0.2.tar.gz
ln -s ~/.sbcl/site/xml-emitter-1.0.1/xml-emitter-1.0.2.tar.gz.asc ~/.sbcl/site/xml-emitter-1.0.1/xml-emitter-latest.tar.gz.asc
rm -Rf xml-emitter-1.0.2/

scp xml-emitter-1.0.2.tar.gz pscott@common-lisp.net:/project/asdf-packaging/public_html/xml-emitter-1.0.2.tar.gz
scp xml-emitter-1.0.2.tar.gz.asc pscott@common-lisp.net:/project/asdf-packaging/public_html/xml-emitter-1.0.2.tar.gz.asc
scp xml-emitter-latest.tar.gz pscott@common-lisp.net:/project/asdf-packaging/public_html/xml-emitter-latest.tar.gz
scp xml-emitter-latest.tar.gz.asc pscott@common-lisp.net:/project/asdf-packaging/public_html/xml-emitter-latest.tar.gz.asc
