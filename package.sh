#!/bin/sh

mkdir xml-emitter-1.0.3
cp xml-emitter.asd package.sh package.lisp README xml.lisp rss2.lisp mailbox.lisp xml-emitter-1.0.3/
rm -f xml-emitter-latest.tar.gz
tar -czvf xml-emitter-1.0.3.tar.gz xml-emitter-1.0.3/
rm -Rf xml-emitter-1.0.3/
