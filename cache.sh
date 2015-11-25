#!/bin/sh

mkdir -p cached
cd cached
for url in `grep "http" ../default.urp | sed -E "s/^.*http(.*)$/http\\1/"`
do
    filename=`echo $url | sed -E "s/^.*\/([^\/]*)$/\\1/"`
    curl $url -o $filename
done

fonts=`grep "http.*bootstrap.min.css" ../default.urp | sed -E "s/^.*http(.*)\/css\/bootstrap\.min\.css$/http\\1\/fonts\//"`
curl $fonts/glyphicons-halflings-regular.woff2 -o glyphicons-halflings-regular.woff2
curl $fonts/glyphicons-halflings-regular.woff -o glyphicons-halflings-regular.woff
curl $fonts/glyphicons-halflings-regular.ttf -o glyphicons-halflings-regular.ttf
