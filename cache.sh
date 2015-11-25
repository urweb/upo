#!/bin/sh

mkdir -p cached
cd cached
for url in `grep "http" ../default.urp | sed -E "s/^.*http(.*)$/http\\1/"`
do
    filename=`echo $url | sed -E "s/^.*\/([^\/]*)$/\\1/"`
    curl $url -o $filename
done
