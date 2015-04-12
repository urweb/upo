.PHONY: visit_testing.exe visit_production.exe redb extendb package production

visit_testing.exe:
	urweb -output visit_testing.exe visit

visit_production.exe:
	urweb -protocol fastcgi -output visit_production.exe visit

redb:
	-dropdb visit
	createdb visit
	psql -f visit.sql visit

extendb:
	psql -f visit.sql visit

LIB=/usr/local/lib

package: visit_production.exe
	mkdir -p dist
	rm -rf dist/*
	mkdir dist/public_html
	cp index.html dist/public_html/
	cp visit_production.exe dist/
	cp visit.sql dist/
	cp -d ${LIB}/liburweb.so* dist/
	cp -d ${LIB}/liburweb_fastcgi.so* dist/
	cp Makefile.prod dist/Makefile

production: package
	rsync -az dist/* ubuntu@upo.csail.mit.edu:
