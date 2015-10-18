.PHONY: visit_testing.exe visit_production.exe \
	jfac_testing.exe jfac_production.exe \
	redb extendb package production

visit_testing.exe:
	urweb -output visit_testing.exe visit

visit_production.exe:
	urweb -protocol fastcgi -output visit_production.exe visit

jfac_testing.exe:
	urweb -output jfac_testing.exe jfac

jfac_production.exe:
	urweb -protocol fastcgi -output jfac_production.exe jfac

redb:
	-dropdb visit
	createdb visit
	psql -f visit.sql visit
	-dropdb jfac
	createdb jfac
	psql -f jfac.sql jfac

extendb:
	psql -f visit.sql visit
	psql -f jfac.sql jfac

LIB=/usr/local/lib

package: visit_production.exe jfac_production.exe
	mkdir -p dist
	rm -rf dist/*
	mkdir dist/public_html
	cp index.html dist/public_html/
	cp visit_production.exe dist/
	cp visit.sql dist/
	cp jfac_production.exe dist/
	cp jfac.sql dist/
	cp -d ${LIB}/liburweb.so* dist/
	cp -d ${LIB}/liburweb_fastcgi.so* dist/
	cp -d ${LIB}/liburweb_ajaxUpload.so* dist/
	cp Makefile.prod dist/Makefile

production: package
	rsync -az dist/* ubuntu@upo.csail.mit.edu:
