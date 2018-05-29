INSTALL_DIRECTORY = ~/bin/.
POSTGREST_PACKAGE = "https://github.com/PostgREST/postgrest/releases/download/v0.5.0.0/postgrest-v0.5.0.0-freebsd.tar.xz"

default: build

build:
	time stack build

install_postgrest:
	curl ${POSTGREST_PACKAGE} > postgrest.tgz
	tar xf postgrest.tgz

initialize_database:
	psql -Upostgres -f db/schema.sql

test:
	time stack test

run:
	stack exec jobs

quickbuild:
	time stack build --fast

install:
	cp `find . | grep bin/jobs` ${INSTALL_DIRECTORY}

shell:
	stack ghci

docs:
	stack haddock
