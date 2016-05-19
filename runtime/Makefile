ZIP=/usr/bin/zip -q

SRC_DIR = src
DIST_DIR = dist
ZIP_DIR = zip
ELI_DIR = eli
EL_DIR = ../el

ifndef VERSION
  VERSION = "incremental-$(shell git describe --tags)"
endif

ZIPFILE="emacs-runtime-${VERSION}.zip"

ZIPTMPDIR=/tmp/runtime_`whoami`

clean:
	rm -rf ${DIST_DIR}
	rm -rf ${ZIP_DIR}

init: clean
	mkdir -p ${DIST_DIR}    
	mkdir -p ${ZIP_DIR}    

copyfiles:
	@@echo "Building emacs runtime"

	@@echo "Copying source files"
	@@cp -r ${SRC_DIR}/* ${DIST_DIR}

	@@echo "Copying eli files"
	@@mkdir -p ${DIST_DIR}/eli
	@@cp -r ${ELI_DIR}/* ${DIST_DIR}/eli

	@@echo "Copying el files"
	@@mkdir -p ${DIST_DIR}/el
	@@cp -r ${EL_DIR}/* ${DIST_DIR}/el

	@@echo "Adding version.txt"
	@@echo ${VERSION} > ${DIST_DIR}/version.txt

buildzip:
	@@echo "Zip files to ${ZIPFILE}"
	@@mkdir -p ${ZIPTMPDIR}
	@@rm -rf ${ZIPTMPDIR}
	@@mkdir -p ${ZIPTMPDIR}
	@@cp -r dist/* ${ZIPTMPDIR}
	@@cd ${ZIPTMPDIR}; ${ZIP} -r ${ZIPFILE} * -x \*CVS\*
	@@echo "Move zip file to ${ZIP_DIR}."
	@@mv ${ZIPTMPDIR}/${ZIPFILE} ${ZIP_DIR}

zip: init copyfiles buildzip
