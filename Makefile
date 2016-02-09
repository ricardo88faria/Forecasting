.PHONY: run tail

run:
	./prev_ftp_ncep_gfs0p25_grib.R >& log

tail:
	tail -f log
