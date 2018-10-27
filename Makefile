all:
	fpc4amiga.sh -FUamiga -oExtronAmiga -Fu/home/alb/AROS1/Sources/MUIClass/src ExtronControl.pas
	fpc4mos.sh -FUmos -oExtronMOS -Fu/home/alb/AROS1/Sources/MUIClass/src ExtronControl.pas
	
lha: all
	rm -rf ExtronControl
	mkdir ExtronControl
	cp ExtronAmiga ExtronControl
	cp ExtronMOS ExtronControl
	cp Readme.txt ExtronControl
	mkdir ExtronControl/Source
	cp *.pas ExtronControl/Source
	lha ao5 ExtronControl.lha ExtronControl
	