all:
	fpc4amiga.sh -FUamiga -oExtronAmiga -Fu/home/alb/AROS1/Sources/MUIClass/src ExtronControl.pas
	fpc4mos.sh -FUmos -oExtronMOS -Fu/home/alb/AROS1/Sources/MUIClass/src ExtronControl.pas
	
lha: all
	rm -rf ExtronControl
	mkdir ExtronControl
	cp ExtronAmiga ExtronControl
	cp ExtronControl.info ExtronControl/ExtronAmiga.info
	cp ExtronMOS ExtronControl
	cp ExtronControl.info ExtronControl/ExtronMOS.info
	cp Readme.txt ExtronControl
	mkdir ExtronControl/Source
	cp *.pas ExtronControl/Source
	lha ao5 ExtronControl.lha ExtronControl
	rm -rf ExtronControl
	
clean:
	rm -rf amiga/*.ppu amiga/*.o mos/*.o mos/*.ppu *.lha ExtronAmiga ExtronMOS ExtronControl
	