all:
	fpc4amiga.sh -FUamiga -oExtronAmiga -Fu/home/alb/AROS1/Sources/MUIClass/src ExtronControl.pas
	fpc4mos.sh -FUmos -oExtronMOS -Fu/home/alb/AROS1/Sources/MUIClass/src ExtronControl.pas