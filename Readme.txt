ExtronControl 0.1
-----------------

Extron DSC 301 HD control program for Amiga68k and MorphOS.
Tested with Firmware Version 1.25

currently supported (ordered by GUI Lines):
- Switch Input
- Output Resolution chooser
- Auto Input Switch and Auto align picture
- Test images
- Lock device front panel
- Temperature

default Settings: Baud 9600
Amiga68k: serial.device unit 0 (device is connected via serial port at backside)
MorphOS: usbmodem.device unit 0 (device is connected via USB at from panel)

You can change the settings in the icons Tooltypes:
e.g.
DEVICE=usbmodem.device
UNIT=0
BAUD=9600

The text on the input switch buttons you can set using Tooltypes:
NAME1=input1Name
NAME2=input2Name
NAME3=input3Name
if not given or empty the type of input is given (Composite, VGA, HDMI)

you can also hide parts of the GUI with the Tooltypes:
HIDESWITCH     - hide input switch buttons
HIDERESOLUTION - Hide Resolution Settings
HIDEAUTO       - hide Auto input and Autoadjust settings
HIDETEST       - hide Testpatterns settings
HIDELOCK       - hide device Front Lock

The AREXX port is named "EXTRON.1" and know two commands:

Switch to the input number 1-3:
input INPUTNUM/N
e.g.
input 1    /* switch to input 1 */

choose output resolution and frequency 10-92:
output OUTRESFREQ/N
e.g.
output 73   /* set output to 720p 60Hz */
output 84   /* set output to 1080p 60Hz */
Check the Figure 17. SIS Command EDID Table in "DSC 301 HD User Guide" p.25
for the needed number

example script are included in the rexx folder.

Source included, GUI created used MUIIDE and MUIClass
Licence: CC0
