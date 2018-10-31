program ExtronControl;
{$mode objfpc}{$H+}
uses
  MUIClass.Base, Window1Unit, serialunit;

begin
  if Assigned(ST) then
  begin
    MUIApp.Base := 'EXTRON';
    MUIApp.Title := 'ExtronControl';
    MUIApp.Version := '$VER ExtronControl 0.1 (31.10.2018)';
    MUIApp.Description := 'Control a Extron DSC 301 HD';
    Window1 := TWindow1.Create;
    MUIApp.Run;
  end;
end.
