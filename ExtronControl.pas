program ExtronControl;
{$mode objfpc}{$H+}
uses
  MUIClass.Base, Window1Unit, serialunit;

begin
  if Assigned(ST) then
  begin
    Window1 := TWindow1.Create;
    MUIApp.Run;
  end;
end.
