unit Window1Unit;
{$mode objfpc}{$H+}
interface
uses
  MUIClass.Base, MUIClass.Window, MUIClass.Menu, MUIClass.Group, MUIClass.Area,
  MUIClass.Image,
  serialunit, sysutils, MUI;
type
  TWindow1 = class(TMUIWindow)
  private
    CurImg: Integer;
  public
    IdleTimer: TMUITimer;
    Timer: TMUITimer;
    Group1: TMUIGroup;
    Group3: TMUIGroup;
    Switch1: TMUIButton;
    Switch2: TMUIButton;
    Switch3: TMUIButton;
    Group2: TMUIGroup;
    StatusText: TMUIText;
    ChooseAuto: TMUICycle;
    AutoInput: TMUICycle;
    TestPatt: TMUICycle;
    procedure Window1Show(Sender: TObject);
    procedure Switch1Click(Sender: TObject);
    procedure Switch2Click(Sender: TObject);
    procedure Switch3Click(Sender: TObject);
    constructor Create; override;
    procedure IdleEvent(Sender: TObject);
    procedure TimerEvent(Sender: TObject);
    procedure AutoClick(Sender: TObject);
    procedure AutoInputEvent(Sender: TObject);
    procedure TestPattEvent(Sender: TObject);
  end;
var
  Window1: TWindow1;
implementation


procedure ParseMessage(Msg: string);
var
  i: Integer;
begin
  Msg := Trim(Msg);
  if Msg = '' then
    Exit;
  //writeln('Parse Message: "' + Msg + '"');
  // Input 1, 2, 3
  if Copy(Msg, 1, 2) = 'In' then
  begin
    i := StrToIntDef(Msg[3], -1);
    //writeln('Found input: ', i);
    if (i > 0) and (Window1.CurImg <> i) then
    begin
      Window1.CurImg := i;
      case Window1.CurImg of
        1: Window1.Switch1.Selected := True;
        2: Window1.Switch2.Selected := True;
        3: Window1.Switch3.Selected := True;
      end;
      Window1.StatusText.Contents := 'Input switched to ' + IntToStr(i);
    end;
    Exit;
  end;
  if Copy(Msg, 1, 4) = 'Ausw' then
  begin
    Window1.AutoInput.Active := StrToIntDef(Msg[5], Window1.AutoInput.Active);
    if Window1.AutoInput.Active > 0 then
      Window1.StatusText.Contents := 'Auto Input switch enabled'
    else
      Window1.StatusText.Contents := 'Auto Input switch disabled';
    Exit;
  end;
  if Copy(Msg, 1, 4) = 'Test' then
  begin
    Window1.TestPatt.Active := StrToIntDef(Msg[5], Window1.TestPatt.Active);
    if Window1.TestPatt.Active > 0 then
      Window1.StatusText.Contents := 'Test Image '+IntToStr(Window1.TestPatt.Active)+' enabled'
    else
      Window1.StatusText.Contents := 'Test Image disabled';
    Exit;
  end;
  if Copy(Msg, 1, 4) = 'Img' then
  begin
    Window1.StatusText.Contents := 'Auto adjust done.';
    Exit;
  end;
  Window1.StatusText.Contents := 'Msg: "' + Msg + '"';
end;

constructor TWindow1.Create;
begin
  inherited;
  OnShow := @Window1Show;
  Title := 'Extron Control';
  Group1 := TMUIGroup.Create;
  with Group1 do
  begin
    Horiz := True;
    SameWidth := True;
    Parent := Self;
  end;

  Switch1 := TMUIButton.Create;
  with Switch1 do
  begin
    Contents := '1: Composite';
    InputMode := MUIV_InputMode_Immediate;
    OnSelected := @Switch1Click;
    Parent := Group1;
  end;

  Switch2 := TMUIButton.Create;
  with Switch2 do
  begin
    Contents := '2: VGA';
    InputMode := MUIV_InputMode_Immediate;
    OnSelected := @Switch2Click;
    Parent := Group1;
  end;

  Switch3 := TMUIButton.Create;
  with Switch3 do
  begin
    Contents := '3: HDMI';
    InputMode := MUIV_InputMode_Immediate;
    OnSelected := @Switch3Click;
    Parent := Group1;
  end;

  Group2 := TMUIGroup.Create;
  with Group2 do
  begin
    Horiz := True;
    Parent := Self;
  end;

  with TMUIText.Create('Auto input:') do
  begin
    Frame := 0;
    PreParse := #27'r';
    Parent := Group2;
  end;

  AutoInput := TMUICycle.Create;
  with AutoInput do
  begin
    Entries := ['Disabled', 'Prio High', 'Prio Low'];
    OnActiveChange := @AutoInputEvent;
    Parent := Group2;
  end;

  with TMUIButton.Create do
  begin
    Contents := 'Auto adjust';
    OnClick := @AutoClick;
    Parent := Group2;
  end;

  ChooseAuto := TMUICycle.Create;
  with ChooseAuto do
  begin
    Entries := ['Default', 'Fill Screen', 'Follow Aspect'];
    Parent := Group2;
  end;

  Group3 := TMUIGroup.Create;
  with Group3 do
  begin
    Horiz := True;
    Parent := Self;
  end;

  with TMUIText.Create('Test Patterns:') do
  begin
    Frame := 0;
    PreParse := #27'r';
    Parent := Group3;
  end;

  TestPatt := TMUICycle.Create;
  with TestPatt do
  begin
    Entries := ['Off', 'Crop', 'Alternating Pixel', 'Crosshatch', 'Color bars', 'Grayscale', 'Blue mode', 'Audio Test'];
    OnActiveChange := @TestPattEvent;
    Parent := Group3;
  end;

  StatusText := TMUIText.Create;
  with StatusText do
  begin
    Parent := Self;
    Contents := '';
  end;

  IdleTimer := TMUITimer.Create;
  IdleTimer.Interval := 100;
  IdleTimer.OnTimer := @IdleEvent;
  Timer := TMUITimer.Create;
  Timer.Interval := 1000;
  Timer.OnTimer := @TimerEvent;
  // clear for old status messages
  repeat
  until ST.SendGetText('') = '';
  // already show right setting from start
  TimerEvent(nil);
end;

procedure TWindow1.Window1Show(Sender: TObject);
begin

  // Get current Input channel
  ParseMessage('In' + ST.SendGetText('!'));
  // Get AutoInput modus
  ParseMessage('Ausw' + ST.SendGetText('WAUSW'#13));
  // is a test image active?
  ParseMessage('Test' + ST.SendGetText('WTEST'#13));

  StatusText.Contents := Trim(ST.SendGetText('0Q'));

  Timer.Enabled := True;
  IdleTimer.Enabled := True;
end;


procedure TWindow1.Switch1Click(Sender: TObject);
begin
  if not Switch1.Selected then
    Exit;
  Switch2.Selected := False;
  Switch3.Selected := False;
  ST.SendGetText('1!');
  Window1.CurImg := 1;
end;


procedure TWindow1.Switch2Click(Sender: TObject);
begin
  if not Switch2.Selected then
    Exit;
  Switch1.Selected := False;
  Switch3.Selected := False;
  ST.SendGetText('2!');
  Window1.CurImg := 2;
end;


procedure TWindow1.Switch3Click(Sender: TObject);
begin
  if not Switch3.Selected then
    Exit;
  Switch1.Selected := False;
  Switch2.Selected := False;
  ST.SendGetText('3!');
  Window1.CurImg := 3;
end;

procedure TWindow1.IdleEvent(Sender: TObject);
begin
  ParseMessage(Trim(ST.CheckAsyncMsg));
end;

procedure TWindow1.TimerEvent(Sender: TObject);
var
  s: String;
  b1,b2,b3: boolean;
  c: Integer;
begin
  s := ST.SendGetText('W0LS'#13);

  c := Pos('*', s);
  if c < 2 then
    Exit;
  b1 := s[c - 1] = '1';
  b2 := s[c + 1] = '1';
  b3 := s[c + 3] = '1';
  if b1 then
    Switch1.PreParse := #27'b'
  else
    Switch1.PreParse := '';
  if b2 then
    Switch2.PreParse := #27'b'
  else
    Switch2.PreParse := '';
  if b3 then
    Switch3.PreParse := #27'b'
  else
    Switch3.PreParse := '';
  //
  //s := ST.SendGetText('WXIMG'#13);
  //writeln(s)
end;

procedure TWindow1.AutoClick(Sender: TObject);
begin
  case ChooseAuto.Active of
    1: ST.SendGetText('1*A');
    2: ST.SendGetText('2*A');
  else
    ST.SendGetText('A');
  end;
end;

procedure TWindow1.AutoInputEvent(Sender: TObject);
begin
  case AutoInput.Active of
    1: ST.SendGetText('W1AUSW'#13);
    2: ST.SendGetText('W2AUSW'#13);
  else
    ST.SendGetText('W0AUSW'#13);
  end;
end;

procedure TWindow1.TestPattEvent(Sender: TObject);
begin
  writeln(ST.SendGetText('W' + IntToStr(TestPatt.Active) + 'TEST'#13));
end;

end.
