unit Window1Unit;
{$mode objfpc}{$H+}
interface
uses
  MUIClass.Base, MUIClass.Window, MUIClass.Menu, MUIClass.Group, MUIClass.Area,
  MUIClass.Image,
  serialunit, sysutils, MUI;

{$define Debugout}

type
	TValidList = class
	private
		List: array of record
	    Index: Integer;
	    Res: Integer;
	    Freq: Integer;
	  end;
	public
    procedure Add(AIndex: Integer; ARes, AFreq: Integer);
    function GetIndex(ARes, AFreq: Integer): Integer;
    function GetFreq(Index: Integer): Integer;	
	  function GetRes(Index: Integer): Integer;	
	end;

const
  Freqs: array[0..8] of string = 
	  ('23.98 Hz',
		 '24 Hz',
		 '25 Hz',
		 '29.97 Hz',
		 '30 Hz',
		 '50 Hz',
		 '59.94 Hz',
		 '60 Hz',
		 '75 Hz');
  Resolutions: array[0..25] of string = 
	  ( '640x480',     // 0
			'800x600',     // 1 
      '852x480',     // 2
			'1024x768',    // 3
			'1024x852',    // 4
			'1024x1024',   // 5
			'1280x768',    // 6
			'1280x800',    // 7
			'1280x1024',   // 8
			'1360x765',    // 9
			'1360x768',    // 10
			'1365x768',    // 11
			'1366x768',    // 12
			'1365x1024',   // 13
			'1440x900',    // 14
			'1400x1050',   // 15
			'1600x900',    // 16
			'1680x1050',   // 17
			'1600x1200',   // 18
			'1920x1200',   // 19
			'480p',        // 10
			'576p',        // 21
			'720p',        // 22
			'1080i',       // 23
			'1080p',       // 24
			'2048x1080 2k' // 25
			);


type
  TWindow1 = class(TMUIWindow)
  private
    CurImg: Integer;
  public
    IdleTimer: TMUITimer;
    Timer: TMUITimer;
    Group1: TMUIGroup;
	  Group4: TMUIGroup;
    Group3: TMUIGroup;
    Switch1: TMUIButton;
    Switch2: TMUIButton;
    Switch3: TMUIButton;
    Group2: TMUIGroup;
    StatusText: TMUIText;
    ChooseAuto: TMUICycle;
    AutoInput: TMUICycle;
    TestPatt: TMUICycle;
	  ChooseResolution: TMUICycle;
	  ChooseFreq: TMUICycle;
	  SetRes, GetRes: TMUIButton;
	  ValidList: TValidList;
	  FreezeBtn: TMUIButton;
	  Exec: array[0..2] of TMUIButton;
    procedure Window1Show(Sender: TObject);
    procedure Switch1Click(Sender: TObject);
    procedure Switch2Click(Sender: TObject);
    procedure Switch3Click(Sender: TObject);
    constructor Create; override;
		destructor Destroy; override;
    procedure IdleEvent(Sender: TObject);
    procedure TimerEvent(Sender: TObject);
    procedure AutoClick(Sender: TObject);
    procedure AutoInputEvent(Sender: TObject);
    procedure TestPattEvent(Sender: TObject);
		procedure FreqResEvent(Sender: TObject);
		procedure SetResEvent(Sender: TObject);
		procedure GetResEvent(Sender: TObject);
		procedure FreezeClickEvent(Sender: TObject);
		procedure ExecClick(Sender: TObject);
  end;
var
  Window1: TWindow1;
	Showing: Boolean = False;
implementation

procedure TValidList.Add(AIndex: Integer; ARes, AFreq: Integer);
var
	Idx: Integer;
begin
	if AIndex < 0 then
	  Exit;
  Idx := Length(List);
	SetLength(List, Idx + 1);
	List[Idx].Index := AIndex;
	List[Idx].Res := ARes;
  List[Idx].Freq := AFreq;	
end;

function TValidList.GetIndex(ARes, AFreq: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(List) do
  begin
    if (List[i].Res = ARes) and (List[i].Freq = AFreq) then
    begin
			Result := List[i].Index;
      Break;			
    end;			
  end;		
end;

function TValidList.GetFreq(Index: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
	if Index < 0 then
	  Exit;
	if Index < 0 then
	  Exit;
  for i := 0 to High(List) do
  begin
    if List[i].Index = Index then
    begin
			Result := List[i].Freq;
      Break;			
    end;			
  end;		
end;

function TValidList.GetRes(Index: Integer): Integer;	
var
  i: Integer;
begin
  Result := -1;
	if Index < 0 then
	  Exit;
  for i := 0 to High(List) do
  begin
    if List[i].Index = Index then
    begin
			Result := List[i].Res;
      Break;			
    end;			
  end;		
end;


procedure ParseMessage(Msg: string);
var
  i,j, n: Integer;
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
  if Copy(Msg, 1, 3) = 'Img' then
  begin
    Window1.StatusText.Contents := 'Auto adjust done.';
    Exit;
  end;
	if Copy(Msg, 1, 3) = 'Frz' then
  begin
		i := StrToIntDef(Msg[4], 0);
		Window1.FreezeBtn.Selected := i = 1;
		ParseMessage('In' + ST.SendGetText('!'));
    Exit;
  end;
	if Copy(Msg, 1, 3) = 'Exe' then
  begin
		j := StrToIntDef(Msg[4], 0);
    for i := 0 to High(Window1.Exec) do
    begin
			Window1.Exec[i].Selected := i = j;
    end;			
    Exit;
  end;
	if Copy(Msg, 1, 4) = 'Stat' then
  begin
    Window1.StatusText.Contents := 'Temperature: ' + Copy(Msg, 5, 2) + '°C';
    //writeln();
		Exit;
  end;


	if Copy(Msg, 1, 4) = 'Rate' then
  begin
    i := StrToIntDef(Copy(Msg, 5,2), -1);
		j := Window1.ValidList.GetRes(i);
		n := Window1.ValidList.GetFreq(i);
		if j >= 0 then
			Window1.ChooseResolution.Active := j;
		if n >= 0 then
			Window1.ChooseFreq.Active := n;
		Window1.GetRes.Disabled := True;
    Exit;
  end;
	if Length(Msg) = 5 then
	begin
		if (Msg[2] = '*') or (Msg[4] = '*') then
		begin
			ParseMessage('In' + Msg);
			Exit;
    end;			
  end;		
	{$ifdef Debugout}
	writeln('Msg: "' + Msg + '"');
	{$endif}
  Window1.StatusText.Contents := 'Msg: "' + Msg + '"';
end;

constructor TWindow1.Create;
var
  Group: TMUIGroup;
begin
  inherited;
	ValidList := TValidList.Create;
	ValidList.Add(10, 0, 5);
	ValidList.Add(11, 0, 7);
	ValidList.Add(12, 0, 8);
	ValidList.Add(13, 1, 5);
	ValidList.Add(14, 1, 7);
	ValidList.Add(15, 1, 8);
	ValidList.Add(16, 2, 5);
	ValidList.Add(17, 2, 7);
	ValidList.Add(18, 2, 8);
	ValidList.Add(19, 3, 5);
	ValidList.Add(20, 3, 7);
	ValidList.Add(21, 3, 8);
	ValidList.Add(22, 4, 5);
	ValidList.Add(23, 4, 7);
	ValidList.Add(24, 4, 8);
	ValidList.Add(25, 5, 5);
	ValidList.Add(26, 5, 7);
	ValidList.Add(27, 5, 8);
	ValidList.Add(28, 6, 5);
	ValidList.Add(29, 6, 7);
	ValidList.Add(30, 6, 8);
	ValidList.Add(31, 7, 5);
	ValidList.Add(32, 7, 7);
	ValidList.Add(33, 7, 8);
	ValidList.Add(34, 8, 5);
	ValidList.Add(35, 8, 7);
	ValidList.Add(36, 8, 8);
	ValidList.Add(37, 9, 5);
	ValidList.Add(38, 9, 7);
	ValidList.Add(39, 9, 8);
	ValidList.Add(40, 10, 5);
	ValidList.Add(41, 10, 7);
	ValidList.Add(42, 10, 8);
	ValidList.Add(43, 11, 5);
	ValidList.Add(44, 11, 7);
	ValidList.Add(45, 11, 8);
	ValidList.Add(46, 12, 5);
	ValidList.Add(47, 12, 7);
	ValidList.Add(48, 12, 8);
	ValidList.Add(49, 13, 5);
	ValidList.Add(50, 13, 7);
	ValidList.Add(51, 13, 8);
	ValidList.Add(52, 14, 5);
	ValidList.Add(53, 14, 7);
	ValidList.Add(54, 14, 8);
	ValidList.Add(55, 15, 5);
	ValidList.Add(56, 15, 7);
	ValidList.Add(57, 16, 5);
	ValidList.Add(58, 16, 7);
	ValidList.Add(59, 17, 5);
	ValidList.Add(60, 17, 7);
	ValidList.Add(61, 18, 5);
	ValidList.Add(62, 18, 7);
	ValidList.Add(63, 19, 5);
	ValidList.Add(64, 19, 7);
	ValidList.Add(65, 20, 6);
	ValidList.Add(66, 20, 7);
	ValidList.Add(67, 21, 5);
	ValidList.Add(68, 22, 2);
	ValidList.Add(69, 22, 3);
  ValidList.Add(70, 22, 4);
	ValidList.Add(71, 22, 5);
	ValidList.Add(72, 22, 6);
	ValidList.Add(73, 22, 7);
  ValidList.Add(74, 23, 5);
	ValidList.Add(75, 23, 6);
	ValidList.Add(76, 23, 7);
  ValidList.Add(77, 24, 0);
  ValidList.Add(78, 24, 1);
	ValidList.Add(79, 24, 2);
  ValidList.Add(80, 24, 3);
  ValidList.Add(81, 24, 4);
  ValidList.Add(82, 24, 5);
  ValidList.Add(83, 24, 6);
  ValidList.Add(84, 24, 7);
  ValidList.Add(85, 25, 0);
  ValidList.Add(86, 25, 1);
	ValidList.Add(87, 25, 2);
  ValidList.Add(88, 25, 3);
  ValidList.Add(89, 25, 4);
  ValidList.Add(90, 25, 5);
  ValidList.Add(91, 25, 6);
  ValidList.Add(92, 25, 7);

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
	
  Group4 := TMUIGroup.Create;
  with Group4 do
  begin
    Horiz := True;
    Parent := Self;
  end;
  
  with TMUIText.Create('Output resolution:') do
  begin
    Frame := 0;
    PreParse := #27'r';
    Parent := Group4;
  end;
	
	ChooseResolution := TMUICycle.Create;
  with ChooseResolution do
  begin
    Entries := Resolutions;
    Parent := Group4;
  end;
	
	ChooseFreq := TMUICycle.Create;
  with ChooseFreq do
  begin
    Entries := Freqs;
		OnActiveChange := @FreqResEvent;
    Parent := Group4;
  end;
	
	SetRes := TMUIButton.Create;
	with SetRes do
	begin
		Contents := 'Set';  
    Parent := Group4;
		Disabled := True;
		OnClick := @SetResEvent;
  end;	
	
	GetRes := TMUIButton.Create;
	with GetRes do
	begin
		Contents := 'Get';  
    Parent := Group4;
		Disabled := False;
		OnClick := @GetResEvent;
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
	
	FreezeBtn := TMUIButton.Create;
	with FreezeBtn do
	begin
		Contents := 'Freeze';
		InputMode := MUIV_InputMode_Toggle;
    OnSelected := @FreezeClickEvent;
		Parent := Group3;
  end;		
	
	Group := TMUIGroup.Create;
	with Group do
  begin
    Horiz := True;
    Parent := Self;
  end;
	
	with TMUIText.Create('Lock Buttons:') do
  begin
    Frame := 0;
    PreParse := #27'r';
    Parent := Group;
  end;
	
	Exec[0] := TMUIButton.Create;
	with Exec[0] do
	begin
		Contents := 'Unlock';
    InputMode := MUIV_InputMode_Immediate;
    OnSelected := @ExecClick;
		Tag := 0;
    Parent := Group;
  end;

	Exec[1] := TMUIButton.Create;
	with Exec[1] do
	begin
		Contents := 'Lock All';
    InputMode := MUIV_InputMode_Immediate;
    OnSelected := @ExecClick;
		Tag := 1;
    Parent := Group;
  end;

  Exec[2] := TMUIButton.Create;
	with Exec[2] do
	begin
		Contents := 'Lock Menu';
    InputMode := MUIV_InputMode_Immediate;
    OnSelected := @ExecClick;
		Tag := 2;
    Parent := Group;
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

destructor TWindow1.Destroy;
begin
	ValidList.Free;  
	inherited;
end;


procedure TWindow1.Window1Show(Sender: TObject);
begin

  // Get current Input channel
  ParseMessage('In' + ST.SendGetText('!'));
  // Get AutoInput modus
  ParseMessage('Ausw' + ST.SendGetText('WAUSW'#13));
  // is a test image active?
  ParseMessage('Test' + ST.SendGetText('WTEST'#13));
	
	// Get Resolution and Frequency
  ParseMessage('Rate' + ST.SendGetText(#27'RATE'#13));
	
	// Freeze Status
  ParseMessage('Frz' + ST.SendGetText('F'));
  
	// Exec Status
  ParseMessage('Exec' + ST.SendGetText('X'));
  
	//
  StatusText.Contents := Trim(ST.SendGetText('0Q'));
	

  Timer.Enabled := True;
  IdleTimer.Enabled := True;
	Showing := True;
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
	if Sender <> nil then
    ParseMessage('Stat' + ST.SendGetText(#27'20Stat'#13));
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
  ST.SendGetText('W' + IntToStr(TestPatt.Active) + 'TEST'#13);
end;

procedure TWindow1.FreqResEvent(Sender: TObject);
begin
	SetRes.Disabled := ValidList.GetIndex(ChooseResolution.Active, ChooseFreq.Active) < 0;
	GetRes.Disabled := False;
end;

procedure TWindow1.SetResEvent(Sender: TObject);
var
  Idx: Integer;
begin
	Idx := ValidList.GetIndex(ChooseResolution.Active, ChooseFreq.Active);
	if Idx > 0 then
	  ST.SendGetText(#27 + IntToStr(Idx) + 'RATE'#13);
end;

procedure TWindow1.GetResEvent(Sender: TObject);
begin
	writeln('Get');
  ParseMessage('Rate' + ST.SendGetText(#27'RATE'#13));	
end;

procedure TWindow1.FreezeClickEvent(Sender: TObject);
begin
	if FreezeBtn.Selected then
		
		ST.SendGetText('1F')
	else
    ST.SendGetText('0F');
end;

procedure TWindow1.ExecClick(Sender: TObject);
var
  i: Integer;
begin
	if not TMUIButton(Sender).Selected then
	  Exit;
	for i := 0 to High(Exec) do
	begin
		if (Exec[i] = Sender) and (Exec[i].Selected) then
		begin	
			Exec[i].Selected := True;
      ST.SendGetText(IntToStr(TMUIButton(Sender).Tag) + 'X')
    end
		else
      Exec[i].Selected := False;			
  end;		
  
end;

end.
