unit mpMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterAny, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls, ActnList, IniFiles, RegExpr;

type

  { TFormMain }

  TFormMain = class(TForm)
    acPing: TAction;
    acOptions: TAction;
    ActionList1: TActionList;
    cbIpAddress: TComboBox;
    ImageList1: TImageList;
    pbStatus: TPaintBox;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure acOptionsExecute(Sender: TObject);
    procedure acPingExecute(Sender: TObject);
    procedure acPingUpdate(Sender: TObject);
    procedure cbIpAddressKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pbStatusPaint(Sender: TObject);
  private
    FConfigFile: string;
    FIpSegment: string;
    FPingIpCount: integer;
    FError: TStrings;
    FStatusText: string;
    FStartTick: int64;
    FStatus: array[1..254] of Boolean;
    FAutoSize: boolean;
    FColor1, FColor2, FColor3: TColor;
    FCharWidth: integer;
    FIpWidth: integer;
    FCharHeight: integer;
    FLineHeight: integer;
    procedure LogPingStatus(constip: string; status: boolean);
    procedure AutoResizeWindow;
    procedure PaintEmpty(pb: TPaintBox);
    procedure PaintError(pb: TPaintBox);
    procedure PaintProgress(pb: TPaintBox);
    procedure PaintIPList(pb: TPaintBox);
    procedure UpdateCharSize;
    procedure LoadConfig;
    procedure SaveConfig;
    function ValidateIpSegment(const ipseg: string): boolean;
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

uses mpClasses, mpOptions;

const
  csFontTestText = '823';
  ciLineHeightRate = 1.5;
  ciTopOffset = 10;
  ciLeftOffset = 10;
  ciBottomOffset = 10;
  ciRightOffset = 30;
  ciLineOffset = 5;
  ciIpCols = 16;
  ciIpRows = 16;
  ciTitleRows = 1.5;
  ciTimeoutSeconds = 30;

{ TFormMain }

procedure TFormMain.AutoResizeWindow;
var
  w, h: integer;
begin
  w := ciLeftOffset + FIpWidth * ciIpCols + ciRightOffset - pbStatus.ClientWidth;
  h := ciTopOffset + Round(FLineHeight * (ciIpRows + ciTitleRows)) + ciBottomOffset - pbStatus.ClientHeight;
  if (w <> 0) or (h <> 0) then
  begin
    Width := Width + w;
    Height := Height + h;
  end;
end;

procedure TFormMain.PaintEmpty(pb: TPaintBox);
begin
  with pb do
  begin
    // Draw Background
    Canvas.Brush.Color := FColor3;
    Canvas.FillRect(0, 0, ClientWidth - 1, ClientHeight - 1);
  end;
end;

procedure TFormMain.PaintError(pb: TPaintBox);
var
  x, y, i: integer;
begin
  PaintEmpty(pb);
  with pb do
  begin
    y := ciTopOffset;
    x := ciLeftOffset;
    // Draw title
    Canvas.Font.Color := FColor1;
    Canvas.TextOut(x, y, 'Error: ' + FError[0]);
    inc(y, FLineHeight);
    // Draw title line
    Canvas.Pen.Color := FColor1;
    Canvas.Line(ciLineOffset, y, ClientWidth - ciLineOffset - 1, y);
    inc(y, FLineHeight div 2);
    // Draw Status Text
    Canvas.Font.Color := FColor2;
    for i := 1 to FError.Count - 1 do
    begin
      Canvas.TextOut(x, y, FError[i]);
      inc(y, FLineHeight);
    end;
  end;
end;

procedure TFormMain.PaintProgress(pb: TPaintBox);
var
  y: integer;
  x: integer;
begin
  PaintEmpty(pb);
  with pb do
  begin
    y := ciTopOffset;
    x := ciLeftOffset;
    // Draw title
    Canvas.Font.Color := FColor1;
    Canvas.TextOut(x, y, 'Ping ' + FIpSegment + '.* :');
    inc(y, FLineHeight);
    // Draw title line
    Canvas.Pen.Color := FColor1;
    Canvas.Line(ciLineOffset, y, ClientWidth - ciLineOffset - 1, y);
    inc(y, FLineHeight div 2);
    // Draw Status Text
    Canvas.TextOut(x, y, FStatusText);
  end;
end;

procedure TFormMain.PaintIPList(pb: TPaintBox);
var
  s: string;
  y: integer;
  x1: integer;
  x: integer;
  i: integer;
begin
  PaintEmpty(pb);
  with pb do
  begin
    if acPing.Enabled and (FIpSegment <> '') then
    begin
      y := ciTopOffset;
      x := ciLeftOffset;
      // Draw title
      Canvas.Font.Color := FColor1;
      Canvas.TextOut(x, y, 'Ping ' + FIpSegment + '.* :');
      inc(y, FLineHeight);
      // Draw title line
      Canvas.Pen.Color := FColor1;
      Canvas.Line(ciLineOffset, y, ClientWidth - ciLineOffset - 1, y);
      inc(y, FLineHeight div 2);
      // Draw IP map
      for i := 1 to 254 do
      begin
        if FStatus[i] then Canvas.Font.Color := FColor1
        else Canvas.Font.Color := FColor2;
        s := IntToStr(i);
        x1 := x + FIpWidth - Canvas.TextWidth(s);
        Canvas.TextOut(x1, y, s);
        if (i > 1) and (i mod ciIpCols = 0) then
        begin
          x := ciLeftOffset;
          inc(y, FLineHeight);
        end
        else inc(x, FIpWidth);
      end;
    end;
  end;
end;

procedure TFormMain.UpdateCharSize;
begin
  with pbStatus.Canvas do
  begin
    FCharWidth := TextWidth(csFontTestText) div Length(csFontTestText);
    FIpWidth := FCharWidth * 4;
    FCharHeight := TextHeight(csFontTestText);
    FLineHeight := Round(FCharHeight * ciLineHeightRate);
  end;
end;

procedure TFormMain.pbStatusPaint(Sender: TObject);
var
  pb: TPaintBox;
begin
  pb := TPaintBox(Sender);
  if FError.Count > 0 then PaintError(pb)
  else if FPingIpCount < 0 then PaintEmpty(pb)
  else if FPingIpCount < 254 then PaintProgress(pb)
  else PaintIPList(pb);
end;

procedure TFormMain.cbIpAddressKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Key := #0;
    acPing.Execute;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FError := TStringList.Create;
  FPingIpCount := -2;
  FConfigFile := ChangeFileExt(Application.ExeName, '.cfg');
  LoadConfig;
  UpdateCharSize;
  AutoResizeWindow;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  SaveConfig;
  FError.Free;
end;

procedure TFormMain.acPingExecute(Sender: TObject);
var
  tick: QWord;
  len, i: integer;
  s: string;
  procedure UpdateStatus;
  begin
    FStatusText := Format(' %5.1f%% completed, used %5.2f seconds.',
      [FPingIpCount * 100 / 254, (GetTickCount64 - FStartTick) / 1000]);
    StatusBar.SimpleText := FStatusText;
    pbStatus.Invalidate;
  end;
begin
  FError.Clear;
  FStartTick := GetTickCount64;
  FIpSegment := cbIpAddress.Text;
  if ValidateIpSegment(FIpSegment) then
  begin
    len := Length(FIpSegment);
    if Copy(FIpSegment, len - 1, 2) = '.*' then
      FIpSegment := Copy(FIpSegment, 1, len - 2);
    s := FIpSegment + '.*';
    i := cbIpAddress.Items.IndexOf(s);
    if i < 0 then cbIpAddress.Items.Insert(0, s)
    else if i > 0 then cbIpAddress.Items.Move(i, 0);
    cbIpAddress.ItemIndex := 0;
    pbStatus.Invalidate;
    Application.ProcessMessages;
    FPingIpCount := 0;
    PingIpSeg(FIpSegment, @LogPingStatus);
    while FPingIpCount < 254 do
    begin
      tick := GetTickCount64 - FStartTick;
      if tick > ciTimeoutSeconds * 1000 then
      begin
        FPingIpCount := -1;
        FError.Add('%d seconds timeout!', [ciTimeoutSeconds]);
        pbStatus.Invalidate;
        Exit;
      end;
      if tick mod 1000 = 0 then UpdateStatus;
      Application.ProcessMessages;
    end;
    UpdateStatus;
  end;
end;

procedure TFormMain.acOptionsExecute(Sender: TObject);
begin
  if FormOptions.GetOptions(FColor1, FColor2, FColor3, pbStatus.Font, FAutoSize) then
  begin
    UpdateCharSize;
    AutoResizeWindow;
    pbStatus.Invalidate;
  end;
end;

procedure TFormMain.acPingUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (FPingIpCount < 0) or (FPingIpCount > 253);
end;

procedure TFormMain.LogPingStatus(const ip: string; status: boolean);
var
  ipb: string;
begin
  ipb := Copy(ip, Length(FIpSegment) + 2, 3);
  FStatus[StrToInt(ipb)] := status;
  inc(FPingIpCount);
end;

procedure TFormMain.LoadConfig;
var
  ini: TIniFile;
begin
  ini := TIniFile.Create(FConfigFile);
  try
    FColor1 := ini.ReadInteger('Colors', 'PingOK', clYellow);
    FColor2 := ini.ReadInteger('Colors', 'PingFailed', clGray);
    FColor3 := ini.ReadInteger('Colors', 'Background', clNavy);
    pbStatus.Font.Name := ini.ReadString('Font', 'Name', 'Consolas');
    pbStatus.Font.Size := ini.ReadInteger('Font', 'Size', 14);
    FAutoSize := ini.ReadBool('Main', 'AutoSize', true);
    ini.ReadSection('Ip4Address', cbIpAddress.Items);
    if cbIpAddress.Items.Count > 0 then
      cbIpAddress.ItemIndex := 0;
  finally
    ini.Free;
  end;
end;

procedure TFormMain.SaveConfig;
var
  ini: TIniFile;
  i: integer;
begin
  if FileExists(FConfigFile) then DeleteFile(FConfigFile);
  ini := TIniFile.Create(FConfigFile);
  try
    ini.WriteInteger('Colors', 'PingOK', FColor1);
    ini.WriteInteger('Colors', 'PingFailed', FColor2);
    ini.WriteInteger('Colors', 'Background', FColor3);
    ini.WriteString('Font', 'Name', pbStatus.Font.Name);
    ini.WriteInteger('Font', 'Size', pbStatus.Font.Size);
    ini.WriteBool('Main', 'AutoSize', FAutoSize);
    for i := 0 to cbIpAddress.Items.Count - 1 do
      ini.WriteString('Ip4Address', cbIpAddress.Items[i], '1');
  finally
    ini.Free;
  end;
end;

function TFormMain.ValidateIpSegment(const ipseg: string): boolean;
const
  csExpr = '^' + // line start
    '(((\d{1,2})|([0-1]\d\d)|(2[0-4]\d)|(25[0-4]))\.){2}' + // two digit part(0-254), e.g. "12.234."
    '((\d{1,2})|([0-1]\d\d)|(2[0-4]\d)|(25[0-4]))' + // one digit part(0-254), e.g. "123"
    '(\.\*){0,1}' + // option wild part, ".*"
    '$'; // line end
begin
  Result := ExecRegExpr(csExpr, ipseg);
  if not Result then
  begin
    FError.Add('Bad ip segment format - "' + ipseg + '"!');
    FError.Add('  Valid format e.g. "192.168.81.*", "192.168.81"');
    pbStatus.Invalidate;
  end;
end;

end.

