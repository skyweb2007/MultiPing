unit mpOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ColorBox,
  StdCtrls, ExtCtrls;

type

  { TFormOptions }

  TFormOptions = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    cbBackground: TColorBox;
    cbPingOK: TColorBox;
    cbPingFailed: TColorBox;
    cbAutoSize: TCheckBox;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    mmDemo: TMemo;
    procedure Button3Click(Sender: TObject);
  private

  public
    function GetOptions(var Color1, Color2, Color3: TColor;
      const AFont: TFont; var AAutoSize: boolean): boolean;
  end;

var
  FormOptions: TFormOptions;

implementation

{$R *.lfm}

{ TFormOptions }

procedure TFormOptions.Button3Click(Sender: TObject);
begin
  FontDialog1.Font.Assign(mmDemo.Font);
  if FontDialog1.Execute then
    mmDemo.Font.Assign(FontDialog1.Font);
end;

function TFormOptions.GetOptions(var Color1, Color2, Color3: TColor;
  const AFont: TFont; var AAutoSize: boolean): boolean;
begin
  cbPingOK.Selected := Color1;
  cbPingFailed.Selected := Color2;
  cbBackground.Selected := Color3;
  mmDemo.Font.Assign(AFont);
  cbAutoSize.Checked := AAutoSize;
  Result := ShowModal = mrOK;
  if Result then
  begin
    Color1 := cbPingOK.Selected;
    Color2 := cbPingFailed.Selected;
    Color3 := cbBackground.Selected;
    AFont.Assign(mmDemo.Font);
    AAutoSize := cbAutoSize.Checked;
  end;
end;

end.

