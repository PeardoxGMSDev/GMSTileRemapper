unit TileBitmapMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Colors, FMX.Layouts;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    ColorButton4: TColorButton;
    ColorButton3: TColorButton;
    ColorButton2: TColorButton;
    ColorButton5: TColorButton;
    ColorButton1: TColorButton;
    ColorButton6: TColorButton;
    ColorButton7: TColorButton;
    ColorButton8: TColorButton;
    Mask: TLabel;
    MVal: TLabel;
    Switch1: TSwitch;
    SwitchLabel: TLabel;
    MBin: TLabel;
    procedure ColorButton4Click(Sender: TObject);
    procedure ColorButton3Click(Sender: TObject);
    procedure ColorButton2Click(Sender: TObject);
    procedure ColorButton5Click(Sender: TObject);
    procedure ColorButton1Click(Sender: TObject);
    procedure ColorButton6Click(Sender: TObject);
    procedure ColorButton7Click(Sender: TObject);
    procedure ColorButton8Click(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
      FMaskValue: BYTE;
      ExitMode: Boolean;
      function GetMaskValue: BYTE;
      procedure SetMaskValue(AValue: BYTE);
      procedure UpdateMark;
  public
    { Public declarations }
      property MaskValue: BYTE read GetMaskValue write SetMaskValue;
  end;

var
  Form1: TForm1;

function IntToBinary8(Value: Integer): string;

implementation

{$R *.fmx}

function IntToBinary8(Value: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 7 downto 0 do
  begin
    if (Value and (1 shl i)) <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
  end;
end;

procedure TForm1.ColorButton1Click(Sender: TObject);
const
  B: Byte = $01;
begin

  if ColorButton1.Color = $FFFFFFFF then
    begin
      ColorButton1.Color := $FF000000;
      MaskValue := MaskValue OR B
    end
  else
    begin
      ColorButton1.Color := $FFFFFFFF;
      MaskValue := MaskValue AND (not B);
    end;

  UpdateMark;
end;

procedure TForm1.ColorButton2Click(Sender: TObject);
const
  B: Byte = $02;
begin
  if ColorButton2.Color = $FFFFFFFF then
    begin
      ColorButton2.Color := $FF000000;
      MaskValue := MaskValue OR B
    end
  else
    begin
      ColorButton2.Color := $FFFFFFFF;
      MaskValue := MaskValue AND (not B);
    end;

  UpdateMark;
end;

procedure TForm1.ColorButton3Click(Sender: TObject);
const
  B: Byte = $04;
begin
  if ColorButton3.Color = $FFFFFFFF then
    begin
      ColorButton3.Color := $FF000000;
      MaskValue := MaskValue OR B
    end
  else
    begin
      ColorButton3.Color := $FFFFFFFF;
      MaskValue := MaskValue AND (not B);
    end;

  UpdateMark;
end;

procedure TForm1.ColorButton4Click(Sender: TObject);
const
  B: Byte = $08;
begin
  if ColorButton4.Color = $FFFFFFFF then
    begin
      ColorButton4.Color := $FF000000;
      MaskValue := MaskValue OR B
    end
  else
    begin
      ColorButton4.Color := $FFFFFFFF;
      MaskValue := MaskValue AND (not B);
    end;

  UpdateMark;
end;

procedure TForm1.ColorButton5Click(Sender: TObject);
const
  B: Byte = $10;
begin
  if ColorButton5.Color = $FFFFFFFF then
    begin
      ColorButton5.Color := $FF000000;
      MaskValue := MaskValue OR B
    end
  else
    begin
      ColorButton5.Color := $FFFFFFFF;
      MaskValue := MaskValue AND (not B);
    end;

  UpdateMark;
end;

procedure TForm1.ColorButton6Click(Sender: TObject);
const
  B: Byte = $20;
begin
  if ColorButton6.Color = $FFFFFFFF then
    begin
      ColorButton6.Color := $FF000000;
      MaskValue := MaskValue OR B
    end
  else
    begin
      ColorButton6.Color := $FFFFFFFF;
      MaskValue := MaskValue AND (not B);
    end;

  UpdateMark;
end;

procedure TForm1.ColorButton7Click(Sender: TObject);
const
  B: Byte = $40;
begin
  if ColorButton7.Color = $FFFFFFFF then
    begin
      ColorButton7.Color := $FF000000;
      MaskValue := MaskValue OR B
    end
  else
    begin
      ColorButton7.Color := $FFFFFFFF;
      MaskValue := MaskValue AND (not B);
    end;

  UpdateMark;
end;

procedure TForm1.ColorButton8Click(Sender: TObject);
const
  B: Byte = $80;
begin
  if ColorButton8.Color = $FFFFFFFF then
    begin
      ColorButton8.Color := $FF000000;
      MaskValue := MaskValue OR B
    end
  else
    begin
      ColorButton8.Color := $FFFFFFFF;
      MaskValue := MaskValue AND (not B);
    end;

  UpdateMark;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ExitMode := True;
  MaskValue := 0;
  Switch1Switch(Nil);
  UpdateMark;
end;

function TForm1.GetMaskValue: BYTE;
begin
  Result := FMaskValue;
end;

procedure TForm1.SetMaskValue(AValue: BYTE);
begin
  FMaskValue := AValue;
end;

procedure TForm1.Switch1Switch(Sender: TObject);
begin
  If Switch1.IsChecked then
    begin
      SwitchLabel.Text := 'Wall';
      ExitMode := False;
    end
  else
    begin
      SwitchLabel.Text := 'Exit';
      ExitMode := True;
    end;

  UpdateMark;
end;

procedure TForm1.UpdateMark;
begin
  if ExitMode then
    begin
      Mask.Text := Format('%x', [not MaskValue]);
      MVal.Text := Format('%d', [not MaskValue]);
      MBin.Text := Format('%s', [IntToBinary8(not MaskValue)]);
    end
  else
    begin
      Mask.Text := Format('%x', [MaskValue]);
      MVal.Text := Format('%d', [MaskValue]);
      MBin.Text := Format('%s', [IntToBinary8(MaskValue)]);
    end;
end;

end.
