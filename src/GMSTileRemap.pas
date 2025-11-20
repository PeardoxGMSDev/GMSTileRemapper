unit GMSTileRemap;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Ani,
  FMX.ListBox, FMX.Menus;

type
  TRemapArray = Array[0..47] of Integer;

  TBitmapFit = record
    GoodFit: Boolean;
    RemapID: Integer;
    Width: Integer;
    Height: Integer;
  end;

  TForm1 = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    OriginalImage: TImage;
    Button1: TButton;
    Splitter1: TSplitter;
    OutputImage: TImage;
    Layout5: TLayout;
    Layout6: TLayout;
    Label1: TLabel;
    OriginalImageGrid: TImage;
    OutputImageGrid: TImage;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    MainMenu1: TMainMenu;
    MenuFile: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    MenuInDir: TMenuItem;
    MenuOutDir: TMenuItem;
    SaveDialog1: TSaveDialog;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
  private
    { Private declarations }
    Resizing: Boolean;
    procedure LoadTile(fn: String);
    function IsImageCorrect(b: TBitmap; Width, Height: Integer): TBitmapFit;
    function RewmapBitmap(InBmp, OutBmp: TBitmap; Remap: TRemapArray; SizeX,
      SizeY: Integer): Boolean;
  public
    { Public declarations }
  end;
var
  Form1: TForm1;

const
  TileX: Integer = 64;
  TileY: Integer = 64;
  TileTransGrid: Integer = 4;
  Remaps: Array[0..2] of TRemapArray = (
    (
    36, 38, 33, 24, 32, 46, 45, 16,
    42, 40, 34, 20, 28, 43, 44, 35,
     5,  9, 13,  4, 17, 25, 48, 41,
     3,  2,  7, 10, 21, 29, 47, 37,
    12,  8, 18, 27, 23, 22,  6, 39,
    14, 15, 19, 26, 30, 31, 11,  1
    ),(
    36, 38, 33, 24, 32, 46, 45, 16,
    42, 40, 34, 20, 28, 43, 44, 35,
     5,  9, 13,  4, 17, 25,  1, 41,
     3,  2,  7, 10, 21, 29, 47, 37,
    12,  8, 18, 27, 23, 22,  6, 39,
    14, 15, 19, 26, 30, 31, 11, 48
    ),(
    48,  1,  2,  3,  4,  5,  6,  7,
     8,  9, 10, 11, 12, 13, 14, 15,
    16, 17, 18, 19, 20, 21, 22, 23,
    24, 25, 26, 27, 28, 29, 30, 31,
    32, 33, 34, 35, 36, 37, 38, 39,
    40, 41, 42, 43, 44, 45, 46, 47
    ));


function MakeGrid(const SizeX, SizeY, GridX, GridY: Integer; const ZeroTransparent: Boolean = False): TBitmap;

implementation


{$R *.fmx}

procedure SetLayoutColorsWithRectangle(ALayout: TLayout; AForegroundColor, ABackgroundColor: TAlphaColor);
var
  BackgroundRect: TRectangle;
begin
  if not Assigned(ALayout) then
    Exit;

  // Remove existing background rectangle if present
  if ALayout.FindComponent('LayoutBackground') <> nil then
    ALayout.FindComponent('LayoutBackground').Free;

  // Create background rectangle
  BackgroundRect := TRectangle.Create(ALayout);
  BackgroundRect.Name := 'LayoutBackground';
  BackgroundRect.Parent := ALayout;
  BackgroundRect.Align := TAlignLayout.Client;
  BackgroundRect.SendToBack;
  BackgroundRect.HitTest := False;

  // Set background color
  BackgroundRect.Fill.Kind := TBrushKind.Solid;
  BackgroundRect.Fill.Color := ABackgroundColor;

  // Set border color
  BackgroundRect.Stroke.Kind := TBrushKind.Solid;
  BackgroundRect.Stroke.Color := AForegroundColor;
  BackgroundRect.Stroke.Thickness := 1;
end;

function TForm1.IsImageCorrect(b: TBitmap; Width: Integer; Height: Integer): TBitmapFit;
begin
  Result := Default(TBitmapFit);
  if (b.Width < 1) then
    exit;
  if (b.Height < 1) then
    exit;
  if ((b.Width mod Width) <> 0) then
    exit;
  if ((b.Height mod Height) <> 0) then
    exit;

  Result.Width := b.Width div Width;
  Result.Height := b.Height div Height;

  if((Result.Width * Result.Height) <> 48) then
    exit;


  Result.RemapID := ComboBox1.ItemIndex;

  Result.GoodFit := True;

end;

function OutMap(idx, sx, sy: Integer): TRect;
var
  x,y: Integer;
begin
  x := idx mod 7;
  y := idx div 7;

  Result := Rect(x * sx, y * sy, sx + (x * sx), sy + (y * sy));
end;

function InMap(idx, sx, sy, tx, ty: Integer): TRect;
var
  x,y: Integer;
begin
  x := idx mod tx;
  y := idx div tx;

  Result := Rect(x * sx, y * sy, sx + (x * sx), sy + (y * sy));
end;

function TForm1.RewmapBitmap(InBmp, OutBmp: TBitmap; Remap: TRemapArray; SizeX, SizeY: Integer): Boolean;
var
  surf: TCanvas;
  inrect, outrect: TRect;
  ingrid: TBitmapFit;
  I: Integer;
begin
  Result := False;
  ingrid := IsImageCorrect(inbmp, SizeX, SizeY);
  if not ingrid.GoodFit then
    Exit;
  surf := OutBmp.Canvas;
  surf.BeginScene;
  for I := 0 to 47 do
    begin
      inrect := InMap(I, SizeX, SizeY, ingrid.width, ingrid.height);
      outrect := OutMap(Remap[I], SizeX, SizeY);
      surf.DrawBitmap(InBmp, inrect, outrect, 1.0);
    end;
  surf.EndScene;

  Result := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
//  LoadTile('C:\src\GMSTileRemapper\tilevariant.png');
  LoadTile('C:\src\GMSTileRemapper\No_Shade.png');

end;

procedure TForm1.LoadTile(fn: String);
var
  fit: TBitmapFit;
  r : TRect;
begin
  OriginalImage.Bitmap.LoadFromFile(fn);

  fit := IsImageCorrect(OriginalImage.Bitmap, TileX, TileY);
  if fit.GoodFit then
    begin
      Label3.Text := Format('Good Image : %d x %d - %d x %d', [OriginalImage.Bitmap.Width, OriginalImage.Bitmap.Height, fit.Width, fit.Height]);
      OriginalImageGrid.Bitmap := MakeGrid(OriginalImage.Bitmap.Width, OriginalImage.Bitmap.Height, fit.Width, fit.Height);

      OutputImage.Bitmap := TBitmap.Create(7 * TileX, 7 * TileY);
      OutputImage.Bitmap.Clear($00FFFFFF);
      RewmapBitmap(OriginalImage.Bitmap, OutputImage.Bitmap, Remaps[fit.RemapID], TileX, TileY);
      OutputImageGrid.Bitmap := MakeGrid(OutputImage.Bitmap.Width, OutputImage.Bitmap.Height, 7, 7, True);

      OutputImage.Bitmap.SaveToFile('C:\src\GMSTileRemapper\out.png');

    end
  else
    Label3.Text := 'Incorrect Image Dimensions';

end;

procedure TForm1.MenuOpenClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    begin
      LoadTile(OpenDialog1.FileName);
    end;
end;

function MakeGrid(const SizeX, SizeY, GridX, GridY: Integer; const ZeroTransparent: Boolean): TBitmap;
var
  res: TBitmap;
  surf: TCanvas;
  X, Y, Line: Integer;
  t: TRectF;
begin

  res := TBitmap.Create(SizeX, SizeY);
  res.Clear($00FFFFFF);
  surf := res.Canvas;
  surf.BeginScene;
  surf.Stroke.Kind := TBrushKind.Solid;
  surf.Stroke.Color := $FF000000;
  surf.Stroke.Thickness := 1;
  surf.Fill.Kind := TBrushKind.Solid;
  for X := 0 to GridX - 1 do
    begin
      surf.DrawLine(Pointf(X * (SizeX / GridX), 0), Pointf(X * (SizeX / GridX), SizeY - 1), 1.0);
    end;
  for Y := 0 to GridY - 1 do
    begin
      surf.DrawLine(Pointf(0, Y * (SizeY / GridY)), Pointf(SizeX - 1, Y * (SizeY / GridY)), 1.0);
    end;
  if ZeroTransparent then
    begin
      for Y := 0 to (TileY div TileTransGrid) - 1 do
        begin
          Line :=  (Y Mod 2);
          for X := 0 to (TileX div TileTransGrid) - 1 do
            begin
              t := RectF(X * TileTransGrid, Y * TileTransGrid, (X + 1) * TileTransGrid, (Y + 1) * TileTransGrid);
              if ((X + Y) Mod 2) = 0 then
                surf.Fill.Color := $FFBFBFBF
              else
                surf.Fill.Color := $FF8F8F8F;

              surf.FillRect(t, 1.0);

            end;
        end;
    end;
  surf.EndScene;
  Result := res;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLayoutColorsWithRectangle(Layout3, TAlphaColorRec.LightGray, TAlphaColorRec.Gray);
end;

end.
