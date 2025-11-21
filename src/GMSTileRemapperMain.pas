unit GMSTileRemapperMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Layouts, FMX.Ani,
  FMX.ListBox, FMX.Menus,
  Remapper;

type
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
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    Resizing: Boolean;
    procedure LoadTile(fn: String);
    function IsImageCorrect(b: TBitmap; Width, Height: Integer): TBitmapFit;
    function RewmapBitmap(InBmp, OutBmp: TBitmap; Remap: TRemapArray; SizeX,
      SizeY: Integer): Boolean;
    procedure GenarateOutput;
  public
    { Public declarations }
  end;
var
  Form1: TForm1;


implementation

{$R *.fmx}

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
//  LoadTile('C:\src\GMSTileRemapper\No_Shade.png');

end;

procedure TForm1.LoadTile(fn: String);
begin
  OriginalImage.Bitmap.LoadFromFile(fn);
  GenarateOutput();
end;

procedure TForm1.GenarateOutput();
var
  fit: TBitmapFit;
begin
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

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  if not OriginalImage.Bitmap.IsEmpty then
    begin
      Label3.Text := 'Changed to ' + ComboBox1.Items[ComboBox1.ItemIndex];
      GenarateOutput();
    end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLayoutColorsWithRectangle(Layout3, TAlphaColorRec.LightGray, TAlphaColorRec.Gray);
end;

end.
