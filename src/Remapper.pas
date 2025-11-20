unit Remapper;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Objects, FMX.Layouts;

type
  TRemapArray = Array[0..47] of Integer;

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
procedure SetLayoutColorsWithRectangle(ALayout: TLayout; AForegroundColor, ABackgroundColor: TAlphaColor);
function OutMap(idx, sx, sy: Integer): TRect;
function InMap(idx, sx, sy, tx, ty: Integer): TRect;

implementation

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

end.
