program TileBitmap;

uses
  System.StartUpCopy,
  FMX.Forms,
  TileBitmapMain in 'TileBitmapMain.pas' {Form1};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
