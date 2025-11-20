program GMSTileRemapper;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  GMSTileRemap in 'src\GMSTileRemap.pas' {Form1},
  Remapper in 'src\Remapper.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
