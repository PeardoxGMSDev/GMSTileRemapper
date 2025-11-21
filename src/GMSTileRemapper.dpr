program GMSTileRemapper;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  GMSTileRemapperMain in 'GMSTileRemapperMain.pas' {Form1},
  Remapper in 'Remapper.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
