program FMXSavePDF;

uses
  System.StartUpCopy,
  FMX.Forms,
  frxMain in 'frxMain.pas' {frmMain},
  unFMXPDF in 'unFMXPDF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
