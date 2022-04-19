program VCLSavePDF;

uses
  Vcl.Forms,
  frMain in 'frMain.pas' {frmMain},
  unVCLPDF in 'unVCLPDF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
