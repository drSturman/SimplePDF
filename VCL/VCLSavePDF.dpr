program VCLSavePDF;

uses
  Vcl.Forms,
  frMain in 'frMain.pas' {Form1},
  unVCLPDF in 'unVCLPDF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
