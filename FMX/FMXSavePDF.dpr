program FMXSavePDF;

uses
  System.StartUpCopy,
  FMX.Forms,
  frxMain in 'frxMain.pas' {Form1},
  unFMXPDF in 'unFMXPDF.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
