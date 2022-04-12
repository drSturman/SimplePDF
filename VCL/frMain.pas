unit frMain;
// © drSturman, 2022
// https://github.com/drSturman

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, unVCLPDF, Vcl.Samples.Spin,
  Vcl.Imaging.pngimage;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Splitter1: TSplitter;
    btOpenImage: TButton;
    btNewPDF: TButton;
    btAddPage: TButton;
    btSavePDF: TButton;
    btPageFromImage: TButton;
    btPDFFromImages: TButton;
    SpinEdit1: TSpinEdit;
    Panel1: TPanel;
    Image1: TImage;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure btNewPDFClick(Sender: TObject);
    procedure btSavePDFClick(Sender: TObject);
    procedure btAddPageClick(Sender: TObject);
    procedure btOpenImageClick(Sender: TObject);
    procedure btPageFromImageClick(Sender: TObject);
    procedure btPDFFromImagesClick(Sender: TObject);
  private
    { Private declarations }
    FPDF: TSimplePDF;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btAddPageClick(Sender: TObject);
var
  i, n: integer;
  Scale, W, H, X, Y, SX, SY: double;
begin
  if Assigned(FPDF) then
  begin
    n := SpinEdit1.Value;
    FPDF.AddPage(cPageWidthA4, cPageHeightA4, n);
    Memo1.Lines.Add('New page with ' + n.ToString + ' images added');
    btSavePDF.Enabled := true;
    if n = 0 then
      exit;
    if cPageWidthA4 / Image1.Picture.Graphic.Width < cPageHeightA4 /
      Image1.Picture.Graphic.Height then
      Scale := cPageWidthA4 / Image1.Picture.Graphic.Width / 2
    else
      Scale := cPageHeightA4 / Image1.Picture.Graphic.Height / 2;
    W := Image1.Picture.Graphic.Width * Scale;
    H := Image1.Picture.Graphic.Height * Scale;
    SX := (cPageWidthA4 - 10 - W) / n;
    SY := (cPageHeightA4 - 10 - H) / n;
    for i := 0 to n - 1 do
    begin
      X := 10 + i * SX;
      Y := cPageHeightA4 - H - (10 + i * SY);
      FPDF.AddImagePosition(X, Y, W, H);
      Memo1.Lines.Add('  Image added at ' + FloatToStrF(X, ffFixed, 3, 1) + ', '
        + FloatToStrF(Y, ffFixed, 3, 1));
    end;
    FPDF.SaveImagePositions;
    for i := 0 to n - 1 do
      FPDF.SaveImage(Image1.Picture.Graphic, i);
  end;
end;

procedure TForm1.btNewPDFClick(Sender: TObject);
begin
  if Assigned(FPDF) then
    FreeAndNil(FPDF);
  FPDF := TSimplePDF.Create;
  btAddPage.Enabled := true;
  btPageFromImage.Enabled := true;
  btSavePDF.Enabled := false;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('New PDF created');
end;

procedure TForm1.btOpenImageClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    exit;
  Image1.Picture.LoadFromFile(OpenDialog1.FileName)
end;

procedure TForm1.btPageFromImageClick(Sender: TObject);
begin
  if Assigned(FPDF) then
  begin
    FPDF.AddBitmapPage(Image1.Picture.Graphic, 150);
    btSavePDF.Enabled := true;
    Memo1.Lines.Add('Image page added');
  end;
end;

procedure TForm1.btPDFFromImagesClick(Sender: TObject);
var
  i: integer;
  Pic: Vcl.Graphics.TPicture;
begin
  if not OpenDialog1.Execute then
    exit;
  if Assigned(FPDF) then
    FreeAndNil(FPDF);
  FPDF := TSimplePDF.Create;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('New PDF created');
  for i := 0 to OpenDialog1.Files.Count - 1 do
  begin
    Pic := Vcl.Graphics.TPicture.Create;
    Pic.LoadFromFile(OpenDialog1.Files[i]);
    if Pic.Graphic.Width > 0 then
    begin
      FPDF.AddBitmapPage(Pic.Graphic, 150);
      Memo1.Lines.Add('Image page added from ' +
        ExtractFileName(OpenDialog1.Files[i]));
    end;
    Pic.Free;
  end;
  btSavePDF.Enabled := Memo1.Lines.Count > 0;
end;

procedure TForm1.btSavePDFClick(Sender: TObject);
begin
  if not SaveDialog1.Execute then
    exit;
  FPDF.SaveClose(SaveDialog1.FileName);
  FreeAndNil(FPDF);
  Memo1.Lines.Add('PDF saved and closed');
  btAddPage.Enabled := false;
  btPageFromImage.Enabled := false;
  btSavePDF.Enabled := false;
end;

end.
