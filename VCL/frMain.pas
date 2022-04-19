unit frMain;
// Sample for using TSimplePDF v.0.6.5
// https://github.com/drSturman

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.Imaging.jpeg, unVCLPDF, Vcl.Samples.Spin, Vcl.Imaging.pngimage, ShellAPI;

type
  TfrmMain = class(TForm)
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
    chbSaveRefTable: TCheckBox;
    chbOpenAfterSaving: TCheckBox;
    procedure btNewPDFClick(Sender: TObject);
    procedure btSavePDFClick(Sender: TObject);
    procedure btAddPageClick(Sender: TObject);
    procedure btOpenImageClick(Sender: TObject);
    procedure btPageFromImageClick(Sender: TObject);
    procedure btPDFFromImagesClick(Sender: TObject);
  private
    { Private declarations }
    FSimplePDF: TSimplePDF;
    FStream: TStream;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btAddPageClick(Sender: TObject);
var
  i, n: integer;
  Scale, W, H, X, Y, SX, SY: double;
begin
  if Assigned(FSimplePDF) then
  begin
    n := SpinEdit1.Value;
    FSimplePDF.AddPage(cPageWidthA4, cPageHeightA4, n);
    Memo1.Lines.Add('New page with ' + n.ToString + ' images added');
    btSavePDF.Enabled := true;
    if n = 0 then
      exit;
    if cPageWidthA4 / Image1.Picture.Graphic.Width < cPageHeightA4 / Image1.Picture.Graphic.Height then
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
      FSimplePDF.AddImagePosition(X, Y, W, H);
      Memo1.Lines.Add('  Image added at ' + FloatToStrF(X, ffFixed, 3, 1) + ', ' + FloatToStrF(Y, ffFixed, 3, 1));
    end;
    FSimplePDF.SaveImagePositions;
    for i := 0 to n - 1 do
      FSimplePDF.SaveImageAsJpeg(Image1.Picture.Graphic, i, 85);
  end;
end;

procedure TfrmMain.btNewPDFClick(Sender: TObject);
begin
  if Assigned(FSimplePDF) then
    FreeAndNil(FSimplePDF);
  if Assigned(FStream) then
    FreeAndNil(FStream);
  FStream := TMemoryStream.Create;
  FSimplePDF := TSimplePDF.Create(FStream);
  btAddPage.Enabled := true;
  btPageFromImage.Enabled := true;
  btSavePDF.Enabled := false;
  Memo1.Lines.Clear;
  Memo1.Lines.Add('New PDF created');
end;

procedure TfrmMain.btOpenImageClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    exit;
  Image1.Picture.LoadFromFile(OpenDialog1.FileName)
end;

procedure TfrmMain.btPageFromImageClick(Sender: TObject);
begin
  if Assigned(FSimplePDF) then
  begin
    FSimplePDF.AddImagePageForceJpeg(Image1.Picture.Graphic, 150, 85);
    btSavePDF.Enabled := true;
    Memo1.Lines.Add('Image page added');
  end;
end;

procedure TfrmMain.btPDFFromImagesClick(Sender: TObject);
var
  i: integer;
  Pic: Vcl.Graphics.TPicture;
begin
  if not OpenDialog1.Execute then
    exit;
  if not SaveDialog1.Execute then
    exit;
  if Assigned(FSimplePDF) then
    FreeAndNil(FSimplePDF);
  if Assigned(FStream) then
    FreeAndNil(FStream);

  FStream := TFileStream.Create(SaveDialog1.FileName, fmCreate);
  FSimplePDF := TSimplePDF.Create(FStream);
  Memo1.Lines.Clear;
  Memo1.Lines.Add('New PDF created');
  for i := 0 to OpenDialog1.Files.Count - 1 do
    if FSimplePDF.AddImagePage(OpenDialog1.Files[i], 150) then
      Memo1.Lines.Add('Image page added from ' + ExtractFileName(OpenDialog1.Files[i]));

  FSimplePDF.SaveEndOfPDF(chbSaveRefTable.Checked);
  Memo1.Lines.Add('PDF saved and closed');

  FreeAndNil(FSimplePDF);
  FreeAndNil(FStream);

  btAddPage.Enabled := false;
  btPageFromImage.Enabled := false;
  btSavePDF.Enabled := false;

  if chbOpenAfterSaving.Checked then
    ShellExecute(0, 'open', PChar(SaveDialog1.FileName), '', '', SW_SHOWNORMAL);
end;

procedure TfrmMain.btSavePDFClick(Sender: TObject);
begin
  if not(FStream is TMemoryStream) then
    exit;
  if not SaveDialog1.Execute then
    exit;
  FSimplePDF.SaveEndOfPDF(chbSaveRefTable.Checked);
  (FStream as TMemoryStream).SaveToFile(SaveDialog1.FileName);
  FreeAndNil(FSimplePDF);
  FreeAndNil(FStream);
  Memo1.Lines.Add('PDF saved and closed');

  btAddPage.Enabled := false;
  btPageFromImage.Enabled := false;
  btSavePDF.Enabled := false;

  if chbOpenAfterSaving.Checked then
    ShellExecute(0, 'open', PChar(SaveDialog1.FileName), '', '', SW_SHOWNORMAL);
end;

end.
