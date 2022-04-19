unit frxMain;
// © drSturman, 2022
// https://github.com/drSturman

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls,
  unFMXPDF, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.EditBox,
  FMX.SpinBox {$IFDEF MSWINDOWS}, ShellAPI, Winapi.Windows{$ENDIF};

type
  TForm1 = class(TForm)
    btPageFromImage: TButton;
    btSavePDF: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    btAddPage: TButton;
    btOpenImage: TButton;
    btNewPDF: TButton;
    Splitter1: TSplitter;
    Memo1: TMemo;
    btPDFFromImages: TButton;
    SpinBox1: TSpinBox;
    Panel1: TPanel;
    Image1: TImage;
    chbSaveRefTable: TCheckBox;
    chbOpenAfterSaving: TCheckBox;
    procedure btSavePDFClick(Sender: TObject);
    procedure btOpenImageClick(Sender: TObject);
    procedure btNewPDFClick(Sender: TObject);
    procedure btAddPageClick(Sender: TObject);
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
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btSavePDFClick(Sender: TObject);
begin
  if not(FStream is TMemoryStream) then
    exit;
  if not SaveDialog1.Execute then
    exit;

  FSimplePDF.SaveEndOfPDF(chbSaveRefTable.IsChecked);
  (FStream as TMemoryStream).SaveToFile(SaveDialog1.FileName);
  FreeAndNil(FSimplePDF);
  FreeAndNil(FStream);
  Memo1.Lines.Add('PDF saved and closed');

  btAddPage.Enabled := false;
  btPageFromImage.Enabled := false;
  btSavePDF.Enabled := false;

{$IFDEF MSWINDOWS}
  if chbOpenAfterSaving.IsChecked then
    ShellExecute(0, 'open', PChar(SaveDialog1.FileName), '', '', SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TForm1.btOpenImageClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then
    exit;
  Image1.Bitmap.LoadFromFile(OpenDialog1.FileName)
end;

procedure TForm1.btPageFromImageClick(Sender: TObject);
begin
  if Assigned(FSimplePDF) then
  begin
    FSimplePDF.AddImagePageForceJpeg(Image1.Bitmap, 150, 85);
    btSavePDF.Enabled := true;
    Memo1.Lines.Add('Image page added');
  end;
end;

procedure TForm1.btPDFFromImagesClick(Sender: TObject);
var
  i: integer;
  Bmp: FMX.Graphics.TBitmap;
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

  FSimplePDF.SaveEndOfPDF(chbSaveRefTable.IsChecked);

  FreeAndNil(FSimplePDF);
  FreeAndNil(FStream);

  btAddPage.Enabled := false;
  btPageFromImage.Enabled := false;
  btSavePDF.Enabled := false;

{$IFDEF MSWINDOWS}
  if chbOpenAfterSaving.IsChecked then
    ShellExecute(0, 'open', PChar(SaveDialog1.FileName), '', '', SW_SHOWNORMAL);
{$ENDIF}
end;

procedure TForm1.btAddPageClick(Sender: TObject);
var
  i, n: integer;
  Scale, W, H, X, Y, SX, SY: double;
begin
  if Assigned(FSimplePDF) then
  begin
    n := trunc(SpinBox1.Value);
    FSimplePDF.AddPage(cPageWidthA4, cPageHeightA4, n);
    Memo1.Lines.Add('New page with ' + n.ToString + ' images added');
    btSavePDF.Enabled := true;
    if n = 0 then
      exit;
    if cPageWidthA4 / Image1.Bitmap.Width < cPageHeightA4 / Image1.Bitmap.Height
    then
      Scale := cPageWidthA4 / Image1.Bitmap.Width / 2
    else
      Scale := cPageHeightA4 / Image1.Bitmap.Height / 2;
    W := Image1.Bitmap.Width * Scale;
    H := Image1.Bitmap.Height * Scale;
    SX := (cPageWidthA4 - 10 - W) / n;
    SY := (cPageHeightA4 - 10 - H) / n;
    for i := 0 to n - 1 do
    begin
      X := 10 + i * SX;
      Y := cPageHeightA4 - H - (10 + i * SY);
      FSimplePDF.AddImagePosition(X, Y, W, H);
      Memo1.Lines.Add('  Image added at ' + FloatToStrF(X, ffFixed, 3, 1) + ', '
        + FloatToStrF(Y, ffFixed, 3, 1));
    end;
    FSimplePDF.SaveImagePositions;
    for i := 0 to n - 1 do
      FSimplePDF.SaveImageAsJpeg(Image1.Bitmap, i, 85);
  end;
end;

procedure TForm1.btNewPDFClick(Sender: TObject);
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

end.
