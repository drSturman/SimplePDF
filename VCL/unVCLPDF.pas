unit unVCLPDF;
// Copyright (c) 2022 Andrei Dergachev
// https://github.com/drSturman
// TSimplePDF v.0.6.5

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, VCL.Graphics, VCL.Imaging.jpeg, VCL.Imaging.pngimage,
  System.Generics.Collections;

type

  TImagePosition = record
    X, Y, Width, Heigt: double;
  end;

  TSimplePDF = class
  private
    FImageCount: integer; // Image counter for the whole document
    FDirects: TList<integer>; // List of direct links for Cross-Reference Table
    FPageObjects: TList<integer>; // List of indirect pages links
    FImgPositions: TList<TImagePosition>; // Positions of images on the current page
    FImgNames: TList<AnsiString>; // Names of Images on the current page
    FPDFStream: TStream;
    destructor Destroy; override;
  public
    constructor Create(AStream: TStream);
    procedure AddPage(AWidth, AHeigt: double; AImagesCount: integer);
    procedure AddImagePosition(AX, AY, AWidth, AHeigt: double);
    procedure SaveImagePositions;
    function SaveImageAsJpeg(AGraphic: VCL.Graphics.TGraphic; AImageIndex: integer; AJpegQuality: integer): boolean;
    function SaveJpegImageAsIs(const AFileName: string; AImageIndex, AWidth, AHeight: integer): boolean;
    function AddImagePageForceJpeg(AGraphic: VCL.Graphics.TGraphic; ADPI: integer; AJpegQuality: integer): boolean;
    function AddImagePage(const AFileName: string; ADPI: integer): boolean;
    procedure SaveEndOfPDF(ASaveRefTable: boolean);
  end;

const
  cPageWidthA4 = 595.3;
  cPageHeightA4 = 841.9;

implementation

const
  cParamCount = 2; // numbers reserved for pages catalog

procedure TSimplePDF.AddImagePosition(AX, AY, AWidth, AHeigt: double);
var
  LImagePosition: TImagePosition;
begin
  LImagePosition.X := AX;
  LImagePosition.Y := AY;
  LImagePosition.Width := AWidth;
  LImagePosition.Heigt := AHeigt;
  FImgPositions.Add(LImagePosition);
end;

procedure TSimplePDF.AddPage(AWidth, AHeigt: double; AImagesCount: integer);
var
  i: integer;
  s: AnsiString;
  LImgName: AnsiString;
begin
  FImgPositions.Clear;
  FImgNames.Clear;

  FDirects.Add(FPDFStream.Position);
  FPageObjects.Add(FDirects.Count + cParamCount);
  s := (FDirects.Count + cParamCount).ToString + ' 0 obj' + #10 + '<<' + #10 + '/Type /Page' + #10 + '/Parent 2 0 R' +
    #10 + '/MediaBox [0 0 ' + FloatToStrF(AWidth, ffFixed, 5, 3) + ' ' + FloatToStrF(AHeigt, ffFixed, 5, 3) + ']' + #10;

  if AImagesCount > 0 then
  begin
    s := s + '/Resources <<  /ProcSet [/PDF /ImageC]' + #10 + '/XObject' + #10 + '<<' + #10;

    for i := 1 to AImagesCount do
    begin
      LImgName := 'Img' + (FImageCount + i).ToString;
      FImgNames.Add(LImgName);
      s := s + '/' + LImgName + ' ' + (FDirects.Count + i * 2 + cParamCount).ToString + ' 0 R' + #10;
    end;

    s := s + '>>' + #10 + '>>' + #10 + '/Contents [ ' + (FDirects.Count + 1 + cParamCount).ToString + ' 0 R ]' + #10;
  end;
  s := s + '>>' + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));
end;

function GetImageSize(const AFileName: string; out AWidth, AHeight: integer;
  out APicture: VCL.Graphics.TPicture): boolean;
begin
  Result := false;
  APicture := VCL.Graphics.TPicture.Create;
  try
    APicture.LoadFromFile(AFileName);
    AWidth := APicture.Graphic.Width;
    AHeight := APicture.Graphic.Height;
    Result := (AWidth > 0) and (AHeight > 0);
  except
    APicture.Free;
  end;
end;

function TSimplePDF.SaveJpegImageAsIs(const AFileName: string; AImageIndex, AWidth, AHeight: integer): boolean;
var
  LImageSize: integer;
  LStream: TMemoryStream;
  s: AnsiString;
  AFilter: AnsiString;
begin
  Result := false;
  if AImageIndex >= FImgNames.Count then
    exit;

  LStream := TMemoryStream.Create;
  // Make image body
  try
    LStream.LoadFromFile(AFileName);

    inc(FImageCount);

    if AFilter > '' then
      AFilter := '/Filter /' + AFilter + #10;

    // Image properties
    FDirects.Add(FPDFStream.Position);
    s := (FDirects.Count + cParamCount).ToString + ' 0 obj' + #10 + '<<' + #10 + '/Type /XObject' + #10 +
      '/Subtype /Image' + #10 + '/Name /' + FImgNames[AImageIndex] + #10 + '/Width ' + AWidth.ToString + ' /Height ' +
      AHeight.ToString + ' /Length ' + (FDirects.Count + 1 + cParamCount).ToString + ' 0 R' + #10 + '/Filter /DCTDecode'
      + #10 + '/ColorSpace /DeviceRGB' + #10 + '/BitsPerComponent 8' + #10 + '>>' + #10 + 'stream' + #10;
    FPDFStream.Write(s[1], Length(s));

    LImageSize := FPDFStream.Position;
    LStream.SaveToStream(FPDFStream);
    LImageSize := FPDFStream.Position - LImageSize;
    Result := true;
  finally
    LStream.Free;
  end;
  s := #10 + 'endstream' + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));

  // Image size
  FDirects.Add(FPDFStream.Position);
  s := (FDirects.Count + cParamCount).ToString + ' 0 obj' + #10 + LImageSize.ToString + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));
end;

function TSimplePDF.AddImagePage(const AFileName: string; ADPI: integer): boolean;
var
  LImageSize: integer;
  LImageWidth, LImageHeight: integer;
  LStream: TMemoryStream;
  s: AnsiString;
  LPageWidth, LPageHeight: double;
  LPicture: VCL.Graphics.TPicture;
begin
  Result := false;
  if not GetImageSize(AFileName, LImageWidth, LImageHeight, LPicture) then
    exit;
  try
    LPageWidth := LImageWidth * 72 / ADPI;
    LPageHeight := LImageHeight * 72 / ADPI;

    AddPage(LPageWidth, LPageHeight, 1);
    AddImagePosition(0, 0, LPageWidth, LPageHeight);
    SaveImagePositions;

    if LPicture.Graphic is TJPEGImage then
      Result := SaveJpegImageAsIs(AFileName, 0, LImageWidth, LImageHeight)
    else
      Result := SaveImageAsJpeg(LPicture.Graphic, 0, 85);

  finally
    FreeAndNil(LPicture);
  end;
end;

function TSimplePDF.SaveImageAsJpeg(AGraphic: TGraphic; AImageIndex: integer; AJpegQuality: integer): boolean;
var
  LImageSize: integer;
  LJpegImage: TJPEGImage;
  LBitmap: TBitmap;
  s: AnsiString;
begin
  Result := false;
  if AImageIndex >= FImgNames.Count then
    exit;

  // Make Image body
  LJpegImage := TJPEGImage.Create;
  try
    if AGraphic is TPngImage then
    begin
      LBitmap := TBitmap.Create;
      try
        LBitmap.Assign(AGraphic);
        LJpegImage.Assign(LBitmap);
      finally
        LBitmap.Free;
      end;
    end
    else
    begin
      LJpegImage.Assign(AGraphic);
    end;

    LJpegImage.CompressionQuality := AJpegQuality; // 100 - maximum quality, but big size

    inc(FImageCount);

    // Image properties
    FDirects.Add(FPDFStream.Position);
    s := (FDirects.Count + cParamCount).ToString + ' 0 obj' + #10 + '<<' + #10 + '/Type /XObject' + #10 +
      '/Subtype /Image' + #10 + '/Name /' + FImgNames[AImageIndex] + #10 + '/Width ' + AGraphic.Width.ToString +
      ' /Height ' + AGraphic.Height.ToString + ' /Length ' + (FDirects.Count + 1 + cParamCount).ToString + ' 0 R' + #10
      + '/Filter /DCTDecode' + #10 + '/ColorSpace /DeviceRGB' + #10 + '/BitsPerComponent 8' + #10 + '>>' + #10 +
      'stream' + #10;
    FPDFStream.Write(s[1], Length(s));

    LImageSize := FPDFStream.Position;
    LJpegImage.SaveToStream(FPDFStream);
    LImageSize := FPDFStream.Position - LImageSize;
    Result := true;
  finally
    LJpegImage.Free;
  end;

  s := #10 + 'endstream' + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));

  // Image size
  FDirects.Add(FPDFStream.Position);
  s := (FDirects.Count + cParamCount).ToString + ' 0 obj' + #10 + LImageSize.ToString + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));
end;

function TSimplePDF.AddImagePageForceJpeg(AGraphic: TGraphic; ADPI: integer; AJpegQuality: integer): boolean;
var
  LPageWidth, LPageHeight: double;
begin
  LPageWidth := AGraphic.Width * 72 / ADPI;
  LPageHeight := AGraphic.Height * 72 / ADPI;
  AddPage(LPageWidth, LPageHeight, 1);
  AddImagePosition(0, 0, LPageWidth, LPageHeight);
  SaveImagePositions;
  Result := SaveImageAsJpeg(AGraphic, 0, AJpegQuality);
end;

procedure TSimplePDF.SaveImagePositions;
var
  s, LImgS: AnsiString;
begin
  LImgS := '';
  for var i: integer := 0 to FImgPositions.Count - 1 do
    LImgS := LImgS + 'q' + #10 + FloatToStrF(FImgPositions[i].Width, ffFixed, 5, 3) + ' 0 0 ' +
      FloatToStrF(FImgPositions[i].Heigt, ffFixed, 5, 3) + ' ' + FloatToStrF(FImgPositions[i].X, ffFixed, 5, 3) + ' ' +
      FloatToStrF(FImgPositions[i].Y, ffFixed, 5, 3) + ' cm' + #10 + '/' + FImgNames[i] + ' Do' + #10 + 'Q' + #10;

  FDirects.Add(FPDFStream.Position);
  s := (FDirects.Count + cParamCount).ToString + ' 0 obj' + #10 + '<</Length ' + Length(LImgS).ToString + #10 + '>>' +
    #10 + 'stream' + #10 + LImgS + 'endstream' + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));
end;

constructor TSimplePDF.Create(AStream: TStream);
var
  s: AnsiString;
begin
  FDirects := TList<integer>.Create;
  FPageObjects := TList<integer>.Create;
  FImgPositions := TList<TImagePosition>.Create;
  FImgNames := TList<AnsiString>.Create;
  FPDFStream := AStream;
  FImageCount := 0;
  s := '%PDF-1.2' + #10;
  FPDFStream.Write(s[1], Length(s));
end;

destructor TSimplePDF.Destroy;
begin
  FDirects.Free;
  FPageObjects.Free;
  FImgPositions.Free;
  FImgNames.Free;
  inherited;
end;

procedure TSimplePDF.SaveEndOfPDF(ASaveRefTable: boolean);
var
  LStartXref: integer;
  s: AnsiString;
begin
  if FPageObjects.Count = 0 then
    AddPage(cPageWidthA4, cPageHeightA4, 0);

  // Page objects list generate
  FDirects.Insert(0, FPDFStream.Position); // insert link to first object
  s := '1 0 obj' + #10 + '<<' + #10 + '/Type /Catalog' + #10 + '/Pages ' + '2 0 R>>' + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));

  FDirects.Insert(1, FPDFStream.Position); // insert link to second object
  s := '2 0 obj' + #10 + '<<' + #10 + '/Type /Pages' + #10 + '/Count ' + FPageObjects.Count.ToString + #10 + '/Kids [ ';

  for var i: integer := 0 to FPageObjects.Count - 1 do
    s := s + FPageObjects[i].ToString + ' 0 R' + #10;

  s := s + ']' + #10 + '>>' + #10 + 'endobj' + #10;

  FPDFStream.Write(s[1], Length(s));

  // Save Cross-Reference Table if needed
  if ASaveRefTable then
  begin
    LStartXref := FPDFStream.Position;

    s := 'xref' + #10 + '0 ' + (FDirects.Count + 1).ToString() + '' + #10 + '0000000000 65535 f' + #10;

    FPDFStream.Write(s[1], Length(s));
    for var L: integer in FDirects do
    begin
      s := (10000000000 + L).ToString().Substring(1) + ' 00000 n' + #10;
      FPDFStream.Write(s[1], Length(s));
    end;

    s := 'trailer' + #10 + '<<' + #10 + '  /Size ' + (FDirects.Count + 1).ToString + #10 + '  /Root 1 0 R' + #10 + '>>'
      + #10 + 'startxref' + #10 + LStartXref.ToString + #10 + '%%EOF';
  end
  else
    // or save trailer only
    s := 'trailer' + #10 + '<<' + #10 + '  /Root 1 0 R' + #10 + '>>';

  FPDFStream.Write(s[1], Length(s));
end;

initialization

FormatSettings.DecimalSeparator := '.';

end.
