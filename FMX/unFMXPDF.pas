unit unFMXPDF;
// © drSturman, 2022
// https://github.com/drSturman

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Surfaces,
  FMX.Types, FMX.Graphics, FMX.Objects, System.Generics.Collections;

type
  TImagePos = record
    X, Y, Width, Heigt: double;
  end;

  TSimplePDF = class
  private
    FImageCount: integer;
    FDirects: TList<Integer>;
    FPageObjects: TList<Integer>;
    FImgPositions: TList<TImagePos>;
    FImgNames: TList<AnsiString>;
    FPDFStream: TMemoryStream;
    destructor Destroy; override;
  public
    constructor Create;
    procedure AddPage(Width, Heigt: double; ImgCount: integer);
    procedure AddImagePosition(X, Y, Width, Heigt: double);
    procedure SaveImagePositions;
    procedure SaveImage(Bmp: FMX.Graphics.TBitmap; ImgId: integer);
    procedure AddBitmapPage(Bmp: FMX.Graphics.TBitmap; dpi: integer);
    procedure SaveClose(const FName: string);
  end;

const
  cPageWidthA4 = 595.3;
  cPageHeightA4 = 841.9;

implementation

const
  cParamCount = 2; //numbers reserved for pages catalog

procedure TSimplePDF.AddImagePosition(X, Y, Width, Heigt: double);
var ImgPos: TImagePos;
begin
 ImgPos.X := X;
 ImgPos.Y := Y;
 ImgPos.Width := Width;
 ImgPos.Heigt := Heigt;
 FImgPositions.Add(ImgPos);
end;

procedure TSimplePDF.AddPage(Width, Heigt: double; ImgCount: integer);
var
  i: integer;
  s, ImgName: AnsiString;
begin
  FPageObjects.Add(FDirects.Count + cParamCount);
  s := (FDirects.Count + cParamCount).ToString +
    ' 0 obj' + #10 + '<<' + #10 +
    '/Type /Page' + #10 +
    '/Parent 2 0 R' + #10 +
    '/MediaBox [0 0 ' + FloatToStrF(Width, ffFixed, 5, 3) + ' ' + FloatToStrF(Heigt, ffFixed, 5, 3) + ']' + #10;
  FImgPositions.Clear;
  FImgNames.Clear;
  if ImgCount>0 then
  begin
    s := s +  '/Resources <<  /ProcSet [/PDF /ImageC]' + #10 + '/XObject' + #10 + '<<' + #10;

    for i:=1 to ImgCount do
    begin
     ImgName := 'Img'+(FImageCount+i).ToString;
     FImgNames.Add(ImgName);
     s := s+ '/'+ ImgName +' '+ (FDirects.Count +i*2 + cParamCount).ToString+' 0 R' + #10;
    end;

    s := s  + '>>' + #10 +  '>>' + #10 +'/Contents ' + (FDirects.Count + 1 + cParamCount).ToString + ' 0 R' + #10;
  end;
  s := s+ '>>' + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));
  FDirects.Add(FPDFStream.Position);
end;

procedure TSimplePDF.SaveImage(Bmp: FMX.Graphics.TBitmap; ImgId: integer);
var
  sz: integer;
  aSurf: TBitmapSurface;
  sp: TBitmapCodecSaveParams;
  s: AnsiString;
begin
  if ImgId>= FImgNames.Count then exit;
  inc(FImageCount);

  //Image description
  s := (FDirects.Count + cParamCount).ToString +
    ' 0 obj' + #10 + '<<' + #10 + '/Type /XObject' + #10 + '/Subtype /Image' +
    #10 + '/Name /' + FImgNames[ImgId] + #10 + '/Width ' + Bmp.Width.ToString + '/Height ' +
    Bmp.Height.ToString + '/Length ' + (FDirects.Count + 1 + cParamCount).ToString +
    ' 0 R' + #10 + '/Filter /DCTDecode' + #10 + '/ColorSpace /DeviceRGB' + #10 +
    '/BitsPerComponent 8' + #10 + '>>' + #10 + 'stream' + #10;
  FPDFStream.Write(s[1], Length(s));

  sz := FPDFStream.Position;

  //Image body
  aSurf := TBitmapSurface.Create;
  aSurf.Assign(Bmp);
  sp.Quality := 90; //100 - maximum quality, but big size
  TBitmapCodecManager.SaveToStream(FPDFStream, aSurf, '.jpg', @sp);
  aSurf.Free;

  sz := FPDFStream.Position - sz;

  s := #10 + 'endstream' + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));

  FDirects.Add(FPDFStream.Position);

  //Image size
  s := (FDirects.Count + cParamCount).ToString +
    ' 0 obj' + #10 + sz.ToString + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));
  FDirects.Add(FPDFStream.Position);
end;

procedure TSimplePDF.SaveImagePositions;
var
 i: integer;
  s, ImS: AnsiString;
begin
  ImS := 'Q' + #10;
  for i:=0 to FImgPositions.Count-1 do
   ImS := ImS+ 'q'+#10+  FloatToStrF(FImgPositions[i].Width, ffFixed, 5, 3)+ ' 0 0 '+FloatToStrF(FImgPositions[i].Heigt, ffFixed, 5, 3)+' '+
   FloatToStrF(FImgPositions[i].X, ffFixed, 5, 3)+' '+FloatToStrF(FImgPositions[i].Y, ffFixed, 5, 3)+' cm' + #10 +
    '/' + FImgNames[i] + ' Do' + #10 + 'Q' + #10;

  s := (FDirects.Count + cParamCount).ToString +  ' 0 obj' + #10 + '<</Length ' + Length(ImS).ToString + #10 +
    '>>' + #10 + 'stream' + #10 + ImS + 'endstream' + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));
  FDirects.Add(FPDFStream.Position);
end;

procedure TSimplePDF.AddBitmapPage(Bmp: FMX.Graphics.TBitmap; dpi: integer);
var
  PageW, PageH: double;
begin
  PageW := Bmp.Width*72/dpi;
  PageH := Bmp.Height*72/dpi;

  AddPage(PageW, PageH, 1);
  AddImagePosition(0, 0, PageW, PageH);
  SaveImagePositions;
  SaveImage(Bmp, 0);
end;

constructor TSimplePDF.Create;
var
  s: AnsiString;
begin
  FDirects := TList<Integer>.Create;
  FPageObjects := TList<Integer>.Create;
  FImgPositions := TList<TImagePos>.Create;
  FImgNames := TList<AnsiString>.Create;
  FPDFStream := TMemoryStream.Create;
  s := '%PDF-1.5' + #10;
  FPDFStream.Write(s[1], Length(s));
  FDirects.Add(FPDFStream.Position);
end;

destructor TSimplePDF.Destroy;
begin
  FDirects.Free;
  FPageObjects.Free;
  FImgPositions.Free;
  FImgNames.Free;
  FPDFStream.Free;
  inherited;
end;

procedure TSimplePDF.SaveClose(const FName: string);
var
  i: integer;
  startxref, L: Integer;
  s: AnsiString;
begin
  //Page objects list generate
  s := '1 0 obj' + #10 + '<<' + #10 + '/Type /Catalog' + #10 + '/Pages ' +
       '2 0 R>>' + #10 + 'endobj' + #10;
  FPDFStream.Write(s[1], Length(s));
  FDirects.Add(FPDFStream.Position);
  s := '2 0 obj' + #10 + '<<' + #10 + '/Type /Pages' + #10 + '/Count ' +
    FPageObjects.Count.ToString + #10 + '/Kids [' + #10;
  for i := 0 to FPageObjects.Count - 1 do
    s := s + FPageObjects[i].ToString + ' 0 R' + #10;
  s := s + ']' + #10 + '>>' + #10 + 'endobj' + #10;

  FPDFStream.Write(s[1], Length(s));
  FDirects.Add(FPDFStream.Position);

 //Direct links table generate
  startxref := FPDFStream.Position;
  s := 'xref' + #10 + '0 ' + (FDirects.Count + 1).ToString() + '' + #10 +
    '0000000000 65535 f' + #10;
  FPDFStream.Write(s[1], Length(s));
  for L in FDirects do
  begin
    s := (10000000000 + L).ToString().Substring(1) + ' 00000 n' + #10;
    FPDFStream.Write(s[1], Length(s));
  end;

  s := 'trailer' + #10 + '<<' + #10 + '  /Size ' + (FDirects.Count + 1).ToString + #10 + '  /Root 1 0 R' + #10 + '>>'
    + #10 + 'startxref' + #10 + startxref.ToString + #10 + '%%EOF';
  FPDFStream.Write(s[1], Length(s));

  FPDFStream.SaveToFile(FName);
end;

initialization

FormatSettings.DecimalSeparator := '.';

end.
