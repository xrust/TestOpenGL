unit uGLFontRenderer2;
{//----------------------------------------------------------------------------
    OpenGL Font Renderer with Font Atlas
    Качественный рендеринг текста в OpenGL с использованием шрифтового атласа
    Улучшено: поддержка Unicode, word wrap по словам, динамический атлас
}//----------------------------------------------------------------------------
interface
//-----------------------------------------------------------------------------
uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Generics.Collections,
  VCL.Graphics,
  OpenGL;

//-----------------------------------------------------------------------------
const
  GLFONT_ATLAS_INIT_WIDTH  = 1024;
  GLFONT_ATLAS_INIT_HEIGHT = 512;
  GLFONT_CHAR_PADDING      = 4;
  GLFONT_DEFAULT_PADDING   = 5;

//-----------------------------------------------------------------------------
type
  TTextAlignment = (taLeft, taCenter, taRight);
  TTextVAlignment = (vaTop, vaMiddle, vaBottom);

//-----------------------------------------------------------------------------
type
  TGLFontRenderer = class
  private
    type
      TCharInfo = record
        X, Y: Integer;          // позиция символа в атласе
        Width, Height: Integer; // размер символа в пикселях
        OffsetX, OffsetY: Integer; // смещение для корректного позиционирования
        AdvanceX: Integer;      // расстояние до следующего символа
      end;
   private
    FTextureID: GLuint;
    FCharMap: TDictionary<Integer, TCharInfo>;
    FFontName: string;
    FFontSize: Integer;
    FLineHeight: Integer;
    FAtlasWidth: Integer;
    FAtlasHeight: Integer;
    FBold: Boolean;
    FItalic: Boolean;
    FAntialiased: Boolean;
    FInitialized: Boolean;
    FAtlasBmp: TBitmap;

    procedure BuildFontAtlas;
    procedure FreeFontAtlas;
    function GetTextureData(Bmp: TBitmap): TBytes;
    procedure EnsureGlyph(CharCode: Integer);
    function WordWrapText(const Text: string; MaxWidth: Integer): TStringList;

  public
    constructor Create(const FontName: string = 'Tahoma'; FontSize: Integer = 10;
                       Bold: Boolean = False; Italic: Boolean = False; Antialiased: Boolean = True);
    destructor Destroy; override;

    /// <summary>Рисует текст в указанных координатах</summary>
    procedure DrawText(X, Y: Single; const Text: string); overload;
    /// <summary>Рисует цветной текст в указанных координатах</summary>
    procedure DrawText(X, Y: Single; const Text: string; R, G, B: Single; Alpha: Single = 1.0); overload;
    /// <summary>Рисует текст с выравниванием в прямоугольнике</summary>
    procedure DrawTextAligned(X, Y, Width, Height: Single; const Text: string;
                              HAlign: TTextAlignment = taLeft; VAlign: TTextVAlignment = vaTop);
    /// <summary>Рисует текстовый блок с переносом строк</summary>
    procedure DrawTextBox(X, Y, Width, Height: Single; const Text: string;
                          Padding: Integer = GLFONT_DEFAULT_PADDING; WordWrap: Boolean = True);

    function GetTextWidth(const Text: string): Integer;
    function GetTextHeight(const Text: string): Integer;
    function GetCharWidth(Ch: Char): Integer;
    procedure SetColor(R, G, B: Single; Alpha: Single = 1.0); overload;
    procedure SetColor(Color: TColor; Alpha: Single = 1.0); overload;

    property FontName: string read FFontName;
    property FontSize: Integer read FFontSize;
    property LineHeight: Integer read FLineHeight;
    property Bold: Boolean read FBold;
    property Italic: Boolean read FItalic;
    property Antialiased: Boolean read FAntialiased;
    property Initialized: Boolean read FInitialized;
  end;

//-----------------------------------------------------------------------------
implementation

uses
  Math;

//-----------------------------------------------------------------------------
{ TGLFontRenderer }

constructor TGLFontRenderer.Create(const FontName: string; FontSize: Integer;
                                   Bold, Italic, Antialiased: Boolean);
begin
  inherited Create;
  FFontName := FontName;
  FFontSize := FontSize;
  FBold := Bold;
  FItalic := Italic;
  FAntialiased := Antialiased;
  FTextureID := 0;
  FInitialized := False;
  FCharMap := TDictionary<Integer, TCharInfo>.Create;
  FAtlasBmp := nil;
  BuildFontAtlas;
end;

//-----------------------------------------------------------------------------
destructor TGLFontRenderer.Destroy;
begin
  FreeFontAtlas;
  FCharMap.Free;
  inherited;
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.BuildFontAtlas;
var
  FontStyle: TFontStyles;
begin
  FreeAndNil(FAtlasBmp);
  FAtlasWidth := GLFONT_ATLAS_INIT_WIDTH;
  FAtlasHeight := GLFONT_ATLAS_INIT_HEIGHT;
  FCharMap.Clear;

  FAtlasBmp := TBitmap.Create;
  FAtlasBmp.PixelFormat := pf32bit;
  FAtlasBmp.Width := FAtlasWidth;
  FAtlasBmp.Height := FAtlasHeight;
  FAtlasBmp.Canvas.Brush.Color := clBlack;
  FAtlasBmp.Canvas.FillRect(Rect(0, 0, FAtlasWidth, FAtlasHeight));
  FAtlasBmp.Canvas.Font.Name := FFontName;
  FAtlasBmp.Canvas.Font.Size := FFontSize;
  FAtlasBmp.Canvas.Font.Color := clWhite;

  FontStyle := [];
  if FBold then FontStyle := FontStyle + [fsBold];
  if FItalic then FontStyle := FontStyle + [fsItalic];
  FAtlasBmp.Canvas.Font.Style := FontStyle;

  if FAntialiased then
    FAtlasBmp.Canvas.Font.Quality := fqAntialiased
  else
    FAtlasBmp.Canvas.Font.Quality := fqNonAntialiased;

  FLineHeight := FAtlasBmp.Canvas.TextHeight('Аy') + 4;

  // Изначально не добавляем символы, добавление по мере необходимости (EnsureGlyph)
  // Но можно сразу добавить базовые символы (ASCII)
  // for i := 32 to 127 do EnsureGlyph(i);

  // Создаём OpenGL текстуру
  if FTextureID <> 0 then
    glDeleteTextures(1, @FTextureID);
  glGenTextures(1, @FTextureID);
  glBindTexture(GL_TEXTURE_2D, FTextureID);

  if FAntialiased then begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  end else begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  end;

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

  // Загружаем пустую текстуру
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, FAtlasWidth, FAtlasHeight,
               0, GL_RGBA, GL_UNSIGNED_BYTE, nil);

  FInitialized := True;
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.FreeFontAtlas;
begin
  if FTextureID <> 0 then begin
    glDeleteTextures(1, @FTextureID);
    FTextureID := 0;
  end;
  FreeAndNil(FAtlasBmp);
  FInitialized := False;
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.EnsureGlyph(CharCode: Integer);
var
  Info: TCharInfo;
  CharStr: string;
  CharWidth, CharHeight: Integer;
  x, y, maxH: Integer;
  TextureData: TBytes;
  Found: Boolean;
begin
  if FCharMap.TryGetValue(CharCode, Info) then
    Exit; // Уже есть

  CharStr := Char(CharCode);
  CharWidth := FAtlasBmp.Canvas.TextWidth(CharStr);
  CharHeight := FAtlasBmp.Canvas.TextHeight(CharStr);

  if (CharWidth <= 0) or (CharHeight <= 0) then
  begin
    Info.X := 0;
    Info.Y := 0;
    Info.Width := 0;
    Info.Height := 0;
    Info.OffsetX := 0;
    Info.OffsetY := 0;
    Info.AdvanceX := FLineHeight div 2;
    FCharMap.Add(CharCode, Info);
    Exit;
  end;

  // Поиск свободного места в атласе
  // (Простой алгоритм: ищем первый свободный y-ряд, можно улучшить)
  x := GLFONT_CHAR_PADDING;
  y := GLFONT_CHAR_PADDING;
  maxH := 0;
  Found := False;
  while not Found do
  begin
    Found := True;
    for var Pair in FCharMap.Values do
    begin
      if (Abs(Pair.Y - y) < CharHeight + GLFONT_CHAR_PADDING) and
         (x + CharWidth + GLFONT_CHAR_PADDING > Pair.X) and
         (x < Pair.X + Pair.Width + GLFONT_CHAR_PADDING) then
      begin
        x := Pair.X + Pair.Width + GLFONT_CHAR_PADDING;
        if x + CharWidth + GLFONT_CHAR_PADDING > FAtlasWidth then
        begin
          x := GLFONT_CHAR_PADDING;
          y := y + maxH + GLFONT_CHAR_PADDING;
          maxH := 0;
        end;
        Found := False;
        Break;
      end;
      if Pair.Height > maxH then maxH := Pair.Height;
    end;
    if y + CharHeight + GLFONT_CHAR_PADDING > FAtlasHeight then
      raise Exception.Create('Font atlas overflow. Increase atlas size.');
  end;

  // Рисуем символ на атласе
  FAtlasBmp.Canvas.Brush.Style := bsClear;
  FAtlasBmp.Canvas.TextOut(x, y, CharStr);

  Info.X := x;
  Info.Y := y;
  Info.Width := CharWidth;
  Info.Height := CharHeight;
  Info.OffsetX := 0;
  Info.OffsetY := 0;
  Info.AdvanceX := CharWidth + 1;
  FCharMap.Add(CharCode, Info);

  // Обновляем текстуру OpenGL только для изменённой области (или всю)
  TextureData := GetTextureData(FAtlasBmp);
  glBindTexture(GL_TEXTURE_2D, FTextureID);
  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, FAtlasWidth, FAtlasHeight,
                  GL_RGBA, GL_UNSIGNED_BYTE, @TextureData[0]);
end;

//-----------------------------------------------------------------------------
function TGLFontRenderer.GetTextureData(Bmp: TBitmap): TBytes;
var
  x, y: Integer;
  Pixel: TColor;
  Brightness: Byte;
  Index: Integer;
begin
  SetLength(Result, FAtlasWidth * FAtlasHeight * 4); // RGBA
  Index := 0;

  for y := 0 to FAtlasHeight - 1 do
    for x := 0 to FAtlasWidth - 1 do
    begin
      Pixel := ColorToRGB(Bmp.Canvas.Pixels[x, y]);
      Brightness := GetRValue(Pixel);
      Result[Index] := 255;
      Result[Index + 1] := 255;
      Result[Index + 2] := 255;
      Result[Index + 3] := Brightness;
      Inc(Index, 4);
    end;
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.DrawText(X, Y: Single; const Text: string);
begin
  DrawText(X, Y, Text, 1.0, 1.0, 1.0, 1.0);
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.DrawText(X, Y: Single; const Text: string; R, G, B, Alpha: Single);
var
  i: Integer;
  Ch: Char;
  CharCode: Integer;
  Info: TCharInfo;
  XPos: Single;
  tx1, ty1, tx2, ty2: Single;
  x1, y1, x2, y2: Single;
begin
  if not FInitialized then Exit;
  if Text = '' then Exit;

  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, FTextureID);
  glColor4f(R, G, B, Alpha);

  XPos := X;

  glBegin(GL_QUADS);
  for i := 1 to Length(Text) do
  begin
    Ch := Text[i];
    CharCode := Ord(Ch);
    EnsureGlyph(CharCode);

    if not FCharMap.TryGetValue(CharCode, Info) then
    begin
      XPos := XPos + FLineHeight div 2;
      Continue;
    end;

    if (Info.Width <= 0) or (Info.Height <= 0) then
    begin
      if (CharCode = 32) or (CharCode = 9) then
        XPos := XPos + Info.AdvanceX;
      Continue;
    end;

    tx1 := Info.X / FAtlasWidth;
    ty1 := Info.Y / FAtlasHeight;
    tx2 := (Info.X + Info.Width) / FAtlasWidth;
    ty2 := (Info.Y + Info.Height) / FAtlasHeight;

    x1 := XPos + Info.OffsetX;
    y1 := Y + Info.OffsetY;
    x2 := x1 + Info.Width;
    y2 := y1 + Info.Height;

    glTexCoord2f(tx1, ty1); glVertex2f(x1, y1);
    glTexCoord2f(tx2, ty1); glVertex2f(x2, y1);
    glTexCoord2f(tx2, ty2); glVertex2f(x2, y2);
    glTexCoord2f(tx1, ty2); glVertex2f(x1, y2);

    XPos := XPos + Info.AdvanceX;
  end;
  glEnd;

  glDisable(GL_TEXTURE_2D);
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.DrawTextAligned(X, Y, Width, Height: Single;
  const Text: string; HAlign: TTextAlignment; VAlign: TTextVAlignment);
var
  TextW, TextH: Integer;
  DrawX, DrawY: Single;
begin
  TextW := GetTextWidth(Text);
  TextH := GetTextHeight(Text);

  case HAlign of
    taLeft:   DrawX := X;
    taCenter: DrawX := X + (Width - TextW) / 2;
    taRight:  DrawX := X + Width - TextW;
  else
    DrawX := X;
  end;

  case VAlign of
    vaTop:    DrawY := Y;
    vaMiddle: DrawY := Y + (Height - TextH) / 2;
    vaBottom: DrawY := Y + Height - TextH;
  else
    DrawY := Y;
  end;

  DrawText(DrawX, DrawY, Text);
end;

//-----------------------------------------------------------------------------
function TGLFontRenderer.WordWrapText(const Text: string; MaxWidth: Integer): TStringList;
var
  Lines: TStringList;
  Words: TArray<string>;
  Line, Word: string;
  i: Integer;
begin
  Lines := TStringList.Create;
  for var RawLine in Text.Replace(#13, '').Split([#10]) do
  begin
    Words := RawLine.Split([' ']);
    Line := '';
    for i := 0 to High(Words) do
    begin
      Word := Words[i];
      if Line = '' then
        Line := Word
      else if GetTextWidth(Line + ' ' + Word) > MaxWidth then
      begin
        Lines.Add(Line);
        Line := Word;
      end
      else
        Line := Line + ' ' + Word;
    end;
    if Line <> '' then Lines.Add(Line);
  end;
  Result := Lines;
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.DrawTextBox(X, Y, Width, Height: Single;
  const Text: string; Padding: Integer; WordWrap: Boolean);
var
  Lines: TStringList;
  i: Integer;
  CurrentY: Single;
begin
  if WordWrap then
    Lines := WordWrapText(Text, Round(Width - Padding * 2))
  else
  begin
    Lines := TStringList.Create;
    Lines.Text := Text;
  end;

  try
    CurrentY := Y + Padding;
    for i := 0 to Lines.Count - 1 do
    begin
      if CurrentY + FLineHeight > Y + Height - Padding then Break;
      DrawText(X + Padding, CurrentY, Lines[i]);
      CurrentY := CurrentY + FLineHeight;
    end;
  finally
    Lines.Free;
  end;
end;

//-----------------------------------------------------------------------------
function TGLFontRenderer.GetTextWidth(const Text: string): Integer;
var
  i: Integer;
  CharCode: Integer;
  Info: TCharInfo;
begin
  Result := 0;
  for i := 1 to Length(Text) do
  begin
    CharCode := Ord(Text[i]);
    EnsureGlyph(CharCode);
    if FCharMap.TryGetValue(CharCode, Info) then
      Result := Result + Info.AdvanceX;
  end;
end;

//-----------------------------------------------------------------------------
function TGLFontRenderer.GetTextHeight(const Text: string): Integer;
begin
  if Text = '' then
    Result := 0
  else
    Result := FLineHeight;
end;

//-----------------------------------------------------------------------------
function TGLFontRenderer.GetCharWidth(Ch: Char): Integer;
var
  CharCode: Integer;
  Info: TCharInfo;
begin
  CharCode := Ord(Ch);
  EnsureGlyph(CharCode);
  if FCharMap.TryGetValue(CharCode, Info) then
    Result := Info.AdvanceX
  else
    Result := FLineHeight div 2;
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.SetColor(R, G, B, Alpha: Single);
begin
  glColor4f(R, G, B, Alpha);
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.SetColor(Color: TColor; Alpha: Single);
var
  RGB: TColor;
begin
  RGB := ColorToRGB(Color);
  glColor4f(GetRValue(RGB) / 255.0,
            GetGValue(RGB) / 255.0,
            GetBValue(RGB) / 255.0,
            Alpha);
end;

//-----------------------------------------------------------------------------
end.
