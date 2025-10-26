unit uGLFontRenderer;
{//----------------------------------------------------------------------------
    OpenGL Font Renderer with Unicode Font Atlas
    Качественный рендеринг Unicode текста в OpenGL с использованием шрифтового атласа
    Ver 4.0 support unoicode
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
type
  TTextAlignment = (taLeft, taCenter, taRight);
  TTextVAlignment = (vaTop, vaMiddle, vaBottom);

//-----------------------------------------------------------------------------
type TGLFontRenderer = class
  private
    type TCharInfo = record
      X, Y: Integer;          // позиция символа в атласе
      Width, Height: Integer; // размер символа в пикселях
      OffsetX, OffsetY: Integer; // смещение для корректного позиционирования
      AdvanceX: Integer;      // расстояние до следующего символа
    end;

  private
    FTextureID: GLuint;
    FCharMap: TDictionary<WideChar, TCharInfo>; // Unicode символы
    FFontName: string;
    FFontSize: Integer;
    FLineHeight: Integer;
    FAtlasWidth: Integer;
    FAtlasHeight: Integer;
    FBold: Boolean;
    FItalic: Boolean;
    FAntialiased: Boolean;
    FInitialized: Boolean;

    procedure BuildFontAtlas;
    procedure FreeFontAtlas;
    function GetTextureData(Bmp: TBitmap): TBytes;
    function GetDefaultCharSet: TArray<WideChar>;

  public
    constructor Create(const FontName: string = 'Tahoma'; FontSize: Integer = 10;
                       Bold: Boolean = False; Italic: Boolean = False; Antialiased: Boolean = True);
    destructor Destroy; override;

    // Основные методы рисования
    procedure DrawText(X, Y: Single; const Text: string); overload;
    procedure DrawText(X, Y: Single; const Text: string; R, G, B: Single; Alpha: Single = 1.0); overload;
    procedure DrawTextAligned(X, Y, Width, Height: Single; const Text: string;
                              HAlign: TTextAlignment = taLeft; VAlign: TTextVAlignment = vaTop);
    procedure DrawTextBox(X, Y, Width, Height: Single; const Text: string;
                          Padding: Integer = 5; WordWrap: Boolean = True);

    // Вспомогательные методы
    function GetTextWidth(const Text: string): Integer;
    function GetTextHeight(const Text: string): Integer;
    function GetCharWidth(Ch: WideChar): Integer;
    procedure SetColor(R, G, B: Single; Alpha: Single = 1.0); overload;
    procedure SetColor(Color: TColor; Alpha: Single = 1.0); overload;

    // Свойства
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

  FCharMap := TDictionary<WideChar, TCharInfo>.Create;

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
function TGLFontRenderer.GetDefaultCharSet: TArray<WideChar>;
var
  List: TList<WideChar>;
  i: Integer;
begin
  List := TList<WideChar>.Create;
  try
    // Базовые ASCII символы (32-126)
    for i := 32 to 126 do
      List.Add(WideChar(i));

    // Кириллица (А-Я, а-я) - основные символы
    for i := $0410 to $044F do // А-Я, а-я
      List.Add(WideChar(i));

    List.Add(WideChar($0401)); // Ё
    List.Add(WideChar($0451)); // ё

    // Дополнительные популярные символы
    List.Add('€'); // Евро
    List.Add('°'); // Градус
    List.Add('±'); // Плюс-минус
    List.Add('×'); // Умножение
    List.Add('÷'); // Деление
    List.Add('№'); // Номер

    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.BuildFontAtlas;
var
  Bmp: TBitmap;
  i, x, y, maxH: Integer;
  Ch: WideChar;
  CharWidth, CharHeight: Integer;
  TextureData: TBytes;
  FontStyle: TFontStyles;
  CharSet: TArray<WideChar>;
  CharInfo: TCharInfo;
  TextToDraw: string;
begin
  Bmp := TBitmap.Create;
  try
    // Настройка шрифта
    Bmp.Canvas.Font.Name := FFontName;
    Bmp.Canvas.Font.Size := FFontSize;
    Bmp.Canvas.Font.Color := clWhite;
    Bmp.Canvas.Font.Charset := DEFAULT_CHARSET;

    FontStyle := [];
    if FBold then FontStyle := FontStyle + [fsBold];
    if FItalic then FontStyle := FontStyle + [fsItalic];
    Bmp.Canvas.Font.Style := FontStyle;

    if FAntialiased then
      Bmp.Canvas.Font.Quality := fqAntialiased
    else
      Bmp.Canvas.Font.Quality := fqNonAntialiased;

    Bmp.PixelFormat := pf24bit;

    // Размер атласа
    FAtlasWidth := 1024;
    FAtlasHeight := 512;
    FLineHeight := Bmp.Canvas.TextHeight('Щy') + 4; // Учитываем высокие символы кириллицы

    Bmp.Width := FAtlasWidth;
    Bmp.Height := FAtlasHeight;
    Bmp.Canvas.Brush.Color := clBlack;
    Bmp.Canvas.FillRect(Rect(0, 0, FAtlasWidth, FAtlasHeight));

    // Получаем набор символов для атласа
    CharSet := GetDefaultCharSet;

    // Рисуем все символы в атлас
    x := 4;
    y := 4;
    maxH := 0;

    for i := 0 to Length(CharSet) - 1 do begin
      Ch := CharSet[i];
      TextToDraw := string(Ch);

      CharWidth := Bmp.Canvas.TextWidth(TextToDraw);
      CharHeight := Bmp.Canvas.TextHeight(TextToDraw);

      if (CharWidth <= 0) or (CharHeight <= 0) then Continue;

      // Переход на новую строку если не влезает
      if x + CharWidth + 4 > FAtlasWidth then begin
        x := 4;
        y := y + maxH + 4;
        maxH := 0;

        if y + CharHeight + 4 > FAtlasHeight then
          Break; // Атлас заполнен
      end;

      // Рисуем символ
      Bmp.Canvas.TextOut(x, y, TextToDraw);

      // Сохраняем информацию о символе
      CharInfo.X := x;
      CharInfo.Y := y;
      CharInfo.Width := CharWidth;
      CharInfo.Height := CharHeight;
      CharInfo.OffsetX := 0;
      CharInfo.OffsetY := 0;
      CharInfo.AdvanceX := CharWidth + 1;

      FCharMap.AddOrSetValue(Ch, CharInfo);

      x := x + CharWidth + 4;
      if CharHeight > maxH then maxH := CharHeight;
    end;

    // Получаем данные текстуры
    TextureData := GetTextureData(Bmp);

    // Создаем OpenGL текстуру
    glGenTextures(1, @FTextureID);
    glBindTexture(GL_TEXTURE_2D, FTextureID);

    // Настройка параметров текстуры
    if FAntialiased then begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    end else begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    end;

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

    // Загружаем текстуру
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, FAtlasWidth, FAtlasHeight,
                 0, GL_RGBA, GL_UNSIGNED_BYTE, @TextureData[0]);

    FInitialized := True;

  finally
    Bmp.Free;
  end;
end;

//-----------------------------------------------------------------------------
function TGLFontRenderer.GetTextureData(Bmp: TBitmap): TBytes;
type
  PRGBTriple = ^TRGBTriple;
  TRGBTriple = packed record
    B, G, R: Byte;
  end;
var
  x, y: Integer;
  Brightness: Byte;
  Index: Integer;
  ScanLine: Pointer;
  PixelPtr: PRGBTriple;
begin
  SetLength(Result, FAtlasWidth * FAtlasHeight * 4); // RGBA
  Index := 0;

  // Быстрое чтение через ScanLine
  for y := 0 to FAtlasHeight - 1 do begin
    ScanLine := Bmp.ScanLine[y];
    PixelPtr := PRGBTriple(ScanLine);

    for x := 0 to FAtlasWidth - 1 do begin
      // Берем яркость (для белого текста все компоненты равны)
      Brightness := PixelPtr^.R;

      // RGBA формат
      Result[Index] := 255;        // R - белый
      Result[Index + 1] := 255;    // G - белый
      Result[Index + 2] := 255;    // B - белый
      Result[Index + 3] := Brightness; // A - прозрачность

      Inc(Index, 4);
      Inc(PixelPtr);
    end;
  end;
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.FreeFontAtlas;
begin
  if FTextureID <> 0 then begin
    glDeleteTextures(1, @FTextureID);
    FTextureID := 0;
  end;
  FCharMap.Clear;
  FInitialized := False;
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
  Ch: WideChar;
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
  for i := 1 to Length(Text) do begin
    Ch := Text[i];

    // Проверяем наличие символа в атласе
    if not FCharMap.TryGetValue(Ch, Info) then begin
      // Символ не найден - используем пробел или пропускаем
      if Ch = ' ' then
        XPos := XPos + FLineHeight div 3
      else
        XPos := XPos + FLineHeight div 2;
      Continue;
    end;

    // Пропускаем символы без размера
    if (Info.Width <= 0) or (Info.Height <= 0) then begin
      if Ch = ' ' then
        XPos := XPos + FLineHeight div 3;
      Continue;
    end;

    // Вычисляем текстурные координаты
    tx1 := Info.X / FAtlasWidth;
    ty1 := Info.Y / FAtlasHeight;
    tx2 := (Info.X + Info.Width) / FAtlasWidth;
    ty2 := (Info.Y + Info.Height) / FAtlasHeight;

    // Вычисляем экранные координаты
    x1 := XPos + Info.OffsetX;
    y1 := Y + Info.OffsetY;
    x2 := x1 + Info.Width;
    y2 := y1 + Info.Height;

    // Рисуем квад с текстурой символа
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

  // Горизонтальное выравнивание
  case HAlign of
    taLeft: DrawX := X;
    taCenter: DrawX := X + (Width - TextW) / 2;
    taRight: DrawX := X + Width - TextW;
  else
    DrawX := X;
  end;

  // Вертикальное выравнивание
  case VAlign of
    vaTop: DrawY := Y;
    vaMiddle: DrawY := Y + (Height - TextH) / 2;
    vaBottom: DrawY := Y + Height - TextH;
  else
    DrawY := Y;
  end;

  DrawText(DrawX, DrawY, Text);
end;

//-----------------------------------------------------------------------------
procedure TGLFontRenderer.DrawTextBox(X, Y, Width, Height: Single;
  const Text: string; Padding: Integer; WordWrap: Boolean);
var
  Lines: TStringList;
  i: Integer;
  CurrentY: Single;
  Line: string;
begin
  Lines := TStringList.Create;
  try
    if WordWrap then begin
      // Простой word wrap
      Line := '';
      for i := 1 to Length(Text) do begin
        Line := Line + Text[i];
        if (Text[i] = ' ') or (i = Length(Text)) then begin
          if GetTextWidth(Line) > Width - Padding * 2 then begin
            Lines.Add(Line);
            Line := '';
          end;
        end;
      end;
      if Line <> '' then Lines.Add(Line);
    end else begin
      Lines.Text := Text;
    end;

    CurrentY := Y + Padding;
    for i := 0 to Lines.Count - 1 do begin
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
  Ch: WideChar;
  Info: TCharInfo;
begin
  Result := 0;
  for i := 1 to Length(Text) do begin
    Ch := Text[i];
    if FCharMap.TryGetValue(Ch, Info) then
      Result := Result + Info.AdvanceX
    else if Ch = ' ' then
      Result := Result + FLineHeight div 3
    else
      Result := Result + FLineHeight div 2;
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
function TGLFontRenderer.GetCharWidth(Ch: WideChar): Integer;
var
  Info: TCharInfo;
begin
  if FCharMap.TryGetValue(Ch, Info) then
    Result := Info.AdvanceX
  else if Ch = ' ' then
    Result := FLineHeight div 3
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
