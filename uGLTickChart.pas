unit uGLTickChart;
{//----------------------------------------------------------------------------
    OpenGL Tick Chart Component
    Компонент финансового графика с использованием OpenGL
}//----------------------------------------------------------------------------
interface
//-----------------------------------------------------------------------------
uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.ExtCtrls,
  OpenGL;

//-----------------------------------------------------------------------------
type
  TGLTickChart = class
  private
    FPanel: TPanel;
    FPaintBox: TPaintBox;
    DC: HDC;
    RC: HGLRC;
    
    // Параметры графика
    FChartLeft: Integer;
    FChartTop: Integer;
    FChartRight: Integer;
    FChartBottom: Integer;
    FChartWidth: Integer;
    FChartHeight: Integer;
    
    // Параметры отступов
    FMarginLeft: Integer;
    FMarginTop: Integer;
    FMarginRight: Integer;
    FMarginBottom: Integer;
    
    // Параметры сетки
    FGridSize: Integer;
    
    // Цвета
    FBackgroundColor: TColor;
    FGridColor: TColor;
    FBorderColor: TColor;
    
    procedure InitOpenGL;
    procedure SetupViewport;
    procedure UpdateChartBounds;
    procedure OnPaintBoxPaint(Sender: TObject);
    procedure OnPaintBoxResize(Sender: TObject);
    
  protected
    procedure DrawGrid;
    procedure DrawBorder;
    
  public
    constructor Create(APanel: TPanel);
    destructor Destroy; override;
    
    procedure Render;
    procedure Invalidate;
    
    // Свойства отступов
    property MarginLeft: Integer read FMarginLeft write FMarginLeft;
    property MarginTop: Integer read FMarginTop write FMarginTop;
    property MarginRight: Integer read FMarginRight write FMarginRight;
    property MarginBottom: Integer read FMarginBottom write FMarginBottom;
    
    // Свойства сетки
    property GridSize: Integer read FGridSize write FGridSize;
    
    // Свойства цветов
    property BackgroundColor: TColor read FBackgroundColor write FBackgroundColor;
    property GridColor: TColor read FGridColor write FGridColor;
    property BorderColor: TColor read FBorderColor write FBorderColor;
  end;

//-----------------------------------------------------------------------------
implementation

//-----------------------------------------------------------------------------
{ TGLTickChart }

constructor TGLTickChart.Create(APanel: TPanel);
begin
  inherited Create;
  
  FPanel := APanel;
  
  // Значения по умолчанию для отступов
  FMarginLeft := 5;
  FMarginTop := 5;
  FMarginRight := 75;
  FMarginBottom := 25;
  
  // Размер сетки
  FGridSize := 32;
  
  // Цвета по умолчанию
  FBackgroundColor := RGB(13, 13, 26);  // Темный фон
  FGridColor := clWhite;
  FBorderColor := clWhite;
  
  // Создаем PaintBox для рисования
  FPaintBox := TPaintBox.Create(FPanel);
  FPaintBox.Parent := FPanel;
  FPaintBox.Align := alClient;
  FPaintBox.OnPaint := OnPaintBoxPaint;
  FPaintBox.OnResize := OnPaintBoxResize;
  
  // Инициализация OpenGL
  InitOpenGL;
  SetupViewport;
  UpdateChartBounds;
end;

//-----------------------------------------------------------------------------
destructor TGLTickChart.Destroy;
begin
  // Освобождаем OpenGL контекст
  if RC <> 0 then begin
    wglMakeCurrent(0, 0);
    wglDeleteContext(RC);
  end;
  
  if DC <> 0 then
    ReleaseDC(FPaintBox.Handle, DC);
  
  FPaintBox.Free;
  
  inherited;
end;

//-----------------------------------------------------------------------------
procedure TGLTickChart.InitOpenGL;
var
  pfd: TPixelFormatDescriptor;
  nPixelFormat: Integer;
begin
  DC := GetDC(FPaintBox.Handle);
  
  FillChar(pfd, SizeOf(pfd), 0);
  with pfd do begin
    nSize := SizeOf(pfd);
    nVersion := 1;
    dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER;
    iPixelType := PFD_TYPE_RGBA;
    cColorBits := 24;
    cDepthBits := 16;
    iLayerType := PFD_MAIN_PLANE;
  end;
  
  nPixelFormat := ChoosePixelFormat(DC, @pfd);
  SetPixelFormat(DC, nPixelFormat, @pfd);
  
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);
  
  // Настройка OpenGL
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POINT_SMOOTH);
end;

//-----------------------------------------------------------------------------
procedure TGLTickChart.SetupViewport;
begin
  if RC = 0 then Exit;
  
  wglMakeCurrent(DC, RC);
  
  glViewport(0, 0, FPaintBox.Width, FPaintBox.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, FPaintBox.Width, FPaintBox.Height, 0, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

//-----------------------------------------------------------------------------
procedure TGLTickChart.UpdateChartBounds;
begin
  FChartLeft := FMarginLeft;
  FChartTop := FMarginTop;
  FChartRight := FPaintBox.Width - FMarginRight;
  FChartBottom := FPaintBox.Height - FMarginBottom;
  FChartWidth := FChartRight - FChartLeft;
  FChartHeight := FChartBottom - FChartTop;
end;

//-----------------------------------------------------------------------------
procedure TGLTickChart.DrawGrid;
var
  x, y: Single;
begin
  // Рисуем сетку точками (пунктирная линия)
  glColor4f(GetRValue(FGridColor) / 255.0, 
            GetGValue(FGridColor) / 255.0, 
            GetBValue(FGridColor) / 255.0, 
            1.0);
  glPointSize(1.0);
  
  glBegin(GL_POINTS);
  
  // Вертикальные линии сетки
  x := FChartLeft;
  while x <= FChartRight do begin
    y := FChartTop;
    while y <= FChartBottom do begin
      // Рисуем точку через каждые 2 пикселя (эффект пунктира)
      if Trunc(y - FChartTop) mod 4 < 2 then
        glVertex2f(x, y);
      y := y + 1;
    end;
    x := x + FGridSize;
  end;
  
  // Горизонтальные линии сетки
  y := FChartTop;
  while y <= FChartBottom do begin
    x := FChartLeft;
    while x <= FChartRight do begin
      // Рисуем точку через каждые 2 пикселя (эффект пунктира)
      if Trunc(x - FChartLeft) mod 4 < 2 then
        glVertex2f(x, y);
      x := x + 1;
    end;
    y := y + FGridSize;
  end;
  
  glEnd;
end;

//-----------------------------------------------------------------------------
procedure TGLTickChart.DrawBorder;
begin
  // Рисуем рамку графика
  glColor4f(GetRValue(FBorderColor) / 255.0, 
            GetGValue(FBorderColor) / 255.0, 
            GetBValue(FBorderColor) / 255.0, 
            1.0);
  glLineWidth(1.0);
  
  glBegin(GL_LINE_LOOP);
    glVertex2f(FChartLeft, FChartTop);
    glVertex2f(FChartRight, FChartTop);
    glVertex2f(FChartRight, FChartBottom);
    glVertex2f(FChartLeft, FChartBottom);
  glEnd;
end;

//-----------------------------------------------------------------------------
procedure TGLTickChart.Render;
begin
  if RC = 0 then Exit;
  
  wglMakeCurrent(DC, RC);
  
  // Очистка экрана
  glClearColor(GetRValue(FBackgroundColor) / 255.0, 
               GetGValue(FBackgroundColor) / 255.0, 
               GetBValue(FBackgroundColor) / 255.0, 
               1.0);
  glClear(GL_COLOR_BUFFER_BIT);
  
  // Рисуем элементы графика
  DrawGrid;
  DrawBorder;
  
  SwapBuffers(DC);
end;

//-----------------------------------------------------------------------------
procedure TGLTickChart.Invalidate;
begin
  if Assigned(FPaintBox) then
    FPaintBox.Invalidate;
end;

//-----------------------------------------------------------------------------
procedure TGLTickChart.OnPaintBoxPaint(Sender: TObject);
begin
  Render;
end;

//-----------------------------------------------------------------------------
procedure TGLTickChart.OnPaintBoxResize(Sender: TObject);
begin
  SetupViewport;
  UpdateChartBounds;
  Render;
end;

//-----------------------------------------------------------------------------
end.