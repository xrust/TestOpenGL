unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  OpenGL;

type
  TGLPanel = class(TPanel)
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  end;

  TForm1 = class(TForm)
    Timer1: TTimer;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    Panel1: TGLPanel;
    DC: HDC;
    RC: HGLRC;
    FCounter: Integer;
    procedure InitOpenGL;
    procedure SetupViewport;
    procedure RenderScene;
    procedure Panel1Resize(Sender: TObject);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TGLPanel }

procedure TGLPanel.WMPaint(var Message: TWMPaint);
begin
  // Перехватываем WM_PAINT и вызываем рендеринг через форму
  if Assigned(Owner) and (Owner is TForm1) then
    TForm1(Owner).RenderScene;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Настройка таймера для анимации (до создания панели)
  Timer1.Interval := 16; // ~60 FPS
  Timer1.Enabled := False;

  // Настройка кнопки (до создания панели)
  Button1.Caption := 'Start';
  Button1.Left := 10;
  Button1.Top := 10;
  Button1.BringToFront;

  // Создаем кастомную панель (после кнопки, чтобы она не перекрывала)
  Panel1 := TGLPanel.Create(Self);
  Panel1.Parent := Self;
  Panel1.Align := alClient;
  Panel1.Color := clBlack;
  Panel1.DoubleBuffered := True;
  Panel1.OnResize := Panel1Resize;

  // Кнопка должна быть поверх панели
  Button1.Parent := Panel1;
  Button1.BringToFront;

  FCounter := 0;

  // Инициализация OpenGL
  InitOpenGL;
  SetupViewport;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;

  // Освобождаем OpenGL контекст
  wglMakeCurrent(0, 0);
  wglDeleteContext(RC);
  ReleaseDC(Panel1.Handle, DC);

  // Освобождаем панель
  Panel1.Free;
end;

procedure TForm1.InitOpenGL;
var
  pfd: TPixelFormatDescriptor;
  nPixelFormat: Integer;
begin
  DC := GetDC(Panel1.Handle);

  // Настройка формата пикселей
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

  // Создание контекста OpenGL
  RC := wglCreateContext(DC);
  wglMakeCurrent(DC, RC);

  // Настройка OpenGL
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // Отключаем сглаживание линий для четких пикселей
  glDisable(GL_LINE_SMOOTH);
  glDisable(GL_POINT_SMOOTH);
end;

procedure TForm1.SetupViewport;
begin
  glViewport(0, 0, Panel1.Width, Panel1.Height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  glOrtho(0, Panel1.Width, Panel1.Height, 0, -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
end;

procedure TForm1.RenderScene;
var
  ChartLeft, ChartTop, ChartRight, ChartBottom: Integer;
  ChartWidth, ChartHeight: Integer;
  GridSize: Integer;
  x, y: Single;
  i: Integer;
begin
  wglMakeCurrent(DC, RC);

  // Очистка экрана (темный фон)
  glClearColor(0.05, 0.05, 0.1, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);

  // Параметры графика
  ChartLeft := 5;
  ChartTop := 5;
  ChartRight := Panel1.Width - 75;
  ChartBottom := Panel1.Height - 25;
  ChartWidth := ChartRight - ChartLeft;
  ChartHeight := ChartBottom - ChartTop;
  GridSize := 32;

  // Рисуем сетку точками (белый цвет, непрозрачная)
  glColor4f(1.0, 1.0, 1.0, 1.0);
  glPointSize(1.0);

  glBegin(GL_POINTS);

  // Вертикальные линии сетки (точками)
  x := ChartLeft+32;
  while x <= ChartRight-1 do begin
    y := ChartTop +1;
    while y <= ChartBottom do begin
      // Рисуем точку через каждые 2 пикселя (эффект пунктира)
      if Trunc(y - ChartTop) mod 4 < 2 then
        glVertex2f(x, y);
      y := y + 1;
    end;
    x := x + GridSize;
  end;

  // Горизонтальные линии сетки (точками)
  y := ChartTop+1;
  while y <= ChartBottom do begin
    x := ChartLeft;
    while x <= ChartRight-1 do begin
      // Рисуем точку через каждые 2 пикселя (эффект пунктира)
      if Trunc(x - ChartLeft) mod 4 < 2 then
        glVertex2f(x, y);
      x := x + 1;
    end;
    y := y + GridSize;
  end;

  glEnd;

  // Рисуем рамку графика (белый цвет, сплошная линия, толщина 1 пиксель)
  glColor4f(1.0, 1.0, 1.0, 1.0);
  glLineWidth(1.0);

  glBegin(GL_LINE_LOOP);
    glVertex2f(ChartLeft, ChartTop);
    glVertex2f(ChartRight, ChartTop);
    glVertex2f(ChartRight, ChartBottom);
    glVertex2f(ChartLeft, ChartBottom);
  glEnd;

  SwapBuffers(DC);
end;

procedure TForm1.Panel1Resize(Sender: TObject);
begin
  if RC <> 0 then begin
    SetupViewport;
    RenderScene;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Отрисовка при показе формы
  RenderScene;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Inc(FCounter);
  RenderScene;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if Timer1.Enabled then begin
    Timer1.Enabled := False;
    Button1.Caption := 'Start';
  end else begin
    Timer1.Enabled := True;
    Button1.Caption := 'Stop';
  end;
end;

end.
