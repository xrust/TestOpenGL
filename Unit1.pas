unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  OpenGL, uGLFontRenderer;

type
  TGLPanel = class(TPanel)
  protected
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  public
    Panel1: TGLPanel;
    DC: HDC;
    RC: HGLRC;
    FontRenderer: TGLFontRenderer;
    FontBold: TGLFontRenderer;
    FCounter: Integer;
    procedure InitOpenGL;
    procedure SetupViewport;
    procedure RenderScene;
    procedure Panel1Resize(Sender: TObject);
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
  // Создаем кастомную панель
  Panel1 := TGLPanel.Create(Self);
  Panel1.Parent := Self;
  Panel1.Align := alClient;
  Panel1.Color := clBlack;
  Panel1.DoubleBuffered := True;
  Panel1.OnResize := Panel1Resize;

  // Настройка таймера для анимации
  Timer1.Interval := 16; // ~60 FPS
  Timer1.Enabled := False;

  // Настройка кнопки
  Button1.Caption := 'Start';
  Button1.Parent := Self;
  Button1.Left := 10;
  Button1.Top := 10;
  Button1.BringToFront;

  FCounter := 0;

  // Инициализация OpenGL
  InitOpenGL;
  SetupViewport;

  // ВАЖНО: Создаем рендереры ПОСЛЕ инициализации OpenGL контекста
  FontRenderer := TGLFontRenderer.Create('Tahoma', 10, False, False, True);
  FontBold := TGLFontRenderer.Create('Arial', 14, True, False, True);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;

  // Освобождаем рендереры
  FontRenderer.Free;
  FontBold.Free;

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
  glEnable(GL_LINE_SMOOTH);
  glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
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
  txt: string;
  offset: Single;
begin
  wglMakeCurrent(DC, RC);

  // Очистка экрана (темно-синий фон)
  glClearColor(0, 0, 0, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);

  // Рисуем прямоугольник для демонстрации
  glColor4f(0.2, 0.2, 0.3, 1.0);
  glBegin(GL_QUADS);
    glVertex2f(50, 150);
    glVertex2f(Panel1.Width - 50, 150);
    glVertex2f(Panel1.Width - 50, 250);
    glVertex2f(50, 250);
  glEnd;

  // === Примеры рисования текста ===

  // 1. Простой белый текст
  FontRenderer.DrawText(10, 10, 'Hello OpenGL Font Renderer!');

  // 2. Цветной текст (красный)
  FontRenderer.DrawText(10, 30, 'Red Text Example', 1.0, 0.0, 0.0, 1.0);

  // 3. Зеленый текст с прозрачностью
  FontRenderer.DrawText(10, 50, 'Green Semi-Transparent', 0.0, 1.0, 0.0, 0.6);

  // 4. Жирный шрифт (синий)
  FontBold.DrawText(10, 70, 'Bold Font Example', 0.3, 0.6, 1.0, 1.0);

  // 5. Анимированный текст
  offset := Sin(FCounter * 0.05) * 20;
  FontBold.DrawText(10 + offset, 100, 'Animated Text', 1.0, 1.0, 0.0, 1.0);

  // 6. Выравнивание по центру
  FontBold.DrawTextAligned(50, 160, Panel1.Width - 100, 80,
                           'Text Centered in Box',
                           taCenter, vaMiddle);

  // 7. Текст с использованием TColor
  FontRenderer.SetColor(clYellow, 1.0);
  FontRenderer.DrawText(10, 270, 'Using TColor (Yellow)');

  // 8. Счетчик кадров
  txt := Format('Frame: %d', [FCounter]);
  FontRenderer.SetColor(clWhite, 1.0);
  FontRenderer.DrawText(Panel1.Width - 100, 10, txt);

  // 9. Информация о размерах
  txt := Format('Width: %d  Height: %d',
                [FontRenderer.GetTextWidth('Sample Text'),
                 FontRenderer.GetTextHeight('Sample Text')]);
  FontRenderer.DrawText(10, Panel1.Height - 30, txt);

  // 10. Многострочный текст в рамке
  FontRenderer.DrawTextBox(Panel1.Width - 250, 300, 230, 100,
    'This is a multi-line text example with automatic word wrapping. ' +
    'The text will wrap when it reaches the edge of the box.',
    10, True);

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
    Inc(FCounter);
  RenderScene;

  if Timer1.Enabled then begin
    Timer1.Enabled := False;
    Button1.Caption := 'Start';
  end else begin
    Timer1.Enabled := True;
    Button1.Caption := 'Stop';
  end;
end;

end.
