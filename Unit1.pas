unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  OpenGL, Math;

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
    procedure GenerateRandomCandles;
  end;
// Данные для свечей
type TCandleData = record
  Open: Single;
  High: Single;
  Low: Single;
  Close: Single;
end;
var
  Form1: TForm1;
  FCandles: array of TCandleData;
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

  // Генерируем случайные свечи для графика
  Randomize;
  GenerateRandomCandles;
end;

procedure TForm1.GenerateRandomCandles;
var
  i, CandlesCount: Integer;
  ChartWidth: Integer;
  BasePrice, Change: Single;
  Candle: TCandleData;
begin
  ChartWidth := Panel1.Width - 75 - 5; // Ширина графика
  CandlesCount := (ChartWidth div 16) + 1; // Количество свечей (каждые 16 пикселей)

  SetLength(FCandles, CandlesCount);

  // Генерируем случайные свечи
  BasePrice := 0.5; // Начальная цена (середина графика)

  for i := 0 to CandlesCount - 1 do begin
    // Open - цена открытия (текущая базовая цена)
    Candle.Open := BasePrice;

    // Генерируем High и Low
    Candle.High := BasePrice + Random * 0.08; // Максимум выше на 0-8%
    Candle.Low := BasePrice - Random * 0.08;  // Минимум ниже на 0-8%

    // Close - цена закрытия (случайно выше или ниже Open)
    Change := (Random - 0.5) * 0.06; // Изменение от -3% до +3%
    Candle.Close := BasePrice + Change;

    // Убеждаемся, что High действительно максимум
    if Candle.Close > Candle.High then Candle.High := Candle.Close;
    if Candle.Open > Candle.High then Candle.High := Candle.Open;

    // Убеждаемся, что Low действительно минимум
    if Candle.Close < Candle.Low then Candle.Low := Candle.Close;
    if Candle.Open < Candle.Low then Candle.Low := Candle.Open;

    // Ограничиваем значения от 0.1 до 0.9
    if Candle.High > 0.9 then Candle.High := 0.9;
    if Candle.Low < 0.1 then Candle.Low := 0.1;
    if Candle.Open > 0.9 then Candle.Open := 0.9;
    if Candle.Open < 0.1 then Candle.Open := 0.1;
    if Candle.Close > 0.9 then Candle.Close := 0.9;
    if Candle.Close < 0.1 then Candle.Close := 0.1;

    FCandles[i] := Candle;

    // Следующая базовая цена = Close текущей свечи
    BasePrice := Candle.Close;
  end;
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
  DataX, DataY: Single;
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
  x := ChartLeft;
  while x <= ChartRight do begin
    y := ChartTop;
    while y <= ChartBottom do begin
      // Рисуем точку через каждые 2 пикселя (эффект пунктира)
      if Trunc(y - ChartTop) mod 4 < 2 then
        glVertex2f(x, y);
      y := y + 1;
    end;
    x := x + GridSize;
  end;

  // Горизонтальные линии сетки (точками)
  y := ChartTop;
  while y <= ChartBottom do begin
    x := ChartLeft;
    while x <= ChartRight do begin
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

  // Рисуем финансовые свечи
  if Length(FCandles) > 0 then begin
    var CandleWidth: Integer := 12; // Ширина тела свечи

    for i := 0 to Length(FCandles) - 1 do begin
      // Вычисляем координаты свечи
      DataX := ChartLeft + (i * 16) + 8; // Центр свечи (каждые 16 пикселей)

      // Проверяем, что свеча в пределах графика
      if DataX > ChartRight then Break;

      // Преобразуем цены в координаты Y
      var YOpen: Single := ChartBottom - (FCandles[i].Open * ChartHeight);
      var YHigh: Single := ChartBottom - (FCandles[i].High * ChartHeight);
      var YLow: Single := ChartBottom - (FCandles[i].Low * ChartHeight);
      var YClose: Single := ChartBottom - (FCandles[i].Close * ChartHeight);

      // Определяем цвет свечи (зеленая если выросла, красная если упала)
      var IsGreen: Boolean := FCandles[i].Close > FCandles[i].Open;

      if IsGreen then
        glColor4f(0.0, 1.0, 0.0, 1.0) // Зеленый
      else
        glColor4f(1.0, 0.0, 0.0, 1.0); // Красный

      // Рисуем тень (фитиль) - тонкая линия от Low до High
      glLineWidth(1.0);
      glBegin(GL_LINES);
        glVertex2f(DataX, YHigh);
        glVertex2f(DataX, YLow);
      glEnd;

      // Рисуем тело свечи - прямоугольник от Open до Close
      var BodyTop: Single := Min(YOpen, YClose);
      var BodyBottom: Single := Max(YOpen, YClose);
      var BodyHeight: Single := BodyBottom - BodyTop;

      // Если тело слишком маленькое (цены почти равны), рисуем горизонтальную линию
      if BodyHeight < 2 then begin
        glLineWidth(1.0);
        glBegin(GL_LINES);
          glVertex2f(DataX - CandleWidth div 2, YOpen);
          glVertex2f(DataX + CandleWidth div 2, YOpen);
        glEnd;
      end else begin
        // Рисуем закрашенное тело свечи
        glBegin(GL_QUADS);
          glVertex2f(DataX - CandleWidth div 2, BodyTop);
          glVertex2f(DataX + CandleWidth div 2, BodyTop);
          glVertex2f(DataX + CandleWidth div 2, BodyBottom);
          glVertex2f(DataX - CandleWidth div 2, BodyBottom);
        glEnd;
      end;
    end;
  end;

  SwapBuffers(DC);
end;

procedure TForm1.Panel1Resize(Sender: TObject);
begin
  if RC <> 0 then begin
    SetupViewport;
    GenerateRandomCandles; // Пересоздаем свечи при изменении размера
    RenderScene;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  // Отрисовка при показе формы
  RenderScene;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  NewCandle: TCandleData;
  Change: Single;
begin
  Inc(FCounter);

  // Обновляем свечи (добавляем новую свечу)
  if Length(FCandles) > 0 then begin
    // Сдвигаем свечи влево
    Move(FCandles[1], FCandles[0], (Length(FCandles) - 1) * SizeOf(TCandleData));

    // Создаем новую свечу на основе последней
    var LastClose: Single := FCandles[Length(FCandles) - 2].Close;

    NewCandle.Open := LastClose;

    // Генерируем High и Low
    NewCandle.High := LastClose + Random * 0.08;
    NewCandle.Low := LastClose - Random * 0.08;

    // Close - случайное изменение
    Change := (Random - 0.5) * 0.06;
    NewCandle.Close := LastClose + Change;

    // Корректируем High и Low
    if NewCandle.Close > NewCandle.High then NewCandle.High := NewCandle.Close;
    if NewCandle.Open > NewCandle.High then NewCandle.High := NewCandle.Open;
    if NewCandle.Close < NewCandle.Low then NewCandle.Low := NewCandle.Close;
    if NewCandle.Open < NewCandle.Low then NewCandle.Low := NewCandle.Open;

    // Ограничиваем значения
    if NewCandle.High > 0.9 then NewCandle.High := 0.9;
    if NewCandle.Low < 0.1 then NewCandle.Low := 0.1;
    if NewCandle.Open > 0.9 then NewCandle.Open := 0.9;
    if NewCandle.Open < 0.1 then NewCandle.Open := 0.1;
    if NewCandle.Close > 0.9 then NewCandle.Close := 0.9;
    if NewCandle.Close < 0.1 then NewCandle.Close := 0.1;

    FCandles[Length(FCandles) - 1] := NewCandle;
  end;

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
