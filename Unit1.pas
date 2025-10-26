unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  OpenGL,Math;

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
    procedure CalculateMovingAverage(Period: Integer);
  public
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
  FMovingAverage: array of Single; // Массив для скользящей средней
implementation

{$R *.dfm}

{ TGLPanel }

procedure TGLPanel.WMPaint(var Message: TWMPaint);
begin
  if Assigned(Owner) and (Owner is TForm1) then
    TForm1(Owner).RenderScene;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Timer1.Interval := 16;
  Timer1.Enabled := False;

  Button1.Caption := 'Start';
  Button1.Left := 10;
  Button1.Top := 10;
  Button1.BringToFront;

  Panel1 := TGLPanel.Create(Self);
  Panel1.Parent := Self;
  Panel1.Align := alClient;
  Panel1.Color := clBlack;
  Panel1.DoubleBuffered := True;
  Panel1.OnResize := Panel1Resize;

  Button1.Parent := Panel1;
  Button1.BringToFront;

  FCounter := 0;

  InitOpenGL;
  SetupViewport;

  Randomize;
  GenerateRandomCandles;
  CalculateMovingAverage(10); // Рассчитываем скользящую среднюю с периодом 10
end;

procedure TForm1.CalculateMovingAverage(Period: Integer);
var
  i, j: Integer;
  Sum: Single;
  Count: Integer;
begin
  SetLength(FMovingAverage, Length(FCandles));

  for i := 0 to Length(FCandles) - 1 do begin
    Sum := 0;
    Count := 0;

    // Суммируем Close цены за последние Period свечей
    for j := Max(0, i - Period + 1) to i do begin
      Sum := Sum + FCandles[j].Close;
      Inc(Count);
    end;

    // Вычисляем среднее
    if Count > 0 then
      FMovingAverage[i] := Sum / Count
    else
      FMovingAverage[i] := 0;
  end;
end;

procedure TForm1.GenerateRandomCandles;
var
  i, CandlesCount: Integer;
  ChartWidth: Integer;
  BasePrice, Change: Single;
  Candle: TCandleData;
begin
  ChartWidth := Panel1.Width - 75 - 5;
  CandlesCount := (ChartWidth div 16) + 1;

  SetLength(FCandles, CandlesCount);

  BasePrice := 0.5;

  for i := 0 to CandlesCount - 1 do begin
    Candle.Open := BasePrice;

    Candle.High := BasePrice + Random * 0.08;
    Candle.Low := BasePrice - Random * 0.08;

    Change := (Random - 0.5) * 0.06;
    Candle.Close := BasePrice + Change;

    if Candle.Close > Candle.High then Candle.High := Candle.Close;
    if Candle.Open > Candle.High then Candle.High := Candle.Open;

    if Candle.Close < Candle.Low then Candle.Low := Candle.Close;
    if Candle.Open < Candle.Low then Candle.Low := Candle.Open;

    if Candle.High > 0.9 then Candle.High := 0.9;
    if Candle.Low < 0.1 then Candle.Low := 0.1;
    if Candle.Open > 0.9 then Candle.Open := 0.9;
    if Candle.Open < 0.1 then Candle.Open := 0.1;
    if Candle.Close > 0.9 then Candle.Close := 0.9;
    if Candle.Close < 0.1 then Candle.Close := 0.1;

    FCandles[i] := Candle;

    BasePrice := Candle.Close;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Timer1.Enabled := False;

  wglMakeCurrent(0, 0);
  wglDeleteContext(RC);
  ReleaseDC(Panel1.Handle, DC);

  Panel1.Free;
end;

procedure TForm1.InitOpenGL;
var
  pfd: TPixelFormatDescriptor;
  nPixelFormat: Integer;
begin
  DC := GetDC(Panel1.Handle);

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

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

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
  DataX: Single;
begin
  wglMakeCurrent(DC, RC);

  glClearColor(0.05, 0.05, 0.1, 1.0);
  glClear(GL_COLOR_BUFFER_BIT);

  ChartLeft := 5;
  ChartTop := 5;
  ChartRight := Panel1.Width - 75;
  ChartBottom := Panel1.Height - 25;
  ChartWidth := ChartRight - ChartLeft;
  ChartHeight := ChartBottom - ChartTop;
  GridSize := 32;

  // Рисуем сетку точками
  glColor4f(1.0, 1.0, 1.0, 1.0);
  glPointSize(1.0);

  glBegin(GL_POINTS);

  x := ChartLeft;
  while x <= ChartRight do begin
    y := ChartTop;
    while y <= ChartBottom do begin
      if Trunc(y - ChartTop) mod 4 < 2 then
        glVertex2f(x, y);
      y := y + 1;
    end;
    x := x + GridSize;
  end;

  y := ChartTop;
  while y <= ChartBottom do begin
    x := ChartLeft;
    while x <= ChartRight do begin
      if Trunc(x - ChartLeft) mod 4 < 2 then
        glVertex2f(x, y);
      x := x + 1;
    end;
    y := y + GridSize;
  end;

  glEnd;

  // Рисуем рамку
  glColor4f(1.0, 1.0, 1.0, 1.0);
  glLineWidth(1.0);

  glBegin(GL_LINE_LOOP);
    glVertex2f(ChartLeft, ChartTop);
    glVertex2f(ChartRight, ChartTop);
    glVertex2f(ChartRight, ChartBottom);
    glVertex2f(ChartLeft, ChartBottom);
  glEnd;

  // Рисуем финансовые свечи (лаймовый цвет)
  if Length(FCandles) > 0 then begin
    var CandleWidth: Integer := 12;

    for i := 0 to Length(FCandles) - 1 do begin
      DataX := ChartLeft + (i * 16) + 8;

      if DataX > ChartRight then Break;

      var YOpen: Single := ChartBottom - (FCandles[i].Open * ChartHeight);
      var YHigh: Single := ChartBottom - (FCandles[i].High * ChartHeight);
      var YLow: Single := ChartBottom - (FCandles[i].Low * ChartHeight);
      var YClose: Single := ChartBottom - (FCandles[i].Close * ChartHeight);

      var IsGreen: Boolean := FCandles[i].Close > FCandles[i].Open;

      // Все свечи лаймовые
      glColor4f(0.0, 1.0, 0.0, 1.0);

      // Рисуем тень (фитиль)
      glLineWidth(1.0);
      glBegin(GL_LINES);
        glVertex2f(DataX, YHigh);
        glVertex2f(DataX, YLow);
      glEnd;

      var BodyTop: Single := Min(YOpen, YClose);
      var BodyBottom: Single := Max(YOpen, YClose);
      var BodyHeight: Single := BodyBottom - BodyTop;

      if BodyHeight < 2 then begin
        glLineWidth(1.0);
        glBegin(GL_LINES);
          glVertex2f(DataX - CandleWidth div 2, YOpen);
          glVertex2f(DataX + CandleWidth div 2, YOpen);
        glEnd;
      end else begin
        if IsGreen then begin
          // Восходящая свеча - только контур (не закрашиваем)
          glLineWidth(1.0);
          glBegin(GL_LINE_LOOP);
            glVertex2f(DataX - CandleWidth div 2, BodyTop);
            glVertex2f(DataX + CandleWidth div 2, BodyTop);
            glVertex2f(DataX + CandleWidth div 2, BodyBottom);
            glVertex2f(DataX - CandleWidth div 2, BodyBottom);
          glEnd;
        end else begin
          // Нисходящая свеча - закрашенное тело
          glBegin(GL_QUADS);
            glVertex2f(DataX - CandleWidth div 2, BodyTop);
            glVertex2f(DataX + CandleWidth div 2, BodyTop);
            glVertex2f(DataX + CandleWidth div 2, BodyBottom);
            glVertex2f(DataX - CandleWidth div 2, BodyBottom);
          glEnd;
        end;
      end;
    end;
  end;

  // Рисуем скользящую среднюю (Moving Average) - красная линия
  if Length(FMovingAverage) > 1 then begin
    glColor4f(1.0, 0.0, 0.0, 1.0); // Красный цвет
    glLineWidth(2.0); // Толщина линии 2 пикселя

    glBegin(GL_LINE_STRIP);

    for i := 0 to Length(FMovingAverage) - 1 do begin
      DataX := ChartLeft + (i * 16) + 8;

      if DataX > ChartRight then Break;

      // Пропускаем нулевые значения (недостаточно данных для MA)
      if FMovingAverage[i] <= 0 then Continue;

      var YMA: Single := ChartBottom - (FMovingAverage[i] * ChartHeight);

      glVertex2f(DataX, YMA);
    end;

    glEnd;
  end;

  SwapBuffers(DC);
end;

procedure TForm1.Panel1Resize(Sender: TObject);
begin
  if RC <> 0 then begin
    SetupViewport;
    GenerateRandomCandles;
    CalculateMovingAverage(10);
    RenderScene;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  RenderScene;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  NewCandle: TCandleData;
  Change: Single;
begin
  Inc(FCounter);

  if Length(FCandles) > 0 then begin
    Move(FCandles[1], FCandles[0], (Length(FCandles) - 1) * SizeOf(TCandleData));

    var LastClose: Single := FCandles[Length(FCandles) - 2].Close;

    NewCandle.Open := LastClose;

    NewCandle.High := LastClose + Random * 0.08;
    NewCandle.Low := LastClose - Random * 0.08;

    Change := (Random - 0.5) * 0.06;
    NewCandle.Close := LastClose + Change;

    if NewCandle.Close > NewCandle.High then NewCandle.High := NewCandle.Close;
    if NewCandle.Open > NewCandle.High then NewCandle.High := NewCandle.Open;
    if NewCandle.Close < NewCandle.Low then NewCandle.Low := NewCandle.Close;
    if NewCandle.Open < NewCandle.Low then NewCandle.Low := NewCandle.Open;

    if NewCandle.High > 0.9 then NewCandle.High := 0.9;
    if NewCandle.Low < 0.1 then NewCandle.Low := 0.1;
    if NewCandle.Open > 0.9 then NewCandle.Open := 0.9;
    if NewCandle.Open < 0.1 then NewCandle.Open := 0.1;
    if NewCandle.Close > 0.9 then NewCandle.Close := 0.9;
    if NewCandle.Close < 0.1 then NewCandle.Close := 0.1;

    FCandles[Length(FCandles) - 1] := NewCandle;

    // Пересчитываем скользящую среднюю
    CalculateMovingAverage(10);
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
