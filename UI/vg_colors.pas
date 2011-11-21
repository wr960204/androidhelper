unit vg_colors;

{$I vg_define.inc}

interface

uses dialogs, Controls, Classes, SysUtils, vg_utils,
  vg_scene, vg_objects, StringConsts;

const
  colorPickSize = 10;

type

  TvgColorBox = class(TvgRectangle)
  private
    procedure SetColor(const Value: TvgColor);
    function GetColor: TvgColor;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    property Color: TvgColor read GetColor write SetColor;
  published
  end;

  TvgColorQuad = class(TvgShape)
  private
    FColorBox: TvgColorBox;
    FColorBitmap: TvgBitmap;
    FHue: single;
    FSat: single;
    FLum: single;
    FOnChange: TNotifyEvent;
    FAlpha: single;
    procedure SetHue(const Value: single);
    procedure SetLum(const Value: single);
    procedure SetSat(const Value: single);
    procedure SetAlpha(const Value: single);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    function GetAbsoluteRect: TvgRect; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Hue: single read FHue write SetHue;
    property Lum: single read FLum write SetLum;
    property Sat: single read FSat write SetSat;
    property Alpha: single read FAlpha write SetAlpha;
    property ColorBox: TvgColorBox read FColorBox write FColorBox;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgColorPicker = class(TvgShape)
  private
    FHueBitmap: TvgBitmap;
    FHue: single;
    FColorQuad: TvgColorQuad;
    procedure SetHue(const Value: single);
    function GetColor: TvgColor;
    procedure SetColor(const Value: TvgColor);
  protected
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    function GetAbsoluteRect: TvgRect; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    property Color: TvgColor read GetColor write SetColor;
  published
    property Hue: single read FHue write SetHue;
    property ColorQuad: TvgColorQuad read FColorQuad write FColorQuad;
  end;

  TvgGradientEdit = class(TvgVisualObject)
  private
    FBitmap: TvgBitmap;
    FGradient: TvgGradient;
    FCurrentPoint: integer;
    FCurrentPointInvisible: boolean;
    FMoving: boolean;
    FOnChange: TNotifyEvent;
    FOnSelectPoint: TNotifyEvent;
    FColorPicker: TvgColorPicker;
    procedure SetGradient(const Value: TvgGradient);
    function GetPointRect(const Point: integer): TvgRect;
    procedure DoChanged(Sender: TObject);
    procedure SetCurrentPoint(const Value: integer);
    procedure SetColorPicker(const Value: TvgColorPicker);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure UpdateGradient;
    property Gradient: TvgGradient read FGradient write SetGradient;
    property CurrentPoint: integer read FCurrentPoint write SetCurrentPoint;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnSelectPoint: TNotifyEvent read FOnSelectPoint write FOnSelectPoint;
    property ColorPicker: TvgColorPicker read FColorPicker write SetColorPicker;
  end;

implementation {===============================================================}

{ TvgColorBox }

constructor TvgColorBox.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TvgColorBox.Paint;
var
  i, j: integer;
  SaveIndex: integer;
begin
  SaveIndex := Canvas.SaveCanvas;
  Canvas.IntersectClipRect(GetShapeRect);
  Canvas.Stroke.Style := vgBrushNone;
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.FillRect(GetShapeRect, 0, 0, AllCorners, AbsoluteOpacity);
  Canvas.Fill.SolidColor := $FFD3D3D3;
  for i := 0 to Trunc(Width / 10) + 1 do
    for j := 0 to Trunc(Height / 10) + 1 do
    begin
      if Odd(i + j) then
      begin
        Canvas.FillRect(vgRect(i * 10, j * 10, (i + 1) * 10, (j + 1) * 10), 0, 0, AllCorners, AbsoluteOpacity);
      end;
    end;
  Canvas.RestoreCanvas(SaveIndex);
  BeforePaint;
  inherited;
end;

function TvgColorBox.GetColor: TvgColor;
begin
  Result := Fill.SolidColor;
end;

procedure TvgColorBox.SetColor(const Value: TvgColor);
begin
  Fill.Style := vgBrushSolid;
  Fill.SolidColor := Value;
end;

{ TvgColorQuad }

constructor TvgColorQuad.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := true;
  StrokeThickness := 0;
end;

destructor TvgColorQuad.Destroy;
begin
  if (FColorBitmap <> nil) and (Canvas <> nil) then
    FColorBitmap.Free;
  inherited;
end;

function TvgColorQuad.GetAbsoluteRect: TvgRect;
begin
  Result := inherited GetAbsoluteRect;
  vgInflateRect(Result, colorPickSize / 2, colorPickSize / 2);
end;

procedure TvgColorQuad.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if ssLeft in Shift then
  begin
    if Height <> 0 then
      Lum := 1 - ((Y - StrokeThickness) / (Height - (StrokeThickness * 2)));
    if Width <> 0 then
      Sat := ((X - StrokeThickness) / (Width - (StrokeThickness * 2)));
  end;
end;

procedure TvgColorQuad.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  MouseMove([ssLeft], X, Y, 0, 0);
end;

procedure TvgColorQuad.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorBox) then
    ColorBox := nil;
end;

procedure TvgColorQuad.Paint;
var
  i, j: integer;
  R: TvgRect;
begin
  if FColorBitmap = nil then
  begin
    FColorBitmap := TvgBitmap.Create(Trunc(Width - (StrokeThickness * 2)), Trunc(Height - (StrokeThickness * 2)));
    if FColorBitmap <> nil then
    begin
      for i := 0 to FColorBitmap.Width - 1 do
      begin
        for j := 0 to FColorBitmap.Height - 1 do
        begin
          FColorBitmap.Scanline[j][i] := vgCorrectColor(HSLtoRGB(FHue, i / FColorBitmap.Width, (1 - (j / FColorBitmap.Height))));
          {$ifdef FPC_BIG_ENDIAN}
          ReverseBytes(@FColorBitmap.Scanline[j][i], 4);
          {$endif}
        end;
      end;
    end;
  end;
  if FColorBitmap <> nil then
    Canvas.DrawBitmap(FColorBitmap, vgRect(0, 0, FColorBitmap.Width, FColorBitmap.Height),
      vgRect(StrokeThickness, StrokeThickness, Width - StrokeThickness, Height - StrokeThickness), AbsoluteOpacity);
  { current }
  R := vgRect(FSat * (Width - (StrokeThickness * 2)), (1 - FLum) * (Height - (StrokeThickness * 2)),
    FSat * (Width - (StrokeThickness * 2)), (1 - FLum) * (Height - (StrokeThickness * 2)));
  vgInflateRect(R, colorPickSize / 2, colorPickSize / 2);
  vgOffsetRect(R, StrokeThickness, StrokeThickness);
  Canvas.Stroke.Style := vgBrushSolid;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.SolidColor := $FF000000;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  vgInflateRect(R, -1, -1);
  Canvas.Stroke.SolidColor := $FFFFFFFF;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  vgInflateRect(R, -1, -1);
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := HSLtoRGB(Hue, sat, Lum);
  Canvas.FillEllipse(R, AbsoluteOpacity);
end;

procedure TvgColorQuad.SetAlpha(const Value: single);
begin
  if FAlpha <> Value then
  begin
    FAlpha := Value;
    if FAlpha < 0 then FAlpha := 0;
    if FAlpha > 1 then FAlpha := 1;
    if FColorBox <> nil then
      FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TvgColorQuad.SetHue(const Value: single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    if FHue < 0 then FHue := 0;
    if FHue > 1 then FHue := 1;
    if FColorBitmap <> nil then
      FreeAndNil(FColorBitmap);
    if FColorBox <> nil then
      FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TvgColorQuad.SetLum(const Value: single);
begin
  if FLum <> Value then
  begin
    FLum := Value;
    if FLum < 0 then FLum := 0;
    if FLum > 1 then FLum := 1;
    if FColorBox <> nil then
      FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

procedure TvgColorQuad.SetSat(const Value: single);
begin
  if FSat <> Value then
  begin
    FSat := Value;
    if FSat < 0 then FSat := 0;
    if FSat > 1 then FSat := 1;
    if FColorBox <> nil then
      FColorBox.Color := HSLtoRGB(Hue, Sat, Lum) and $FFFFFF or (Round(Alpha * $FF) shl 24);
    if Assigned(FOnChange) then
      FOnChange(Self);
    Repaint;
  end;
end;

{ TvgColorPicker ==============================================================}

constructor TvgColorPicker.Create(AOwner: TComponent);
begin
  inherited;
  StrokeThickness := 0;
  AutoCapture := true;
end;

destructor TvgColorPicker.Destroy;
begin
  if (FHueBitmap <> nil) and (Canvas <> nil) then
    FreeAndNil(FHueBitmap);
  inherited;
end;

procedure TvgColorPicker.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorQuad) then
    ColorQuad := nil;
end;

function TvgColorPicker.GetAbsoluteRect: TvgRect;
begin
  Result := inherited GetAbsoluteRect;
  vgInflateRect(Result, 0, colorPickSize / 2);
end;

procedure TvgColorPicker.MouseMove(Shift: TShiftState; X, Y, Dx,
  Dy: single);
begin
  inherited;
  if ssLeft in Shift then
  begin
    if Height <> 0 then
      Hue := ((Y - StrokeThickness) / (Height - (StrokeThickness * 2)));
  end;
end;

procedure TvgColorPicker.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  MouseMove([ssLeft], X, Y, 0, 0);
end;

procedure TvgColorPicker.Paint;
var
  i, j: integer;
  R: TvgRect;
begin
  if FHueBitmap = nil then
  begin
    FHueBitmap := TvgBitmap.Create(Trunc(Width - (StrokeThickness * 2)), Trunc(Height - (StrokeThickness * 2)));
    if FHueBitmap <> nil then
    begin
      for j := 0 to FHueBitmap.Height - 1 do
      begin
        for i := 0 to FHueBitmap.Width - 1 do
        begin
          FHueBitmap.Scanline[j][i] := vgCorrectColor(HSLtoRGB(j / FHueBitmap.Height, 0.9, 0.5));
          {$ifdef FPC_BIG_ENDIAN}
          ReverseBytes(@FHueBitmap.Scanline[j][i], 4);
          {$endif}
        end;
      end;
    end;
  end;

  if FHueBitmap <> nil then
    Canvas.DrawBitmap(FHueBitmap, vgRect(0, 0, FHueBitmap.Width, FHueBitmap.Height),
      vgRect(StrokeThickness, StrokeThickness, Width - StrokeThickness, Height - StrokeThickness), AbsoluteOpacity);

  { hue pos }
  R := vgRect(Width / 2, FHue * (Height - (StrokeThickness * 2)),
    Width / 2, FHue * (Height - (StrokeThickness * 2)));
  vgInflateRect(R, colorPickSize / 2, colorPickSize / 2);
  vgOffsetRect(R, StrokeThickness - 1, StrokeThickness);
  Canvas.Stroke.Style := vgBrushSolid;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.SolidColor := $FF000000;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  vgInflateRect(R, -1, -1);
  Canvas.Stroke.SolidColor := $FFFFFFFF;
  Canvas.DrawEllipse(R, AbsoluteOpacity);
  vgInflateRect(R, -1, -1);
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := HSLtoRGB(Hue, 0.9, 0.5);
  Canvas.FillEllipse(R, AbsoluteOpacity);
end;

function TvgColorPicker.GetColor: TvgColor;
begin
end;

procedure TvgColorPicker.SetColor(const Value: TvgColor);
var
  H, S, L: single;
  SaveChange: TNotifyEvent;
begin
  RGBtoHSL(Value, H, S, L);
  Hue := H;
  if FColorQuad <> nil then
  begin
    FColorQuad.Alpha := TvgColorRec(Value).A / $FF;
    FColorQuad.Hue := H;
    FColorQuad.Sat := S;
    FColorQuad.Lum := L;
  end;
end;

procedure TvgColorPicker.SetHue(const Value: single);
begin
  if FHue <> Value then
  begin
    FHue := Value;
    if FHue < 0 then FHue := 0;
    if FHue > 1 then FHue := 1;
    if FColorQuad <> nil then
      FColorQuad.Hue := FHue;
    Repaint;
  end;
end;

{ TvgGradientEdit ==============================================================}

constructor TvgGradientEdit.Create(AOwner: TComponent);
begin
  inherited;
  FGradient := TvgGradient.Create;
  FGradient.OnChanged := DoChanged;
  Width := 200;
  Height := 20;
  AutoCapture := true;
end;

destructor TvgGradientEdit.Destroy;
begin
  FGradient.Free;
  inherited;
end;

function TvgGradientEdit.GetPointRect(const Point: integer): TvgRect;
begin
  if (Point >= 0) and (Point < FGradient.Points.Count) then
  with FGradient do
  begin
    Result := vgRect(0 + colorPickSize + (Points[Point].Offset * (Width - ((0 + colorPickSize) * 2))), Height - 0 - colorPickSize,
      0 + colorPickSize + (Points[Point].Offset * (Width - ((0 + colorPickSize) * 2))), Height - 0);
    vgInflateRect(Result, colorPickSize / 2, 0);
  end
  else
    Result := vgRect(0, 0, 0, 0);
end;

procedure TvgGradientEdit.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
var
  NewOffset: single;
  NewColor: TvgColor;
  i: integer;
begin
  inherited;
  FMoving := false;
  if Button = mbLeft then
  begin
    { select point }
    for i := 0 to FGradient.Points.Count - 1 do
      if vgPtInRect(vgPoint(X, Y), GetPointRect(i)) then
      begin
        CurrentPoint := i;
        if Assigned(OnSelectPoint) then
          OnSelectPoint(Self);
        FMoving := true;
        Repaint;
        Exit;
      end;
    { add new point }
    if (Y > 0) and (Y < Height - 0 - colorPickSize) then
    begin
      NewOffset := ((X - 0 - colorPickSize) / (Width - ((0 + colorPickSize)* 2)));
      if NewOffset < 0 then NewOffset := 0;
      if NewOffset > 1 then NewOffset := 1;
      NewColor := FGradient.InterpolateColor(NewOffset);
      for i := 1 to FGradient.Points.Count - 1 do
        if NewOffset < FGradient.Points[i].Offset then
          with TvgGradientPoint(FGradient.Points.Add) do
          begin
            Index := i;
            CurrentPoint := Index;
            IntColor := NewColor;
            Offset := NewOffset;
            Repaint;
            DoChanged(Self);
            Break;
          end;
    end;
  end;
end;

procedure TvgGradientEdit.MouseMove(Shift: TShiftState; X, Y, Dx,
  Dy: single);
begin
  inherited;
  if ssLeft in Shift then
  begin
    if FMoving then
    begin
      FCurrentPointInvisible := ((Y < -10) or (Y > Height + 10)) and (FGradient.Points.Count > 1) and
        (CurrentPoint <> 0) and (CurrentPoint <> FGradient.Points.Count - 1);
      { move }
      FGradient.Points[CurrentPoint].Offset := ((X - 0 - colorPickSize) / (Width - ((0 + colorPickSize) * 2)));
      if FGradient.Points[CurrentPoint].Offset < 0 then
        FGradient.Points[CurrentPoint].Offset := 0;
      if FGradient.Points[CurrentPoint].Offset > 1 then
        FGradient.Points[CurrentPoint].Offset := 1;
      { move right }
      if CurrentPoint < FGradient.Points.Count - 1 then
        if FGradient.Points[CurrentPoint].Offset > FGradient.Points[CurrentPoint + 1].Offset then
        begin
          FGradient.Points[CurrentPoint].Index := FGradient.Points[CurrentPoint].Index + 1;
          CurrentPoint := CurrentPoint + 1;
        end;
      { move left }
      if CurrentPoint > 0 then
        if FGradient.Points[CurrentPoint].Offset < FGradient.Points[CurrentPoint - 1].Offset then
        begin
          FGradient.Points[CurrentPoint].Index := FGradient.Points[CurrentPoint].Index - 1;
          CurrentPoint := CurrentPoint - 1;
        end;
      Repaint;
      DoChanged(Self);
    end;
  end;
end;

procedure TvgGradientEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
begin
  inherited;
  FCurrentPointInvisible := false;
  if FMoving then
  begin
    { delete }
    if (Y > Height + 10) and (FGradient.Points.Count > 1) then
    begin
      FGradient.Points.Delete(CurrentPoint);
      CurrentPoint := CurrentPoint - 1;
      if CurrentPoint < 0 then CurrentPoint := 0;
      Repaint;
      DoChanged(Self);
      FMoving := false;
      Exit;
    end;
  end;
  FMoving := false;
end;

procedure TvgGradientEdit.Paint;
var
  i, j: integer;
  R: TvgRect;
  SaveIndex: integer;
begin
//  Canvas.DrawRect(vgRect(0, 0, Width, Height));
  if FBitmap = nil then
  begin
    FBitmap := TvgBitmap.Create(Trunc(Width - (0 * 2)), Trunc(Height - (0 * 2) - colorPickSize));
  end;
  if FBitmap <> nil then
  begin
    for j := 0 to FBitmap.Height - 1 do
    begin
      for i := 0 to FBitmap.Width - 1 do
      begin
        FBitmap.Scanline[j][i] := vgCorrectColor(FGradient.InterpolateColor(i / FBitmap.Width));
        {$ifdef FPC_BIG_ENDIAN}
        ReverseBytes(@FBitmap.Scanline[j][i], 4);
        {$endif}
      end;
    end;
  end;

  { draw back }
  R := vgRect(0 + colorPickSize, 0, Width - 0 - colorPickSize, Height - 0 - colorPickSize);
  SaveIndex := Canvas.SaveCanvas;
  Canvas.IntersectClipRect(R);
  Canvas.Stroke.Style := vgBrushNone;
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.FillRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  Canvas.Fill.SolidColor := $FFD3D3D3;
  for i := 0 to Trunc(Width / 10) + 1 do
    for j := 0 to Trunc(Height / 10) + 1 do
    begin
      if Odd(i + j) then
      begin
        Canvas.FillRect(vgRect(i * 10, j * 10, (i + 1) * 10, (j + 1) * 10), 0, 0, AllCorners, AbsoluteOpacity);
      end;
    end;
  Canvas.RestoreCanvas(SaveIndex);
  { draw gradient }
  Canvas.Stroke.Style := vgBrushSolid;
  Canvas.StrokeThickness := 0;
  if FBitmap <> nil then
  begin
    Canvas.DrawBitmap(FBitmap, vgRect(0, 0, FBitmap.Width, FBitmap.Height),
      vgRect(0 + colorPickSize, 0, Width - 0 - colorPickSize, Height - 0 - colorPickSize), AbsoluteOpacity);
  end;
  { points }
  for i := 0 to FGradient.Points.Count - 1 do
  begin
    if FCurrentPointInvisible and (i = CurrentPoint) then Continue;
    R := GetPointRect(i);
    vgInflateRect(R, -1, -1);
    Canvas.Stroke.SolidColor := $FF000000;
    Canvas.Fill.SolidColor := FGradient.Points[i].IntColor;
    Canvas.FillEllipse(R, AbsoluteOpacity);
    Canvas.DrawEllipse(R, AbsoluteOpacity);
    { color }
    if CurrentPoint = i then
    begin
      vgInflateRect(R, 1, 1);
      Canvas.Stroke.SolidColor := $FF000000;
      Canvas.Stroke.SolidColor := $FFFFFFFF;
      Canvas.DrawEllipse(R, AbsoluteOpacity);
    end;
  end;
end;

procedure TvgGradientEdit.SetGradient(const Value: TvgGradient);
begin
  FGradient.Assign(Value);
end;

procedure TvgGradientEdit.DoChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
  UpdateGradient;
end;

procedure TvgGradientEdit.SetCurrentPoint(const Value: integer);
begin
  if FCurrentPoint <> Value then
  begin
    FCurrentPoint := Value;
    if Assigned(OnSelectPoint) then
      OnSelectPoint(Self);
    if (FColorPicker <> nil) and (CurrentPoint >= 0) then
      FColorPicker.Color := Gradient.Points[CurrentPoint].IntColor;
  end;
end;

procedure TvgGradientEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FColorPicker) then
    ColorPicker := nil;
end;

procedure TvgGradientEdit.SetColorPicker(const Value: TvgColorPicker);
begin
  FColorPicker := Value;
  if (FColorPicker <> nil) and (CurrentPoint >= 0) then
    FColorPicker.Color := Gradient.Points[CurrentPoint].IntColor;
end;

procedure TvgGradientEdit.UpdateGradient;
begin
  if (FColorPicker <> nil) and (CurrentPoint >= 0) then
    FColorPicker.Color := Gradient.Points[CurrentPoint].IntColor;
end;

initialization
  RegisterVGObjects(VG_I_COLORS, [TvgColorQuad, TvgColorPicker, TvgGradientEdit, TvgColorBox]);
end.



