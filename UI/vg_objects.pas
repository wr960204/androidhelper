unit vg_objects;

{$I vg_define.inc}
{$H+}

interface

uses Classes, Controls, SysUtils, vg_utils, vg_scene;

type

  { TvgShape }

  TvgShape = class(TvgVisualObject)
  private
    FFill: TvgBrush;
    FStrokeThickness: single;
    FStroke: TvgBrush;
    FStrokeCap: TvgStrokeCap;
    FStrokeJoin: TvgStrokeJoin;
    FStrokeDash: TvgStrokeDash;
    procedure SetFill(const Value: TvgBrush);
    procedure SetStroke(const Value: TvgBrush);
    procedure SetStrokeThickness(const Value: single);
    function isStrokeThicknessStored: Boolean;
    procedure SetStrokeCap(const Value: TvgStrokeCap);
    procedure SetStrokeJoin(const Value: TvgStrokeJoin);
    procedure SetStrokeDash(const Value: TvgStrokeDash);
  protected
    procedure FillChanged(Sender: TObject); virtual;
    procedure StrokeChanged(Sender: TObject); virtual;
//    function GetUpdateRect: TvgRect; override;
    function GetShapeRect: TvgRect;
    procedure BeforePaint; override;
    procedure AfterPaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Fill: TvgBrush read FFill write SetFill;
    property Stroke: TvgBrush read FStroke write SetStroke;
    property StrokeThickness: single read FStrokeThickness write SetStrokeThickness stored isStrokeThicknessStored;
    property StrokeCap: TvgStrokeCap read FStrokeCap write SetStrokeCap default vgCapFlat;
    property StrokeDash: TvgStrokeDash read FStrokeDash write SetStrokeDash default vgDashSolid;
    property StrokeJoin: TvgStrokeJoin read FStrokeJoin write SetStrokeJoin default vgJoinMiter;
    property ShapeRect: TvgRect read GetShapeRect;
  published
  end;

  TvgLineType = (
    vgLineNormal,
    vgLineHorizontal,
    vgLineVertical
  );

  TvgLine = class(TvgShape)
  private
    FLineType: TvgLineType;
    procedure SetLineType(const Value: TvgLineType);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
  published
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
    property LineType: TvgLineType read FLineType write SetLineType;
  end;

  TvgRectangle = class(TvgShape)
  private
    FyRadius: single;
    FxRadius: single;
    FCorners: TvgCorners;
    FCornerType: TvgCornerType;
    function IsCornersStored: Boolean;
  protected
    procedure SetxRadius(const Value: single); virtual;
    procedure SetyRadius(const Value: single); virtual;
    procedure SetCorners(const Value: TvgCorners); virtual;
    procedure SetCornerType(const Value: TvgCornerType); virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
    property xRadius: single read FxRadius write SetxRadius;
    property yRadius: single read FyRadius write SetyRadius;
    property Corners: TvgCorners read FCorners write SetCorners stored IsCornersStored;
    property CornerType: TvgCornerType read FCornerType write SetCornerType;
  end;

  TvgBlurRectangle = class(TvgRectangle)
  private
    FBuffer: TvgBitmap;
    FSoftness: single;
    FRecreate: boolean;
    procedure SetSoftness(const Value: single);
  protected
    procedure FillChanged(Sender: TObject); override;
    procedure SetxRadius(const Value: single); override;
    procedure SetyRadius(const Value: single); override;
    procedure SetCorners(const Value: TvgCorners); override;
    procedure SetCornerType(const Value: TvgCornerType); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Softness: single read FSoftness write SetSoftness;
  end;

  TvgRoundRect = class(TvgShape)
  private
    FCorners: TvgCorners;
    function IsCornersStored: Boolean;
  protected
    procedure SetCorners(const Value: TvgCorners); virtual;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
    property Corners: TvgCorners read FCorners write SetCorners stored IsCornersStored;
  end;

  TvgBlurRoundRect = class(TvgRoundRect)
  private
    FBuffer: TvgBitmap;
    FSoftness: single;
    FRecreate: boolean;
    procedure SetSoftness(const Value: single);
  protected
    procedure SetCorners(const Value: TvgCorners); override;
    procedure FillChanged(Sender: TObject); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Softness: single read FSoftness write SetSoftness;
  end;

  TvgEllipse = class(TvgShape)
  private
  protected
    function pointInObject(X, Y: single): boolean; override;
    procedure Paint; override;
  public
  published
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
  end;

  TvgPathWrap = (
    vgPathOriginal,
    vgPathFit,
    vgPathStretch,
    vgPathTile
  );

  TvgPath = class(TvgShape)
  private
    FData: TvgPathData;
    FWrapMode: TvgPathWrap;
    procedure SetData(const Value: TvgPathData);
    procedure SetWrapMode(const Value: TvgPathWrap);
  protected
    function pointInObject(X, Y: single): boolean; override;
    procedure DoChanged(Sender: TObject);
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Data: TvgPathData read FData write SetData;
    property Fill;
    property Stroke;
    property StrokeCap;
    property StrokeDash;
    property StrokeJoin;
    property StrokeThickness;
    property WrapMode: TvgPathWrap read FWrapMode write SetWrapMode default vgPathStretch;
  end;

  TvgText = class(TvgShape)
  private
    FText: WideString;
    FFont: TvgFont;
    FVertTextAlign: TvgTextAlign;
    FHorzTextAlign: TvgTextAlign;
    FWordWrap: boolean;
    FAutoSize: boolean;
    FStretch: boolean;
    procedure SetText(const Value: WideString);
    procedure SetFont(const Value: TvgFont);
    procedure SetHorzTextAlign(const Value: TvgTextAlign);
    procedure SetVertTextAlign(const Value: TvgTextAlign);
    procedure SetWordWrap(const Value: boolean);
    procedure SetAutoSize(const Value: boolean);
    procedure SetStretch(const Value: boolean);
  protected
    procedure FontChanged(Sender: TObject); virtual;
    procedure Paint; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    procedure AdjustSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default false;
    property Fill;
    property Font: TvgFont read FFont write SetFont;
    property HorzTextAlign: TvgTextAlign read FHorzTextAlign write SetHorzTextAlign default vgTextAlignCenter;
    property VertTextAlign: TvgTextAlign read FVertTextAlign write SetVertTextAlign default vgTextAlignCenter;
    property Text: WideString read FText write SetText;
    property Stretch: boolean read FStretch write SetStretch default false;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;
  end;

  TvgImageWrap = (
    vgImageOriginal,
    vgImageFit,
    vgImageStretch,
    vgImageTile
  );

  { TvgImage }

  TvgImage = class(TvgVisualObject)
  private
    FBitmap: TvgBitmap;
    FBuffer: TvgBitmap;
    FStretchThread: TThread;
    FOnBitmapLoaded: TNotifyEvent;
    FBitmapMargins: TvgBounds;
    FWrapMode: TvgImageWrap;
    FDisableInterpolation: boolean;
    procedure SetBitmap(const Value: TvgBitmap);
    procedure SetWrapMode(const Value: TvgImageWrap);
  protected
    procedure DoBitmapLoaded(Sender: TObject);
    procedure DoBitmapDestroy(Sender: TObject);
    procedure DoBitmapChanged(Sender: TObject);
    procedure Loaded; override;
    procedure Realign; override;
    procedure Paint; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap: TvgBitmap read FBitmap write SetBitmap;
    property BitmapMargins: TvgBounds read FBitmapMargins write FBitmapMargins;
    property WrapMode: TvgImageWrap read FWrapMode write SetWrapMode;
    property DisableInterpolation: boolean read FDisableInterpolation write FDisableInterpolation default false;
    property OnBitmapLoaded: TNotifyEvent read FOnBitmapLoaded write FOnBitmapLoaded;
  end;

  TvgPaintEvent = procedure (Sender: TObject; const Canvas: TvgCanvas) of object;

  TvgPaintBox = class(TvgVisualObject)
  private
    FOnPaint: TvgPaintEvent;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnPaint: TvgPaintEvent read FOnPaint write FOnPaint;
  end;

  TvgSelection = class(TvgVisualObject)
  private
    FParentBounds: boolean;
    FOnChange: TNotifyEvent;
    FHideSelection: boolean;
    FMinSize: integer;
    FOnTrack: TNotifyEvent;
    FProportional: boolean;
    FGripSize: single;
    procedure SetHideSelection(const Value: boolean);
    procedure SetMinSize(const Value: integer);
    procedure SetGripSize(const Value: single);
  protected
    FMove, FLeftTop, FLeftBottom, FRightTop, FRightBottom: boolean;
    FLeftTopHot, FLeftBottomHot, FRightTopHot, FRightBottomHot: boolean;
    FDownPos, FMovePos: TvgPoint;
    function GetAbsoluteRect: TvgRect; override;
    function GetUpdateRect: TvgRect; override;
    function pointInObject(X, Y: single): boolean; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseLeave; override;
  published
    property GripSize: single read FGripSize write SetGripSize;
    property ParentBounds: boolean read FParentBounds write FParentBounds default true;
    property HideSelection: boolean read FHideSelection write SetHideSelection;
    property MinSize: integer read FMinSize write SetMinSize default 15;
    property Proportional: boolean read FProportional write FProportional;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
  end;

  TvgSelectionPoint = class(TvgVisualObject)
  private
    FOnChange: TNotifyEvent;
    FOnTrack: TNotifyEvent;
    FParentBounds: boolean;
  protected
    FPressed: boolean;
    procedure Paint; override;
    procedure SetHeight(const Value: single); override;
    procedure SetWidth(const Value: single); override;
    function pointInObject(X, Y: single): boolean; override;
    function GetUpdateRect: TvgRect; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
  published
    property ParentBounds: boolean read FParentBounds write FParentBounds default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTrack: TNotifyEvent read FOnTrack write FOnTrack;
  end;

implementation {===============================================================}

uses vg_effects;

{ TvgShape ====================================================================}

constructor TvgShape.Create(AOwner: TComponent);
begin
  inherited;
  FFill := TvgBrush.Create(vgBrushSolid, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
  FStroke := TvgBrush.Create(vgBrushSolid, $FF000000);
  FStroke.SolidColor := $FF000000;
  FStroke.OnChanged := StrokeChanged;
  FStrokeThickness := 1;
end;

destructor TvgShape.Destroy;
begin
  FStroke.Free;
  FFill.Free;
  inherited;
end;

function TvgShape.GetShapeRect: TvgRect;
begin
  Result := LocalRect;
  if FStroke.Style <> vgBrushNone then
    if Odd(Round(FStrokeThickness)) then
    begin
      vgInflateRect(Result, -(FStrokeThickness / 2), -(FStrokeThickness / 2));
    end
    else
      vgInflateRect(Result, -(FStrokeThickness / 2), -(FStrokeThickness / 2));
end;

procedure TvgShape.FillChanged(Sender: TObject);
begin
  Repaint;
  FUpdateEffects := true;
end;

procedure TvgShape.StrokeChanged(Sender: TObject);
begin
  Repaint;
  FUpdateEffects := true;
end;

procedure TvgShape.BeforePaint;
begin
  inherited;
  Canvas.Fill.Assign(FFill);
  Canvas.Stroke.Assign(FStroke);
  Canvas.StrokeThickness := FStrokeThickness;
  Canvas.StrokeCap := FStrokeCap;
  Canvas.StrokeJoin := FStrokeJoin;
  Canvas.StrokeDash := FStrokeDash;
end;

procedure TvgShape.AfterPaint;
begin
  inherited AfterPaint;
  Canvas.StrokeDash := vgDashSolid;
  Canvas.StrokeThickness := 1;
end;

function TvgShape.isStrokeThicknessStored: Boolean;
begin
  Result := StrokeThickness <> 1;
end;

procedure TvgShape.SetFill(const Value: TvgBrush);
begin
  FFill.Assign(Value);
end;

procedure TvgShape.SetStroke(const Value: TvgBrush);
begin
  FStroke.Assign(Value);
end;

procedure TvgShape.SetStrokeThickness(const Value: single);
begin
  if FStrokeThickness <> Value then
  begin
    FStrokeThickness := Value;
    FUpdateEffects := true;
    Repaint;
  end;
end;

procedure TvgShape.SetStrokeCap(const Value: TvgStrokeCap);
begin
  if FStrokeCap <> Value then
  begin
    FStrokeCap := Value;
    FUpdateEffects := true;
    Repaint;
  end;
end;

procedure TvgShape.SetStrokeJoin(const Value: TvgStrokeJoin);
begin
  if FStrokeJoin <> Value then
  begin
    FStrokeJoin := Value;
    FUpdateEffects := true;
    Repaint;
  end;
end;

procedure TvgShape.SetStrokeDash(const Value: TvgStrokeDash);
begin
  if FStrokeDash <> Value then
  begin
    FStrokeDash := Value;
    FUpdateEffects := true;
    Repaint;
  end;
end;

{ TvgLine }

constructor TvgLine.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TvgLine.Paint;
begin
  case FLineType of
    vgLineHorizontal: Canvas.DrawLine(GetShapeRect.TopLeft, vgPoint(GetShapeRect.Right, GetShapeRect.Top), AbsoluteOpacity);
    vgLineVertical: Canvas.DrawLine(GetShapeRect.TopLeft, vgPoint(GetShapeRect.Left, GetShapeRect.Bottom), AbsoluteOpacity);
  else
    Canvas.DrawLine(GetShapeRect.TopLeft, GetShapeRect.BottomRight, AbsoluteOpacity);
  end;
end;

procedure TvgLine.SetLineType(const Value: TvgLineType);
begin
  if FLineType <> Value then
  begin
    FLineType := Value;
    Repaint;
  end;
end;

{ TvgEllipse }

procedure TvgEllipse.Paint;
begin
  Canvas.FillEllipse(GetShapeRect, AbsoluteOpacity);
  Canvas.DrawEllipse(GetShapeRect, AbsoluteOpacity);
end;

function TvgEllipse.pointInObject(X, Y: single): boolean;
var
  P: TvgPoint;
begin
  Result := false;
  P := AbsoluteToLocal(vgPoint(X, Y));
  if Width * Height = 0 then Exit;
  if (Sqr((P.X*2 - Width) / Width) + Sqr((P.Y*2 - Height) / Height) <= 1) then
  begin
    Result := true;
  end;
end;

{ TvgRectangle }

constructor TvgRectangle.Create(AOwner: TComponent);
begin
  inherited;
  FCorners := AllCorners;
  FxRadius := 0;
  FyRadius := 0;
end;

function TvgRectangle.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

procedure TvgRectangle.Paint;
begin
  Canvas.FillRect(GetShapeRect, xRadius, yRadius, FCorners, AbsoluteOpacity, CornerType);
  Canvas.DrawRect(GetShapeRect, xRadius, yRadius, FCorners, AbsoluteOpacity, CornerType);
end;

procedure TvgRectangle.SetCorners(const Value: TvgCorners);
begin
  if FCorners <> Value then
  begin
    FCorners := Value;
    Repaint;
  end;
end;

procedure TvgRectangle.SetCornerType(const Value: TvgCornerType);
begin
  if FCornerType <> Value then
  begin
    FCornerType := Value;
    FUpdateEffects := true;
    Repaint;
  end;
end;

procedure TvgRectangle.SetxRadius(const Value: single);
begin
  if FxRadius <> Value then
  begin
    FxRadius := Value;
    FUpdateEffects := true;
    Repaint;
  end;
end;

procedure TvgRectangle.SetyRadius(const Value: single);
begin
  if FyRadius <> Value then
  begin
    FyRadius := Value;
    FUpdateEffects := true;
    Repaint;
  end;
end;

{ TvgBlurRectangle }

constructor TvgBlurRectangle.Create(AOwner: TComponent);
begin
  inherited;
  Stroke.Style := vgBrushNone;
  FSoftness := 0.3;
  FRecreate := true;
end;

destructor TvgBlurRectangle.Destroy;
begin
  if FBuffer <> nil then
    FreeAndNil(FBuffer);
  inherited;
end;

procedure TvgBlurRectangle.Paint;
var
  R: TvgRect;
  Radius: integer;
begin
  Radius := Trunc(FSoftness * 15);
  if FBuffer = nil then
  begin
    FBuffer := TvgBitmap.Create(Round(Width), Round(Height));
    FRecreate := true;
  end;
  if (FBuffer.Width <> Round(Width)) or (FBuffer.Height <> Round(Height)) then
  begin
    FBuffer.SetSize(Round(Width), Round(Height));
    FRecreate := true;
  end;

  if FRecreate then
  begin
    R := vgRect(Radius, Radius, Width - Radius, Height - Radius);
    FBuffer.Clear(0);
    FBuffer.Canvas.Fill.Assign(Fill);
    FBuffer.Canvas.FillRect(R, xRadius, yRadius, FCorners, AbsoluteOpacity, CornerType);
    FBuffer.Canvas.Stroke.Assign(Stroke);
    FBuffer.Canvas.DrawRect(R, xRadius, yRadius, FCorners, AbsoluteOpacity, CornerType);
    Blur(nil, FBuffer, Radius);
    FRecreate := false;
  end;
  Canvas.DrawBitmap(FBuffer, LocalRect, LocalRect, AbsoluteOpacity, true);
end;

procedure TvgBlurRectangle.SetSoftness(const Value: single);
begin
  if FSoftness <> Value then
  begin
    FSoftness := Value;
    FRecreate := true;
    Repaint;
  end;
end;

procedure TvgBlurRectangle.SetCorners(const Value: TvgCorners);
begin
  if Value <> Corners then
  begin
    inherited;
    FRecreate := true;
  end;
end;

procedure TvgBlurRectangle.SetCornerType(const Value: TvgCornerType);
begin
  if Value <> CornerType then
  begin
    inherited;
    FRecreate := true;
  end;
end;

procedure TvgBlurRectangle.SetxRadius(const Value: single);
begin
  if Value <> xRadius then
  begin
    inherited;
    FRecreate := true;
  end;
end;

procedure TvgBlurRectangle.SetyRadius(const Value: single);
begin
  if Value <> yRadius then
  begin
    inherited;
    FRecreate := true;
  end;
end;

procedure TvgBlurRectangle.FillChanged(Sender: TObject);
begin
  inherited;
  FRecreate := true;
end;

{ TvgRoundRect }

constructor TvgRoundRect.Create(AOwner: TComponent);
begin
  inherited;
  FCorners := AllCorners;
end;

function TvgRoundRect.IsCornersStored: Boolean;
begin
  Result := FCorners <> AllCorners;
end;

procedure TvgRoundRect.Paint;
var
  Radius: single;
begin
  Radius := vgRectHeight(GetShapeRect) / 2;
  Canvas.FillRect(GetShapeRect, Radius, Radius, FCorners, AbsoluteOpacity);
  Canvas.DrawRect(GetShapeRect, Radius, Radius, FCorners, AbsoluteOpacity);
end;

procedure TvgRoundRect.SetCorners(const Value: TvgCorners);
begin
  if FCorners <> Value then
  begin
    FCorners := Value;
    Repaint;
  end;
end;

{ TvgBlurRoundRect }

constructor TvgBlurRoundRect.Create(AOwner: TComponent);
begin
  inherited;
  Stroke.Style := vgBrushNone;
  FSoftness := 0.3;
  FRecreate := true;
end;

destructor TvgBlurRoundRect.Destroy;
begin
  if FBuffer <> nil then
    FreeAndNil(FBuffer);
  inherited;
end;

procedure TvgBlurRoundRect.Paint;
var
  R: TvgRect;
  Radius: integer;
  rectRadius: single;
begin
  Radius := Trunc(FSoftness * 15);
  if FBuffer = nil then
  begin
    FBuffer := TvgBitmap.Create(Round(Width), Round(Height));
    FRecreate := true;
  end;
  if (FBuffer.Width <> Round(Width)) or (FBuffer.Height <> Round(Height)) then
  begin
    FBuffer.SetSize(Round(Width), Round(Height));
    FRecreate := true;
  end;

  if FRecreate then
  begin
    R := vgRect(Radius, Radius, Width - Radius, Height - Radius);
    rectRadius := vgRectHeight(R) / 2;
    FBuffer.Clear(0);
    FBuffer.Canvas.Fill.Assign(Fill);
    FBuffer.Canvas.FillRect(R, rectRadius, rectRadius, FCorners, AbsoluteOpacity);
    FBuffer.Canvas.Stroke.Assign(Stroke);
    FBuffer.Canvas.DrawRect(R, rectRadius, rectRadius, FCorners, AbsoluteOpacity);
    Blur(nil, FBuffer, Radius);
    FRecreate := false;
  end;
  Canvas.DrawBitmap(FBuffer, LocalRect, LocalRect, AbsoluteOpacity, true);
end;

procedure TvgBlurRoundRect.SetSoftness(const Value: single);
begin
  if FSoftness <> Value then
  begin
    FSoftness := Value;
    FRecreate := true;
    Repaint;
  end;
end;

procedure TvgBlurRoundRect.SetCorners(const Value: TvgCorners);
begin
  if Value <> Corners then
  begin
    inherited;
    FRecreate := true;
  end;
end;

procedure TvgBlurRoundRect.FillChanged(Sender: TObject);
begin
  inherited;
  FRecreate := true;
end;

{ TvgText }

constructor TvgText.Create(AOwner: TComponent);
begin
  inherited;
  FHorzTextAlign := vgTextAlignCenter;
  FVertTextAlign := vgTextAlignCenter;
  FWordWrap := true;
  FText := 'text';
  FFont := TvgFont.Create;
  FFont.OnChanged := FontChanged;
  Fill.DefaultColor := $FF000000;
  Fill.SolidColor := $FF000000;
  Stroke.DefaultStyle := vgBrushNone;
  Stroke.Style := vgBrushNone;
end;

destructor TvgText.Destroy;
begin
  FFont.Free;
  inherited;
end;

function TvgText.GetData: Variant;
begin
  Result := Text;
end;

procedure TvgText.SetData(const Value: Variant);
begin
  Text := Value;
end;

procedure TvgText.FontChanged(Sender: TObject);
begin
  FUpdateEffects := true;
  if FAutoSize then AdjustSize;
  Repaint;
end;

procedure TvgText.Paint;
var
  R: TvgRect;
  M: TvgMatrix;
begin
  Canvas.Font.Assign(FFont);
  if FStretch then
  begin
    R := vgRect(0, 0, 1000, 1000);
    Canvas.MeasureText(R, R, FText, false, FHorzTextAlign, FVertTextAlign);
    vgOffsetRect(R, -R.Left, -R.Top);
    M := IdentityMatrix;
    if not vgIsRectEmpty(R) then
    begin
      M.m11 := vgRectWidth(LocalRect) / vgRectWidth(R);
      M.m22 := vgRectHeight(LocalRect) / vgRectHeight(R);
    end;
    Canvas.MultyMatrix(M);
    vgInflateRect(R, Font.Size / 3, Font.Size / 3);
    Canvas.FillText(R, R, FText, false, AbsoluteOpacity, FHorzTextAlign, FVertTextAlign);
  end
  else
    Canvas.FillText(LocalRect, LocalRect, FText, FWordWrap, AbsoluteOpacity, FHorzTextAlign, FVertTextAlign);
end;

procedure TvgText.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Repaint;
  end;
end;

procedure TvgText.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    AdjustSize;
  end;
end;

procedure TvgText.SetStretch(const Value: boolean);
begin
  if FStretch <> Value then
  begin
    FUpdateEffects := true;
    FStretch := Value;
    if Stretch then
      AutoSize := false;
    Repaint;
  end;
end;

procedure TvgText.AdjustSize;
var
  R: TvgRect;
begin
  if FDisableAlign then Exit;
  FDisableAlign := true;
  if FAutoSize and (Canvas <> nil) and (Canvas.FBufferBits <> nil) and (FText <> '') then
  begin
    Canvas.Font.Assign(FFont);
    if WordWrap then
      R := vgRect(0, 0, Width, 1000)
    else
      R := vgRect(0, 0, 1000, 1000);
    Canvas.MeasureText(R, R, FText, WordWrap, vgTextAlignNear, vgTextAlignNear);
    if not WordWrap then
      Width := R.Right + (Font.Size / 2);
    if VertTextAlign <> vgTextAlignCenter then
      Height := R.Bottom;
  end;
  FDisableAlign := false;
  if FAutoSize and (Canvas <> nil) and (Canvas.FBufferBits <> nil) and (FText <> '') then
    if Parent <> nil then
      Parent.Visual.Realign;
end;

procedure TvgText.Realign;
begin
  inherited;
  if FAutoSize then
    AdjustSize;
end;

procedure TvgText.SetFont(const Value: TvgFont);
begin
  FFont.Assign(Value);
  if FAutoSize then AdjustSize;
end;

procedure TvgText.SetHorzTextAlign(const Value: TvgTextAlign);
begin
  if FHorzTextAlign <> Value then
  begin
    FHorzTextAlign := Value;
    FUpdateEffects := true;
    Repaint;
  end;
end;

procedure TvgText.SetVertTextAlign(const Value: TvgTextAlign);
begin
  if FVertTextAlign <> Value then
  begin
    FVertTextAlign := Value;
    FUpdateEffects := true;
    Repaint;
  end;
end;

procedure TvgText.SetText(const Value: WideString);
begin
  if FText <> Value then
  begin
    FText := Value;
    FUpdateEffects := true;
    if FAutoSize then
      AdjustSize;
    Repaint;
  end;
end;

{ TvgImage ====================================================================}

constructor TvgImage.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TvgBitmap.Create(1, 1);
  FBitmap.OnThreadLoaded := DoBitmapLoaded;
  FBitmap.OnBitmapDestroy := DoBitmapDestroy;
  FBitmap.OnChange := DoBitmapChanged;
  FBitmapMargins := TvgBounds.Create(vgRect(0, 0, 0, 0));
  FWrapMode := vgImageFit;
end;

destructor TvgImage.Destroy;
begin
  FreeAndNil(FBitmapMargins);
  FBitmap.Free;
  if FBuffer <> nil then
    FreeAndNil(FBuffer);
  inherited;
end;

function TvgImage.GetData: Variant;
begin
  Result := '';
end;

procedure TvgImage.SetData(const Value: Variant);
begin
  FBitmap.LoadFromFile(Value);
end;

procedure TvgImage.DoBitmapLoaded(Sender: TObject);
begin
  { Update buffer }
  if Assigned(FOnBitmapLoaded) then
    FOnBitmapLoaded(Self);
  if not (csDestroying in ComponentState) then
    Repaint;
end;

procedure TvgImage.DoBitmapDestroy(Sender: TObject);
begin
end;

procedure TvgImage.DoBitmapChanged(Sender: TObject);
begin
  FUpdateEffects := true;
  Repaint;
end;

procedure TvgImage.Loaded;
begin
  inherited;
end;

procedure TvgImage.Realign;
begin
  inherited;
end;

procedure TvgImage.Paint;
var
  R: TvgRect;
  State: integer;
  i, j: integer;
begin
  if Assigned(Scene) and Scene.GetDesignTime and not Locked and not FInPaintTo then
  begin
    R := LocalRect;
    vgInflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := vgDashDash;
    Canvas.Stroke.Style := vgBrushSolid;
    Canvas.Stroke.SolidColor := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := vgDashSolid;
  end;

  if not FBitmapMargins.MarginEmpty then
  begin
    { lefttop }
    R := vgRect(0, 0, FBitmapMargins.Left, FBitmapMargins.Top);
    Canvas.DrawBitmap(FBitmap, vgRect(0, 0, FBitmapMargins.Left, FBitmapMargins.Top), R, AbsoluteOpacity, true);
    { top }
    R := vgRect(FBitmapMargins.Left, 0, Width - FBitmapMargins.Right, FBitmapMargins.Top);
    Canvas.DrawBitmap(FBitmap, vgRect(FBitmapMargins.Left, 0, FBitmap.Width - FBitmapMargins.Right, FBitmapMargins.Top), R, AbsoluteOpacity, true);
    { righttop }
    R := vgRect(Width - FBitmapMargins.Right, 0, Width, FBitmapMargins.Top);
    Canvas.DrawBitmap(FBitmap, vgRect(FBitmap.Width - FBitmapMargins.Right, 0, FBitmap.Width, FBitmapMargins.Top), R, AbsoluteOpacity, true);
    { left }
    R := vgRect(0, FBitmapMargins.Top, FBitmapMargins.Left, Height - FBitmapMargins.Bottom);
    Canvas.DrawBitmap(FBitmap, vgRect(0, FBitmapMargins.Top, FBitmapMargins.Left, FBitmap.Height - FBitmapMargins.Bottom), R, AbsoluteOpacity, true);
    { center }
    R := vgRect(FBitmapMargins.Left, FBitmapMargins.Top, Width - FBitmapMargins.Right, Height - FBitmapMargins.Bottom);
    Canvas.DrawBitmap(FBitmap, vgRect(FBitmapMargins.Left, FBitmapMargins.Top, FBitmap.Width - FBitmapMargins.Right, FBitmap.Height - FBitmapMargins.Bottom), R, AbsoluteOpacity, true);
    { right }
    R := vgRect(Width - FBitmapMargins.Right, FBitmapMargins.Top, Width, Height - FBitmapMargins.Bottom);
    Canvas.DrawBitmap(FBitmap, vgRect(FBitmap.Width - FBitmapMargins.Right, FBitmapMargins.Top, FBitmap.Width, FBitmap.Height - FBitmapMargins.Bottom), R, AbsoluteOpacity, true);
    { leftbottom }
    R := vgRect(0, Height - FBitmapMargins.Bottom, FBitmapMargins.Left, Height);
    Canvas.DrawBitmap(FBitmap, vgRect(0, FBitmap.Height - FBitmapMargins.Bottom, FBitmapMargins.Left, FBitmap.Height), R, AbsoluteOpacity, true);
    { bottom }
    R := vgRect(FBitmapMargins.Left, Height - FBitmapMargins.Bottom, Width - FBitmapMargins.Right, Height);
    Canvas.DrawBitmap(FBitmap, vgRect(FBitmapMargins.Left, FBitmap.Height - FBitmapMargins.Bottom, FBitmap.Width - FBitmapMargins.Right, FBitmap.Height), R, AbsoluteOpacity, true);
    { rightbottom }
    R := vgRect(Width - FBitmapMargins.Right, Height - FBitmapMargins.Bottom, Width, Height);
    Canvas.DrawBitmap(FBitmap, vgRect(FBitmap.Width - FBitmapMargins.Right, FBitmap.Height - FBitmapMargins.Bottom, FBitmap.Width, FBitmap.Height), R, AbsoluteOpacity, true);
  end
  else
  begin
    case FWrapMode of
      vgImageOriginal:
        begin
          State := Canvas.SaveCanvas;
          Canvas.IntersectClipRect(LocalRect);
          R := vgRect(0, 0, FBitmap.Width, FBitmap.Height);
          Canvas.DrawBitmap(FBitmap, vgRect(0, 0, FBitmap.Width, FBitmap.Height), R, AbsoluteOpacity, true);
          Canvas.RestoreCanvas(State);
        end;
      vgImageFit:
        begin
          R := vgRect(0, 0, FBitmap.Width, FBitmap.Height);
          vgFitRect(R, LocalRect);
          Canvas.DrawBitmap(FBitmap, vgRect(0, 0, FBitmap.Width, FBitmap.Height), R, AbsoluteOpacity, DisableInterpolation)
        end;
      vgImageStretch:
        begin
          R := LocalRect;
          Canvas.DrawBitmap(FBitmap, vgRect(0, 0, FBitmap.Width, FBitmap.Height), R, AbsoluteOpacity, DisableInterpolation)
        end;
      vgImageTile:
        begin
          State := Canvas.SaveCanvas;
          Canvas.IntersectClipRect(LocalRect);

          for i := 0 to round(Width / FBitmap.Width) do
            for j := 0 to round(Height / FBitmap.Height) do
            begin
              R := vgRect(0, 0, FBitmap.Width, FBitmap.Height);
              vgOffsetRect(R, i * FBitmap.Width, j * Bitmap.Height);
              Canvas.DrawBitmap(FBitmap, vgRect(0, 0, FBitmap.Width, FBitmap.Height), R, AbsoluteOpacity, true);
            end;

          Canvas.RestoreCanvas(State);
        end;
    end;
  end;
end;

procedure TvgImage.SetBitmap(const Value: TvgBitmap);
begin
  FBitmap.Assign(Value);
  Repaint;
end;

procedure TvgImage.SetWrapMode(const Value: TvgImageWrap);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    repaint;
  end;
end;

{ TvgPath =====================================================================}

constructor TvgPath.Create(AOwner: TComponent);
begin
  inherited;
  FWrapMode := vgPathStretch;
  FData := TvgPathData.Create;
  FData.OnChanged := DoChanged;
end;

destructor TvgPath.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

procedure TvgPath.DoChanged(Sender: TObject);
begin
  Repaint;
end;

function TvgPath.pointInObject(X, Y: single): boolean;
begin
  if (FData.Handle <> 0) and (Canvas <> nil) and (Length(FData.PathData) > 1) then
  begin
    Result := Canvas.PtInPath(AbsoluteToLocal(vgPoint(X, Y)), GetShapeRect, FData)
  end
  else
    Result := inherited pointInObject(X, Y);
end;

procedure TvgPath.Paint;
var
  R: TvgRect;
  i, j: integer;
  State: integer;
begin
  if not FData.IsEmpty then
  begin
    case FWrapMode of
      vgPathOriginal:
        begin
          State := Canvas.SaveCanvas;
          Canvas.IntersectClipRect(LocalRect);

          R := FData.GetBounds;
          vgOffsetRect(R, shapeRect.Left, shapeRect.Top);
          Canvas.FillPath(FData, R, AbsoluteOpacity);
          Canvas.DrawPath(FData, R, AbsoluteOpacity);

          Canvas.RestoreCanvas(State);
        end;
      vgPathFit:
        begin
          R := FData.GetBounds;
          vgFitRect(R, ShapeRect);
          Canvas.FillPath(FData, R, AbsoluteOpacity);
          Canvas.DrawPath(FData, R, AbsoluteOpacity);
        end;
      vgPathStretch:
        begin
          Canvas.FillPath(FData, ShapeRect, AbsoluteOpacity);
          Canvas.DrawPath(FData, ShapeRect, AbsoluteOpacity);
        end;
      vgPathTile:
        begin
          State := Canvas.SaveCanvas;
          Canvas.IntersectClipRect(LocalRect);

          R := FData.GetBounds;
          for i := 0 to round(Width / vgRectWidth(R)) do
            for j := 0 to round(Height / vgRectHeight(R)) do
            begin
              R := FData.GetBounds;
              vgOffsetRect(R, shapeRect.Left, shapeRect.Top);
              vgOffsetRect(R, i * (vgRectWidth(R) + shapeRect.Left * 2), j * (vgRectHeight(R) + shapeRect.Top * 2));
              Canvas.FillPath(FData, R, AbsoluteOpacity);
              Canvas.DrawPath(FData, R, AbsoluteOpacity);
            end;

          Canvas.RestoreCanvas(State);
        end;
    end;
  end;
  if Assigned(Scene) and Scene.GetDesignTime and not FLocked and not FInPaintTo then
  begin
    R := LocalRect;
    vgInflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := vgDashDash;
    Canvas.Stroke.Style := vgBrushSolid;
    Canvas.Stroke.SolidColor := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
    Canvas.StrokeDash := vgDashSolid;
  end;
end;

procedure TvgPath.SetWrapMode(const Value: TvgPathWrap);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    Repaint;
  end;
end;

procedure TvgPath.SetData(const Value: TvgPathData);
begin
  FData.Assign(Value);
end;

{ TvgPaintBox }

constructor TvgPaintBox.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TvgPaintBox.Destroy;
begin
  inherited;
end;

procedure TvgPaintBox.Paint;
begin
  inherited;
  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas);
end;

{ TvgSelection }

constructor TvgSelection.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := true;
  ParentBounds := true;
  FMinSize := 15;
  FGripSize := 3;
end;

destructor TvgSelection.Destroy;
begin
  inherited;
end;

function TvgSelection.GetAbsoluteRect: TvgRect;
begin
  Result := inherited GetAbsoluteRect;
  vgInflateRect(Result, 5, 5);
end;

function TvgSelection.GetUpdateRect: TvgRect;
begin
  Result := inherited GetUpdateRect;
  vgInflateRect(Result, FGripSize / 2, FGripSize / 2);
end;

procedure TvgSelection.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
var
  R: TvgRect;
begin
  inherited;
  FDownPos := vgPoint(X, Y);
  if Button = mbLeft then
  begin
    R := LocalRect;
    R := vgRect(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize));
    if vgPtInRect(FDownPos, R) then
    begin
      FLeftTop := true;
      Exit;
    end;
    R := LocalRect;
    R := vgRect(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize));
    if vgPtInRect(FDownPos, R) then
    begin
      FRightTop := true;
      Exit;
    end;
    R := LocalRect;
    R := vgRect(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize));
    if vgPtInRect(FDownPos, R) then
    begin
      FRightBottom := true;
      Exit;
    end;
    R := LocalRect;
    R := vgRect(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize));
    if vgPtInRect(FDownPos, R) then
    begin
      FLeftBottom := true;
      Exit;
    end;

    FMove := true;
  end;
end;

procedure TvgSelection.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
var
  P, OldPos, Size: TvgPoint;
  S: single;
  R: TvgRect;
begin
  inherited;
  if Shift = [] then
  begin
    FMovePos := vgPoint(X, Y);
    P := LocalToAbsolute(FMovePos);
    if Parent <> nil then
      P := TvgVisualObject(Parent).AbsolutetoLocal(P);
    R := LocalRect;
    R := vgRect(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize));
    if vgPtInRect(FMovePos, R) and not FLeftTopHot then
    begin
      FLeftTopHot := true;
      Repaint;
    end
    else
      if not vgPtInRect(FMovePos, R) and FLeftTopHot then
      begin
        FLeftTopHot := false;
        Repaint;
      end;
    R := LocalRect;
    R := vgRect(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize));
    if vgPtInRect(FMovePos, R) and not FRightTopHot then
    begin
      FRightTopHot := true;
      Repaint;
    end
    else
      if not vgPtInRect(FMovePos, R) and FRightTopHot then
      begin
        FRightTopHot := false;
        Repaint;
      end;
    R := LocalRect;
    R := vgRect(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize));
    if vgPtInRect(FMovePos, R) and not FRightBottomHot then
    begin
      FRightBottomHot := true;
      Repaint;
    end
    else
      if not vgPtInRect(FMovePos, R) and FRightBottomHot then
      begin
        FRightBottomHot := false;
        Repaint;
      end;
    R := LocalRect;
    R := vgRect(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize));
    if vgPtInRect(FMovePos, R) and not FLeftBottomHot then
    begin
      FLeftBottomHot := true;
      Repaint;
    end
    else
      if not vgPtInRect(FMovePos, R) and FLeftBottomHot then
      begin
        FLeftBottomHot := false;
        Repaint;
      end;
  end;
  if ssLeft in Shift then
  begin
    FMovePos := vgPoint(X, Y);
    if FMove then
    begin
      Position.X := Position.X + (FMovePos.X - FDownPos.X);
      Position.Y := Position.Y + (FMovePos.Y - FDownPos.Y);
      if ParentBounds then
      begin
        if Position.X < 0 then Position.X := 0;
        if Position.Y < 0 then Position.Y := 0;
        if Position.X + Width > TvgVisualObject(Parent).Width then Position.X := TvgVisualObject(Parent).Width - Width;
        if Position.Y + Height > TvgVisualObject(Parent).Height then Position.Y := TvgVisualObject(Parent).Height - Height;
      end;
      if Assigned(FOnTrack) then FOnTrack(Self);
      Exit;
    end;
    OldPos := Position.Point;
    P := LocalToAbsolute(FMovePos);
    if Parent <> nil then
      P := TvgVisualObject(Parent).AbsolutetoLocal(P);
    if ParentBounds then
    begin
      if P.Y < 0 then P.Y := 0;
      if P.X < 0 then P.X := 0;
      if P.X > TvgVisualObject(Parent).Width then
        P.X := TvgVisualObject(Parent).Width;
      if P.Y > TvgVisualObject(Parent).Height then
        P.Y := TvgVisualObject(Parent).Height;
    end;
    if FLeftTop then
    begin
      Repaint;
      Size := vgPoint((Position.X - (P.X + FDownPos.X)), (Position.Y - (P.Y + FDownPos.Y)));
      if Parent <> nil then
        Size := vgPointFromVector(TvgVisualObject(Parent).LocalToAbsoluteVector(vgVector(Size)));
      Size := vgPointFromVector(AbsoluteToLocalVector(vgVector(Size)));

      Width := Width + Size.X;
      Height := Height + Size.Y;
      Position.X := P.X + FDownPos.X;
      Position.Y := P.Y + FDownPos.Y;
      if FProportional then
      begin
        S := Round((Width + Height) / 2);
        Position.X := Position.X + Width - S;
        Position.Y := Position.Y + Height - S;
        Width := S;
        Height := S;
      end;
      if Width < FMinSize then Width := FMinSize;
      if Height < FMinSize then Height := FMinSize;
      if Assigned(FOnTrack) then FOnTrack(Self);
      Repaint;
    end;
    if FRightTop then
    begin
      Repaint;
      Size := vgPoint((P.X{ + FDownPos.X}) - Position.X, (Position.Y - (P.Y + FDownPos.Y)));
      if Parent <> nil then
        Size := vgPointFromVector(TvgVisualObject(Parent).LocalToAbsoluteVector(vgVector(Size)));
      Size := vgPointFromVector(AbsoluteToLocalVector(vgVector(Size)));

      Width := Size.X;
      Height := Height + Size.Y;
      Position.Y := P.Y + FDownPos.Y;
      if FProportional then
      begin
        Width := Round((Width + Height) / 2);
        Position.Y := Position.Y + Height - Width;
        Height := Width;
      end;
      if Width < FMinSize then Width := FMinSize;
      if Height < FMinSize then Height := FMinSize;
      if Assigned(FOnTrack) then FOnTrack(Self);
      Repaint;
    end;
    if FRightBottom then
    begin
      Repaint;
      Size := vgPoint((P.X{ + FDownPos.X}) - Position.X, (P.Y{ + FDownPos.Y)}) - Position.Y);
      if Parent <> nil then
        Size := vgPointFromVector(TvgVisualObject(Parent).LocalToAbsoluteVector(vgVector(Size)));
      Size := vgPointFromVector(AbsoluteToLocalVector(vgVector(Size)));

      Width := Size.X;
      Height := Size.Y;
      if FProportional then
      begin
        Width := Round((Width + Height) / 2);
        Height := Width;
      end;
      if Width < FMinSize then Width := FMinSize;
      if Height < FMinSize then Height := FMinSize;
      if Assigned(FOnTrack) then FOnTrack(Self);
      Repaint;
    end;
    if FLeftBottom then
    begin
      Repaint;
      Size := vgPoint((Position.X - (P.X + FDownPos.X)), (P.Y{ + FDownPos.Y)}) - Position.Y);
      if Parent <> nil then
        Size := vgPointFromVector(TvgVisualObject(Parent).LocalToAbsoluteVector(vgVector(Size)));
      Size := vgPointFromVector(AbsoluteToLocalVector(vgVector(Size)));

      Width := Width + Size.X;
      Position.X := P.X + FDownPos.X;
      Height := Size.Y;
      if FProportional then
      begin
        Height := Round((Width + Height) / 2);
        Position.X := Position.X + Width - Height;
        Width := Height;
      end;
      if Width < FMinSize then Width := FMinSize;
      if Height < FMinSize then Height := FMinSize;
      if Assigned(FOnTrack) then FOnTrack(Self);
      Repaint;
    end;
  end;
end;

function TvgSelection.pointInObject(X, Y: single): boolean;
var
  R: TvgRect;
  P: TvgPoint;
begin
  Result := inherited pointInObject(X, Y);
  if not Result then
  begin
    P := AbsoluteToLocal(vgPoint(X, Y));
    R := LocalRect;
    R := vgRect(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize));
    if vgPtInRect(P, R) then
    begin
      Result := true;
      Exit;
    end;
    R := LocalRect;
    R := vgRect(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize));
    if vgPtInRect(P, R) then
    begin
      Result := true;
      Exit;
    end;
    R := LocalRect;
    R := vgRect(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize));
    if vgPtInRect(P, R) then
    begin
      Result := true;
      Exit;
    end;
    R := LocalRect;
    R := vgRect(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize));
    if vgPtInRect(P, R) then
    begin
      Result := true;
      Exit;
    end;
  end;
end;

procedure TvgSelection.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if FMove or FLeftTop or FLeftBottom or FRightTop or FRightBottom then
  begin
     if Assigned(FOnChange) then FOnChange(Self);
  end;
  FMove := false;
  FLeftTop := false;
  FLeftBottom := false;
  FRightTop := false;
  FRightBottom := false;
end;

procedure TvgSelection.Paint;
var
  R: TvgRect;
begin
  if FHideSelection then Exit;
  R := LocalRect;
  vgInflateRect(R, -0.5, -0.5);
  Canvas.Fill.Style := vgBrushSolid;
  Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Style := vgBrushSolid;
  Canvas.Stroke.SolidColor := $FF1072C5;
  Canvas.StrokeDash := vgDashDash;
  Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  Canvas.StrokeDash := vgDashSolid;
  { angles }
  R := LocalRect;
  if FLeftTopHot then
    Canvas.Fill.SolidColor := $FFFF0000
  else
    Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.Fillellipse(vgRect(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(vgRect(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);
  R := LocalRect;
  if FRightTopHot then
    Canvas.Fill.SolidColor := $FFFF0000
  else
    Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.Fillellipse(vgRect(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(vgRect(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize)), AbsoluteOpacity);
  R := LocalRect;
  if FLeftBottomHot then
    Canvas.Fill.SolidColor := $FFFF0000
  else
    Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.Fillellipse(vgRect(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(vgRect(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
  R := LocalRect;
  if FRightBottomHot then
    Canvas.Fill.SolidColor := $FFFF0000
  else
    Canvas.Fill.SolidColor := $FFFFFFFF;
  Canvas.FillEllipse(vgRect(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(vgRect(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize)), AbsoluteOpacity);
end;

procedure TvgSelection.MouseLeave;
begin
  inherited;
  FLeftTopHot := false;
  FLeftBottomHot := false;
  FRightTopHot := false;
  FRightBottomHot := false;
  Repaint;
end;

procedure TvgSelection.SetHideSelection(const Value: boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    Repaint;
  end;
end;

procedure TvgSelection.SetMinSize(const Value: integer);
begin
  if FMinSize <> Value then
  begin
    FMinSize := Value;
    if FMinSize < 1 then FMinSize := 1;
  end;
end;

procedure TvgSelection.SetGripSize(const Value: single);
begin
  if FGripSize <> Value then
  begin
    FGripSize := Value;
    if FGripSize > 20 then
      FGripSize := 20;
    if FGripSize < 1 then
      FGripSize := 1;
    Repaint;
  end;
end;

{ TvgSelectionPoint }

constructor TvgSelectionPoint.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := true;
  DisableDesignResize := true;
  ParentBounds := true;
  Width := 6;
  Height := 6;
end;

destructor TvgSelectionPoint.Destroy;
begin
  inherited;
end;

function TvgSelectionPoint.pointInObject(X, Y: single): boolean;
var
  P: TvgPoint;
begin
  Result := false;
  P := AbsoluteToLocal(vgPoint(X, Y));
  if (Abs(P.X) < GripSize) and (Abs(P.Y) < GripSize) then
  begin
    Result := true;
  end;
end;

procedure TvgSelectionPoint.Paint;
begin
  inherited;
  Canvas.StrokeThickness := 1;
  Canvas.Stroke.Style := vgBrushSolid;
  Canvas.Stroke.SolidColor := $FF1072C5;
  Canvas.Fill.Style := vgBrushSolid;
  if IsMouseOver then
    Canvas.Fill.SolidColor := $FFFF0000
  else
    Canvas.Fill.SolidColor := $FFFFFFFF;
  if Assigned(FScene) and (FScene.GetSelected = Self) then
    Canvas.Fill.SolidColor := $FFFF0000;
    
  Canvas.Fillellipse(vgRect(- (GripSize), - (GripSize), (GripSize), (GripSize)), AbsoluteOpacity);
  Canvas.DrawEllipse(vgRect(- (GripSize), - (GripSize), (GripSize), (GripSize)), AbsoluteOpacity);
end;

procedure TvgSelectionPoint.SetHeight(const Value: single);
begin
  inherited SetWidth(Gripsize * 2);
end;

procedure TvgSelectionPoint.SetWidth(const Value: single);
begin
  inherited SetHeight(Gripsize * 2);
end;

procedure TvgSelectionPoint.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
  if Button = mbLeft then
    FPressed := true;
end;

procedure TvgSelectionPoint.MouseMove(Shift: TShiftState; X, Y, Dx,
  Dy: single);
var
  P: TvgPoint;
begin
  inherited;
  if FPressed and (Parent <> nil) then
  begin
    P := LocalToAbsolute(vgPoint(X, Y));
    P := Parent.Visual.AbsoluteToLocal(P);
    if ParentBounds then
    begin
      if P.X < 0 then P.X := 0;
      if P.Y < 0 then P.Y := 0;
      if P.X > Parent.Visual.Width then P.X := Parent.Visual.Width;
      if P.Y > Parent.Visual.Height then P.Y := Parent.Visual.Height;
    end;
    Position.X := P.X;
    Position.Y := P.Y;
    if Assigned(FOnTrack) then FOnTrack(Self);
  end;
end;

procedure TvgSelectionPoint.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
  if FPressed then
  begin
    if Assigned(FOnChange) then FOnChange(Self);
  end;
  FPressed := false;
end;

procedure TvgSelectionPoint.MouseEnter;
begin
  inherited;
  Repaint;
end;

procedure TvgSelectionPoint.MouseLeave;
begin
  inherited;
  Repaint;
end;

function TvgSelectionPoint.GetUpdateRect: TvgRect;
begin
  Result := inherited GetUpdateRect;
  vgInflateRect(Result, GripSize, Gripsize);
end;

initialization
  RegisterVGObjects('Shapes', [TvgLine, TvgRectangle, TvgBlurRectangle, TvgRoundRect, TvgBlurRoundRect, TvgEllipse, TvgText, TvgPath, TvgImage, TvgPaintBox]);
  RegisterVGObjects('Design', [TvgSelection, TvgSelectionPoint]);
end.


