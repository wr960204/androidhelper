unit vg_controls;

{$I vg_define.inc}

interface

uses
  {$IFDEF DARWIN}
  macosall,
  CarbonProc, CarbonDef, CarbonPrivate, carboncanvas,
  {$ENDIF}
  {$IFDEF FPC}
  LCLProc, LCLIntf, LCLType, LMessages, LResources,
  {$ENDIF}
  {$IFNDEF FPC}
  Windows, Messages,
  {$ENDIF}
  Classes, Menus, Forms, Controls, SysUtils, ExtCtrls, vg_utils, vg_scene,
  vg_classes, vg_objects, vg_ani, StringConsts;

type

  TvgPlacement = (
    vgPlacementBottom,
    vgPlacementTop,
    vgPlacementLeft,
    vgPlacementRight,
    vgPlacementCenter,
    vgPlacementAbsolute,
    vgPlacementMouse
  );

  TvgPopup = class(TvgControl)
  private
    FSaveParent: TvgObject;
    FSaveScene: IvgScene;
    FPopupForm: TCustomForm;
    FPopupScene: TvgScene;
    FIsOpen: boolean;
    FStaysOpen: boolean;
    FPlacement: TvgPlacement;
    FPlacementTarget: TvgVisualObject;
    FPlacementRectangle: TvgBounds;
    FHorizontalOffset: single;
    FVerticalOffset: single;
    FDragWithParent: boolean;
    FDragTimer: TTimer;
    FAnimating: boolean;
    FStyle: TvgResources;
    procedure SetIsOpen(const Value: boolean);
    procedure SetPlacementRectangle(const Value: TvgBounds);
    procedure Dotimer(Sender: TObject);
  protected
    procedure ApplyPlacement; virtual;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure FreeNotify(AObject: TvgObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PopupModal: TModalResult; virtual;
    procedure Popup; virtual;
    procedure ClosePopup; virtual;
  published
    property IsOpen: boolean read FIsOpen write SetIsOpen;
    property HorizontalOffset: single read FHorizontalOffset write FHorizontalOffset;
    property VerticalOffset: single read FVerticalOffset write FVerticalOffset;
    property Placement: TvgPlacement read FPlacement write FPlacement;
    property PlacementTarget: TvgVisualObject read FPlacementTarget write FPlacementTarget;
    property PlacementRectangle: TvgBounds read FPlacementRectangle write SetPlacementRectangle;
    property StaysOpen: boolean read FStaysOpen write FStaysOpen default false;
    property Style: TvgResources read FStyle write FStyle;
    property DragWithParent: boolean read FDragWithParent write FDragWithParent default false;
    property Resource;
    property Visible default false;
  end;

  TvgMessageType = (vgMessageWarning, vgMessageError, vgMessageInformation, vgMessageConfirmation, vgMessageCustom);
  TvgMessageButton = (vgButtonYes, vgButtonNo, vgButtonOK, vgButtonCancel, vgButtonAbort, vgButtonRetry, vgButtonIgnore,
    vgButtonAll, vgButtonNoToAll, vgButtonYesToAll, vgButtonHelp);
  TvgMessageButtons = set of TvgMessageButton;

  TvgMessagePopup = class(TvgPopup)
  private
  protected
    procedure ApplyPlacement; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PopupModal: TModalResult; override;
  published
    property Resource;
    property StaysOpen default true;
    property DragWithParent default true;
  end;

function MessagePopup(const ACaption, AMessage: WideString; AType: TvgMessageType;
  Buttons: TvgMessageButtons; const AOwner: TvgScene; const Target: TvgVisualObject = nil;
  const ABitmap: TvgBitmap = nil;
  const AStyle: TvgResources = nil): integer;

type

  TvgPanel = class(TvgControl)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Resource;
  end;

  TvgStatusBar = class(TvgControl)
  private
    FShowSizeGrip: boolean;
    procedure SetShowSizeGrip(const Value: boolean);
  protected
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Resource;
    property ShowSizeGrip: boolean read FShowSizeGrip write SetShowSizeGrip;
  end;

  TvgToolBar = class(TvgControl)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Resource;
  end;

  TvgTextControl = class(TvgControl)
  private
    FFont: TvgFont;
    FTextAlign: TvgTextAlign;
    FVertTextAlign: TvgTextAlign;
    FFontFill: TvgBrush;
    function GetText: WideString;
    procedure SetFont(const Value: TvgFont);
    procedure SetTextAlign(const Value: TvgTextAlign);
    procedure SetVertTextAlign(const Value: TvgTextAlign);
    procedure SetFontFill(const Value: TvgBrush);
    procedure FontFillChanged(Sender: TObject);
  protected
    FText: WideString;
    procedure ApplyStyle; override;
    procedure SetText(const Value: WideString); virtual;
    procedure FontChanged(Sender: TObject); virtual;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Font: TvgFont read FFont write SetFont;
    property FontFill: TvgBrush read FFontFill write SetFontFill;
    property Text: WideString read GetText write SetText;
    property VertTextAlign: TvgTextAlign read FVertTextAlign write SetVertTextAlign;
    property TextAlign: TvgTextAlign read FTextAlign write SetTextAlign;
  published
  end;

  TvgLabel = class(TvgTextControl)
  private
    FWordWrap: boolean;
    FAutoSize: boolean;
    procedure SetWordWrap(const Value: boolean);
    procedure SetAutoSize(const Value: boolean);
  protected
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSize: boolean read FAutoSize write SetAutoSize default false;
    property Font;
    property TextAlign;
    property Text;
    property Resource;
    property HitTest default false;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;
  end;

  TvgValueLabel = class(TvgLabel)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property WordWrap default false;
  end;

  TvgButton = class(TvgTextControl)
  private
    FPressing: boolean;
    FIsPressed: boolean;
    FModalResult: TModalResult;
    FStaysPressed: boolean;
    procedure SetIsPressed(const Value: boolean);
  protected
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
  published
    { triggers }
    property IsPressed: boolean read FIsPressed write SetIsPressed;
    { props }
    property StaysPressed: boolean read FStaysPressed write FStaysPressed;
    property Font;
    property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;
    property TextAlign;
    property Text;
    property Resource;
  end;

  TvgButtonLayout = (
    vgGlyphLeft,
    vgGlyphRight,
    vgGlyphTop,
    vgGlyphBottom
  );

  TvgBitmapButton = class(TvgButton)
  private
    FBitmap: TvgBitmap;
    FBitmapLayout: TvgButtonLayout;
    FBitmapSpacing: single;
    FBitmapSize: single;
    FBitmapPadding: single;
    procedure SetBitmap(const Value: TvgBitmap);
    procedure SetBitmapLayout(const Value: TvgButtonLayout);
    procedure SetBitmapSpacing(const Value: single);
    procedure SetBitmapSize(const Value: single);
    procedure SetBitmapPadding(const Value: single);
  protected
    procedure DoBitmapChanged(Sender: TObject);
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Bitmap: TvgBitmap read FBitmap write SetBitmap;
    property BitmapLayout: TvgButtonLayout read FBitmapLayout write SetBitmapLayout default vgGlyphLeft;
    property BitmapSpacing: single read FBitmapSpacing write SetBitmapSpacing;
    property BitmapSize: single read FBitmapSize write SetBitmapSize;
    property BitmapPadding: single read FBitmapPadding write SetBitmapPadding;
  end;

  TvgPathButton = class(TvgButton)
  private
    FPath: TvgPathData;
    FPathLayout: TvgButtonLayout;
    FPathSize: single;
    FPathSpacing: single;
    FPathPadding: single;
    FStrokeThickness: single;
    FFill: TvgBrush;
    FStroke: TvgBrush;
    FStrokeCap: TvgStrokeCap;
    FStrokeDash: TvgStrokeDash;
    FStrokeJoin: TvgStrokeJoin;
    procedure SetPath(const Value: TvgPathData);
    procedure SetPathLayout(const Value: TvgButtonLayout);
    procedure SetPathPadding(const Value: single);
    procedure SetPathSize(const Value: single);
    procedure SetPathSpacing(const Value: single);
    function isStrokeThicknessStored: Boolean;
    procedure SetFill(const Value: TvgBrush);
    procedure SetStroke(const Value: TvgBrush);
    procedure SetStrokeCap(const Value: TvgStrokeCap);
    procedure SetStrokeDash(const Value: TvgStrokeDash);
    procedure SetStrokeJoin(const Value: TvgStrokeJoin);
    procedure SetStrokeThickness(const Value: single);
  protected
    procedure DoPathChanged(Sender: TObject);
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Path: TvgPathData read FPath write SetPath;
    property PathLayout: TvgButtonLayout read FPathLayout write SetPathLayout default vgGlyphLeft;
    property PathSpacing: single read FPathSpacing write SetPathSpacing;
    property PathSize: single read FPathSize write SetPathSize;
    property PathPadding: single read FPathPadding write SetPathPadding;
    property PathFill: TvgBrush read FFill write SetFill;
    property PathStroke: TvgBrush read FStroke write SetStroke;
    property PathStrokeThickness: single read FStrokeThickness write SetStrokeThickness stored isStrokeThicknessStored;
    property PathStrokeCap: TvgStrokeCap read FStrokeCap write SetStrokeCap default vgCapFlat;
    property PathStrokeDash: TvgStrokeDash read FStrokeDash write SetStrokeDash default vgDashSolid;
    property PathStrokeJoin: TvgStrokeJoin read FStrokeJoin write SetStrokeJoin default vgJoinMiter;
  end;

  TvgToolButton = class(TvgBitmapButton)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BitmapLayout default vgGlyphTop;
  end;

  TvgToolPathButton = class(TvgPathButton)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    property PathLayout default vgGlyphTop;
  end;

  TvgBitmapStateButton = class(TvgButton)
  private
    FBitmap: TvgBitmap;
    FBitmapDown: TvgBitmap;
    FBitmapHot: TvgBitmap;
    procedure SetBitmap(const Value: TvgBitmap);
    procedure SetBitmapDown(const Value: TvgBitmap);
    procedure SetBitmapHot(const Value: TvgBitmap);
  protected
    procedure DoBitmapChanged(Sender: TObject);
    procedure ApplyStyle; override;
    procedure Paint; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartTriggerAnimation(AInstance: TvgObject; ATrigger: string); override;
  published
    property Bitmap: TvgBitmap read FBitmap write SetBitmap;
    property BitmapHot: TvgBitmap read FBitmapHot write SetBitmapHot;
    property BitmapDown: TvgBitmap read FBitmapDown write SetBitmapDown;
  end;

  TvgSpeedButton = class(TvgButton)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TvgCheckBox = class(TvgTextControl)
  private
    FPressing: boolean;
    FOnChange: TNotifyEvent;
    FIsPressed: boolean;
    FIsChecked: boolean;
    procedure SetIsChecked(const Value: boolean);
  protected
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  published
    { triggers }
    property IsPressed: boolean read FIsPressed;
    property IsChecked: boolean read FIsChecked write SetIsChecked;
    { props }
    property Font;
    property TextAlign;
    property Text;
    property Resource;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgRadioButton = class(TvgTextControl)
  private
    FPressing: boolean;
    FOnChange: TNotifyEvent;
    FIsPressed: boolean;
    FIsChecked: boolean;
    FGroupName: string;
    procedure SetIsChecked(const Value: boolean);
  protected
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  published
    { triggers }
    property IsPressed: boolean read FIsPressed;
    property IsChecked: boolean read FIsChecked write SetIsChecked;
    { props }
    property Font;
    property TextAlign;
    property Text;
    property Resource;
    property GroupName: string read FGroupName write FGroupName;  
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgGroupBox = class(TvgTextControl)
  private
  protected
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Font;
    property TextAlign;
    property Text;
    property Resource;
  end;

  TvgCloseButton = class(TvgControl)
  private
    FPressing: boolean;
    FOnClick: TNotifyEvent;
    FCloseForm: boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
  published
    { props }
    property Resource;
    property CloseForm: boolean read FCloseForm write FCloseForm default true;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TvgSizeGrip = class(TvgControl, IvgSizeGrip)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Resource;
  end;

  TvgSplitter = class(TvgControl)
  private
    FPressed: boolean;
    FControl: TvgVisualObject;
    FDownPos: TvgPoint;
    FMinSize: single;
    FMaxSize: single;
    FNewSize, FOldSize: single;
    FSplit: single;
  protected
    function FindObject: TvgVisualObject;
    procedure CalcSplitSize(X, Y: single; var NewSize, Split: single);
    procedure UpdateSize(X, Y: single);
    function DoCanResize(var NewSize: single): Boolean;
    procedure UpdateControlSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
  published
    property MinSize: single read FMinSize write FMinSize;
  end;

  TvgProgressBar = class(TvgControl)
  private
    FMin: single;
    FValue: single;
    FMax: single;
    FOrientation: TvgOrientation;
    procedure SetMax(const Value: single);
    procedure SetMin(const Value: single);
    procedure SetOrientation(const Value: TvgOrientation);
    procedure SetValue(const Value: single);
  protected
    procedure ApplyStyle; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
  published
    property Min: single read FMin write SetMin;
    property Max: single read FMax write SetMax;
    property Orientation: TvgOrientation read FOrientation write SetOrientation;
    property Value: single read FValue write SetValue;
    property Resource;
  end;

  TvgCustomTrack = class;
  TvgScrollBar = class;

  TvgThumb = class(TvgControl)
  private
    FTrack: TvgCustomTrack;
    FDownOffset: TvgPoint;
    FPressed: boolean;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
  published
    property Resource;
  end;

  TvgCustomTrack = class(TvgControl)
  private
    function GetThumb: TvgThumb;
    procedure SetFrequency(const Value: single);
    function GetIsTracking: boolean;
  protected
    FOnChange, FOnTracking: TNotifyEvent;
    FValue: single;
    FMin: single;
    FMax: single;
    FViewportSize: single;
    FOrientation: TvgOrientation;
    FTracking: boolean;
    FFrequency: single;
    procedure SetMax(const Value: single); virtual;
    procedure SetMin(const Value: single); virtual;
    procedure SetValue(Value: single);
    procedure SetViewportSize(const Value: single);
    procedure SetOrientation(const Value: TvgOrientation);
    function GetThumbRect: TvgRect;
    property Thumb: TvgThumb read GetThumb;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
    property IsTracking: boolean read GetIsTracking;
    property Min: single read FMin write SetMin;
    property Max: single read FMax write SetMax;
    property Frequency: single read FFrequency write SetFrequency;
    property Orientation: TvgOrientation read FOrientation write SetOrientation;
    property Value: single read FValue write SetValue;
    property ViewportSize: single read FViewportSize write SetViewportSize;
    property Tracking: boolean read FTracking write FTracking default true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnTracking: TNotifyEvent read FOnTracking write FOnTracking;
  end;

  TvgTrack = class(TvgCustomTrack)
  private
  protected
  public
  published
    property Resource;
    property Min: single read FMin write SetMin;
    property Max: single read FMax write SetMax;
    property Frequency: single read FFrequency write SetFrequency;
    property Orientation: TvgOrientation read FOrientation write SetOrientation;
    property Value: single read FValue write SetValue;
    property ViewportSize: single read FViewportSize write SetViewportSize;
    property Tracking: boolean read FTracking write FTracking;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgTrackBar = class(TvgCustomTrack)
  private
  protected
    procedure SetMax(const Value: single); override;
    procedure SetMin(const Value: single); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Resource;
    property Min: single read FMin write SetMin;
    property Max: single read FMax write SetMax;
    property Frequency: single read FFrequency write SetFrequency;
    property Orientation: TvgOrientation read FOrientation write SetOrientation;
    property Value: single read FValue write SetValue;
    property Tracking: boolean read FTracking write FTracking;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgScrollBar = class(TvgControl)
  private
    FOnChange: TNotifyEvent;
    FValue: single;
    FMin: single;
    FMax: single;
    FViewportSize: single;
    FOrientation: TvgOrientation;
    FSmallChange: single;
    procedure SetMax(const Value: single);
    procedure SetMin(const Value: single);
    procedure SetValue(const Value: single);
    procedure SetViewportSize(const Value: single);
    procedure SetOrientation(const Value: TvgOrientation);
  protected
    procedure DoTrackChanged(Sender: TObject);
    procedure DoMinButtonClick(Sender: TObject);
    procedure DoMaxButtonClick(Sender: TObject);
    function Track: TvgCustomTrack;
    function MinButton: TvgButton;
    function MaxButton: TvgButton;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
  published
    property Resource;
    property Min: single read FMin write SetMin;
    property Max: single read FMax write SetMax;
    property Orientation: TvgOrientation read FOrientation write SetOrientation;
    property Value: single read FValue write SetValue;
    property ViewportSize: single read FViewportSize write SetViewportSize;
    property SmallChange: single read FSmallChange write FSmallChange;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgAniIndicatorStyle = (
    vgAniIndicatorLine,
    vgAniIndicatorCircle
  );

  TvgAniIndicator = class(TvgControl)
  private
    FDragTimer: TTimer;
    FLayout: TvgVisualObject;
    FAni: TvgFloatAnimation;
    FEnabled: boolean;
    FStyle: TvgAniIndicatorStyle;
    procedure SetEnabled(const Value: boolean);
    procedure SetStyle(const Value: TvgAniIndicatorStyle);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Enabled: boolean read FEnabled write SetEnabled;
    property Style: TvgAniIndicatorStyle read FStyle write SetStyle;
  end;

  TvgAngleButton = class(TvgControl)
  private
    FPressing: boolean;
    FOnChange: TNotifyEvent;
    FOldPos: TvgPoint;
    FSaveValue, FValue: single;
    FFrequency: single;
    FTracking: boolean;
    FShowValue: boolean;
    procedure SetValue(const Value: single);
    procedure SetShowValue(const Value: boolean);
  protected
    function Tick: TvgVisualObject;
    function Text: TvgText;
    procedure ApplyStyle; override;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); override;
  published
    { props }
    property Resource;
    property Frequency: single read FFrequency write FFrequency;
    property Tracking: boolean read FTracking write FTracking default true;
    property ShowValue: boolean read FShowValue write SetShowValue default false;
    property Value: single read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgExpanderButton = class(TvgButton)
  private
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TvgExpander = class(TvgTextControl)
  private
    procedure DoButtonClick(Sender: TObject);
  protected
    FIsExpanded: boolean;
    FContent: TvgContent;
    FButton: TvgButton;
    procedure ApplyStyle; override;
    procedure SetIsExpanded(const Value: boolean); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadContentSize(Reader: TReader);
    procedure WriteContentSize(Writer: TWriter);
    procedure DesignClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
    procedure AddObject(AObject: TvgObject); override;
  published
    property Font;
    property TextAlign;
    property Text;
    property Resource;
    property IsExpanded: boolean read FIsExpanded write SetIsExpanded default true;
  end;

  TvgPopupBox = class(TvgButton)
  private
    FItems: TvgWideStrings;
    FItemIndex: integer;
    FPopup: TPopupMenu;
    FOnChange: TNotifyEvent;
    procedure SetItems(const Value: TvgWideStrings);
    procedure SetItemIndex(const Value: integer);
  protected
    procedure ApplyStyle; override;
    procedure Click; override;
    procedure DoItemsChanged(Sender: TObject); virtual;
    procedure DoItemClick(Sender: TObject);
    procedure DoPopup; virtual;
    function GetData: Variant; override;
    procedure SetData(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Items: TvgWideStrings read FItems write SetItems;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TvgWindow = class(TvgTextControl)
  private
    FShowCloseButton: boolean;
    FShowSizeGrip: boolean;
    FOnCloseClick: TNotifyEvent;
    procedure SetShowCloseButton(const Value: boolean);
    procedure SetShowSizeGrip(const Value: boolean);
  protected
    procedure ApplyStyle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ShowCloseButton: boolean read FShowCloseButton write SetShowCloseButton default true;
    property ShowSizeGrip: boolean read FShowSizeGrip write SetShowSizeGrip default true;
    property OnCloseClick: TNotifyEvent read FOnCloseClick write FOnCloseClick;
    { props }
    property Font;
    property TextAlign;
    property Text;
    property Resource;
  end;

  TvgCloseAlign = (
    vgButtonAlignLeft,
    vgButtonAlignRight
  );

  TvgHudWindow = class(TvgWindow)
  private
    FDisableShadowOnOSX: boolean;
    FFill: TvgBrush;
    FStrokeThickness: single;
    FStroke: TvgBrush;
    FStrokeCap: TvgStrokeCap;
    FStrokeDash: TvgStrokeDash;
    FStrokeJoin: TvgStrokeJoin;
    FCloseAlign: TvgCloseAlign;
    FShowCaption: boolean;
    procedure SetDisableShadowOnOSX(const Value: boolean);
    procedure SetFill(const Value: TvgBrush);
    function isStrokeThicknessStored: Boolean;
    procedure SetStroke(const Value: TvgBrush);
    procedure SetStrokeCap(const Value: TvgStrokeCap);
    procedure SetStrokeDash(const Value: TvgStrokeDash);
    procedure SetStrokeJoin(const Value: TvgStrokeJoin);
    procedure SetStrokeThickness(const Value: single);
    procedure SetCloseAlign(const Value: TvgCloseAlign);
    procedure SetShowCaption(const Value: boolean);
  protected
    procedure ApplyStyle; override;
    procedure DoFillChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DisableShadowOnOSX: boolean read FDisableShadowOnOSX write SetDisableShadowOnOSX default true;
    property ButtonAlign: TvgCloseAlign read FCloseAlign write SetCloseAlign default vgButtonAlignLeft;
    property Fill: TvgBrush read FFill write SetFill;
    property Stroke: TvgBrush read FStroke write SetStroke;
    property StrokeThickness: single read FStrokeThickness write SetStrokeThickness stored isStrokeThicknessStored;
    property StrokeCap: TvgStrokeCap read FStrokeCap write SetStrokeCap default vgCapFlat;
    property StrokeDash: TvgStrokeDash read FStrokeDash write SetStrokeDash default vgDashSolid;
    property StrokeJoin: TvgStrokeJoin read FStrokeJoin write SetStrokeJoin default vgJoinMiter;
    property ShowCaption: boolean read FShowCaption write SetShowCaption default true;
  end;

  TvgHudLabel = class(TvgLabel)
  private
  protected
  public
  published
  end;

  TvgHudButton = class(TvgButton)
  private
  protected
  public
  published
  end;

  TvgHudSpeedButton = class(TvgSpeedButton)
  private
  protected
  public
  published
  end;

  TvgHudCheckBox = class(TvgCheckBox)
  private
  protected
  public
  published
  end;

  TvgHudRadioButton = class(TvgRadioButton)
  private
  protected
  public
  published
  end;

  TvgHudGroupBox = class(TvgGroupBox)
  private
  protected
  public
  published
  end;

  TvgHudPopupBox = class(TvgPopupBox)
  private
  protected
  public
  published
  end;

  TvgHudAngleButton = class(TvgAngleButton)
  private
  protected
  public
  published
  end;

  TvgHudTrack = class(TvgTrack)
  private
  protected
  public
  published
  end;

  TvgHudTrackBar = class(TvgTrackBar)
  private
  protected
  public
  published
  end;

  TvgHudScrollBar = class(TvgScrollBar)
  private
  protected
  public
  published
  end;

  TvgLayerWindow = class(TvgHudWindow)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

  TvgHudPanel = class(TvgPanel)
  private
  protected
  public
  published
  end;

  TvgHudCloseButton = class(TvgCloseButton)
  private
  protected
  public
  published
  end;

  TvgHudSizeGrip = class(TvgSizeGrip)
  private
  protected
  public
  published
  end;

  TvgHudStatusBar = class(TvgStatusBar)
  private
  protected
  public
  published
  end;

implementation {===============================================================}

uses Math, vg_layouts;

{$IFNDEF FPC}
{$R *.res}
{$ENDIF}

{ TvgPopupForm }

type

  TvgPopupForm = class(TCustomForm)
  private
    FOwnerForm: TCustomForm;
    FPopup: TvgPopup;
    FNoFree: boolean;
    {$IFNDEF FPC}
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure WMMouseActivate(var Msg: TWMMouseActivate); message WM_MOUSEACTIVATE;
    {$ELSE}
    procedure WMDeactivate(var Msg : TLMActivate); message LM_DEACTIVATE;
    {$ENDIF}
  protected
    procedure DoClose(var Action: TCloseAction); override;
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;

constructor TvgPopupForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner{$ifdef FPC}, 0{$endif});
  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  Position := poDesigned;
end;

destructor TvgPopupForm.Destroy;
begin
  inherited;
end;

procedure TvgPopupForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
  begin
    {$ifndef FPC}
    WindowClass.Style := WindowClass.Style or CS_SAVEBITS;
    {$endif}
  end;
end;

{$IFNDEF FPC}
procedure TvgPopupForm.CMShowingChanged(var Message: TMessage);
const
  ShowFlags: array[Boolean] of UINT = (
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_HIDEWINDOW,
    SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_SHOWWINDOW);
begin
  SetWindowPos(WindowHandle, 0, 0, 0, 0, 0, ShowFlags[Showing]);
end;

procedure TvgPopupForm.WMActivate(var Msg: TWMActivate);
begin
  if (FOwnerForm <> nil) then
  begin
    { Activate OwnerForm }
    SendMessage(FOwnerForm.Handle, WM_NCACTIVATE, 1, 0);
  end;
  if (Msg.Active = WA_INACTIVE) and (FPopup <> nil) and (not FPopup.StaysOpen) then
  begin
    FPopup.ClosePopup;
  end;
end;

procedure TvgPopupForm.WMMouseActivate(var Msg: TWMMouseActivate);
begin
  if (FOwnerForm <> nil) then
  begin
    { Activate OwnerForm }
    SendMessage(FOwnerForm.Handle, WM_NCACTIVATE, 1, 0);
  end;
end;
{$else}
procedure TvgPopupForm.WMDeactivate(var Msg: TLMActivate);
begin
  inherited ;
  if (not Msg.Active) and (FPopup <> nil) and (not FPopup.StaysOpen) then
  begin
    FPopup.ClosePopup;
  end;
end;

{$ENDIF}

procedure TvgPopupForm.DoClose(var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
  if (FPopup <> nil) then
  begin
    FNoFree := true;
    FPopup.ClosePopup;
  end;
end;

{ TvgPopup }

constructor TvgPopup.Create(AOwner: TComponent);
begin
  inherited;
  FPlacementRectangle := TvgBounds.Create(vgRect(0,0,0,0));
  Visible := false;
end;

destructor TvgPopup.Destroy;
begin
  ClosePopup;
  FPlacementRectangle.Free;
  inherited;
end;

procedure TvgPopup.Paint;
var
  R: TvgRect;
begin
  inherited;
  if Assigned(Scene) and Scene.GetDesignTime then
  begin
    R := LocalRect;
    vgInflateRect(R, -0.5, -0.5);
    Canvas.StrokeThickness := 1;
    Canvas.StrokeDash := vgDashDash;
    Canvas.Stroke.Style := vgBrushSolid;
    Canvas.Stroke.SolidColor := $A0909090;
    Canvas.DrawRect(R, 0, 0, AllCorners, AbsoluteOpacity);
  end;
end;

procedure TvgPopup.ApplyPlacement;
var
  Target: TvgVisualObject;
  AbsolutePos, LocalPos: TvgPoint;
  Pos: TPoint;
begin
  if FPopupForm = nil then Exit;

  Target := PlacementTarget;

  if (Target = nil) and (Parent <> nil) then
    Target := Parent.Visual;
  if Target = nil then
  begin
    case Placement of
      vgPlacementBottom:
        begin
          LocalPos := vgPoint(FPlacementRectangle.left + HorizontalOffset, FPlacementRectangle.bottom + VerticalOffset);
        end;
      vgPlacementTop:
        begin
          LocalPos := vgPoint(FPlacementRectangle.left + HorizontalOffset, FPlacementRectangle.top - Height - VerticalOffset);
        end;
      vgPlacementLeft:
        begin
          LocalPos := vgPoint(FPlacementRectangle.left - Width - HorizontalOffset, FPlacementRectangle.top + VerticalOffset);
        end;
      vgPlacementRight:
        begin
          LocalPos := vgPoint(FPlacementRectangle.right + HorizontalOffset, FPlacementRectangle.top + VerticalOffset);
        end;
      vgPlacementCenter:
        begin
          LocalPos := vgPoint(FPlacementRectangle.left + (FPlacementRectangle.right - FPlacementRectangle.left - Width) / 2,
            FPlacementRectangle.top + (FPlacementRectangle.bottom - FPlacementRectangle.top - Height) / 2);
          LocalPos.X := LocalPos.X + HorizontalOffset;
          LocalPos.Y := LocalPos.Y + VerticalOffset;
        end;
      vgPlacementAbsolute:
        begin
          FPopupForm.Left := Round(FPlacementRectangle.Left);
          FPopupForm.Top := Round(FPlacementRectangle.Top);
          Exit;
        end;
      vgPlacementMouse:
        begin
          GetCursorPos(Pos);
          FPopupForm.Left := Pos.X;
          FPopupForm.Top := Pos.Y;
          Exit;
        end;
    end;
    Pos := Point(Trunc(LocalPos.X), Trunc(LocalPos.Y));
  end
  else
  begin
    case Placement of
      vgPlacementBottom:
        begin
          if FPlacementRectangle.empty then
            LocalPos := vgPoint(HorizontalOffset, Target.Height + VerticalOffset)
          else
            LocalPos := vgPoint(FPlacementRectangle.left + HorizontalOffset, FPlacementRectangle.bottom + VerticalOffset);
        end;
      vgPlacementTop:
        begin
          if FPlacementRectangle.empty then
            LocalPos := vgPoint(HorizontalOffset, -Height - HorizontalOffset)
          else
            LocalPos := vgPoint(FPlacementRectangle.left + HorizontalOffset, FPlacementRectangle.top - Height - VerticalOffset);
        end;
      vgPlacementLeft:
        begin
          if FPlacementRectangle.empty then
            LocalPos := vgPoint(-Width - HorizontalOffset, VerticalOffset)
          else
            LocalPos := vgPoint(FPlacementRectangle.left - Width - HorizontalOffset, FPlacementRectangle.top + VerticalOffset);
        end;
      vgPlacementRight:
        begin
          if FPlacementRectangle.empty then
            LocalPos := vgPoint(Target.Width + HorizontalOffset, VerticalOffset)
          else
            LocalPos := vgPoint(FPlacementRectangle.right + HorizontalOffset, FPlacementRectangle.top + VerticalOffset);
        end;
      vgPlacementCenter:
        begin
          if FPlacementRectangle.empty then
            LocalPos := vgPoint((Target.Width - Width) / 2, (Target.Height - Height) / 2)
          else
            LocalPos := vgPoint(FPlacementRectangle.left + (FPlacementRectangle.right - FPlacementRectangle.left - Width) / 2,
              FPlacementRectangle.top + (FPlacementRectangle.bottom - FPlacementRectangle.top - Height) / 2);
          LocalPos.X := LocalPos.X + HorizontalOffset;
          LocalPos.Y := LocalPos.Y + VerticalOffset;
        end;
      vgPlacementAbsolute:
        begin
          FPopupForm.Left := Round(FPlacementRectangle.Left);
          FPopupForm.Top := Round(FPlacementRectangle.Top);
          Exit;
        end;
      vgPlacementMouse:
        begin
          GetCursorPos(Pos);
          FPopupForm.Left := Pos.X;
          FPopupForm.Top := Pos.Y;
          Exit;
        end;
    end;
    AbsolutePos := Target.LocalToAbsolute(LocalPos);
    Pos := Point(Trunc(AbsolutePos.X), Trunc(AbsolutePos.Y));
    if (Target.Scene <> nil) then
      Pos := Target.Scene.ClientToScreen(Pos)
    else
      if (Scene <> nil) then
        Pos := Scene.ClientToScreen(Pos);
  end;

  FPopupForm.Left := Pos.X;
  FPopupForm.Top := Pos.Y;
end;

procedure TvgPopup.Popup;
begin
  if FAnimating then
  begin
    FIsOpen := false;
    Exit;
  end;
  if FPopupForm <> nil then
  begin
    ClosePopup;
    Exit;
  end;

  FPopupForm := TvgPopupForm.Create(nil);
  if Owner is TCustomForm then
    TvgPopupForm(FPopupForm).FOwnerForm := TCustomForm(Owner);
  if (Scene <> nil) and (Scene.GetComponent <> nil) and (Scene.GetComponent.Owner is TCustomForm) then
    TvgPopupForm(FPopupForm).FOwnerForm := TCustomForm(Scene.GetComponent.Owner);
  FPopupForm.Width := Round(Width * AbsoluteMatrix.m11);
  FPopupForm.Height := Round(Height * AbsoluteMatrix.m22);
  ApplyPlacement;
  TvgPopupForm(FPopupForm).FPopup := Self;
  TvgPopupForm(FPopupForm).FNoFree := false;
  FPopupScene := TvgScene.Create(FPopupForm);
  with FPopupScene do
  begin
    Transparency := true;
    Parent := FPopupForm;
    Align := alClient;
    Style := FStyle;
    if (FStyle = nil) and Assigned(FScene) then
      Style := FScene.GetStyle;
  end;
  FPopupScene.Width := Round(Width);
  FPopupScene.Height := Round(Height);
  { show }
  Visible := true;
  { add self}
  FSaveParent := Parent;
  FSaveScene := FScene;
  FPopupScene.AddObject(Self);
  FPopupScene.RealignRoot;
  SetNewScene(FPopupScene);
  { apply resoruces }
  FNeedResource := true;
  ApplyResource;
  { show }
  FPopupForm.Show;
  FPopupScene.SetFocus;
  { trigger }
  FIsOpen := true;
  ApplyTriggerEffect(Self, VG_ISOPEN);
  StartTriggerAnimation(Self, VG_ISOPEN);
  { drag timer }
  if FDragWithParent and FStaysOpen then
  begin
    FDragTimer := TTimer.Create(Self);
    FDragTimer.Interval := 10;
    FDragTimer.OnTimer := DoTimer;
    FDragTimer.Enabled := true;
  end;
end;

function TvgPopup.PopupModal: TModalResult;
begin
  if FAnimating then
  begin
    FIsOpen := false;
    Exit;
  end;
  if FPopupForm <> nil then
  begin
    ClosePopup;
    Exit;
  end;
  FStaysOpen := true;
  Popup;
  repeat
    Application.HandleMessage;
    if Application.Terminated then FPopupForm.ModalResult := mrCancel else
      if FPopupForm.ModalResult <> 0 then Break;
    ApplyPlacement;
  until FPopupForm.ModalResult <> 0;
  Result := FPopupForm.ModalResult;
  ClosePopup;
end;

procedure TvgPopup.ClosePopup;
begin
  if FPopupForm = nil then Exit;
  if FAnimating then Exit;
  { drag timer }
  if (FDragTimer <> nil) then
    FreeAndNil(FDragTimer);
  { trigger }
  FAnimating := true;
  FIsOpen := false;
  if not (csDestroying in ComponentState) then
  begin
    ApplyTriggerEffect(Self, VG_ISOPEN);
    StartTriggerAnimationWait(Self, VG_ISOPEN);
  end;
  FAnimating := false;
  { hide }
  Visible := false;
  { remove self}
  if not (csDestroying in ComponentState) then
  begin
    FPopupScene.RemoveObject(Self);
    Parent := FSaveParent;
    SetNewScene(FSaveScene);
  end
  else
  begin
    FPopupScene.RemoveObject(Self);
    SetNewScene(FSaveScene);
  end;
  { free}
  if not TvgPopupForm(FPopupForm).FNoFree then
  begin
    FPopupScene.Free;
    TvgPopupForm(FPopupForm).FPopup := nil;
    FreeAndNil(FPopupForm);
  end
  else
    FPopupForm := nil;
  FSaveParent := nil;
end;

procedure TvgPopup.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Assigned(FSaveScene) and (FSaveScene.GetComponent = AComponent) then
  begin
    FSaveScene := nil;
    FSaveParent := nil;
  end;
  if (AComponent = FPlacementTarget) and (Operation = opRemove) then
    FPlacementTarget := nil;
end;

procedure TvgPopup.FreeNotify(AObject: TvgObject);
begin
  inherited ;
end;

procedure TvgPopup.SetIsOpen(const Value: boolean);
begin
  if FIsOpen <> Value then
  begin
    if Assigned(Scene) and Scene.GetDesignTime then
    begin
      FIsOpen := false;
      Exit;
    end;
    FIsOpen := Value;
    if FIsOpen then
      Popup
    else
      ClosePopup;
  end;
end;

procedure TvgPopup.Dotimer(Sender: TObject);
begin
  ApplyPlacement;
end;

procedure TvgPopup.SetPlacementRectangle(const Value: TvgBounds);
begin
end;

{ TvgMessagePopup }

const

  ModalResults: array[TvgMessageButton] of Integer = (
    mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0);
  MessageButtonNames: array[TvgMessageButton] of WideString = (

    'Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
    'YesToAll', 'Help');

constructor TvgMessagePopup.Create(AOwner: TComponent);
begin
  inherited;
  FDragWithParent := true;
  FStaysOpen := true;
end;

destructor TvgMessagePopup.Destroy;
begin
  inherited;
end;

procedure TvgMessagePopup.ApplyPlacement;
begin
  if PlacementTarget = nil then
  begin
    if (Owner <> nil) and (Owner is TCustomForm) then
    begin
      with TCustomForm(Owner).ClientToScreen(Point(0, 0)) do
        PlacementRectangle.Rect := vgRect(X, Y, X + Width, Y);
      HorizontalOffset := (TCustomForm(Owner).Width - Width) / 2;
    end;
  end
  else
  begin
    HorizontalOffset := (PlacementTarget.Width - Width) / 2;
  end;
 inherited ;
end;

function MessagePopup(const ACaption, AMessage: WideString; AType: TvgMessageType;
  Buttons: TvgMessageButtons; const AOwner: TvgScene; const Target: TvgVisualObject = nil;
  const ABitmap: TvgBitmap = nil;
  const AStyle: TvgResources = nil): integer;
var
  B, DefaultButton, CancelButton: TvgMessageButton;
  Popup: TvgMessagePopup;
  C, M: TvgLabel;
  L: TvgLayout;
  I: TvgVisualObject;
  Btn: TvgButton;
  SaveEnabled: boolean;
begin
  if (AOwner <> nil) and (AOwner.Root <> nil) then
  begin
    SaveEnabled := AOwner.Root.Visual.Enabled;
    AOwner.Root.Visual.Enabled := false;
  end;
  try
    if AOwner <> nil then
      Popup := TvgMessagePopup.Create(AOwner.Owner)
    else
      Popup := TvgMessagePopup.Create(AOwner);
    Popup.StaysOpen := true;
    Popup.DragWithParent := true;
    Popup.Width := 500;
    Popup.Height := 150;
    Popup.Style := AStyle;
    Popup.PlacementTarget := Target;
    if (AStyle = nil) and (AOwner <> nil) and (AOwner.Style <> nil) then
      Popup.Style := AOwner.Style;

    Popup.ApplyResource;

    if ABitmap <> nil then
    begin
      I := TvgImage.Create(Popup);
      with TvgImage(I) do
      begin
        Parent := Popup;
        SetBounds(30, 10, 64, 64);
        Bitmap.Assign(ABitmap);
      end;
    end
    else
    begin
      I := TvgControl.Create(Popup);
      with I do
      begin
        Parent := Popup;
        SetBounds(30, 10, 64, 64);
      end;
      case AType of
        vgMessageWarning: TvgControl(I).Resource := 'iconwarning';
        vgMessageError: TvgControl(I).Resource := 'iconerror';
        vgMessageInformation: TvgControl(I).Resource := 'iconinformation';
        vgMessageConfirmation: TvgControl(I).Resource := 'iconconfirmation';
      end;
    end;

    C := TvgLabel.Create(Popup);
    with C do
    begin
      Parent := Popup;
      Align := vaTop;
      Height := 30;
      Padding.Rect := vgRect(30, 10, 30, 0);
      if I <> nil then
        Padding.Left := Padding.Rect.Left + I.width + 10;
      Text := ACaption;
      TextAlign := vgTextAlignNear;
      Font.Size := Font.Size * 1.4;
    end;
    M := TvgLabel.Create(Popup);
    with M do
    begin
      Parent := Popup;
      Align := vaClient;
      Height := 50;
      Padding.Rect := vgRect(30, 0, 30, 0);
      if I <> nil then
        Padding.Left := Padding.Rect.Left + I.width + 10;
      Text := AMessage;
      TextAlign := vgTextAlignNear;
    end;
    L := TvgLayout.Create(Popup);
    with L do
    begin
      Parent := Popup;
      Align := vaBottom;
      Height := 20;
      Padding.Rect := vgRect(30, 0, 30, 30);
    end;
    for B := Low(TvgMessageButton) to High(TvgMessageButton) do
    begin
      if not (B in Buttons) then Continue;
      Btn := TvgButton.Create(Popup);
      with Btn do
      begin
        Parent := L;
        Align := vaRight;
        Position.X := 1000;
        Padding.Rect := vgRect(7, 0, 0, 0);
        Text := MessageButtonNames[B];
        ModalResult := ModalResults[B];
      end;
    end;

    Result := Popup.PopupModal;
    Popup.Free;
  finally
    if (AOwner <> nil) and (AOwner.Root <> nil) then
      AOwner.Root.Visual.Enabled := SaveEnabled;
  end;
end;

{ TvgPanel ====================================================================}

constructor TvgPanel.Create(AOwner: TComponent);
begin
  inherited;
end;

function TvgMessagePopup.PopupModal: TModalResult;
var
  SaveEnabled: boolean;
begin
  if (Scene <> nil) and (Scene.GetRoot <> nil) then
  begin
    SaveEnabled := Scene.GetRoot.Visual.Enabled;
    Scene.GetRoot.Visual.Enabled := false;
  end;
  Result := inherited PopupModal;
  if (Scene <> nil) and (Scene.GetRoot <> nil) then
  begin
    Scene.GetRoot.Visual.Enabled := SaveEnabled;
  end;
end;

{ TvgStatusBar }

constructor TvgStatusBar.Create(AOwner: TComponent);
begin
  inherited;
  FShowSizeGrip := true;
  Height := 22;
  Align := vaBottom;
end;

procedure TvgStatusBar.ApplyStyle;
var
  sizeGrip: TvgObject;
begin
  inherited;
  sizeGrip := FindResource('sizegrip');
  if (sizeGrip <> nil) and (sizeGrip is TvgVisualObject) then
  begin
    TvgVisualObject(sizeGrip).visible := FShowSizeGrip;
    if (Scene <> nil) and not (Scene.GetDesignTime) then
    begin
      TvgVisualObject(sizeGrip).Locked := false;
      TvgVisualObject(sizeGrip).HitTest := true;
    end;
  end;
end;

procedure TvgStatusBar.SetShowSizeGrip(const Value: boolean);
begin
  if FShowSizeGrip <> Value then
  begin
    FShowSizeGrip := Value;
    Resource := FResource;
  end;
end;

{ TvgToolBar }

constructor TvgToolBar.Create(AOwner: TComponent);
begin
  inherited;
  Height := 40;
  Align := vaTop;
end;

{ TvgTextControl ===================================================================}

constructor TvgTextControl.Create(AOwner: TComponent);
begin
  inherited;
  FFont := TvgFont.Create;
  FFont.OnChanged := FontChanged;
  FFontFill := TvgBrush.Create(vgBrushSolid, $FF000000);
  FFontFill.OnChanged := FontFillChanged;
end;

destructor TvgTextControl.Destroy;
begin
  FFontFill.Free;
  FFont.Free;
  inherited;
end;

function TvgTextControl.GetData: Variant;
begin
  Result := Text;
end;

procedure TvgTextControl.SetData(const Value: Variant);
begin
  Text := Value;
end;

procedure TvgTextControl.ApplyStyle;
var
  S: TvgObject;
begin
  inherited;
  { from style }
  S := FindResource('foreground');
  if (S <> nil) and (S is TvgBrushObject) then
    FontFill.Assign(TvgBrushObject(S).Brush);
  { to style }
  Text := FText;
  TextAlign := FTextAlign;
  VertTextAlign := FVertTextAlign;
  FontChanged(Self);
end;

procedure TvgTextControl.FontChanged(Sender: TObject);
var
  T: TvgObject;
begin
  T := FindResource('text');
  if (T <> nil) and (T is TvgText) then
    TvgText(T).Font.Assign(FFont);
end;

procedure TvgTextControl.FontFillChanged(Sender: TObject);
var
  T: TvgObject;
begin
  T := FindResource('text');
  if (T <> nil) and (T is TvgText) then
    TvgText(T).Fill.Assign(FontFill);
end;

function TvgTextControl.GetText: WideString;
begin
  Result := FText;
end;

procedure TvgTextControl.SetText(const Value: WideString);
var
  T: TvgObject;
begin
  FText := Value;
  FUpdateEffects := true;
  T := FindResource('text');
  if (T <> nil) and (T is TvgText) then
    TvgText(T).Text := FText
  else
    if (FResourceLink <> nil) and (FResourceLink is TvgText) then
      TvgText(FResourceLink).Text := FText
    else
      Repaint;
end;

procedure TvgTextControl.SetFontFill(const Value: TvgBrush);
begin
  FFontFill := Value;
end;

procedure TvgTextControl.SetFont(const Value: TvgFont);
begin
  FFont.Assign(Value);
end;

procedure TvgTextControl.SetTextAlign(const Value: TvgTextAlign);
var
  T: TvgObject;
begin
  FTextAlign := Value;
  T := FindResource('text');
  if (T <> nil) and (T is TvgText) then
    TvgText(T).HorzTextAlign := FTextAlign
  else
    Repaint;
end;

procedure TvgTextControl.SetVertTextAlign(const Value: TvgTextAlign);
var
  T: TvgObject;
begin
  FVertTextAlign := Value;
  T := FindResource('text');
  if (T <> nil) and (T is TvgText) then
    TvgText(T).VertTextAlign := FVertTextAlign
  else
    Repaint;
end;

{ TvgLabel }

constructor TvgLabel.Create(AOwner: TComponent);
begin
  inherited;
  Height := 15;
  FWordWrap := true;
  HitTest := false;
end;

procedure TvgLabel.ApplyStyle;
var
  T: TvgObject;
  S: TvgAlign;
begin
  inherited;
  T := FindResource('text');
  if (T <> nil) and (T is TvgText) then
  begin
    TvgText(T).WordWrap := WordWrap;
    if AutoSize then
    begin
      S := TvgText(T).Align;
      TvgText(T).Align := vaNone;
      TvgText(T).AutoSize := true;
      Width := TvgText(T).Width;
      Height := TvgText(T).Height;
      TvgText(T).AutoSize := false;
      TvgText(T).Align := S;
    end;
  end;
end;

procedure TvgLabel.SetWordWrap(const Value: boolean);
var
  T: TvgObject;
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    T := FindResource('text');
    if (T <> nil) and (T is TvgText) then
      TvgText(T).WordWrap := Value;
  end;
end;

procedure TvgLabel.SetAutoSize(const Value: boolean);
begin
  if FAutoSize <> Value then
  begin
    FAutoSize := Value;
    ApplyStyle;
  end;
end;

{ TvgValueLabel }

constructor TvgValueLabel.Create(AOwner: TComponent);
begin
  inherited;
  FWordWrap := false;
end;

{ TvgButton ===================================================================}

procedure TvgButton.Click;
var
  O: TComponent;
begin
  inherited;
  if (ModalResult <> mrNone) then
  begin
    O := Scene.GetOwner;
    while O <> nil do
    begin
      if (O is TCustomForm) then
      begin
        TCustomForm(O).ModalResult := FModalResult;
        Break;
      end;
      O := O.Owner;
    end;
  end;
end;

constructor TvgButton.Create(AOwner: TComponent);
begin
  inherited;
  Width := 80;
  Height := 22;
  AutoCapture := true;
  CanFocused := true;
end;

destructor TvgButton.Destroy;
begin
  inherited;
end;

procedure TvgButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    FPressing := true;
    if FStaysPressed then
      FIsPressed := not FIsPressed
    else
      FIsPressed := true;
    StartTriggerAnimation(Self, 'IsPressed');
  end;
end;

procedure TvgButton.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if (ssLeft in Shift) and (FPressing) then
  begin
    if FIsPressed <> vgPtInRect(vgPoint(X, Y), LocalRect) then
    begin
      if not FStaysPressed then
      begin
        FIsPressed := vgPtInRect(vgPoint(X, Y), LocalRect);
        StartTriggerAnimation(Self, 'IsPressed');
      end;
    end;
  end;
end;

procedure TvgButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  if FPressing then
  begin
    FPressing := false;
    if not FStaysPressed then
    begin
      FIsPressed := false;
      StartTriggerAnimation(Self, 'IsPressed');
    end;
  end;
  inherited;
end;

procedure TvgButton.SetIsPressed(const Value: boolean);
begin
  if FStaysPressed then
  begin
    if Value <> FIsPressed then
    begin
      FIsPressed := Value;
      StartTriggerAnimation(Self, 'IsPressed');
    end;
  end;
end;

{ TvgBitmapButton }

constructor TvgBitmapButton.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TvgBitmap.Create(1, 1);
  FBitmap.OnChange := DoBitmapChanged;
  FBitmapLayout := vgGlyphLeft;
  FBitmapSize := 32;
  FBitmapPadding := 2;
  Width := 50;
  Height := 60;
end;

destructor TvgBitmapButton.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TvgBitmapButton.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('image');
  if (T <> nil) and (T is TvgImage) then
  begin
    TvgImage(T).Bitmap.Assign(FBitmap);
    TvgVisualObject(T).Padding.Rect := vgRect(FBitmapPadding, FBitmapPadding, FBitmapPadding, FBitmapPadding);
    if (TvgImage(T).Bitmap.Width = 1) or (TvgImage(T).Bitmap.Height = 1) then
    begin
      TvgVisualObject(T).Align := vaNone;
    end
    else
      case FBitmapLayout of
        vgGlyphLeft: begin
          TvgVisualObject(T).Align := vaLeft;
          TvgVisualObject(T).Width := FBitmapSize;
          TvgVisualObject(T).Padding.right := FBitmapSpacing;
        end;
        vgGlyphRight: begin
          TvgVisualObject(T).Align := vaRight;
          TvgVisualObject(T).Width := FBitmapSize;
          TvgVisualObject(T).Padding.left := FBitmapSpacing;
        end;
        vgGlyphTop: begin
          TvgVisualObject(T).Align := vaTop;
          TvgVisualObject(T).Height := FBitmapSize;
          TvgVisualObject(T).Padding.bottom := FBitmapSpacing;
        end;
        vgGlyphBottom: begin
          TvgVisualObject(T).Align := vaBottom;
          TvgVisualObject(T).Height := FBitmapSize;
          TvgVisualObject(T).Padding.top := FBitmapSpacing;
        end;
      end;
  end;
  T := FindResource('text');
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    TvgVisualObject(T).Padding.Rect := vgRect(FBitmapPadding, FBitmapPadding, FBitmapPadding, FBitmapPadding);
  end;
end;

procedure TvgBitmapButton.DoBitmapChanged(Sender: TObject);
begin
  Repaint;
  ApplyStyle;
end;

procedure TvgBitmapButton.SetBitmap(const Value: TvgBitmap);
begin
  FBitmap.Assign(Value);
end;

procedure TvgBitmapButton.SetBitmapLayout(const Value: TvgButtonLayout);
begin
  if FBitmapLayout <> Value then
  begin
    FBitmapLayout := Value;
    ApplyStyle;
  end;
end;

procedure TvgBitmapButton.SetBitmapSpacing(const Value: single);
begin
  if FBitmapSpacing <> Value then
  begin
    FBitmapSpacing := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgBitmapButton.SetBitmapSize(const Value: single);
begin
  if FBitmapSize <> Value then
  begin
    FBitmapSize := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgBitmapButton.SetBitmapPadding(const Value: single);
begin
  if FBitmapPadding <> Value then
  begin
    FBitmapPadding := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

{ TvgPathButton }

constructor TvgPathButton.Create(AOwner: TComponent);
begin
  inherited;
  FPath := TvgPathData.Create;
  FPath.OnChanged := DoPathChanged;
  FPathLayout := vgGlyphLeft;
  FPathSize := 32;
  FPathPadding := 2;
  FFill := TvgBrush.Create(vgBrushSolid, $FFFFFFFF);
  FFill.OnChanged := DoPathChanged;
  FStroke := TvgBrush.Create(vgBrushSolid, $FF000000);
  FStroke.SolidColor := $FF000000;
  FStroke.OnChanged := DoPathChanged;
  FStrokeThickness := 1;
  Width := 50;
  Height := 60;
end;

destructor TvgPathButton.Destroy;
begin
  FFill.Free;
  FStroke.Free;
  FPath.Free;
  inherited;
end;

procedure TvgPathButton.ApplyStyle;
var
  T: TvgObject;
begin
  inherited;
  T := FindResource('path');
  if (T <> nil) and (T is TvgPath) then
  begin
    TvgPath(T).Data.Assign(FPath);
    TvgPath(T).Fill.Assign(FFill);
    TvgPath(T).Stroke.Assign(FStroke);
    TvgPath(T).StrokeThickness := FStrokeThickness;
    TvgPath(T).StrokeCap := FStrokeCap;
    TvgPath(T).StrokeJoin := FStrokeJoin;
    TvgPath(T).StrokeDash := FStrokeDash;
  end;
  T := FindResource('pathowner');
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    TvgVisualObject(T).Padding.Rect := vgRect(FPathPadding, FPathPadding, FPathPadding, FPathPadding);
    if (Length(FPath.PathData) = 0) then
    begin
      TvgVisualObject(T).Align := vaNone;
    end
    else
      case FPathLayout of
        vgGlyphLeft: begin
          TvgVisualObject(T).Align := vaLeft;
          TvgVisualObject(T).Width := FPathSize;
          TvgVisualObject(T).Padding.right := FPathSpacing;
        end;
        vgGlyphRight: begin
          TvgVisualObject(T).Align := vaRight;
          TvgVisualObject(T).Width := FPathSize;
          TvgVisualObject(T).Padding.left := FPathSpacing;
        end;
        vgGlyphTop: begin
          TvgVisualObject(T).Align := vaTop;
          TvgVisualObject(T).Height := FPathSize;
          TvgVisualObject(T).Padding.bottom := FPathSpacing;
        end;
        vgGlyphBottom: begin
          TvgVisualObject(T).Align := vaBottom;
          TvgVisualObject(T).Height := FPathSize;
          TvgVisualObject(T).Padding.top := FPathSpacing;
        end;
      end;
  end;
  T := FindResource('text');
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    TvgVisualObject(T).Padding.Rect := vgRect(FPathPadding, FPathPadding, FPathPadding, FPathPadding);
  end;
end;

procedure TvgPathButton.DoPathChanged(Sender: TObject);
begin
  Repaint;
  ApplyStyle;
end;

procedure TvgPathButton.SetPath(const Value: TvgPathData);
begin
  FPath.Assign(Value);
end;

procedure TvgPathButton.SetPathLayout(const Value: TvgButtonLayout);
begin
  if FPathLayout <> Value then
  begin
    FPathLayout := Value;
    ApplyStyle;
  end;
end;

procedure TvgPathButton.SetPathPadding(const Value: single);
begin
  if FPathPadding <> Value then
  begin
    FPathPadding := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgPathButton.SetPathSize(const Value: single);
begin
  if FPathSize <> Value then
  begin
    FPathSize := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgPathButton.SetPathSpacing(const Value: single);
begin
  if FPathSpacing <> Value then
  begin
    FPathSpacing := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

function TvgPathButton.isStrokeThicknessStored: Boolean;
begin
  Result := FStrokeThickness <> 1;
end;

procedure TvgPathButton.SetFill(const Value: TvgBrush);
begin
  FFill := Value;
end;

procedure TvgPathButton.SetStroke(const Value: TvgBrush);
begin
  FStroke := Value;
end;

procedure TvgPathButton.SetStrokeCap(const Value: TvgStrokeCap);
begin
  if FStrokeCap <> Value then
  begin
    FStrokeCap := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgPathButton.SetStrokeDash(const Value: TvgStrokeDash);
begin
  if FStrokeDash <> Value then
  begin
    FStrokeDash := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgPathButton.SetStrokeJoin(const Value: TvgStrokeJoin);
begin
  if FStrokeJoin <> Value then
  begin
    FStrokeJoin := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgPathButton.SetStrokeThickness(const Value: single);
begin
  if FStrokeThickness <> Value then
  begin
    FStrokeThickness := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

{ TvgToolButton }

constructor TvgToolButton.Create(AOwner: TComponent);
begin
  inherited;
  FBitmapLayout := vgGlyphTop;
end;

{ TvgToolPathButton }

constructor TvgToolPathButton.Create(AOwner: TComponent);
begin
  inherited;
  FPathLayout := vgGlyphTop;
end;

{ TvgSpeedButton }

constructor TvgSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := false;
  Width := 23;
  Height := 23;
  Text := '';
end;

destructor TvgSpeedButton.Destroy;
begin
  inherited;
end;

{ TvgCheckBox ===================================================================}

constructor TvgCheckBox.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := true;
  CanFocused := true;
  TextAlign := vgTextAlignNear;
  Width := 120;
  Height := 19;
end;

destructor TvgCheckBox.Destroy;
begin
  inherited;
end;

function TvgCheckBox.GetData: Variant;
begin
  Result := IsChecked;
end;

procedure TvgCheckBox.SetData(const Value: Variant);
begin
  IsChecked := Value;
end;

procedure TvgCheckBox.ApplyStyle;
begin
  inherited;
  StartTriggerAnimation(Self, 'IsChecked');
end;

procedure TvgCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    FPressing := true;
    FIsPressed := true;
    StartTriggerAnimation(Self, 'IsPressed');
  end;
end;

procedure TvgCheckBox.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if (ssLeft in Shift) and (FPressing) then
  begin
    if FIsPressed <> vgPtInRect(vgPoint(X, Y), LocalRect) then
    begin
      FIsPressed := vgPtInRect(vgPoint(X, Y), LocalRect);
      StartTriggerAnimation(Self, 'IsPressed');
    end;
  end;
end;

procedure TvgCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if FPressing then
  begin
    FPressing := false;
    FIsPressed := false;
    if vgPtInRect(vgPoint(X, Y), LocalRect) then
    begin
      IsChecked := not IsChecked;
    end
  end;
end;

procedure TvgCheckBox.SetIsChecked(const Value: boolean);
begin
  if FIsChecked <> Value then
  begin
    FIsChecked := Value;
    StartTriggerAnimation(Self, 'IsChecked');
    if Assigned(FOnChange) then
    begin
      FOnChange(Self);
    end;
  end;
end;

{ TvgRadioButton ===================================================================}

constructor TvgRadioButton.Create(AOwner: TComponent);
begin
  inherited;
  AutoCapture := true;
  CanFocused := true;
  TextAlign := vgTextAlignNear;
  Width := 120;
  Height := 19;
end;

destructor TvgRadioButton.Destroy;
begin
  inherited;
end;

function TvgRadioButton.GetData: Variant;
begin
  Result := IsChecked;
end;

procedure TvgRadioButton.SetData(const Value: Variant);
begin
  IsChecked := Value;
end;

procedure TvgRadioButton.ApplyStyle;
begin
  inherited;
  StartTriggerAnimation(Self, 'IsChecked');
end;

procedure TvgRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    FPressing := true;
    FIsPressed := true;
    StartTriggerAnimation(Self, 'IsPressed');
  end;
end;

procedure TvgRadioButton.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if (ssLeft in Shift) and (FPressing) then
  begin
    if FIsPressed <> vgPtInRect(vgPoint(X, Y), LocalRect) then
    begin
      FIsPressed := vgPtInRect(vgPoint(X, Y), LocalRect);
      StartTriggerAnimation(Self, 'IsPressed');
    end;
  end;
end;

procedure TvgRadioButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if FPressing then
  begin
    FPressing := false;
    FIsPressed := false;
    if vgPtInRect(vgPoint(X, Y), LocalRect) then
    begin
      IsChecked := not IsChecked;
    end
  end;
end;

procedure TvgRadioButton.SetIsChecked(const Value: boolean);
var
  i, c, cc: integer;
begin
  if FIsChecked <> Value then
  begin
    if Value then
      FIsChecked := Value;
    { group }
    c := 0;
    cc := 0;
    if (Parent <> nil) then
      for i := 0 to Parent.ChildrenCount - 1 do
        if (Parent.Children[i] is TvgRadioButton) and (Parent.Children[i] <> Self) and
           (TvgRadioButton(Parent.Children[i]).GroupName = GroupName) then
        begin
          if TvgRadioButton(Parent.Children[i]).IsChecked then
            cc := cc + 1;
          if Value then
            TvgRadioButton(Parent.Children[i]).IsChecked := false;
          c := c + 1;
        end;
    { check }
    if not Value and (c = 0) then Exit;
    if not Value and (cc = 0) then Exit;
    FIsChecked := Value;
    StartTriggerAnimation(Self, 'IsChecked');
    { event }
    if Assigned(FOnChange) then
    begin
      FOnChange(Self);
    end;
  end;
end;

{ TvgCloseButton }

constructor TvgCloseButton.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := false;
  CloseForm := true;
  Width := 20;
  Height := 20;
end;

destructor TvgCloseButton.Destroy;
begin

  inherited;
end;

procedure TvgCloseButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: single);
begin
  inherited;
  if Button = mbLeft then
    FPressing := true;
end;

procedure TvgCloseButton.MouseMove(Shift: TShiftState; X, Y, Dx,
  Dy: single);
begin
  inherited;
end;

procedure TvgCloseButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: single);
var
  O: TComponent;
begin
  inherited;
  if FPressing then
  begin
    if FCloseForm and (Scene <> nil) then
    begin
      O := Scene.GetOwner;
      while O <> nil do
      begin
        if (O is TCustomForm) then
        begin
          TCustomForm(O).Close;
          Break;
        end;
        O := O.Owner;
      end;
    end;
    FPressing := false;
    if Assigned(FOnClick) then
      FOnClick(Self);
  end;
end;

{ TvgSizeGrip }

constructor TvgSizeGrip.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TvgSizeGrip.Destroy;
begin
  inherited;
end;

{ TvgGroupBox =================================================================}

constructor TvgGroupBox.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := false;
end;

destructor TvgGroupBox.Destroy;
begin
  inherited;
end;

procedure TvgGroupBox.ApplyStyle;
begin
  inherited;
end;

{ TvgSplitter ===================================================================}

constructor TvgSplitter.Create(AOwner: TComponent);
begin
  inherited;
  FMinSize := 30;
  AutoCapture := true;
  Width := 5;
  Align := vaLeft;
end;

destructor TvgSplitter.Destroy;
begin
  inherited;
end;

function TvgSplitter.FindObject: TvgVisualObject;
var
  P: TvgPoint;
  I: Integer;
  R: TvgRect;
begin
  Result := nil;
  P := Position.Point;
  case Align of
    vaLeft, vaMostLeft: P.X := P.X - 1;
    vaRight, vaMostRight: P.X := P.X + Width + 1;
    vaTop: P.Y := P.Y - 1;
    vaBottom: P.Y := P.Y + Height + 1;
  else
    Exit;
  end;
  if Parent <> nil then
    for I := 0 to Parent.ChildrenCount - 1 do
    begin
      if not Parent.Children[I].IsVisual then Continue;

      Result := Parent.Children[I].Visual;
      if Result.Visible then
      begin
        R := Result.LocalRect;
        vgOffsetRect(R, Result.Position.X, Result.Position.Y);
        if (R.Right - R.Left) = 0 then
          if Align in [vaTop, vaLeft, vaMostLeft] then
            R.Left := R.Left - 1
          else
            R.Right := R.Right + 1;
        if (R.Bottom - R.Top) = 0 then
          if Align in [vaTop, vaLeft, vaMostLeft] then
            R.Top := R.Top - 1
          else
            R.Bottom := R.Bottom + 1;
        if vgPtInRect(P, R) then Exit;
      end;
    end;
  Result := nil;
end;

procedure TvgSplitter.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
var
  i: integer;
begin
  inherited;
  if Button = mbLeft then
  begin
    FPressed := true;
    FDownPos := vgPoint(X, Y);
    FControl := FindObject;
    if Assigned(FControl) then
    begin
      if Align in [vaLeft, vaMostLeft, vaRight, vaMostRight] then
      begin
        FMaxSize := Parent.Visual.Width - FMinSize - Parent.Visual.Margins.left - Parent.Visual.Margins.right;
        for I := 0 to Parent.ChildrenCount - 1 do
        begin
          if not Parent.Children[I].IsVisual then Continue;
          with Parent.Children[I].Visual do
            if (Align in [vaLeft, vaRight, vaMostLeft, vaMostRight]) then
              FMaxSize := FMaxSize - Width - Padding.Left - Padding.Right;
        end;
        FMaxSize := FMaxSize + FControl.Width;
      end
      else
      begin
        FMaxSize := Parent.Visual.Height - FMinSize - Parent.Visual.Margins.top - Parent.Visual.Margins.bottom;
        for I := 0 to Parent.ChildrenCount - 1 do
        begin
          if not Parent.Children[I].IsVisual then Continue;
          with Parent.Children[I].Visual do
            if Align in [vaTop, vaBottom] then
              FMaxSize := FMaxSize - Height - Padding.top - Padding.bottom;
        end;
        FMaxSize := FMaxSize + FControl.Height;
      end;
      UpdateSize(X, Y);
    end;
  end;
end;

procedure TvgSplitter.UpdateSize(X, Y: single);
begin
  CalcSplitSize(X, Y, FNewSize, FSplit);
end;

procedure TvgSplitter.CalcSplitSize(X, Y: single; var NewSize, Split: single);
var
  S: single;
begin
  if Align in [vaLeft, vaRight, vaMostLeft, vaMostRight] then
    Split := X - FDownPos.X
  else
    Split := Y - FDownPos.Y;
  S := 0;
  case Align of
    vaLeft, vaMostLeft: S := FControl.Width + Split;
    vaRight, vaMostRight: S := FControl.Width - Split;
    vaTop: S := FControl.Height + Split;
    vaBottom: S := FControl.Height - Split;
  end;
  NewSize := S;
  if S < FMinSize then
    NewSize := FMinSize
  else
  if S > FMaxSize then
    NewSize := FMaxSize;
  if S <> NewSize then
  begin
    if Align in [vaRight, vaMostRight, vaBottom] then
      S := S - NewSize
    else
      S := NewSize - S;
    Split := Split + S;
  end;
end;

function TvgSplitter.DoCanResize(var NewSize: single): Boolean;
begin
  if (NewSize <= FMinSize) {and FAutoSnap }then
    NewSize := FMinSize;
end;

procedure TvgSplitter.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
var
  NewSize, Split: single;
begin
  inherited;
  if FPressed and Assigned(FControl) then
  begin
    CalcSplitSize(X, Y, NewSize, Split);
    if DoCanResize(NewSize) then
    begin
      FNewSize := NewSize;
      FSplit := Split;
      UpdateControlSize;
    end;
  end;
end;

procedure TvgSplitter.UpdateControlSize;
begin
  if FNewSize <> FOldSize then
  begin
    case Align of
      vaLeft, vaMostLeft: FControl.Width := FNewSize;
      vaTop: FControl.Height := FNewSize;
      vaRight, vaMostRight:
        begin
          FControl.Position.X := FControl.Position.X + (FControl.Width - FNewSize);
          FControl.Width := FNewSize;
        end;
      vaBottom:
        begin
          FControl.Position.Y := FControl.Position.Y + (FControl.Height - FNewSize);
          FControl.Height := FNewSize;
        end;
    end;
//    if Assigned(FOnMoved) then FOnMoved(Self);
    FOldSize := FNewSize;
  end;
end;

procedure TvgSplitter.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  FPressed := false;
  FControl := nil;
end;

{ TvgProgressBar ==============================================================}

constructor TvgProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := false;
  FMax := 100;
  Width := 100;
  Height := 20;
end;

destructor TvgProgressBar.Destroy;
begin
  inherited;
end;

function TvgProgressBar.GetData: Variant;
begin
  Result := Value;
end;

procedure TvgProgressBar.SetData(const Value: Variant);
begin
  Self.Value := Value;
end;

procedure TvgProgressBar.ApplyStyle;
var
  I: TvgObject;
begin
  inherited;
  if Orientation = vgHorizontal then
  begin
    I := FindResource('hindicator');
    if (I <> nil) and (I is TvgVisualObject) then
      TvgVisualObject(I).StartTriggerAnimation(Self, 'IsVisible');
  end
  else
  begin
    I := FindResource('vindicator');
    if (I <> nil) and (I is TvgVisualObject) then
      TvgVisualObject(I).StartTriggerAnimation(Self, 'IsVisible');
  end;
  Realign;
end;

procedure TvgProgressBar.Realign;
var
  hI, vI, T: TvgObject;
begin
  if not FDisableAlign then
  begin
    FDisableAlign := true;
    T := FindResource('Track');
    if (T <> nil) and (T is TvgVisualObject) and (Max > Min) then
    begin
      hI := FindResource('hindicator');
      vI := FindResource('vindicator');
      if Orientation = vgHorizontal then
      begin
        if (hI <> nil) and (hI is TvgVisualObject) then
        begin
          TvgVisualObject(hI).Width := ((Value - Min) / (Max - Min)) * (TvgVisualObject(T).Width - TvgVisualObject(T).Margins.Left - TvgVisualObject(T).Margins.Right -
            TvgVisualObject(hI).Padding.Left - TvgVisualObject(hI).Padding.Right);
          TvgVisualObject(hI).Visible := TvgVisualObject(hI).Width > 2;
        end;
        if (vI <> nil) and (vI is TvgVisualObject) then
          TvgVisualObject(vI).Visible := false;
      end
      else
      begin
        if (vI <> nil) and (vI is TvgVisualObject) then
        begin
          TvgVisualObject(vI).Height := ((Value - Min) / (Max - Min)) * (TvgVisualObject(T).Height - TvgVisualObject(T).Margins.Top - TvgVisualObject(T).Margins.Bottom -
            TvgVisualObject(hI).Padding.Top - TvgVisualObject(hI).Padding.Bottom);
          TvgVisualObject(vI).Visible := TvgVisualObject(vI).Height > 2;
        end;
        if (hI <> nil) and (hI is TvgVisualObject) then
          TvgVisualObject(hI).Visible := false;
      end;
    end;
    FDisableAlign := false;
  end;
  inherited;
end;

procedure TvgProgressBar.SetMax(const Value: single);
begin
  FMax := Value;
end;

procedure TvgProgressBar.SetMin(const Value: single);
begin
  FMin := Value;
end;

procedure TvgProgressBar.SetOrientation(const Value: TvgOrientation);
begin
  FOrientation := Value;
end;

procedure TvgProgressBar.SetValue(const Value: single);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    Realign;
  end;
end;

{ TvgThumb ====================================================================}

constructor TvgThumb.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := false;
  AutoCapture := true;
end;

destructor TvgThumb.Destroy;
begin
  inherited;
end;

procedure TvgThumb.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if (Button = mbLeft) and Enabled then
  begin
    FPressed := true;
    FDownOffset := vgPoint(X, Y);

    if FTrack <> nil then
      FTrack.SetFocus;
  end;
end;

procedure TvgThumb.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if FPressed and (FTrack <> nil) and (Enabled) then
  begin
    if FTrack.Orientation = vgHorizontal then
      FTrack.Value := FTrack.Min + (((Position.X + X - FDownOffset.X) / FTrack.Width) * (FTrack.FMax - FTrack.FMin + FTrack.ViewportSize))
    else
      FTrack.Value := FTrack.Min + (((Position.Y + Y - FDownOffset.Y) / FTrack.Height) * (FTrack.FMax - FTrack.FMin + FTrack.ViewportSize))
  end;
end;

procedure TvgThumb.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if FPressed then
  begin
    if not FTrack.Tracking and Assigned(FTrack.FOnChange) then
      FTrack.FOnChange(Self);
    FPressed := false;
  end;
end;

{ TvgCustomTrack ====================================================================}

constructor TvgCustomTrack.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := true;
  FMax := 1;
  FViewportSize := 0.1;
  Tracking := true;
  Width := 100;
  Height := 15;
end;

destructor TvgCustomTrack.Destroy;
begin
  inherited;
end;

function TvgCustomTrack.GetData: Variant;
begin
  Result := Value;
end;

procedure TvgCustomTrack.SetData(const Value: Variant);
begin
  try
    Self.Value := Value;
  except
  end;
end;

function TvgCustomTrack.GetThumbRect: TvgRect;
begin
  Result := LocalRect;
  if (FMax - FMin) > 0 then
  begin
    if Orientation = vgHorizontal then
    begin
      Result := vgRect(0, 0, (FViewportSize / (FMax - FMin + FViewportSize)) * Width, Height);
      if Result.Right - Result.Left < 12 then
      begin
        Result.Right := Result.Left + 12;
      end;
      vgOffsetRect(Result, Round((((FValue - FMin) / (FMax - FMin + FViewportSize))) * (Width)), 0);
      if Result.Right > Width then
        vgOffsetRect(Result, Width - Result.Right, 0);
    end
    else
    begin
      Result := vgRect(0, 0, Width, (FViewportSize / (FMax - FMin + FViewportSize)) * Height);
      if Result.Bottom - Result.Top < 12 then
      begin
        Result.Bottom := Result.Top + 12;
      end;
      vgOffsetRect(Result, 0, Round((((FValue - FMin) / (FMax - FMin + FViewportSize))) * (Height)));
      if Result.Bottom > Height then
        vgOffsetRect(Result, 0, Height - Result.Bottom);
    end;
  end;
  if (Thumb <> nil) and (Thumb.Parent <> nil) and (Thumb.Parent.IsVisual) then
  begin
    if vgRectWidth(Result) > TvgVisualObject(Thumb.Parent).Margins.Left + Thumb.Padding.Left + TvgVisualObject(Thumb.Parent).Margins.Right - Thumb.Padding.Right then
    begin
      Result.Left := Result.Left + TvgVisualObject(Thumb.Parent).Margins.Left + Thumb.Padding.Left;
      Result.Right := Result.Right - TvgVisualObject(Thumb.Parent).Margins.Right - Thumb.Padding.Right;
    end;
    Result.Top := Result.Top + TvgVisualObject(Thumb.Parent).Margins.Top + Thumb.Padding.Top;
    Result.Bottom := Result.Bottom - TvgVisualObject(Thumb.Parent).Margins.Bottom - Thumb.Padding.Bottom;
  end;
end;

function TvgCustomTrack.GetThumb: TvgThumb;
var
  T: TvgObject;
begin
  T := FindResource('thumb');
  if (T <> nil) and (T is TvgThumb) then
  begin
    Result := TvgThumb(T);
    Result.FTrack := Self;
  end
  else
    Result := nil;
end;

procedure TvgCustomTrack.Realign;
begin
  inherited ;
  if Thumb <> nil then
  begin
    with GetThumbRect do
    begin
      Thumb.Position.X := Left;
      Thumb.Position.Y := Top;
      if Round(Right - Left) > 0 then
        Thumb.Width := Round(Right - Left);
      if Round(Bottom - Top) > 0 then
        Thumb.Height := Round(Bottom - Top);
    end;
  end;
end;

procedure TvgCustomTrack.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
end;

procedure TvgCustomTrack.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
end;

procedure TvgCustomTrack.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
end;

procedure TvgCustomTrack.KeyDown(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
var
  inc: single;
begin
  inc := Frequency;
  if inc = 0 then inc := 1;
  inherited;
  case Key of
    VK_HOME: Value := Min;
    VK_END: Value := Max;
    VK_UP: Value := Value - Frequency;
    VK_DOWN: Value := Value + Frequency;
    VK_LEFT: Value := Value - Frequency;
    VK_RIGHT: Value := Value + Frequency;
  else
    Exit;
  end;
  if not Tracking and Assigned(FOnChange) then
    FOnChange(Self);
  Key := 0;
end;

procedure TvgCustomTrack.SetMax(const Value: single);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMax < FMin then
      FMax := FMin + 0.001;
    if FViewportSize > (FMax - FMin) then
      FViewportSize := FMax - FMin;
    Realign;
  end;
end;

procedure TvgCustomTrack.SetMin(const Value: single);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    Realign;
  end;
end;

procedure TvgCustomTrack.SetOrientation(const Value: TvgOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Realign;
  end;
end;

function TvgCustomTrack.GetIsTracking: boolean;
begin
  Result := (Thumb <> nil) and Thumb.FPressed; 
end;

procedure TvgCustomTrack.SetFrequency(const Value: single);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    if FFrequency <> 0 then
      Self.Value := Round(Self.Value / Frequency) * Frequency;
  end;
end;

procedure TvgCustomTrack.SetValue(Value: single);
begin
  if FFrequency <> 0 then
    Value := Round(Value / Frequency) * Frequency;
  if FValue <> Value then
  begin
    FValue := Value;
    if FValue < FMin then FValue := FMin;
    if FValue > FMax then FValue := FMax;
    if Assigned(FOnTracking) then
      FOnTracking(Self);
    if Tracking and Assigned(FOnChange) then
      FOnChange(Self);
    Realign;
  end;
end;

procedure TvgCustomTrack.SetViewportSize(const Value: single);
begin
  if FViewportSize <> Value then
  begin
    FViewportSize := Value;
    if FViewportSize > (FMax - FMin) then
      FViewportSize := FMax - FMin;
    Realign;
  end;
end;

{ TvgTrackBar }

constructor TvgTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  CanFocused := true;
end;

{ TvgScrollBar ================================================================}

constructor TvgScrollBar.Create(AOwner: TComponent);
begin
  inherited;
  FMax := 1;
  FViewportSize := 0.1;
  FSmallChange := 0.05;
end;

destructor TvgScrollBar.Destroy;
begin
  inherited;
end;

function TvgScrollBar.GetData: Variant;
begin
  Result := Value;
end;

procedure TvgScrollBar.SetData(const Value: Variant);
begin
  Self.Value := Value;
end;

function TvgScrollBar.Track: TvgCustomTrack;
var
  T: TvgObject;
  HT, VT: TvgCustomTrack;
begin
  HT := nil; VT := nil;
  T := FindResource('htrack');
  if (T <> nil) and (T is TvgCustomTrack) then
  begin
    HT := TvgCustomTrack(T);
    HT.FOrientation := vgHorizontal;
    HT.FMax := Max - ViewportSize;
    HT.FMin := Min;
    HT.FValue := Value;
    HT.ViewportSize := ViewportSize;
    HT.Visible := Orientation = vgHorizontal;
    HT.OnChange := DoTrackChanged;
    HT.CanFocused := false;
    if HT.visible then HT.Realign;
  end;
  T := FindResource('vtrack');
  if (T <> nil) and (T is TvgCustomTrack) then
  begin
    VT := TvgCustomTrack(T);
    VT.FOrientation := vgVertical;
    VT.FMax := Max - ViewportSize;
    VT.FMin := Min;
    VT.FValue := Value;
    VT.ViewportSize := ViewportSize;
    VT.Visible := Orientation = vgVertical;
    VT.OnChange := DoTrackChanged;
    VT.CanFocused := false;
    if VT.visible then VT.Realign;
  end;
  if Orientation = vgVertical then
    Result := VT
  else
    Result := HT;
end;

function TvgScrollBar.MinButton: TvgButton;
var
  T: TvgObject;
  LB, TB: TvgButton;
begin
  TB := nil; LB := nil;
  T := FindResource('leftbutton');
  if (T <> nil) and (T is TvgButton) then
  begin
    LB := TvgButton(T);
    LB.OnClick := DoMinButtonClick;
    LB.Visible := Orientation = vgHorizontal;
    LB.CanFocused := false;
  end;

  T := FindResource('topbutton');
  if (T <> nil) and (T is TvgButton) then
  begin
    TB := TvgButton(T);
    TB.OnClick := DoMinButtonClick;
    TB.Visible := Orientation = vgVertical;
    TB.CanFocused := false;
  end;

  if Orientation = vgVertical then
    Result := TB
  else
    Result := LB;
end;

function TvgScrollBar.MaxButton: TvgButton;
var
  T: TvgObject;
  RB, BB: TvgButton;
begin
  RB := nil; BB := nil;
  T := FindResource('rightbutton');
  if (T <> nil) and (T is TvgButton) then
  begin
    RB := TvgButton(T);
    RB.OnClick := DoMaxButtonClick;
    RB.Visible := Orientation = vgHorizontal;
    RB.CanFocused := false;
  end;

  T := FindResource('bottombutton');
  if (T <> nil) and (T is TvgButton) then
  begin
    BB := TvgButton(T);
    BB.OnClick := DoMaxButtonClick;
    BB.Visible := Orientation = vgVertical;
    RB.CanFocused := false;
  end;

  if Orientation = vgVertical then
    Result := BB
  else
    Result := RB;
end;

procedure TvgScrollBar.DoTrackChanged(Sender: TObject);
begin
  Value := TvgCustomTrack(Sender).Value;
end;

procedure TvgScrollBar.DoMinButtonClick(Sender: TObject);
begin
  Value := Value - SmallChange;
end;

procedure TvgScrollBar.DoMaxButtonClick(Sender: TObject);
begin
  Value := Value + SmallChange;
end;

procedure TvgScrollBar.Realign;
begin
  if FDisableAlign then Exit;
  FDisableAlign := true;
  Track;
  MinButton;
  MaxButton;
  FDisableAlign := false;
  inherited;
end;

procedure TvgScrollBar.SetMax(const Value: single);
begin
  if FMax <> Value then
  begin
    FMax := Value;
    if FMax < FMin then
      FMax := FMin + 0.001;
    if FViewportSize > (FMax - FMin) then
      FViewportSize := FMax - FMin;
    Realign;
  end;
end;

procedure TvgScrollBar.SetMin(const Value: single);
begin
  if FMin <> Value then
  begin
    FMin := Value;
    Realign;
  end;
end;

procedure TvgScrollBar.SetOrientation(const Value: TvgOrientation);
begin
  if FOrientation <> Value then
  begin
    FOrientation := Value;
    Realign;
  end;
end;

procedure TvgScrollBar.SetValue(const Value: single);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    if FValue < FMin then FValue := FMin;
    if FValue > FMax - FViewportsize then FValue := FMax - FViewportsize;
    if Assigned(FOnChange) then
      FOnChange(Self);
    Realign;
  end;
end;

procedure TvgScrollBar.SetViewportSize(const Value: single);
begin
  if FViewportSize <> Value then
  begin
    FViewportSize := Value;
    if FViewportSize > (FMax - FMin) then
      FViewportSize := FMax - FMin;
    Realign;
  end;
end;

{ TvgAniIndicator =============================================================}

constructor TvgAniIndicator.Create(AOwner: TComponent);
begin
  inherited;
  FLayout := TvgVisualObject.Create(Self);
  FLayout.Parent := Self;
  FLayout.Align := vaContents;
  FLayout.Locked := true;
  FLayout.Stored := false;
  FAni := TvgFloatAnimation.Create(Self);
  FAni.Parent := FLayout;
  FAni.Loop := true;
  FAni.StartValue := 0;
  FAni.StopValue := 360;
  FAni.Duration := 10;
  FAni.PropertyName := 'RotateAngle';
end;

destructor TvgAniIndicator.Destroy;
begin
  inherited;
end;

procedure TvgAniIndicator.Paint;
var
  a: integer;
  P, P2: TvgPoint;
  wSize, eSize: single;
  V: single;
begin
  if Width < Height then
    wSize := Width / 2
  else
    wSize := Height / 2;
  eSize := wSize / 3.7;
  wSize := wSize - eSize;

  case FStyle of
    vgAniIndicatorLine:
      begin
        Canvas.Stroke.Style := vgBrushsolid;
        Canvas.StrokeThickness := eSize / 2;
        for a := 0 to 11 do
        begin
          P := vgPoint(Width / 2 + (cos(vgDegToRad(a * 30)) * wSize), Height / 2 + (sin(vgDegToRad(a * 30)) * wSize));
          P2 := vgPoint(Width / 2 + (cos(vgDegToRad(a * 30)) * (wSize / 2)), Height / 2 + (sin(vgDegToRad(a * 30)) * (wSize / 2)));
          Canvas.Fill.SolidColor := $FFBABABA;
          Canvas.Stroke.SolidColor := $FFBABABA;
          Canvas.DrawLine(P, P2, Opacity);
          if FEnabled then
          begin
            V := ((Trunc(FLayout.RotateAngle) + (30 - Trunc((a / 12) * 30))) mod 30) / 30;
            if V > 1 then V := Abs(V - 2);
            V := 1 - V;
            Canvas.Stroke.SolidColor := $FF000000;
            Canvas.DrawLine(P, P2, V * Opacity);
          end;
        end;
      end;
    vgAniIndicatorCircle:
      begin
        Canvas.Stroke.Style := vgBrushNone;
        for a := 0 to 7 do
        begin
          P := vgPoint(Width / 2 + (cos(vgDegToRad(a * 45)) * wSize), Height / 2 + (sin(vgDegToRad(a * 45)) * wSize));
          Canvas.Fill.SolidColor := $FFBABABA;
          Canvas.FillEllipse(vgRect(P.X - eSize, P.Y - eSize, P.X + eSize, P.Y + eSize), Opacity);
          if FEnabled then
          begin
            V := ((Trunc(FLayout.RotateAngle) + (30 - Trunc((a / 7) * 30))) mod 30) / 30;
            if V > 1 then V := Abs(V - 2);
            V := 1 - V;
            Canvas.Fill.SolidColor := $FF000000;
            Canvas.FillEllipse(vgRect(P.X - eSize, P.Y - eSize, P.X + eSize, P.Y + eSize), V * Opacity);
          end;
        end;
      end;
  end;
end;

procedure TvgAniIndicator.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      FAni.Start;
    end
    else
      FAni.Stop;
  end;
end;

procedure TvgAniIndicator.SetStyle(const Value: TvgAniIndicatorStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Repaint;
  end;
end;

{ TvgAngleButton ===================================================================}

constructor TvgAngleButton.Create(AOwner: TComponent);
begin
  inherited;
  Width := 30;
  Height := 30;
  FFrequency := 0;
  FTracking := true;
  AutoCapture := true;
  CanFocused := true;
end;

destructor TvgAngleButton.Destroy;
begin
  inherited;
end;

function TvgAngleButton.GetData: Variant;
begin
  Result := Value;
end;

procedure TvgAngleButton.SetData(const Value: Variant);
begin
  Self.Value := Value;
end;

procedure TvgAngleButton.ApplyStyle;
begin
  inherited;
  Tick;
  Text;
end;

function TvgAngleButton.Tick: TvgVisualObject;
var
  T: TvgObject;
begin
  T := FindResource('tick');
  if (T <> nil) and (T is TvgVisualObject) then
  begin
    Result := TvgVisualObject(T);
    Result.RotateAngle := -FValue;
  end
  else
    Result := nil;
end;

function TvgAngleButton.Text: TvgText;
var
  T: TvgObject;
begin
  T := FindResource('tracktext');
  if (T <> nil) and (T is TvgText) then
  begin
    TvgText(T).Visible := false; //FPressing;
    TvgText(T).Text := IntToStr(Round(Value)) + System.WideChar($B0);
    if FPressing and not FTracking then
      TvgText(T).Opacity := 1
    else
      TvgText(T).Opacity := 0;
  end;

  T := FindResource('text');
  if (T <> nil) and (T is TvgText) then
  begin
    Result := TvgText(T);
    Result.Visible := FShowValue;
    Result.Text := IntToStr(Round(Value)) + System.WideChar($B0);
    if not FShowValue then
      Result.Opacity := 0
    else
      Result.Opacity := 1;
  end
  else
    Result := nil;
end;

procedure TvgAngleButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if Button = mbLeft then
  begin
    FPressing := true;
    FOldPos := vgPoint(X, Y);
    FSaveValue := Value;
    Text;
  end;
end;

procedure TvgAngleButton.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  inherited;
  if (ssLeft in Shift) and (FPressing) then
  begin
    Value := vgVectorAngle(vgVector(1, 0), vgVector(X - (Width / 2), Y - (Height / 2)));
    FOldPos := vgPoint(X, Y);
    Text;
  end;
end;

procedure TvgAngleButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  inherited;
  if FPressing then
  begin
    FPressing := false;
    Text;
    if Value <> FSaveValue then
      if Assigned(FOnChange) then
        FOnChange(Self);
  end;
end;

procedure TvgAngleButton.Paint;
begin
  inherited;
end;

procedure TvgAngleButton.SetValue(const Value: single);
begin
  if (FFrequency = 0) then
  begin
    if (FValue <> Value) then
    begin
      FValue := Value;
      if Tick <> nil then
        Tick.RotateAngle := -FValue
      else
        Repaint;
      Text;
      if Assigned(FOnChange) and (not FPressing or FTracking) then
        FOnChange(Self);
    end;
  end
  else
  begin
    if FValue <> Round(Value / FFrequency) * FFrequency then
    begin
      FValue := Round(Value / FFrequency) * FFrequency;
      if Tick <> nil then
        Tick.RotateAngle := -FValue
      else
        Repaint;
      Text;
      if Assigned(FOnChange) and (not FPressing or FTracking) then
        FOnChange(Self);
    end;
  end;
end;

procedure TvgAngleButton.SetShowValue(const Value: boolean);
begin
  if FShowValue <> Value then
  begin
    FShowValue := Value;
    Text;
    Repaint;
  end;
end;

procedure TvgTrackBar.Loaded;
begin
  inherited;
  FViewportSize := (FMax - FMin) / 5;
end;

procedure TvgTrackBar.SetMax(const Value: single);
begin
  if FMax <> Value then
  begin
    inherited;
    FViewportSize := (FMax - FMin) / 5;
  end;
end;

procedure TvgTrackBar.SetMin(const Value: single);
begin
  inherited;
  if FMin <> Value then
  begin
    inherited;
    FViewportSize := (FMax - FMin) / 5;
  end;
end;

{ TvgExpanderButton }

constructor TvgExpanderButton.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TvgExpanderButton.Destroy;
begin
  inherited;
end;

{ TvgExpander =================================================================}

procedure TvgExpander.AddObject(AObject: TvgObject);
begin
  if (FContent <> nil) and (AObject <> FButton) and (AObject <> FContent) and (AObject.ResourceName = '') then
  begin
    FContent.AddObject(AObject);
    Realign;
  end
  else
    inherited;
end;

procedure TvgExpander.ApplyStyle;
begin
  inherited;
  StartTriggerAnimation(Self, 'IsExpanded');
  if FButton <> nil then
  begin
    FButton.StartTriggerAnimation(Self, 'IsExpanded');
    FButton.CanFocused := false;
  end;
end;

constructor TvgExpander.Create(AOwner: TComponent);
begin
  inherited;
  FIsExpanded := true;

  FContent := TvgContent.Create(Self);
  FContent.Parent := Self;
  FContent.ClipChildren := false;
  FContent.HitTest := false;
  FContent.Locked := true;
  FContent.Stored := false;
  FContent.Padding.Top := 25;

  FButton := TvgExpanderButton.Create(Self);
  FButton.Parent := Self;
  FButton.Stored := false;
  FButton.Width := 24;
  FButton.Height := 24;
  FButton.Locked := true;
  FButton.OnClick := DoButtonClick;
end;

destructor TvgExpander.Destroy;
begin
  inherited;
end;

procedure TvgExpander.DoButtonClick(Sender: TObject);
begin
  IsExpanded := not FIsExpanded;
end;

procedure TvgExpander.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('ContentSize', ReadContentSize, WriteContentSize, true);
end;

procedure TvgExpander.ReadContentSize(Reader: TReader);
begin
  if FContent <> nil then
    FContent.Height := vgStrToFloat(Reader.ReadString);
end;

procedure TvgExpander.WriteContentSize(Writer: TWriter);
begin
  if FContent <> nil then
    Writer.WriteString(vgFloatToStr(FContent.Height));
end;

procedure TvgExpander.Realign;
begin
  inherited;
  if FDisableAlign then Exit;
  FDisableAlign := true;
  { content }
  if (FContent <> nil) and (IsExpanded) then
  begin
    FContent.Position.X := 0;
    FContent.Position.Y := FContent.Padding.Top;
    FContent.Width := Width;
    FContent.Height := Height - FContent.Padding.Top;
  end;
  FDisableAlign := false;
end;

procedure TvgExpander.SetIsExpanded(const Value: boolean);
begin
  if FIsExpanded <> Value then
  begin
    FIsExpanded := Value;
    if FIsExpanded then
    begin
      FContent.Visible := FIsExpanded;
      Height := FButton.Height + FContent.Height;
      Repaint;
    end
    else
    begin
      Repaint;
      FContent.Visible := FIsExpanded;
      Height := FButton.Height;
    end;
    StartTriggerAnimation(Self, 'IsExpanded');
    FButton.StartTriggerAnimation(Self, 'IsExpanded');
  end;
end;

procedure TvgExpander.DesignClick;
begin
  inherited;
  IsExpanded := not IsExpanded;
end;

{ TvgPopupBox =================================================================}

constructor TvgPopupBox.Create(AOwner: TComponent);
begin
  inherited;
  Height := 21;
  FItemIndex := -1;
  FItems := TvgWideStringList.Create;;
  TvgWideStringList(FItems).OnChange := DoItemsChanged;
  FPopup := TPopupMenu.Create(nil);
end;

destructor TvgPopupBox.Destroy;
begin
  FreeAndNil(FPopup);
  FreeAndNil(FItems);
  inherited;
end;

function TvgPopupBox.GetData: Variant;
begin
  Result := Text;
end;

procedure TvgPopupBox.SetData(const Value: Variant);
begin
  ItemIndex := FItems.IndexOf(Value);
end;

procedure TvgPopupBox.ApplyStyle;
begin
  inherited;
end;

procedure TvgPopupBox.Click;
begin
  inherited;
  DoPopup;
end;

procedure TvgPopupBox.DoPopup;
var
  Item: TMenuItem;
  P: TPoint;
  VP: TvgPoint;
  i: integer;
begin
  FPopup.Items.Clear;
  for i := 0 to FItems.Count - 1 do
  begin
    Item := NewItem({$IFDEF FPC}UTF8Encode{$ENDIF}(FItems[i]), 0, i = FItemIndex, true, DoItemClick, 0, '');
    FPopup.Items.Add(Item);
  end;
  if Scene <> nil then
  begin
    VP := LocalToAbsolute(vgPoint(0, Trunc((Height / 2) - ((FItems.Count * 20) div 2))));
    P := Point(Trunc(VP.X), Trunc(VP.Y));
    P := Scene.ClientToScreen(P);
    FPopup.Popup(P.X, P.Y);
  end;
end;

procedure TvgPopupBox.DoItemClick(Sender: TObject);
begin
  ItemIndex := TMenuItem(Sender).MenuIndex;
end;

procedure TvgPopupBox.DoItemsChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TvgPopupBox.SetItemIndex(const Value: integer);
begin
  if FItemIndex <> Value then
  begin
    FItemIndex := Value;
    if FItemIndex >= 0 then
      Text := Items[FItemIndex]
    else
      Repaint;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TvgPopupBox.SetItems(const Value: TvgWideStrings);
begin
  FItems.Assign(Value);
end;

{ TvgWindow ===================================================================}

constructor TvgWindow.Create(AOwner: TComponent);
begin
  inherited;
  FShowCloseButton := true;
  FShowSizeGrip := true;
  HitTest := false;
  TextAlign := vgTextalignNear;
  Width := 200;
  Height := 200;
end;

procedure TvgWindow.ApplyStyle;
var
  sizeGrip, closeBtn: TvgObject;
begin
  inherited;
  closeBtn := FindResource('close');
  if (closeBtn <> nil) and (closeBtn is TvgVisualObject) then
  begin
    TvgVisualObject(closeBtn).visible := FShowCloseButton;
    if (closeBtn is TvgCloseButton) and (Assigned(FOnCloseClick)) then
    begin
      TvgCloseButton(closeBtn).CloseForm := false;
      TvgCloseButton(closeBtn).OnClick := FOnCloseClick;
    end;
  end;
  sizeGrip := FindResource('sizegrip');
  if (sizeGrip <> nil) and (sizeGrip is TvgVisualObject) then
  begin
    TvgVisualObject(sizeGrip).visible := FShowSizeGrip;
  end;
end;

destructor TvgWindow.Destroy;
begin
  inherited;
end;

procedure TvgWindow.SetShowCloseButton(const Value: boolean);
begin
  if FShowCloseButton <> Value then
  begin
    FShowCloseButton := Value;
    Resource := FResource;
  end;
end;

procedure TvgWindow.SetShowSizeGrip(const Value: boolean);
begin
  if FShowSizeGrip <> Value then
  begin
    FShowSizeGrip := Value;
    Resource := FResource;
  end;
end;

{ TvgLayerWindow }

constructor TvgLayerWindow.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TvgLayerWindow.Destroy;
begin
  inherited;
end;

{ TvgHudWindow }

constructor TvgHudWindow.Create(AOwner: TComponent);
begin
  inherited;
  FFill := TvgBrush.Create(vgBrushSolid, $E02F2F2F);
  FFill.OnChanged := DoFillChanged;
  FStroke := TvgBrush.Create(vgBrushSolid, $5B000000);
  FStroke.OnChanged := DoFillChanged;
  FStrokeThickness := 1;
  FDisableShadowOnOSX := true;
  FShowCaption := true;
  Font.Style := vgFontBold;
end;

destructor TvgHudWindow.Destroy;
begin
  FStroke.Free;
  FFill.Free;
  inherited;
end;

procedure TvgHudWindow.ApplyStyle;
var
  {$IFDEF DARWIN}
  shadow: TvgObject;
  {$endif}
  back: TvgObject;
begin
  inherited;
  {$IFDEF DARWIN}
  shadow := FindResource('shadow');
  if (shadow <> nil) and (shadow is TvgVisualObject) and (FDisableShadowOnOSX) then
  begin
    TvgVisualObject(shadow).visible := false;
  end;
  {$ENDIF}
  back := FindResource('close');
  if (back <> nil) and (back is TvgCloseButton) then
  begin
    if FCloseAlign = vgButtonAlignLeft then
      TvgCloseButton(back).Align := vaLeft
    else
      TvgCloseButton(back).Align := vaRight;
    if TvgVisualObject(back).Visible then
    begin
      TvgVisualObject(back).Visible := FShowCaption;
      TvgVisualObject(back).DesignHide := not FShowCaption;
    end;
  end;
  back := FindResource('back');
  if (back <> nil) and (back is TvgShape) then
  begin
    TvgShape(back).Fill.Assign(FFill);
  end;
  back := FindResource('stroke');
  if (back <> nil) and (back is TvgShape) then
  begin
    TvgShape(back).Stroke.Assign(FStroke);
    TvgShape(back).StrokeThickness := FStrokeThickness;
    TvgShape(back).StrokeCap := FStrokeCap;
    TvgShape(back).StrokeDash := FStrokeDash;
    TvgShape(back).StrokeJoin := FStrokeJoin;
    TvgVisualObject(back).Margins.Rect := vgRect(FStrokeThickness, FStrokeThickness, FStrokeThickness, FStrokeThickness);
  end;
  back := FindResource('caption');
  if (back <> nil) and (back is TvgVisualObject) then
  begin
    TvgVisualObject(back).Height := 20 + FStrokeThickness;
    TvgVisualObject(back).Padding.Rect := vgRect(FStrokeThickness, FStrokeThickness, FStrokeThickness, 0);
    TvgVisualObject(back).Visible := FShowCaption;
    TvgVisualObject(back).DesignHide := not FShowCaption;
  end;
  back := FindResource('text');
  if (back <> nil) and (back is TvgVisualObject) then
  begin
    TvgVisualObject(back).Visible := FShowCaption;
    TvgVisualObject(back).DesignHide := not FShowCaption;
  end;
end;

procedure TvgHudWindow.SetDisableShadowOnOSX(const Value: boolean);
begin
  if FDisableShadowOnOSX <> Value then
  begin
    FDisableShadowOnOSX := Value;
    Resource := FResource;
  end;
end;

procedure TvgHudWindow.SetFill(const Value: TvgBrush);
begin
  FFill.Assign(Value);
end;

procedure TvgHudWindow.DoFillChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    ApplyStyle;
end;

function TvgHudWindow.isStrokeThicknessStored: Boolean;
begin
  Result := StrokeThickness <> 1;
end;

procedure TvgHudWindow.SetStroke(const Value: TvgBrush);
begin
  if FStroke <> Value then
  begin
    FStroke := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgHudWindow.SetStrokeCap(const Value: TvgStrokeCap);
begin
  if FStrokeCap <> Value then
  begin
    FStrokeCap := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgHudWindow.SetStrokeDash(const Value: TvgStrokeDash);
begin
  if FStrokeDash <> Value then
  begin
    FStrokeDash := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgHudWindow.SetStrokeJoin(const Value: TvgStrokeJoin);
begin
  if FStrokeJoin <> Value then
  begin
    FStrokeJoin := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgHudWindow.SetStrokeThickness(const Value: single);
begin
  if FStrokeThickness <> Value then
  begin
    FStrokeThickness := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgHudWindow.SetCloseAlign(const Value: TvgCloseAlign);
begin
  if FCloseAlign <> Value then
  begin
    FCloseAlign := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

procedure TvgHudWindow.SetShowCaption(const Value: boolean);
begin
  if FShowCaption <> Value then
  begin
    FShowCaption := Value;
    if not (csLoading in ComponentState) then
      ApplyStyle;
  end;
end;

{ TvgBitmapStateButton }

constructor TvgBitmapStateButton.Create(AOwner: TComponent);
begin
  inherited;
  FBitmap := TvgBitmap.Create(1, 1);
  FBitmap.OnChange := DoBitmapChanged;
  FBitmapHot := TvgBitmap.Create(1, 1);
  FBitmapDown := TvgBitmap.Create(1, 1);
  Width := 64;
  Height := 64;
end;

destructor TvgBitmapStateButton.Destroy;
begin
  FBitmap.Free;
  FBitmapHot.Free;
  FBitmapDown.Free;
  inherited;
end;

procedure TvgBitmapStateButton.ApplyStyle;
begin
  inherited;
end;

procedure TvgBitmapStateButton.DoBitmapChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TvgBitmapStateButton.SetBitmap(const Value: TvgBitmap);
begin
end;

procedure TvgBitmapStateButton.SetBitmapDown(const Value: TvgBitmap);
begin
end;

procedure TvgBitmapStateButton.SetBitmapHot(const Value: TvgBitmap);
begin
end;

procedure TvgBitmapStateButton.Paint;
var
  scale: single;
  R: TvgRect;
  B: TvgBitmap;
begin
  if IsPressed then
    B := FBitmapDown
  else
    if IsMouseOver then
      B := FBitmapHot
    else
      B := FBitmap;

  R := vgRect(0, 0, B.Width, B.Height);
  scale := vgFitRect(R, LocalRect);
  if scale > 1 then
    Canvas.DrawBitmap(B, vgRect(0, 0, B.Width, B.Height), R, AbsoluteOpacity)
  else
  begin
    R := vgRect(0, 0, B.Width, B.Height);
    vgRectCenter(R, LocalRect);
    Canvas.DrawBitmap(B, vgRect(0, 0, B.Width, B.Height), R, AbsoluteOpacity)
  end;
end;

procedure TvgBitmapStateButton.MouseEnter;
begin
  inherited;
  Repaint;
end;

procedure TvgBitmapStateButton.MouseLeave;
begin
  inherited;
  Repaint;
end;

procedure TvgBitmapStateButton.StartTriggerAnimation(AInstance: TvgObject;
  ATrigger: string);
begin
  inherited;
  if Pos('IsPressed', ATrigger) > 0 then
  begin
    Repaint;
  end;
end;

initialization
  {$IFDEF FPC}
  {$I vg_controls.lrs}
  {$ENDIF}
  RegisterVGObjects('Popup', [TvgPopup, TvgMessagePopup]);
  RegisterVGObjects('Windows', [TvgBackground, TvgWindow, TvgLayerWindow, TvgSizeGrip, TvgCloseButton]);
  RegisterVGObjects('Controls', [TvgLabel, TvgValueLabel, TvgButton, TvgBitmapButton, TvgPathButton, TvgSpeedButton, TvgCheckBox, TvgRadioButton,
    TvgGroupBox, TvgProgressBar, TvgTrack, TvgThumb, TvgScrollBar, TvgAniIndicator, TvgExpanderButton,
    TvgExpander, TvgTrackBar, TvgToolButton, TvgToolPathButton, TvgAngleButton, TvgPopupBox, TvgSplitter, TvgStatusBar, TvgToolBar, TvgBitmapStateButton]);
  RegisterVGObjects('Layout', [TvgPanel]);
  RegisterVGObjects('HUD', [TvgHudPanel, TvgHudWindow, TvgHudButton, TvgHudSpeedButton, TvgHudAngleButton, TvgHudTrack, TvgHudTrackBar,
    TvgHudScrollBar, TvgHudPopupBox, TvgHudLabel, TvgHudCheckBox, TvgHudRadioButton, TvgHudGroupBox, TvgHudCloseButton,
    TvgHudStatusBar, TvgHudSizeGrip]);
end.


