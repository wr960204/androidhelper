unit vg_scene;

{$I vg_define.inc}
{$I vg_scene.inc}
{$H+}
{.$DEFINE DARWINBUFFER}
{.$DEFINE UPDATERECT}
{.$DEFINE BOUNDS}

interface

uses
  {$IFDEF LINUX}
  cairo, cairoXlib, xlib, x,xutil, gtkdef, gtkproc, gtk2, gdk2, gdk2x, gdk2pixbuf,
  {$ENDIF}
  {$IFDEF DARWIN}
  macosall,
  CarbonProc, CarbonDef, CarbonPrivate, carboncanvas,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows, Messages, MMSystem, ShellAPI, ActiveX, CommCtrl,
  {$ENDIF}
  {$IFDEF FPC}
  LCLProc, LCLIntf, LCLType, LMessages, LResources,
  {$ELSE}
  Imm,
  {$ENDIF}
  vg_classes,
  Classes, SysUtils, Forms, Controls, Dialogs, Graphics, ExtCtrls, Menus;

{$HPPEMIT '#include <shldisp.h>'}

const
  PaintCount: integer = 0;
  PaintCount2: integer = 0;
  ImageCount: integer = 0;
  FloatCount: single = 0;


{$IFDEF WIN32}
const
  WM_ADDUPDATERECT = WM_USER + 123;
{$ENDIF}

const

   GripSize = 3;
   RotSize = 10;

const

  vcAliceblue = '#FFF0F8FF';
  vcAntiquewhite = '#FFFAEBD7';
  vcAqua = '#FF00FFFF';
  vcAquamarine = '#FF7FFFD4';
  vcAzure = '#FFF0FFFF';
  vcBeige = '#FFF5F5DC';
  vcBisque = '#FFFFE4C4';
  vcBlack = '#FF000000';
  vcBlanchedalmond = '#FFFFEBCD';
  vcBlue = '#FF0000FF';
  vcBlueviolet = '#FF8A2BE2';
  vcBrown = '#FFA52A2A';
  vcBurlywood = '#FFDEB887';
  vcCadetblue = '#FF5F9EA0';
  vcChartreuse = '#FF7FFF00';
  vcChocolate = '#FFD2691E';
  vcCoral = '#FFFF7F50';
  vcCornflowerblue = '#FF6495ED';
  vcCornsilk = '#FFFFF8DC';
  vcCrimson = '#FFDC143C';
  vcCyan = '#FF00FFFF';
  vcDarkblue = '#FF00008B';
  vcDarkcyan = '#FF008B8B';
  vcDarkgoldenrod = '#FFB8860B';
  vcDarkgray = '#FFA9A9A9';
  vcDarkgreen = '#FF006400';
  vcDarkgrey = '#FFA9A9A9';
  vcDarkkhaki = '#FFBDB76B';
  vcDarkmagenta = '#FF8B008B';
  vcDarkolivegreen = '#FF556B2F';
  vcDarkorange = '#FFFF8C00';
  vcDarkorchid = '#FF9932CC';
  vcDarkred = '#FF8B0000';
  vcDarksalmon = '#FFE9967A';
  vcDarkseagreen = '#FF8FBC8F';
  vcDarkslateblue = '#FF483D8B';
  vcDarkslategray = '#FF2F4F4F';
  vcDarkslategrey = '#FF2F4F4F';
  vcDarkturquoise = '#FF00CED1';
  vcDarkviolet = '#FF9400D3';
  vcDeeppink = '#FFFF1493';
  vcDeepskyblue = '#FF00BFFF';
  vcDimgray = '#FF696969';
  vcDimgrey = '#FF696969';
  vcDodgerblue = '#FF1E90FF';
  vcFirebrick = '#FFB22222';
  vcFloralwhite = '#FFFFFAF0';
  vcForestgreen = '#FF228B22';
  vcFuchsia = '#FFFF00FF';
  vcGainsboro = '#FFDCDCDC';
  vcGhostwhite = '#FFF8F8FF';
  vcGold = '#FFFFD700';
  vcGoldenrod = '#FFDAA520';
  vcGray = '#FF808080';
  vcGreen = '#FF008000';
  vcGreenyellow = '#FFADFF2F';
  vcGrey = '#FF808080';
  vcHoneydew = '#FFF0FFF0';
  vcHotpink = '#FFFF69B4';
  vcIndianred = '#FFCD5C5C';
  vcIndigo = '#FF4B0082';
  vcIvory = '#FFFFFFF0';
  vcKhaki = '#FFF0E68C';
  vcLavender = '#FFE6E6FA';
  vcLavenderblush = '#FFFFF0F5';
  vcLawngreen = '#FF7CFC00';
  vcLemonchiffon = '#FFFFFACD';
  vcLightblue = '#FFADD8E6';
  vcLightcoral = '#FFF08080';
  vcLightcyan = '#FFE0FFFF';
  vcLightgoldenrodyellow = '#FFFAFAD2';
  vcLightgray = '#FFD3D3D3';
  vcLightgreen = '#FF90EE90';
  vcLightgrey = '#FFD3D3D3';
  vcLightpink = '#FFFFB6C1';
  vcLightsalmon = '#FFFFA07A';
  vcLightseagreen = '#FF20B2AA';
  vcLightskyblue = '#FF87CEFA';
  vcLightslategray = '#FF778899';
  vcLightslategrey = '#FF778899';
  vcLightsteelblue = '#FFB0C4DE';
  vcLightyellow = '#FFFFFFE0';
  vcLime = '#FF00FF00';
  vcLimegreen = '#FF32CD32';
  vcLinen = '#FFFAF0E6';
  vcMagenta = '#FFFF00FF';
  vcMaroon = '#FF800000';
  vcMediumaquamarine = '#FF66CDAA';
  vcMediumblue = '#FF0000CD';
  vcMediumorchid = '#FFBA55D3';
  vcMediumpurple = '#FF9370DB';
  vcMediumseagreen = '#FF3CB371';
  vcMediumslateblue = '#FF7B68EE';
  vcMediumspringgreen = '#FF00FA9A';
  vcMediumturquoise = '#FF48D1CC';
  vcMediumvioletred = '#FFC71585';
  vcMidnightblue = '#FF191970';
  vcMintcream = '#FFF5FFFA';
  vcMistyrose = '#FFFFE4E1';
  vcMoccasin = '#FFFFE4B5';
  vcNavajowhite = '#FFFFDEAD';
  vcNavy = '#FF000080';
  vcOldlace = '#FFFDF5E6';
  vcOlive = '#FF808000';
  vcOlivedrab = '#FF6B8E23';
  vcOrange = '#FFFFA500';
  vcOrangered = '#FFFF4500';
  vcOrchid = '#FFDA70D6';
  vcPalegoldenrod = '#FFEEE8AA';
  vcPalegreen = '#FF98FB98';
  vcPaleturquoise = '#FFAFEEEE';
  vcPalevioletred = '#FFDB7093';
  vcPapayawhip = '#FFFFEFD5';
  vcPeachpuff = '#FFFFDAB9';
  vcPeru = '#FFCD853F';
  vcPink = '#FFFFC0CB';
  vcPlum = '#FFDDA0DD';
  vcPowderblue = '#FFB0E0E6';
  vcPurple = '#FF800080';
  vcRed = '#FFFF0000';
  vcRosybrown = '#FFBC8F8F';
  vcRoyalblue = '#FF4169E1';
  vcSaddlebrown = '#FF8B4513';
  vcSalmon = '#FFFA8072';
  vcSandybrown = '#FFF4A460';
  vcSeagreen = '#FF2E8B57';
  vcSeashell = '#FFFFF5EE';
  vcSienna = '#FFA0522D';
  vcSilver = '#FFC0C0C0';
  vcSkyblue = '#FF87CEEB';
  vcSlateblue = '#FF6A5ACD';
  vcSlategray = '#FF708090';
  vcSlategrey = '#FF708090';
  vcSnow = '#FFFFFAFA';
  vcSpringgreen = '#FF00FF7F';
  vcSteelblue = '#FF4682B4';
  vcTan = '#FFD2B48C';
  vcTeal = '#FF008080';
  vcThistle = '#FFD8BFD8';
  vcTomato = '#FFFF6347';
  vcTurquoise = '#FF40E0D0';
  vcViolet = '#FFEE82EE';
  vcWheat = '#FFF5DEB3';
  vcWhite = '#FFFFFFFF';
  vcWhitesmoke = '#FFF5F5F5';
  vcYellow = '#FFFFFF00';
  vcYellowgreen = '#FF9ACD32';

type

  TvgColorIdent = record
    Name: string;
    Value: string;
  end;

const

  vgColorIdents: array [0..146] of TvgColorIdent = (
    (Name: 'Aliceblue'; Value: '#FFF0F8FF'),
    (Name: 'Antiquewhite'; Value: '#FFFAEBD7'),
    (Name: 'Aqua'; Value: '#FF00FFFF'),
    (Name: 'Aquamarine'; Value: '#FF7FFFD4'),
    (Name: 'Azure'; Value: '#FFF0FFFF'),
    (Name: 'Beige'; Value: '#FFF5F5DC'),
    (Name: 'Bisque'; Value: '#FFFFE4C4'),
    (Name: 'Black'; Value: '#FF000000'),
    (Name: 'Blanchedalmond'; Value: '#FFFFEBCD'),
    (Name: 'Blue'; Value: '#FF0000FF'),
    (Name: 'Blueviolet'; Value: '#FF8A2BE2'),
    (Name: 'Brown'; Value: '#FFA52A2A'),
    (Name: 'Burlywood'; Value: '#FFDEB887'),
    (Name: 'Cadetblue'; Value: '#FF5F9EA0'),
    (Name: 'Chartreuse'; Value: '#FF7FFF00'),
    (Name: 'Chocolate'; Value: '#FFD2691E'),
    (Name: 'Coral'; Value: '#FFFF7F50'),
    (Name: 'Cornflowerblue'; Value: '#FF6495ED'),
    (Name: 'Cornsilk'; Value: '#FFFFF8DC'),
    (Name: 'Crimson'; Value: '#FFDC143C'),
    (Name: 'Cyan'; Value: '#FF00FFFF'),
    (Name: 'Darkblue'; Value: '#FF00008B'),
    (Name: 'Darkcyan'; Value: '#FF008B8B'),
    (Name: 'Darkgoldenrod'; Value: '#FFB8860B'),
    (Name: 'Darkgray'; Value: '#FFA9A9A9'),
    (Name: 'Darkgreen'; Value: '#FF006400'),
    (Name: 'Darkgrey'; Value: '#FFA9A9A9'),
    (Name: 'Darkkhaki'; Value: '#FFBDB76B'),
    (Name: 'Darkmagenta'; Value: '#FF8B008B'),
    (Name: 'Darkolivegreen'; Value: '#FF556B2F'),
    (Name: 'Darkorange'; Value: '#FFFF8C00'),
    (Name: 'Darkorchid'; Value: '#FF9932CC'),
    (Name: 'Darkred'; Value: '#FF8B0000'),
    (Name: 'Darksalmon'; Value: '#FFE9967A'),
    (Name: 'Darkseagreen'; Value: '#FF8FBC8F'),
    (Name: 'Darkslateblue'; Value: '#FF483D8B'),
    (Name: 'Darkslategray'; Value: '#FF2F4F4F'),
    (Name: 'Darkslategrey'; Value: '#FF2F4F4F'),
    (Name: 'Darkturquoise'; Value: '#FF00CED1'),
    (Name: 'Darkviolet'; Value: '#FF9400D3'),
    (Name: 'Deeppink'; Value: '#FFFF1493'),
    (Name: 'Deepskyblue'; Value: '#FF00BFFF'),
    (Name: 'Dimgray'; Value: '#FF696969'),
    (Name: 'Dimgrey'; Value: '#FF696969'),
    (Name: 'Dodgerblue'; Value: '#FF1E90FF'),
    (Name: 'Firebrick'; Value: '#FFB22222'),
    (Name: 'Floralwhite'; Value: '#FFFFFAF0'),
    (Name: 'Forestgreen'; Value: '#FF228B22'),
    (Name: 'Fuchsia'; Value: '#FFFF00FF'),
    (Name: 'Gainsboro'; Value: '#FFDCDCDC'),
    (Name: 'Ghostwhite'; Value: '#FFF8F8FF'),
    (Name: 'Gold'; Value: '#FFFFD700'),
    (Name: 'Goldenrod'; Value: '#FFDAA520'),
    (Name: 'Gray'; Value: '#FF808080'),
    (Name: 'Green'; Value: '#FF008000'),
    (Name: 'Greenyellow'; Value: '#FFADFF2F'),
    (Name: 'Grey'; Value: '#FF808080'),
    (Name: 'Honeydew'; Value: '#FFF0FFF0'),
    (Name: 'Hotpink'; Value: '#FFFF69B4'),
    (Name: 'Indianred'; Value: '#FFCD5C5C'),
    (Name: 'Indigo'; Value: '#FF4B0082'),
    (Name: 'Ivory'; Value: '#FFFFFFF0'),
    (Name: 'Khaki'; Value: '#FFF0E68C'),
    (Name: 'Lavender'; Value: '#FFE6E6FA'),
    (Name: 'Lavenderblush'; Value: '#FFFFF0F5'),
    (Name: 'Lawngreen'; Value: '#FF7CFC00'),
    (Name: 'Lemonchiffon'; Value: '#FFFFFACD'),
    (Name: 'Lightblue'; Value: '#FFADD8E6'),
    (Name: 'Lightcoral'; Value: '#FFF08080'),
    (Name: 'Lightcyan'; Value: '#FFE0FFFF'),
    (Name: 'Lightgoldenrodyellow'; Value: '#FFFAFAD2'),
    (Name: 'Lightgray'; Value: '#FFD3D3D3'),
    (Name: 'Lightgreen'; Value: '#FF90EE90'),
    (Name: 'Lightgrey'; Value: '#FFD3D3D3'),
    (Name: 'Lightpink'; Value: '#FFFFB6C1'),
    (Name: 'Lightsalmon'; Value: '#FFFFA07A'),
    (Name: 'Lightseagreen'; Value: '#FF20B2AA'),
    (Name: 'Lightskyblue'; Value: '#FF87CEFA'),
    (Name: 'Lightslategray'; Value: '#FF778899'),
    (Name: 'Lightslategrey'; Value: '#FF778899'),
    (Name: 'Lightsteelblue'; Value: '#FFB0C4DE'),
    (Name: 'Lightyellow'; Value: '#FFFFFFE0'),
    (Name: 'Lime'; Value: '#FF00FF00'),
    (Name: 'Limegreen'; Value: '#FF32CD32'),
    (Name: 'Linen'; Value: '#FFFAF0E6'),
    (Name: 'Magenta'; Value: '#FFFF00FF'),
    (Name: 'Maroon'; Value: '#FF800000'),
    (Name: 'Mediumaquamarine'; Value: '#FF66CDAA'),
    (Name: 'Mediumblue'; Value: '#FF0000CD'),
    (Name: 'Mediumorchid'; Value: '#FFBA55D3'),
    (Name: 'Mediumpurple'; Value: '#FF9370DB'),
    (Name: 'Mediumseagreen'; Value: '#FF3CB371'),
    (Name: 'Mediumslateblue'; Value: '#FF7B68EE'),
    (Name: 'Mediumspringgreen'; Value: '#FF00FA9A'),
    (Name: 'Mediumturquoise'; Value: '#FF48D1CC'),
    (Name: 'Mediumvioletred'; Value: '#FFC71585'),
    (Name: 'Midnightblue'; Value: '#FF191970'),
    (Name: 'Mintcream'; Value: '#FFF5FFFA'),
    (Name: 'Mistyrose'; Value: '#FFFFE4E1'),
    (Name: 'Moccasin'; Value: '#FFFFE4B5'),
    (Name: 'Navajowhite'; Value: '#FFFFDEAD'),
    (Name: 'Navy'; Value: '#FF000080'),
    (Name: 'Oldlace'; Value: '#FFFDF5E6'),
    (Name: 'Olive'; Value: '#FF808000'),
    (Name: 'Olivedrab'; Value: '#FF6B8E23'),
    (Name: 'Orange'; Value: '#FFFFA500'),
    (Name: 'Orangered'; Value: '#FFFF4500'),
    (Name: 'Orchid'; Value: '#FFDA70D6'),
    (Name: 'Palegoldenrod'; Value: '#FFEEE8AA'),
    (Name: 'Palegreen'; Value: '#FF98FB98'),
    (Name: 'Paleturquoise'; Value: '#FFAFEEEE'),
    (Name: 'Palevioletred'; Value: '#FFDB7093'),
    (Name: 'Papayawhip'; Value: '#FFFFEFD5'),
    (Name: 'Peachpuff'; Value: '#FFFFDAB9'),
    (Name: 'Peru'; Value: '#FFCD853F'),
    (Name: 'Pink'; Value: '#FFFFC0CB'),
    (Name: 'Plum'; Value: '#FFDDA0DD'),
    (Name: 'Powderblue'; Value: '#FFB0E0E6'),
    (Name: 'Purple'; Value: '#FF800080'),
    (Name: 'Red'; Value: '#FFFF0000'),
    (Name: 'Rosybrown'; Value: '#FFBC8F8F'),
    (Name: 'Royalblue'; Value: '#FF4169E1'),
    (Name: 'Saddlebrown'; Value: '#FF8B4513'),
    (Name: 'Salmon'; Value: '#FFFA8072'),
    (Name: 'Sandybrown'; Value: '#FFF4A460'),
    (Name: 'Seagreen'; Value: '#FF2E8B57'),
    (Name: 'Seashell'; Value: '#FFFFF5EE'),
    (Name: 'Sienna'; Value: '#FFA0522D'),
    (Name: 'Silver'; Value: '#FFC0C0C0'),
    (Name: 'Skyblue'; Value: '#FF87CEEB'),
    (Name: 'Slateblue'; Value: '#FF6A5ACD'),
    (Name: 'Slategray'; Value: '#FF708090'),
    (Name: 'Slategrey'; Value: '#FF708090'),
    (Name: 'Snow'; Value: '#FFFFFAFA'),
    (Name: 'Springgreen'; Value: '#FF00FF7F'),
    (Name: 'Steelblue'; Value: '#FF4682B4'),
    (Name: 'Tan'; Value: '#FFD2B48C'),
    (Name: 'Teal'; Value: '#FF008080'),
    (Name: 'Thistle'; Value: '#FFD8BFD8'),
    (Name: 'Tomato'; Value: '#FFFF6347'),
    (Name: 'Turquoise'; Value: '#FF40E0D0'),
    (Name: 'Violet'; Value: '#FFEE82EE'),
    (Name: 'Wheat'; Value: '#FFF5DEB3'),
    (Name: 'White'; Value: '#FFFFFFFF'),
    (Name: 'Whitesmoke'; Value: '#FFF5F5F5'),
    (Name: 'Yellow'; Value: '#FFFFFF00'),
    (Name: 'Yellowgreen'; Value: '#FF9ACD32')
  );

type

  PIntArray = ^TIntArray;
  TIntArray = array [0..0] of integer;

  TvgPoint = packed record
    X: single;
    Y: single;
  end;

  TvgCubicBezier = array [0..3] of TvgPoint;

  PvgPointArray = ^TvgPointArray;
  TvgPointArray = array [0..0] of TvgPoint;

  TvgRect = packed record
    case Integer of
      0: (Left, Top, Right, Bottom: single);
      1: (TopLeft, BottomRight: TvgPoint);
  end;

  TvgCorner = (
    vgCornerTopLeft,
    vgCornerTopRight,
    vgCornerBottomLeft,
    vgCornerBottomRight
  );

  TvgCorners = set of TvgCorner;

  TvgCornerType = (
    vgCornerRound,
    vgCornerBevel,
    vgCornerInnerRound,
    vgCornerInnerLine
  );

  TvgVectorArray = array [0..2] of single;

  TvgVector = packed record
    case integer of
      0: (
        V: TvgVectorArray;
      );
      1: (
        X: single;
        Y: single;
        W: single;
      );
  end;

  TvgMatrixArray = array [0..2] of TvgVector;

  TvgMatrix = packed record
    case integer of
      0: (
        M: TvgMatrixArray;
      );
      1: (m11, m12, m13: single;
          m21, m22, m23: single;
          m31, m32, m33: single);
   end;

  TvgPolygon = array of TvgPoint;
  PvgPolygon = ^TvgPolygon;

  {$IFDEF FPC}
  TMessage = TLMessage;
  {$ENDIF}

  PvgColor = ^TvgColor;
  TvgColor = cardinal;

  PvgColorRec = ^TvgColorRec;
  TvgColorRec = packed record
    case longword of
      0: (Color: TvgColor);
      2: (HiWord, LoWord: Word);
      {$ifdef  FPC_BIG_ENDIAN}
      3: (A, R, G, B: System.Byte);
      {$else}
      3: (B, G, R, A: System.Byte);
      {$endif}
    end;

  PvgColorArray = ^TvgColorArray;
  TvgColorArray = array [0..0] of TvgColor;

  PvgColorRecArray = ^TvgColorRecArray;
  TvgColorRecArray = array [0..0] of TvgColorRec;

  PvgColor24 = ^TvgColor24;
  TvgColor24 = packed record
    case longword of
      0: (R, G, B: Byte);
    end;

  PvgColor24Array = ^TvgColor24Array;
  TvgColor24Array = array [0..0] of TvgColor24;

const
  cPI: Single =  3.141592654;
  cPIdiv180: Single =  0.017453292;
  c180divPI: Single = 57.29577951;
  c2PI: Single =  6.283185307;
  cPIdiv2: Single =  1.570796326;
  cPIdiv4: Single =  0.785398163;
  c3PIdiv4: Single =  2.35619449;
  cInv2PI: Single = 1/6.283185307;
  cInv360: Single = 1/360;
  c180: Single = 180;
  c360: Single = 360;
  cOneHalf: Single = 0.5;

  Epsilon: Single = 1e-40;
  IdentityMatrix: TvgMatrix = (m11:1.0;m12:0.0;m13:0.0;
                               m21:0.0;m22:1.0;m23:0.0;
                               m31:0.0;m32:0.0;m33:1.0);
  ZeroMatrix: TvgMatrix = (m11:0.0;m12:0.0;m13:0.0;
                           m21:0.0;m22:0.0;m23:0.0;
                           m31:0.0;m32:0.0;m33:0.0);

  AllCorners: TvgCorners = [
    vgCornerTopLeft,
    vgCornerTopRight,
    vgCornerBottomLeft,
    vgCornerBottomRight
  ];

  ClosePolygon: TvgPoint = (X: $FFFF; Y: $FFFF);

procedure vgSinCos(const Theta: single; var Sin, Cos: single);
function vgRadToDeg(const Degrees: single): single;
function vgDegToRad(const Degrees: single): single;
function vgNormalizeAngle(const angle: Single) : Single;
function vgPoint(X, Y: single): TvgPoint;
function vgScalePoint(P: TvgPoint; dx, dy: single): TvgPoint;
function vgRect(ALeft, ATop, ARight, ABottom: single): TvgRect;
function vgNormalizeRect(const Pts: array of TvgPoint): TvgRect;
function vgNormalizeRect2(const ARect: TvgRect): TvgRect;
function vgRectWidth(const R: TvgRect): single;
function vgRectHeight(const R: TvgRect): single;
function vgRectCenter(var R: TvgRect; Bounds: TvgRect): TvgRect;
function vgFitRect(var R: TvgRect; BoundsRect: TvgRect): single;
function vgIsRectEmpty(Rect: TvgRect): boolean;
procedure vgOffsetRect(var R: TvgRect; const Dx, Dy: single);
procedure vgMultiplyRect(var R: TvgRect; const Dx, Dy: single);
procedure vgInflateRect(var R: TvgRect; const Dx, Dy: single);
function vgIntersectRect(const Rect1, Rect2: TvgRect): boolean; overload;
function vgIntersectRect(var R: TvgRect; const Rect1, Rect2: TvgRect): boolean; overload;
function vgPtInRect(const P: TvgPoint; const Rect: TvgRect): boolean;
function vgUnionRect(const ARect1, ARect2: TvgRect): TvgRect;
function vgRectToString(R: TvgRect): Ansistring;
function vgStringToRect(S: Ansistring): TvgRect;

function vgPointFromVector(const v: TvgVector): TvgPoint;

function vgPointToString(R: TvgPoint): Ansistring;
function vgStringToPoint(S: Ansistring): TvgPoint;

function vgMatrixMultiply(const M1, M2: TvgMatrix): TvgMatrix;
function vgMatrixDeterminant(const M: TvgMatrix): single;
procedure vgAdjointMatrix(var M: TvgMatrix);
procedure vgScaleMatrix(var M: TvgMatrix; const factor: single);
procedure vgInvertMatrix(var M: TvgMatrix);
function vgVector(const x, y: Single; const w: single = 1.0) : TvgVector; overload;
function vgVector(const P: TvgPoint; const w: single = 1.0): TvgVector; overload;
function vgVectorTransform(const V: TvgVector; const M: TvgMatrix): TvgVector;
function vgCreateRotationMatrix(angle: single): TvgMatrix;


function vgVectorAdd(const v1: TvgVector; const v2: TvgVector): TvgVector;
function vgVectorSubtract(const v1: TvgVector; const v2: TvgVector): TvgVector;
function vgVectorNorm(const v : TvgVector) : Single;
function vgVectorNormalize(const v: TvgVector): TvgVector;
function vgVectorScale(const v: TvgVector; factor : Single): TvgVector;
function vgVectorLength(const v : TvgVector) : Single;
function vgVectorDotProduct(const V1, V2 : TvgVector): Single;
function vgVectorAngleCosine(const V1, V2: TvgVector): Single;
function vgVectorCrossProductZ(const V1, V2: TvgVector): single;
function vgVectorCombine2(const V1, V2: TvgVector; const F1, F2: Single): TvgVector;
function vgVectorReflect(const V, N: TvgVector): TvgVector;
function vgVectorAngle(const V, N: TvgVector): single;

function vgInterpolateSingle(const start, stop, t: single): single;
function vgInterpolateRotation(start, stop, t : Single) : Single;
function vgInterpolateColor(start, stop: TvgColor; t : single): TvgColor;

function vgAppendColor(start, stop: TvgColor): TvgColor;
function vgSubtractColor(start, stop: TvgColor): TvgColor;

function vgCorrectColor(const C: TvgColor): TvgColor;
function vgPremultyAlpha(const C: TvgColor): TvgColor;
function vgUnpremultyAlpha(const C: TvgColor): TvgColor;
function vgOpacity(const C: TvgColor; const AOpacity: single): TvgColor;
function vgColor(R, G, B: Byte; A: Byte = $FF): TvgColor;
function HSLtoRGB(H, S, L: Single): TvgColor;
procedure RGBtoHSL(RGB: TvgColor; out H, S, L: single);

function vgChangeHSL(const C: TvgColor; dH, dS, dL: single): TvgColor;

function vgColorToStr(Value: TvgColor): string;
function vgStrToColor(Value: string): TvgColor;

function vgFloatToStr(Value: single): string;
function vgStrToFloat(Value: string): single;

type

  TvgCanvas = class;
  TvgCustomScene = class;
  TvgObject = class;
  TvgVisualObject = class;
  TvgObjectClass = class of TvgObject;
  TvgResources = class;


  IvgScene = interface
    ['{16DB110E-DA7D-4e75-BC2D-999FA12E45F5}']
    procedure AddObject(AObject: TvgObject);
    procedure RemoveObject(AObject: TvgObject);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure EndUpdateWOInvalidate;
    procedure BeginDrag;
    procedure BeginResize;
    procedure AddUpdateRect(R: TvgRect);
    procedure InsertObject(const ClassName: string);
    function GetDisableUpdate: boolean;
    procedure SetDisableUpdate(Value: boolean);
    function GetDesignTime: boolean;
    function GetCanvas: TvgCanvas;
    function GetRoot: TvgObject;
    function GetOwner: TComponent;
    function GetComponent: TComponent;
    function GetStyle: TvgResources;
    function GetTransparency: boolean;
    procedure UpdateResource;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    function GetSelected: TvgVisualObject;
    function GetDesignPlaceObject: TvgVisualObject;
    function GetUpdateRectsCount: integer;
    function GetUpdateRect(const Index: integer): TvgRect;
    procedure SetCaptured(const Value: TvgVisualObject);
    function GetCaptured: TvgVisualObject;
    procedure SetFocused(const Value: TvgVisualObject);
    function GetFocused: TvgVisualObject;
    procedure SetDesignRoot(const Value: TvgVisualObject);
    function GetMousePos: TvgPoint;
    function ClientToScreen(const Point: TPoint): TPoint;
    procedure BeginVCLDrag(Source: TObject);
    procedure DoDesignSelect(AObject: TObject);
  end;

  TvgBounds = class(TPersistent)
  private
    FRight: single;
    FBottom: single;
    FTop: single;
    FLeft: single;
    FOnChange: TNotifyEvent;
    FDefaultValue: TvgRect;
    function GetRect: TvgRect;
    procedure SetRect(const Value: TvgRect);
    procedure SetBottom(const Value: single);
    procedure SetLeft(const Value: single);
    procedure SetRight(const Value: single);
    procedure SetTop(const Value: single);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadRect(Reader: TReader);
    procedure WriteRect(Writer: TWriter);
  public
    constructor Create(const ADefaultValue: TvgRect); virtual;
    procedure Assign(Source: TPersistent); override;
    function MarginRect(const R: TvgRect): TvgRect;
    function PaddinRect(const R: TvgRect): TvgRect;
    function Width: single;
    function Height: single;
    property Rect: TvgRect read GetRect write SetRect;
    property DefaultValue: TvgRect read FDefaultValue write FDefaultValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    function empty: boolean;
    function marginEmpty: boolean;
    property left: single read FLeft write SetLeft stored false;
    property top: single read FTop write SetTop stored false;
    property right: single read FRight write SetRight stored false;
    property bottom: single read FBottom write SetBottom stored false;
  end;

  TvgPosition = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FY: single;
    FX: single;
    FDefaultValue: TvgPoint;
    procedure SetPoint(const Value: TvgPoint);
    procedure SetX(const Value: single);
    procedure SetY(const Value: single);
    function GetPoint: TvgPoint;
    function GetVector: TvgVector;
    procedure SetVector(const Value: TvgVector);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPoint(Reader: TReader);
    procedure WritePoint(Writer: TWriter);
  public
    constructor Create(const ADefaultValue: TvgPoint); virtual;
    procedure Assign(Source: TPersistent); override;
    function Empty: boolean;
    procedure Reflect(const Normal: TvgVector);
    property Point: TvgPoint read GetPoint write SetPoint;
    property Vector: TvgVector read GetVector write SetVector;
    property DefaultValue: TvgPoint read FDefaultValue write FDefaultValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property X: single read FX write SetX stored false;
    property Y: single read FY write SetY stored false;
  end;

  TvgTransform = class(TPersistent)
  private
    FMatrix: TvgMatrix;
    FRotateAngle: single;
    FPosition: TvgPosition;
    FScale: TvgPosition;
    FSkew: TvgPosition;
    FRotateCenter: TvgPosition;
    FOnChanged: TNotifyEvent;
    procedure SetRotateAngle(const Value: single);
  protected
    procedure MatrixChanged(Sender: TObject);
    property Skew: TvgPosition read FSkew write FSkew;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Matrix: TvgMatrix read FMatrix;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Position: TvgPosition read FPosition write FPosition;
    property Scale: TvgPosition read FScale write FScale;
    property RotateAngle: single read FRotateAngle write SetRotateAngle;
    property RotateCenter: TvgPosition read FRotateCenter write FRotateCenter;
  end;

  TvgGradientPoint = class(TCollectionItem)
  private
    FColor: TvgColor;
    FOffset: single;
    function GetColor: string;
    procedure SetColor(const Value: string);
  protected
  public
    {$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
    {$ELSE}
    constructor Create(Collection: TCollection); override;
    {$ENDIF}
    procedure Assign(Source: TPersistent); override;
    property IntColor: TvgColor read FColor write FColor;
  published
    property Color: string read GetColor write SetColor;
    property Offset: single read FOffset write FOffset;
  end;

  TvgGradientPoints = class(TCollection)
  private
    function GetPoint(Index: integer): TvgGradientPoint;
  public
    property Points[Index: integer]: TvgGradientPoint read GetPoint; default;
  end;

  TvgGradientStyle = (
    vgLinearGradient,
    vgRadialGradient
  );

  TvgGradient = class(TPersistent)
  private
    FPoints: TvgGradientPoints;
    FOnChanged: TNotifyEvent;
    FStartPosition: TvgPosition;
    FStopPosition: TvgPosition;
    FStyle: TvgGradientStyle;
    FRadialTransform: TvgTransform;
    procedure SetStartPosition(const Value: TvgPosition);
    procedure SetStopPosition(const Value: TvgPosition);
    procedure PositionChanged(Sender: TObject);
    procedure SetColor(const Value: string);
    procedure SetColor1(const Value: string);
    function isLinearStored: Boolean;
    procedure SetStyle(const Value: TvgGradientStyle);
    function isRadialStored: Boolean;
    procedure SetRadialTransform(const Value: TvgTransform);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Change;
    function InterpolateColor(Offset: single): TvgColor;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property Color: string write SetColor;
    property Color1: string write SetColor1;
  published
    property Points: TvgGradientPoints read FPoints write FPoints;
    property Style: TvgGradientStyle read FStyle write SetStyle;
    { linear }
    property StartPosition: TvgPosition read FStartPosition write SetStartPosition stored isLinearStored;
    property StopPosition: TvgPosition read FStopPosition write SetStopPosition stored isLinearStored;
    { radial }
    property RadialTransform: TvgTransform read FRadialTransform write SetRadialTransform stored isRadialStored;
  end;

  TvgVisual = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FVisualObject: TvgVisualObject;
    procedure SetVisualObject(const Value: TvgVisualObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property VisualObject: TvgVisualObject read FVisualObject write SetVisualObject;
  end;

  TvgBrush = class;
  TvgBrushObject = class;

  TvgBrushResource = class(TPersistent)
  private
    FResource: TvgBrushObject;
    FOnChanged: TNotifyEvent;
    function GetBrush: TvgBrush;
    procedure SetResource(const Value: TvgBrushObject);
  public
    destructor Destroy; override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Assign(Source: TPersistent); override;
    property Brush: TvgBrush read GetBrush;
  published
    property Resource: TvgBrushObject read FResource write SetResource;
  end;

  TvgBitmap = class;

  TvgWrapMode = (
    vgWrapTile,
    vgWrapTileOriginal,
    vgWrapTileStretch
  );

  TvgBrushBitmap = class(TPersistent)
  private
    FOnChanged: TNotifyEvent;
    FBitmap: TvgBitmap;
    FWrapMode: TvgWrapMode;
    procedure SetWrapMode(const Value: TvgWrapMode);
  public
    constructor Create;
    destructor Destroy; override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure Assign(Source: TPersistent); override;
  published
    property Bitmap: TvgBitmap read FBitmap write FBitmap;
    property WrapMode: TvgWrapMode read FWrapMode write SetWrapMode;
  end;

  TvgBrushStyle = (
    vgBrushNone,
    vgBrushSolid,
    vgBrushGradient,
    vgBrushBitmap,
    vgBrushResource,
    vgBrushVisual
  );

  TvgBrush = class(TPersistent)
  private
    FColor: TvgColor;
    FStyle: TvgBrushStyle;
    FOnChanged: TNotifyEvent;
    FGradient: TvgGradient;
    FVisual: TvgVisual;
    FDefaultStyle: TvgBrushStyle;
    FDefaultColor: TvgColor;
    FResource: TvgBrushResource;
    FBitmap: TvgBrushBitmap;
    procedure SetColor(const Value: string);
    procedure SetStyle(const Value: TvgBrushStyle);
    procedure SetGradient(const Value: TvgGradient);
    procedure SetVisual(const Value: TvgVisual);
    function isColorStored: Boolean;
    function isGradientStored: Boolean;
    function isVisualStored: Boolean;
    function GetColor: string;
    procedure SetSolidColor(const Value: TvgColor);
    function isStyleStored: Boolean;
    procedure SetResource(const Value: TvgBrushResource);
    function isResourceStored: Boolean;
    function isBitmapStored: Boolean;
    function GetSolidColor: TvgColor;
  protected
    procedure GradientChanged(Sender: TObject);
    procedure VisualChanged(Sender: TObject);
    procedure ResourceChanged(Sender: TObject);
    procedure BitmapChanged(Sender: TObject);
  public
    constructor Create(const ADefaultStyle: TvgBrushStyle; const ADefaultColor: TvgColor);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property SolidColor: TvgColor read GetSolidColor write SetSolidColor;
    property DefaultColor: TvgColor read FDefaultColor write FDefaultColor;
    property DefaultStyle: TvgBrushStyle read FDefaultStyle write FDefaultStyle;
  published
    property Color: string read GetColor write SetColor stored isColorStored; // common
    property Bitmap: TvgBrushBitmap read FBitmap write FBitmap stored isBitmapStored;
    property Style: TvgBrushStyle read FStyle write SetStyle stored isStyleStored;
    property Gradient: TvgGradient read FGradient write SetGradient stored isGradientStored;
    property Resource: TvgBrushResource read FResource write SetResource stored isResourceStored;
    property Visual: TvgVisual read FVisual write SetVisual stored isVisualStored;
  end;

  TvgFontStyle = (
    vgFontRegular,
    vgFontBold,
    vgFontItalic,
    vgFontBoldItalic,
    vgFontUnderline,
    vgFontStrikeout
  );

  TvgFont = class(TPersistent)
  private
    FSize: single;
    FFamily: string;
    FStyle: TvgFontStyle;
    FOnChanged: TNotifyEvent;
    procedure SetFamily(const Value: string);
    procedure SetSize(const Value: single);
    procedure SetStyle(const Value: TvgFontStyle);
    function isFamilyStored: Boolean;
    function isSizeStored: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property Family: string read FFamily write SetFamily stored isFamilyStored;
    property Size: single read FSize write SetSize stored isSizeStored;
    property Style: TvgFontStyle read FStyle write SetStyle;
  end;

  TvgTextAlign = (
    vgTextAlignCenter,
    vgTextAlignNear,
    vgTextAlignFar
  );

  TvgFilter = class(TPersistent)
  private
  public
  published
    class function GetFileTypes: string; virtual;
    class function GetImageSize(const AFileName: string): TvgPoint; virtual;
    function LoadFromFile(const AFileName: string; const Rotate: single; var Bitmap: TvgBitmap): boolean; virtual; abstract;
    function LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: single; const UseEmbedded: boolean;
      var Bitmap: TvgBitmap): boolean; virtual; abstract;
    function SaveToFile(const AFileName: string; var Bitmap: TvgBitmap; const Params: string = ''): boolean; virtual; abstract;
    function LoadFromStream(const AStream: TStream; var Bitmap: TvgBitmap): boolean; virtual; abstract;
    { Format is a string from "jpeg,png,bmp" }
    function SaveToStream(const AStream: TStream; var Bitmap: TvgBitmap; const Format: string;
      const Params: string = ''): boolean; virtual; abstract;
  end;
  TvgFilterClass = class of TvgFilter;

  { TvgBitmap }

  TvgBitmap = class(TPersistent)
  private
    FBits: PvgColorArray;
    FCanvas: TvgCanvas;
    FHandle: cardinal;
    FHeight: integer;
    FOnChange: TNotifyEvent;
    FWidth: integer;
    FNeedUpdate: boolean;
    FOnDestroyHandle: TNotifyEvent;
    FOnThreadLoaded: TNotifyEvent;
    FOnBitmapCreate: TNotifyEvent;
    FOnBitmapDestroy: TNotifyEvent;
    function GetCanvas: TvgCanvas;
    function GetScanline(y: integer): PvgColorArray;
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
    function GetPixels(x, y: integer): TvgColor;
  protected
    { internal }
    procedure Recreate;
    procedure DoLoaded(Sender: TObject);
    { vcl }
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadBitmap(Stream: TStream);
    procedure WriteBitmap(Stream: TStream);
  public
    constructor Create(const AWidth, AHeight: integer; const APremulAlpha: boolean = true); virtual;
    constructor CreateFromStream(const AStream: TStream); virtual;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    procedure SetSize(const AWidth, AHeight: integer);
    procedure Clear(const AColor: TvgColor = 0);
    procedure ClearRect(const ARect: TvgRect; const AColor: TvgColor = 0);
    procedure BitmapChanged;
    { vcl }
    procedure DrawGraphic(const Graphic: TGraphic; const DstRect: TRect);
    { Manipulation }
    procedure Rotate(const Angle: single);
    procedure FlipHorizontal;
    procedure FlipVertical;
    procedure InvertAlpha;
    procedure FillColor(const Color: TvgColor);
    { Mask }
    function CreateMask: PByteArray;
    procedure ApplyMask(const Mask: PByteArray; const DstX: integer = 0; const DstY: integer = 0);
    { Thumb }
    function CreateThumbnail(const Width, Height: integer): TvgBitmap;
    { I/O }
    procedure LoadFromFile(const AFileName: string; const Rotate: single = 0);
    procedure LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: single;
      const UseEmbedded: boolean = true);
    procedure SaveToFile(const AFileName: string; const Params: string = '');
    procedure LoadFromStream(const AStream: TStream);
    procedure SaveToStream(const AStream: TStream);
    { }
    property Canvas: TvgCanvas read GetCanvas;
    property Pixels[x, y: integer]: TvgColor read GetPixels;
    property Scanline[y: integer]: PvgColorArray read GetScanline;
    property StartLine: PvgColorArray read FBits;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    { internal usage only }
    property Handle: cardinal read FHandle write FHandle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDestroyHandle: TNotifyEvent read FOnDestroyHandle write FOnDestroyHandle;
    property OnThreadLoaded: TNotifyEvent read FOnThreadLoaded write FOnThreadLoaded;
    property OnBitmapCreate: TNotifyEvent read FOnBitmapCreate write FOnBitmapCreate;
    property OnBitmapDestroy: TNotifyEvent read FOnBitmapDestroy write FOnBitmapDestroy;
    property NeedUpdate: boolean read FNeedUpdate write FNeedUpdate;
  published
  end;

  TvgPathPointKind = (
    vgPathPointMoveTo,
    vgPathPointLineTo,
    vgPathPointCurveTo,
    vgPathPointClose
  );

  TvgPathPoint = packed record
    Kind: TvgPathPointKind;
    Point: TvgPoint;
  end;

  TvgPathData = class(TPersistent)
  private
    FHandle: cardinal;
    FOnDestroyHandle: TNotifyEvent;
    FOnChanged: TNotifyEvent;
    function GetPathString: Ansistring;
    procedure SetPathString(const Value: Ansistring);
    procedure AddArcSvgPart(const Center, Radius: TvgPoint; StartAngle, SweepAngle: single);
    procedure AddArcSvg(const P1, Radius: TvgPoint; Angle: single; const LargeFlag, SweepFlag: boolean; const P2: TvgPoint);
  protected
    { vcl }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPath(Stream: TStream);
    procedure WritePath(Stream: TStream);
  public
    PathData: array of TvgPathPoint;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    { creation }
    function LastPoint: TvgPoint;
    procedure MoveTo(const P: TvgPoint);
    procedure MoveToRel(const P: TvgPoint);
    procedure LineTo(const P: TvgPoint);
    procedure LineToRel(const P: TvgPoint);
    procedure HLineTo(const x: single);
    procedure HLineToRel(const x: single);
    procedure VLineTo(const y: single);
    procedure VLineToRel(const y: single);
    procedure CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TvgPoint);
    procedure CurveToRel(const ControlPoint1, ControlPoint2, EndPoint: TvgPoint);
    procedure SmoothCurveTo(const ControlPoint2, EndPoint: TvgPoint);
    procedure SmoothCurveToRel(const ControlPoint2, EndPoint: TvgPoint);
    procedure ClosePath;
    { shapes }
    procedure AddEllipse(const ARect: TvgRect);
    procedure AddRectangle(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners;
      const ACornerType: TvgCornerType = vgCornerRound);
    procedure AddArc(const Center, Radius: TvgPoint; StartAngle, SweepAngle: single);
    { modification }
    procedure Clear;
    procedure Flatten(const Flatness: single = 0.25);
    procedure Scale(const scaleX, scaleY: single);
    procedure Offset(const dX, dY: single);
    procedure ApplyMatrix(const M: TvgMatrix);
    { params }
    function GetBounds: TvgRect;
    { convert }
    function FlattenToPolygon(var Polygon: TvgPolygon; const Flatness: single = 0.25): TvgPoint;
    function IsEmpty: boolean;
    { internal usage only }
    property Handle: cardinal read FHandle write FHandle;
    property OnDestroyHandle: TNotifyEvent read FOnDestroyHandle write FOnDestroyHandle;
  published
    property Data: AnsiString read GetPathString write SetPathString stored false;
  end;

  TvgStrokeCap = (
    vgCapFlat,
    vgCapRound
  );

  TvgStrokeJoin = (
    vgJoinMiter,
    vgJoinRound,
    vgJoinBevel
  );

  TvgStrokeDash = (
    vgDashSolid,
    vgDashDash,
    vgDashDot,
    vgDashDashDot,
    vgDashDashDotDot,
    vgDashCustom
  );

  TvgTextRendering = (
    vgAntiAlias,
    vgLowQuality,
    vgClearType
  );

  TvgSaveData = record
    Index: cardinal;
    Matrix: TvgMatrix;
    AbsoluteMatrix, InvertMatrix: TvgMatrix;
    Fill: TvgBrush;
    Stroke: TvgBrush;
    StrokeThickness: single;
    StrokeCap: TvgStrokeCap;
    StrokeJoin: TvgStrokeJoin;
    StrokeDash: TvgStrokeDash;
    Dash: array of single;
    DashOffset: single;
    Font: TvgFont;
  end;

  TvgSaveDataArray = array of TvgSaveData;

  TvgCanvas = class(TPersistent)
  private
  protected
    FWidth, FHeight: integer;
    FMatrix: TvgMatrix;
    FFill: TvgBrush;
    FStroke: TvgBrush;
    FStrokeThickness: single;
    FStrokeCap: TvgStrokeCap;
    FStrokeJoin: TvgStrokeJoin;
    FStrokeDash: TvgStrokeDash;
    FDash: array of single;
    FDashOffset: single;
    FFont: TvgFont;
    FBitmap: TvgBitmap;
    FResized: boolean;
    FSaveData: TvgSaveDataArray;
    procedure FontChanged(Sender: TObject); virtual;
    procedure SetStrokeDash(const Value: TvgStrokeDash);
  public
    {$IFDEF LINUX}
    cr: Pcairo_t;
    sr: Pcairo_surface_t;
    widget: PGtkWidget;
    {$ENDIF}
    {$IFDEF WIN32}
    FBufferDC: Cardinal;
    FBufferHandle: cardinal;
    FBitmapInfo: TBitmapInfo;
    SceneDC, SceneWnd: cardinal;
    {$ENDIF}
    {$IFDEF DARWIN}
    SceneCtx, CtxRef: CGContextRef;
    {$ENDIF}
    FBuffered: boolean;
    FBufferBits: Pointer;
    constructor Create(const AWidth, AHeight: integer); virtual;
    constructor CreateFromBitmap(const ABitmap: TvgBitmap); virtual;
    destructor Destroy; override;
    { scene }
    function BeginScene: boolean; virtual;
    procedure EndScene; virtual;
    { buffer }
    procedure FlushBuffer(const X, Y: integer; const DC: Cardinal); virtual;
    procedure FlushBufferRect(const X, Y: integer; const DC: Cardinal; const ARect: TvgRect); virtual;
    procedure FreeBuffer; virtual;
    procedure ResizeBuffer(const AWidth, AHeight: integer); virtual;
    class function GetBitmapScanline(Bitmap: TvgBitmap; y: integer): PvgColorArray; virtual;
    { i/o }
    procedure SaveToStream(S: TStream);
    procedure SaveToBits(Bits: Pointer);
    { matrix }
    procedure SetMatrix(const M: TvgMatrix); virtual;
    procedure MultyMatrix(const M: TvgMatrix); virtual;
    { cliping }
    function SaveCanvas: cardinal; virtual; abstract;
    procedure RestoreCanvas(const AState: cardinal); virtual; abstract;
    procedure SetClipRects(const ARects: array of TvgRect); virtual; abstract;
    procedure IntersectClipRect(const ARect: TvgRect); virtual; abstract;
    procedure ExcludeClipRect(const ARect: TvgRect); virtual; abstract;
    procedure ResetClipRect; virtual; abstract;
    { drawing }
    procedure Clear(const Color: cardinal);
    procedure ClearRect(const ARect: TvgRect; const AColor: TvgColor = 0);
    procedure DrawLine(const APt1, APt2: TvgPoint; const AOpacity: single); virtual; abstract;
    procedure FillRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
      const ACornerType: TvgCornerType = vgCornerRound); virtual; abstract;
    procedure DrawRect(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners; const AOpacity: single;
      const ACornerType: TvgCornerType = vgCornerRound); virtual; abstract;
    procedure FillEllipse(const ARect: TvgRect; const AOpacity: single); virtual; abstract;
    procedure DrawEllipse(const ARect: TvgRect; const AOpacity: single); virtual; abstract;
    procedure DrawArc(const Center, Radius: TvgPoint; StartAngle, SweepAngle: single; const AOpacity: single);
    function PtInPath(const APoint: TvgPoint; const ARect: TvgRect; const APath: TvgPathData): boolean; virtual; abstract;
    procedure FillPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single); virtual; abstract;
    procedure DrawPath(const APath: TvgPathData; const ARect: TvgRect; const AOpacity: single); virtual; abstract;
    procedure DrawBitmap(const ABitmap: TvgBitmap; const SrcRect, DstRect: TvgRect; const AOpacity: single;
      const HighSpeed: boolean = false); virtual; abstract;
    procedure DrawThumbnail(const ABitmap: TvgBitmap; const Width, Height: single); virtual; abstract;
    { linear polygon }
    procedure FillPolygon(const Points: TvgPolygon; const AOpacity: single); virtual;
    procedure DrawPolygon(const Points: TvgPolygon; const AOpacity: single); virtual;
    { text }
    procedure FillText(const ARect, AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean;
      const AOpacity: single; const ATextAlign: TvgTextAlign; const AVTextAlign: TvgTextAlign = vgTextAlignCenter); virtual; abstract;
    procedure MeasureText(var ARect: TvgRect; AClipRect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter); virtual; abstract;
    function TextToPath(Path: TvgPathData; const ARect: TvgRect; const AText: WideString; const WordWrap: boolean; const ATextAlign: TvgTextAlign;
      const AVTextAlign: TvgTextAlign = vgTextAlignCenter): boolean; virtual; abstract;
    function TextWidth(const AText: WideString): single;
    function TextHeight(const AText: WideString): single;
    procedure SetTextRendering(const ATextRendering: TvgTextRendering); virtual; abstract;
    { dash and cap  }
    procedure SetCustomDash(Dash: array of single; Offset: single);
    { properties }
    property Stroke: TvgBrush read FStroke;
    property StrokeThickness: single read FStrokeThickness write FStrokeThickness;
    property StrokeCap: TvgStrokeCap read FStrokeCap write FStrokeCap;
    property StrokeDash: TvgStrokeDash read FStrokeDash write SetStrokeDash;
    property StrokeJoin: TvgStrokeJoin read FStrokeJoin write FStrokeJoin;
    property Fill: TvgBrush read FFill;
    property Font: TvgFont read FFont;
    { usage in PaintTo }
    property Matrix: TvgMatrix read FMatrix;
    { read only }
    property Width: integer read FWidth;
    property Height: integer read FHeight;
  published
  end;
  TvgCanvasClass = class of TvgCanvas;

  TvgMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: single) of object;
  TvgMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y, Dx, Dy: single) of object;
  TvgMouseWheelEvent = procedure(Sender: TObject; Shift: TShiftState;
    WheelDelta: Integer; MousePos: TvgPoint; var Handled: Boolean) of object;
  TvgKeyEvent = procedure (var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState) of object;

  TvgProcessTickEvent = procedure(Sender: TObject; time, deltaTime: single) of object;

  TvgAnimationType = (
    vgAnimationIn,
    vgAnimationOut,
    vgAnimationInOut
  );

  TvgInterpolationType = (
    vgInterpolationLinear,
    vgInterpolationQuadratic,
    vgInterpolationCubic,
    vgInterpolationQuartic,
    vgInterpolationQuintic,   
    vgInterpolationSinusoidal,
    vgInterpolationExponential,
    vgInterpolationCircular,
    vgInterpolationElastic,
    vgInterpolationBack,
    vgInterpolationBounce
  );

  TvgObject = class(TComponent)
  private
    FStored: boolean;
    FResourceName: string;
    FRecalcHasEffect, FHasEffect: boolean;
    FIsVisual: boolean;
    FNotifyList: TList;
    FTagObject: TObject;
    FTagFloat: single;
    FTagString: string;
    FBindingName: string;
    function GetScene: IvgScene;
    procedure ReaderSetName(Reader: TReader; Component: TComponent;
      var Name: string);
    procedure ReaderError(Reader: TReader; const Message: string;
      var Handled: Boolean);
    procedure SetResourceName(const Value: string);
    procedure SetStored(const Value: boolean);
    function GetChild(Index: integer): TvgObject;
    function GetChildrenCount: integer;
    function GetVisual: TvgVisualObject;
    procedure SetBindingName(const Value: string);
  protected
    FChildren: TvgObjectList;
    FParent: TvgObject;
    FScene: IvgScene;
    FDisableEffect: boolean;
    procedure SetNewScene(AScene: IvgScene);
    procedure UpdateChildScene;
    { VCL }
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetParentComponent(Value: TComponent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    { }
    procedure ChangeParent; virtual;
    procedure SetParent(const Value: TvgObject); virtual;
    function HasClipParent: TvgVisualObject;
    function HasEffect: boolean;
    function HasDisablePaintEffect: boolean;
    function HasAfterPaintEffect: boolean;
    procedure FreeNotify(AObject: TvgObject); virtual;
    procedure LoadFromBinStream(const AStream: TStream);
    procedure SaveToBinStream(const AStream: TStream);
    { binding }
    function GetBinding(Index: string): Variant; virtual;
    procedure SetBinding(Index: string; const Value: Variant); virtual;
    function GetData: Variant; virtual;
    procedure SetData(const Value: Variant); virtual;
    { ani }
    procedure DoAniFinished(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { design }
    function ItemClass: string; virtual;
    { clone }
    function Clone(const AOwner: TComponent): TvgObject;
    procedure CloneChildFromStream(AStream: TStream);
    { childs  }
    procedure AddObject(AObject: TvgObject); virtual;
    procedure RemoveObject(AObject: TvgObject); virtual;
    procedure DeleteChildren; virtual;
    procedure BringToFront;
    procedure SendToBack;
    procedure AddObjectsToList(const AList: TList);
    { notify }
    procedure AddFreeNotify(const AObject: TObject);
    procedure RemoveFreeNotify(const AObject: TObject);
    { i/o }
    procedure LoadFromStream(const AStream: TStream);
    procedure SaveToStream(const Stream: TStream);
    { resoruce }
    function FindResource(const AResource: string): TvgObject;
    procedure UpdateResource; virtual;
    { animations }
    procedure StartAnimation(const AName: WideString); virtual;
    procedure StopAnimation(const AName: WideString); virtual;
    procedure StartTriggerAnimation(AInstance: TvgObject; ATrigger: string); virtual;
    procedure StartTriggerAnimationWait(AInstance: TvgObject; ATrigger: string); virtual;
    procedure StopTriggerAnimation(AInstance: TvgObject); virtual;
    procedure ApplyTriggerEffect(AInstance: TvgObject; ATrigger: string); virtual;
    { animation property }
    procedure AnimateFloat(const APropertyName: string; const NewValue: single; Duration: single = 0.2;
      AType: TvgAnimationType = vgAnimationIn; AInterpolation: TvgInterpolationType = vgInterpolationLinear);
    procedure AnimateColor(const APropertyName: string; const NewValue: string; Duration: single = 0.2;
      AType: TvgAnimationType = vgAnimationIn; AInterpolation: TvgInterpolationType = vgInterpolationLinear);
    procedure AnimateFloatWait(const APropertyName: string; const NewValue: single; Duration: single = 0.2;
      AType: TvgAnimationType = vgAnimationIn; AInterpolation: TvgInterpolationType = vgInterpolationLinear);
    { }
    property IsVisual: boolean read FIsVisual;
    property Visual: TvgVisualObject read GetVisual;
    property Scene: IvgScene read FScene;
    property Stored: boolean read FStored write SetStored;
    { }
    property TagObject: TObject read FTagObject write FTagObject;
    property TagFloat: single read FTagFloat write FTagFloat;
    property TagString: string read FTagString write FTagString;
    { children }
    property ChildrenCount: integer read GetChildrenCount;
    property Children[Index: integer]: TvgObject read GetChild;
    { binding }
    function FindBinding(const ABinding: string): TvgObject;
    property Data: Variant read GetData write SetData;
    property Binding[Index: string]: Variant read GetBinding write SetBinding;
  published
    property Parent: TvgObject read FParent write SetParent stored false;
    property BindingName: string read FBindingName write SetBindingName;
    property ResourceName: string read FResourceName write SetResourceName;
  end;

  TvgAnimation = class(TvgObject)
  private
    FDuration: single;
    FTime: single;
    FInverse: boolean;
    FTrigger: string;
    FLoop: boolean;
    FPause: boolean;
    FRunning: boolean;
    FOnFinish: TNotifyEvent;
    FHideOnFinish: boolean;
    FInterpolation: TvgInterpolationType;
    FAnimationType: TvgAnimationType;
    FEnabled: boolean;
    FAutoReverse: boolean;
    procedure SetEnabled(const Value: boolean);
  protected
    function NormalizedTime: single;
    procedure ProcessAnimation; virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure StartTrigger(AInstance: TvgObject; ATrigger: string); virtual;
    procedure ProcessTick(time, deltaTime: single);
    property Running: boolean read FRunning;
    property Pause: boolean read FPause write FPause;
  published
    property AnimationType: TvgAnimationType read FAnimationType write FAnimationType default vgAnimationIn;
    property AutoReverse: boolean read FAutoReverse write FAutoReverse default false;
    property Enabled: boolean read FEnabled write SetEnabled default false;
    property Duration: single read FDuration write FDuration;
    property Interpolation: TvgInterpolationType read FInterpolation write FInterpolation default vgInterpolationLinear;
    property Inverse: boolean read FInverse write FInverse;
    property HideOnFinish: boolean read FHideOnFinish write FHideOnFinish;
    property Loop: boolean read FLoop write FLoop default false;
    property Trigger: string read FTrigger write FTrigger;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

  TvgEffect = class(TvgObject)
  private
    FEnabled: boolean;
    FTrigger: string;
    procedure SetEnabled(const Value: boolean);
  protected
    DisablePaint: boolean;
    AfterPaint: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRect(const ARect: TvgRect): TvgRect; virtual;
    function GetOffset: TvgPoint; virtual;
    procedure ProcessEffect(Canvas: TvgCanvas; const Visual: TvgBitmap; const Data: single); virtual;
    procedure ApplyTrigger(AInstance: TvgObject; ATrigger: string); virtual;
    procedure UpdateParentEffects;
    property GetDisablePaint: boolean read DisablePaint;
  published
    property Trigger: string read FTrigger write FTrigger;
    property Enabled: boolean read FEnabled write SetEnabled default true;
  end;

  TvgAlign = (
    vaNone,
    vaTopLeft,
    vaTopRight,
    vaBottomLeft,
    vaBottomRight,
    vaTop,
    vaLeft,
    vaRight,
    vaBottom,
    vaMostLeft,
    vaMostRight,
    vaClient,
    vaContents,
    vaCenter,
    vaVertCenter,
    vaHorzCenter,
    vaHorizontal,
    vaVertical,
    vaScale,
    vaFit
  );

  TOnPaintEvent = procedure (Sender: TObject; const Canvas: TvgCanvas; const ARect: TvgRect) of object;

  TvgDragMode = (
    vgDragManual,
    vgDragAutomatic
  );

  TvgDragObject = record
    Source: TObject;
    Files: array of WideString;
    Data: Variant;
  end;

  TvgDragEnterEvent = procedure(Sender: TObject; const Data: TvgDragObject; const Point: TvgPoint) of object;
  TvgDragOverEvent = procedure(Sender: TObject; const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean) of object;
  TvgDragDropEvent = procedure(Sender: TObject; const Data: TvgDragObject; const Point: TvgPoint) of object;
  TvgDragLeaveEvent = procedure(Sender: TObject) of object;

  { TvgVisualObject }

  TvgVisualObject = class(TvgObject)
  private
    FOnMouseUp: TvgMouseEvent;
    FOnMouseDown: TvgMouseEvent;
    FOnMouseMove: TvgMouseMoveEvent;
    FOnMouseWheel: TvgMouseWheelEvent;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FMouseInObject: boolean;
    FHitTest: boolean;
    FClipChildren: boolean;
    FAutoCapture: boolean;
    FMargins: TvgBounds;
    FAlign: TvgAlign;
    FPadding: TvgBounds;
    FTempCanvas: TvgCanvas;
    FRotateAngle: single;
    FPosition: TvgPosition;
    FScale: TvgPosition;
    FSkew: TvgPosition;
    FRotateCenter: TvgPosition;
    FCanFocused: boolean;
    FIsMouseOver: boolean;
    FIsFocused: boolean;
    FClipParent: boolean;
    FVelocity: TvgPosition;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FDesignHide: boolean;
    FOnPaint: TOnPaintEvent;
    FCanClipped: boolean;
    FCursor: TCursor;
    FDragMode: TvgDragMode;
    FDragDisableHighlight: boolean;
    FOnDragEnter: TvgDragEnterEvent;
    FOnDragDrop: TvgDragDropEvent;
    FOnDragLeave: TvgDragLeaveEvent;
    FOnDragOver: TvgDragOverEvent;
    FIsDragOver: boolean;
    FOnKeyDown: TvgKeyEvent;
    FOnKeyUp: TvgKeyEvent;
    FHint: WideString;
    FShowHint: boolean;
    FPopupMenu: TPopupMenu;
    FPressed, FDoubleClick: boolean;
    FRecalcEnabled, FEnabled, FAbsoluteEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
    function GetInvertAbsoluteMatrix: TvgMatrix;
    procedure SetVisible(const Value: boolean);
    procedure SetRotateAngle(const Value: single);
    procedure SetHitTest(const Value: boolean);
    procedure SetClipChildren(const Value: boolean);
    function CheckHitTest(const AHitTest: boolean): boolean;
    procedure SetAlign(const Value: TvgAlign);
    function GetCanvas: TvgCanvas;
    procedure SetLocked(const Value: boolean);
    procedure SetTempCanvas(const Value: TvgCanvas);
    procedure SetOpacity(const Value: single);
    procedure SetDesignHide(const Value: boolean);
    procedure UpdateDesignHide(const Value: boolean);
    function isOpacityStored: Boolean;
    function GetChildrenRect: TvgRect;
    procedure SetCursor(const Value: TCursor);
    function GetAbsoluteWidth: single;
    function GetAbsoluteHeight: single;
  protected
    FHeight, FLastHeight: single;
    FWidth, FLastWidth: single;
    FVisible: boolean;
    FLocalMatrix: TvgMatrix;
    FAbsoluteMatrix: TvgMatrix;
    FRecalcAbsolute: boolean;
    FDisableAlign: boolean;
    FUpdateEffects: boolean;
    FEffectBitmap: TvgBitmap;
    FLocked: boolean;
    FOpacity, FAbsoluteOpacity: single;
    FRecalcOpacity: boolean;
    FInPaintTo: boolean;
    FUpdateRect: TvgRect;
    FRecalcUpdateRect: boolean;
    procedure SetInPaintTo(value: boolean);
    {}
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    function CheckParentVisible: boolean;
    { matrix }
    procedure SetHeight(const Value: single); virtual;
    procedure SetWidth(const Value: single); virtual;
    function GetAbsoluteRect: TvgRect; virtual;
    function GetAbsoluteMatrix: TvgMatrix; virtual;
    function GetChildrenMatrix: TvgMatrix; virtual;
    function GetAbsoluteScale: TvgPoint; virtual;
    function GetLocalRect: TvgRect; virtual;
    function GetUpdateRect: TvgRect; virtual;
    procedure RecalcUpdateRect;
    function GetBoundsRect: TvgRect; virtual;
    function GetParentedRect: TvgRect; virtual;
    function GetClipRect: TvgRect; virtual;
    function GetEffectsRect: TvgRect; virtual;
    function GetAbsoluteEnabled: boolean; virtual;
    procedure SetBoundsRect(const Value: TvgRect); virtual;
    procedure RecalcAbsoluteNow;
    { opacity }
    function GetAbsoluteOpacity: single; virtual;
    { design }
    procedure DesignSelect; virtual;
    procedure DesignClick; virtual;
    procedure DesignInsert; virtual;
    { events }
    procedure Capture;
    procedure ReleaseCapture;
    procedure Click; virtual;
    procedure DblClick; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: single); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: single); virtual;
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); virtual;
    procedure KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); virtual;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); virtual;
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure ContextMenu(const ScreenPosition: TvgPoint); virtual;
    procedure DragEnter(const Data: TvgDragObject; const Point: TvgPoint); virtual;
    procedure DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean); virtual;
    procedure DragDrop(const Data: TvgDragObject; const Point: TvgPoint); virtual;
    procedure DragLeave; virtual;
    procedure EnterFocus; virtual;
    procedure KillFocus; virtual;
    { control resources }
    procedure ApplyResource; virtual;
    { paint }
    procedure BeforePaint; virtual;
    procedure Paint; virtual;
    procedure AfterPaint; virtual;
    procedure PaintChildren; virtual;
    { changes }
    procedure MarginsChanged(Sender: TObject);
    procedure PaddingChanged(Sender: TObject);
    procedure MatrixChanged(Sender: TObject);
    { props }
    property MouseInObject: boolean read FMouseInObject write FMouseInObject;
    property TempCanvas: TvgCanvas read FTempCanvas write SetTempCanvas;
    property Skew: TvgPosition read FSkew write FSkew;
  public
    DisableDesignResize: boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { matrix }
    procedure RecalcAbsolute; virtual;
    function AbsoluteToLocal(P: TvgPoint): TvgPoint; virtual;
    function LocalToAbsolute(P: TvgPoint): TvgPoint; virtual;
    function AbsoluteToLocalVector(P: TvgVector): TvgVector; virtual;
    function LocalToAbsoluteVector(P: TvgVector): TvgVector; virtual;
    function ObjectByPoint(X, Y: single): TvgVisualObject;
    function pointInObject(X, Y: single): boolean; virtual;
    procedure SetBounds(X, Y, AWidth, AHeight: single);
    procedure SetSizeWithoutChange(AWidth, AHeight: single);
    { controls }
    procedure RecalcEnabled;
    { drag and drop }
    function FindTarget(const APoint: TvgPoint; const Data: TvgDragObject): TvgVisualObject;
    { align }
    procedure DisableAlign;
    procedure EnableAlign;
    procedure Realign; virtual;
    { effects }
    procedure UpdateEffects;
    { opacity }
    procedure RecalcOpacity; virtual;
    { }
    procedure SetFocus;
    procedure PaintTo(const ACanvas: TvgCanvas; const ARect: TvgRect; const AParent: TvgObject = nil);
    procedure ApplyEffect;
    procedure Repaint;
    procedure Lock;
    property AbsoluteMatrix: TvgMatrix read GetAbsoluteMatrix;
    property AbsoluteOpacity: single read GetAbsoluteOpacity;
    property AbsoluteWidth: single read GetAbsoluteWidth;
    property AbsoluteHeight: single read GetAbsoluteHeight;
    property AbsoluteScale: TvgPoint read GetAbsoluteScale;
    property AbsoluteEnabled: boolean read GetAbsoluteEnabled;
    property InvertAbsoluteMatrix: TvgMatrix read GetInvertAbsoluteMatrix;
    property LocalRect: TvgRect read GetLocalRect;
    property AbsoluteRect: TvgRect read GetAbsoluteRect;
    property UpdateRect: TvgRect read GetUpdateRect;
    property BoundsRect: TvgRect read GetBoundsRect write SetBoundsRect;
    property ParentedRect: TvgRect read GetParentedRect;
    property ClipRect: TvgRect read GetClipRect;
    property Canvas: TvgCanvas read GetCanvas;
    property AutoCapture: boolean read FAutoCapture write FAutoCapture default false;
  published
    { triggers }
    property IsMouseOver: boolean read FIsMouseOver;
    property IsDragOver: boolean read FIsDragOver;
    property IsFocused: boolean read FIsFocused;
    property IsVisible: boolean read FVisible;
    { props }
    property Align: TvgAlign read FAlign write SetAlign default vaNone;
    property Cursor: TCursor read FCursor write SetCursor default crDefault;
    property DragMode: TvgDragMode read FDragMode write FDragMode default vgDragManual;
    property DragDisableHighlight: boolean read FDragDisableHighlight write FDragDisableHighlight default false;
    property Enabled: boolean read FEnabled write SetEnabled default true;
    property Position: TvgPosition read FPosition write FPosition;
    property RotateAngle: single read FRotateAngle write SetRotateAngle;
    property RotateCenter: TvgPosition read FRotateCenter write FRotateCenter;
    property Locked: boolean read FLocked write SetLocked default false;
    property Width: single read FWidth write SetWidth;
    property Height: single read FHeight write SetHeight;
    property Margins: TvgBounds read FMargins write FMargins;
    property Padding: TvgBounds read FPadding write FPadding;
    property Opacity: single read FOpacity write SetOpacity stored isOpacityStored;
    property ClipChildren: boolean read FClipChildren write SetClipChildren default false;
    property ClipParent: boolean read FClipParent write FClipParent default false;
    property HitTest: boolean read FHitTest write SetHitTest default true;
    property Hint: WideString read FHint write FHint;
    property ShowHint: boolean read FShowHint write FShowHint default false;
    property CanClipped: boolean read FCanClipped write FCanClipped default true;
    property CanFocused: boolean read FCanFocused write FCanFocused default false;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu; 
    property Scale: TvgPosition read FScale write FScale;
    property Visible: boolean read FVisible write SetVisible default true;
    property DesignHide: boolean read FDesignHide write SetDesignHide default false;
    property OnDragEnter: TvgDragEnterEvent read FOnDragEnter write FOnDragEnter;
    property OnDragLeave: TvgDragLeaveEvent read FOnDragLeave write FOnDragLeave;
    property OnDragOver: TvgDragOverEvent read FOnDragOver write FOnDragOver;
    property OnDragDrop: TvgDragDropEvent read FOnDragDrop write FOnDragDrop;
    property OnKeyDown: TvgKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TvgKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnMouseDown: TvgMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TvgMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TvgMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseWheel: TvgMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnPaint: TOnPaintEvent read FOnPaint write FOnPaint;
  end;

  TvgBrushObject = class(TvgObject)
  private
    FBrush: TvgBrush;
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Brush: TvgBrush read FBrush write FBrush;
  end;

  TvgFrame = class(TvgVisualObject)
  private
    FNeedClone: boolean;
    FSceneObject: TvgCustomScene;
    FBuffer: TvgBitmap;
    procedure SetSceneObject(const Value: TvgCustomScene);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property SceneObject: TvgCustomScene read FSceneObject write SetSceneObject;
  end;

  TvgOrientation = (
    vgHorizontal,
    vgVertical
  );

  TvgControl = class(TvgVisualObject)
  private
    procedure SetResource(const Value: string);
  protected
    FResource: string;
    FResourceLink: TvgObject;
    FNeedResource: boolean;
    { control }
    procedure ApplyStyle; virtual;
    procedure BeforePaint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
    procedure ApplyResource; override;
  published
    property Resource: string read FResource write SetResource;
  end;

  TvgBackground = class(TvgControl)
  private
    FFill: TvgBrush;
    procedure SetFill(const Value: TvgBrush);
  protected
    procedure PaintChildren; override;
    procedure FillChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Resource;
    property Fill: TvgBrush read FFill write SetFill;
  end;

  TvgContent = class(TvgVisualObject)
  private
  protected
    function GetParentComponent: TComponent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Realign; override;
    procedure Paint; override;
  published
  end;

  IvgSizeGrip = interface
  ['{181729B7-53B2-45ea-97C7-91E1F3CBAABE}']
  end;

  { TvgCustomScene }

  TvgCustomScene = class(TCustomControl, IvgScene{$IFNDEF FPC}, IDropTarget{$ENDIF})
  private
    {$IFDEF WINDOWS}
    PrevWndProc: WNDPROC;
    FWStyle: string;
    {$ENDIF}
    {$IFDEF FPC}
    FShift: TShiftState;
    {$ENDIF}
    FCanvas: TvgCanvas;
    FDisableUpdate: boolean;
    FChildren: TList;
    FDesignRoot, FSelected, FCaptured, FHovered, FFocused: TvgVisualObject;
    FSelection: array of TvgObject;
    FDesignPlaceObject: TvgVisualObject;
    FDesignGridLines: array of TvgVisualObject;
    FDesignPopup: TPopupMenu;
    FDesignChangeSelection: TNotifyEvent;
    FUnsnapMousePos, FMousePos, FDownPos: TvgPoint;
    FMoving, FLeftTop, FRightTop, FLeftBottom, FRightBottom, FTop, FBottom, FLeft, FRight, FRotate: boolean;
    FLeftTopHot, FRightTopHot, FLeftBottomHot, FRightBottomHot, FTopHot, FBottomHot, FLeftHot, FRightHot, FRotateHot: boolean;
    FResizeSize: TPoint;
    FDragging, FResizing: boolean;
    FDesignTime: boolean;
    FFill: TvgBrush;
    FTransparency: boolean;
    FAllowDrag: boolean;
    FSnapToGrid: boolean;
    FSnapToLines: boolean;
    FSnapGridShow: boolean;
    FSnapGridSize: single;
    FInsertObject: string;
    FAlignRoot: boolean;
    FDesignPopupEnabled: boolean;
    FPopupPos: TPoint;
    FOpenInFrame: TvgFrame;
    FCloneFrame: TForm;
    FDrawing: boolean;
    FOnFlush: TNotifyEvent;
    FShowTimer: TTimer;
    FDBCSLeadChar: Word;
    FStyle: TvgResources;
    FTextRendering: TvgTextRendering;
    FShowUpdateRects: boolean;
    FLoadCursor: TCursor;
    VCLDragSource: TCustomControl;
    procedure DoShowTimer(Sender: TObject);
    {$IFDEF WIN32}
    function GetDataObject: TvgDragObject;
    function DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult; stdcall;
    procedure WMAddUpdateRect(var Msg: TMessage); message WM_ADDUPDATERECT;
    procedure CMMouseLeave(var Message: TMessage); //message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    {$ENDIF}
    {$IFNDEF FPC}
    procedure WMImeStartComposition(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMImeComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKeyUp(var Message: TWMKeyUp); message WM_KEYUP;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    {$ENDIF}
    {$IFDEF LINUX}
    procedure EraseBackground(DC: HDC); override;
    {$ENDIF}
    procedure CMShowingChanged(var Message: {$IFDEF FPC} TLMessage {$ELSE} TMessage {$ENDIF}); message CM_SHOWINGCHANGED;
    procedure CMDesignHitTest(var Msg: {$IFDEF FPC} TLMMouse {$ELSE} TWMMouse {$ENDIF}); message CM_DESIGNHITTEST;
    procedure CMHintShow(var Message: {$IFDEF FPC} TLMessage {$ELSE} TMessage {$ENDIF}); message CM_HINTSHOW;
    function GetCount: integer;
    procedure SetChildren(Index: integer; const Value: TvgObject);
    function GetChildrenObject(Index: integer): TvgObject;
    procedure SetFill(const Value: TvgBrush);
    procedure FillChanged(Sender: TObject);
    procedure SetSnapGridShow(const Value: boolean);
    procedure AddUpdateRectsFromGridLines;
    function SnapToGridValue(Value: single): single;
    procedure SetSnapGridSize(const Value: single);
    procedure SnapToGridLines(AllowChangePosition: boolean);
    function SnapPointToGridLines(const APoint: TvgPoint): TvgPoint;
    procedure ReadDesignSnapGridShow(Reader: TReader);
    procedure WriteDesignSnapGridShow(Writer: TWriter);
    procedure ReadDesignSnapToGrid(Reader: TReader);
    procedure WriteDesignSnapToGrid(Writer: TWriter);
    procedure ReadDesignSnapToLines(Reader: TReader);
    procedure WriteDesignSnapToLines(Writer: TWriter);
    { design }
    procedure OpenDesignPopup;
    procedure doDesignPopupLoadFromFile(Sender: TObject);
    procedure doDesignPopupDesignHide(Sender: TObject);
    procedure doDesignPopupAddItem(Sender: TObject);
    procedure doDesignPopupAdd(Sender: TObject);
    procedure doDesignPopupDel(Sender: TObject);
    procedure doDesignPopupReorder(Sender: TObject);
    procedure doDesignPopupGrid(Sender: TObject);
    procedure doDesignPopupCopy(Sender: TObject);
    procedure popupMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width,
      Height: Integer);
    procedure popupDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect;
      Selected: Boolean);
    function GetRoot: TvgObject;
    procedure SetFocused(const Value: TvgVisualObject);
    procedure DoDesignSelect(AObject: TObject);
    procedure UpdateLayer;
    procedure SetSelected(const Value: TvgVisualObject);
    procedure doDesignPopupPaste(Sender: TObject);
    procedure SetStyle(const Value: TvgResources);
    { IvgScene }
    function GetDisableUpdate: boolean;
    function GetDesignTime: boolean;
    function GetCanvas: TvgCanvas;
    function GetOwner: TComponent;
    function GetComponent: TComponent;
    function GetSelected: TvgVisualObject;
    function GetDesignPlaceObject: TvgVisualObject;
    procedure SetDisableUpdate(Value: boolean);
    function GetUpdateRectsCount: integer;
    function GetUpdateRect(const Index: integer): TvgRect;
    procedure SetCaptured(const Value: TvgVisualObject);
    function GetCaptured: TvgVisualObject;
    function GetFocused: TvgVisualObject;
    procedure SetDesignRoot(const Value: TvgVisualObject);
    function GetMousePos: TvgPoint;
    function GetStyle: TvgResources;
    function GetTransparency: boolean;
    procedure SetTransparency(const Value: boolean);
    procedure SetTextRendering(const Value: TvgTextRendering);
    procedure BeginVCLDrag(Source: TObject);
    procedure EndDragEvent(Sender, Target: TObject; X, Y: Integer);
  protected
    FUpdateRects: array of TvgRect;
    procedure CreateHandle; override;
    {$IFDEF WIN32}
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    {$ENDIF}
    procedure Loaded; override;
    procedure Resize; override;
    procedure Draw; virtual;
    procedure Paint; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure DoDragDrop(Sender, Source: TObject;
      X, Y: Integer);
    {$IFDEF FPC}
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    {$ENDIF}
    procedure UnicodeKeyUp(var Key: Word; var Char: System.WideChar; Shift: TShiftState);
    procedure UnicodeKeyDown(var Key: Word; var Char: System.WideChar; Shift: TShiftState);
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    { }
    function ObjectByPoint(X, Y: single): TvgVisualObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteChildren;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure UpdateBuffer;
    procedure UpdateResource;
    property Canvas: TvgCanvas read FCanvas;
    { children }
    procedure AddObject(AObject: TvgObject);
    procedure RemoveObject(AObject: TvgObject);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure EndUpdateWOInvalidate;
    procedure RealignRoot;
    procedure EmptyObjects;
    { managment }
    procedure BeginDrag;
    procedure BeginResize;
    { paint }
    procedure AddUpdateRect(R: TvgRect);
    { design }
    procedure InsertObject(const ClassName: string);
    property DesignTime: boolean read FDesignTime write FDesignTime stored false;
    { debug }
    property ShowUpdateRects: boolean read FShowUpdateRects write FShowUpdateRects stored false;
    { }
    property Count: integer read GetCount;
    property Root: TvgObject read GetRoot;
    property Children[Index: integer]: TvgObject read GetChildrenObject write SetChildren;
    property Selected: TvgVisualObject read FSelected write SetSelected;
    property Captured: TvgVisualObject read FCaptured;
    property Hovered: TvgVisualObject read FHovered;
    property Focused: TvgVisualObject read FFocused write SetFocused;
    property DisableUpdate: boolean read FDisableUpdate;
    property IsDrawing: boolean read FDrawing;  
    { use as emebbded }
    procedure CreateEmbedded(const AWidth, AHeight: integer; AOnFlush: TNotifyEvent);
    procedure EmbeddedMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EmbeddedMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure EmbeddedMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    function EmbeddedMouseWheel(Shift: TShiftState; WheelDelta: Integer): Boolean;
    procedure EmbeddedKeyUp(var Key: Word; var Char: System.WideChar; Shift: TShiftState);
    procedure EmbeddedKeyDown(var Key: Word; var Char: System.WideChar; Shift: TShiftState);
    property OnFlush: TNotifyEvent read FOnFlush write FOnFlush;
    { design }
    property DesignPopupEnabled: boolean read FDesignPopupEnabled write FDesignPopupEnabled;
    property DesignSnapGridShow: boolean read FSnapGridShow write SetSnapGridShow;
    property DesignSnapToGrid: boolean read FSnapToGrid write FSnapToGrid;
    property DesignSnapToLines: boolean read FSnapToLines write FSnapToLines;
    property DesignChangeSelection: TNotifyEvent read FDesignChangeSelection write FDesignChangeSelection;
    { can be published }
    property Align;
    property AlignRoot: boolean read FAlignRoot write FAlignRoot default true;
    property AllowDrag: boolean read FAllowDrag write FAllowDrag default false;
    property DesignSnapGridSize: single read FSnapGridSize write SetSnapGridSize;
    property Fill: TvgBrush read FFill write SetFill;
    property Transparency: boolean read FTransparency write SetTransparency default false;
    property TextRendering: TvgTextRendering read FTextRendering write SetTextRendering default vgAntiAlias;
    property Style: TvgResources read FStyle write SetStyle;
    property TabStop;
  published
  end;

  TvgScene = class(TvgCustomScene)
  published
    property Align;
    property AlignRoot;
    property AllowDrag;
    property DesignSnapGridSize;
    property Fill;
    property Transparency;
    property TextRendering;
    property Style;
    property TabStop;
  end;

  TvgResources = class(TComponent)
  private
    FResource: TStrings;
    FRoot: TvgObject;
    FSceneList: TList;
    procedure SetResource(const Value: TStrings);
    procedure DoResourceChanged(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    { vcl }
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadResources(Stream: TStream);
    procedure WriteResources(Stream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddSceneUpdater(const Scene: IvgScene);
    procedure RemoveSceneUpdater(const Scene: IvgScene);
    procedure FillStrings;
    procedure UpdateScenes;
    property Root: TvgObject read FRoot;
  published
    property Resource: TStrings read FResource write SetResource stored false;
  end;

  TvgDesigner = class(TComponent)
  private
    FScenes: TList;
  protected
    procedure CallDesignSelect(AObject: TObject);
  public
    procedure SelectObject(ADesigner: TComponent; AObject: TvgObject; MultiSelection: array of TvgObject); virtual; abstract;
    procedure Modified(ADesigner: TComponent); virtual; abstract;
    function UniqueName(ADesigner: TComponent; ClassName: string): string; virtual; abstract;
    function IsSelected(ADesigner: TComponent; const AObject: TObject): boolean; virtual; abstract;
    procedure AddScene(const Scene: IvgScene); virtual;
    procedure RemoveScene(const Scene: IvgScene); virtual;
  end;

var
  vgSceneCount: integer = 0;
  aniThread: TTimer;
  ObjectList: TStringList;
  vgDesigner: TvgDesigner;
  DefaultCanvasClass: TvgCanvasClass;
  DefaultFilterClass: TvgFilterClass;
  DefaultStyles: TvgObject;

procedure RegisterVGObject(const Category: string; const AObject: TvgObjectClass);
procedure RegisterVGObjects(const Category: string; AClasses: array of TvgObjectClass);

function CreateObjectFromStream(AOwner: TComponent; const AStream: TStream): TvgObject;
function LoadObjectFromStream(AObject: TvgObject; const AStream: TStream): TvgObject;

{ Resoruces }

procedure AddResource(const AObject: TvgObject);
procedure RemoveResource(const AObject: TvgObject);
function FindResource(const AResource: string): TvgObject;

implementation {===============================================================}

uses math, vg_effects, vg_dsgn,
  {$IFDEF WIN32} vg_canvas_gdip, {$ENDIF}
  {$IFDEF DARWIN} vg_canvas_macos, {$ENDIF}
  {$IFDEF LINUX} vg_canvas_cairo, {$ENDIF}
  vg_ani,vg_objects, vg_utils, vg_controls, vg_textbox, vg_memo, vg_listbox, typinfo, vg_version, vg_layouts;

{.$R vg_scene.res}

type
  TParentControl = class(TWinControl);
  THackComponent = class(TComponent);
var
  User32Lib: THandle;
  FTarget: TvgVisualObject = nil;

procedure RegisterVGObject(const Category: string; const AObject: TvgObjectClass);
begin
  if ObjectList =  nil then
  begin
    ObjectList := TStringList.Create;
  end;
  ObjectList.InsertObject(0, Category, TObject(AObject));
end;

procedure RegisterVGObjects(const Category: string; AClasses: array of TvgObjectClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
  begin
    RegisterVGObject(Category, AClasses[I]);
    RegisterClass(AClasses[I]);
  end;
end;

{ Resoruces }

var
  ResourceList: TList;

procedure AddResource(const AObject: TvgObject);
begin
  if ResourceList =  nil then
  begin
    ResourceList := TList.Create;
    ResourceList.Capacity := 100;
  end;
  if ResourceList.IndexOf(AObject) < 0 then
    ResourceList.Add(AObject);
end;

procedure RemoveResource(const AObject: TvgObject);
begin
  if ResourceList <> nil then
    ResourceList.Remove(AObject);
end;

function FindResource(const AResource: string): TvgObject;
var
  i: integer;
begin
  Result := nil;
  if ResourceList <> nil then
    for i := ResourceList.Count - 1 downto 0 do
      if TvgObject(ResourceList[i]).Stored then
        if CompareText(TvgObject(ResourceList[i]).ResourceName, AResource) = 0 then
        begin
          Result := TvgObject(ResourceList[i]);
          Break;
        end;
end;

function CreateObjectFromStream(AOwner: TComponent; const AStream: TStream): TvgObject;
var
  Reader: TReader;
  SavePos: Longint;
  I: Integer;
  Flags: TFilerFlags;
  ClassName: string;
  ObjClass: TvgObjectClass;
  BinStream: TStream;
begin
  Result := nil;
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(AStream, BinStream);
      BinStream.Position := 0;

      Reader := TReader.Create(BinStream, 4096);
      {$IFDEF FPC}
      Reader.Driver.BeginRootComponent;
      ClassName := Reader.Driver.ReadStr;
      {$ELSE}
      Reader.ReadSignature;
      Reader.ReadPrefix(Flags, I);
      ClassName := Reader.ReadStr;
      {$ENDIF}

      ObjClass := TvgObjectClass(GetClass(ClassName));
      Result := ObjClass.Create(AOwner);
      if Result <> nil then
      begin
        BinStream.Position := 0;
        Result.LoadFromBinStream(BinStream);
      end;
      Reader.Free;
    finally
      BinStream.Free;
    end;
  except
    Result := nil;
  end;
end;

function CreateObjectFromBinStream(AOwner: TComponent; const AStream: TStream): TvgObject;
var
  Reader: TReader;
  SavePos: Longint;
  I: Integer;
  Flags: TFilerFlags;
  ClassName: string;
  ObjClass: TvgObjectClass;
begin
  Result := nil;
  try
    SavePos := AStream.Position;

    Reader := TReader.Create(AStream, 4096);
    {$IFDEF FPC}
    Reader.Driver.BeginRootComponent;
    ClassName := Reader.Driver.ReadStr;
    {$ELSE}
    Reader.ReadSignature;
    Reader.ReadPrefix(Flags, I);
    ClassName := Reader.ReadStr;
    {$ENDIF}

    ObjClass := TvgObjectClass(GetClass(ClassName));
    Result := ObjClass.Create(AOwner);
    if Result <> nil then
    begin
      AStream.Position := SavePos;
      Result.LoadFromBinStream(AStream);
    end;
    Reader.Free;
  except
    Result := nil;
  end;
end;

function LoadObjectFromStream(AObject: TvgObject; const AStream: TStream): TvgObject;
var
  Reader: TReader;
  SavePos: Longint;
  I: Integer;
  Flags: TFilerFlags;
  ClassName: string;
  ObjClass: TvgObjectClass;
  BinStream: TStream;
begin
  Result := nil;
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(AStream, BinStream);
      BinStream.Position := 0;
      Result := AObject;
      if Result <> nil then
      begin
        BinStream.Position := 0;
        Result.LoadFromBinStream(BinStream);
      end;
      Reader.Free;
    finally
      BinStream.Free;
    end;
  except
    Result := nil;
  end;
end;

{ Geom }

function vgCorrectColor(const C: TvgColor): TvgColor;
begin
  Result := C;
  {$IFDEF DARWIN}
  TvgColorRec(Result).R := TvgColorRec(C).B;
  TvgColorRec(Result).B := TvgColorRec(C).R;
  {$ENDIF}
end;

function vgPremultyAlpha(const C: TvgColor): TvgColor;
begin
  if TvgColorRec(C).A = 0 then
    Result := 0
  else
  if TvgColorRec(C).A = $FF then
    Result := C
  else
  begin
    TvgColorRec(Result).R := trunc(TvgColorRec(C).R * (TvgColorRec(C).A / $FF));
    TvgColorRec(Result).G := trunc(TvgColorRec(C).G * (TvgColorRec(C).A / $FF));
    TvgColorRec(Result).B := trunc(TvgColorRec(C).B * (TvgColorRec(C).A / $FF));
    TvgColorRec(Result).A := TvgColorRec(C).A;
  end;
end;

function vgUnpremultyAlpha(const C: TvgColor): TvgColor;
begin
  if TvgColorRec(C).A = 0 then
    Result := 0
  else
  if TvgColorRec(C).A = $FF then
    Result := C
  else
  begin
    TvgColorRec(Result).R := trunc(TvgColorRec(C).R / (TvgColorRec(C).A / $FF));
    TvgColorRec(Result).G := trunc(TvgColorRec(C).G / (TvgColorRec(C).A / $FF));
    TvgColorRec(Result).B := trunc(TvgColorRec(C).B / (TvgColorRec(C).A / $FF));
    TvgColorRec(Result).A := TvgColorRec(C).A;
  end;
end;

function vgOpacity(const C: TvgColor; const AOpacity: single): TvgColor;
begin
  Result := C;
  if AOpacity < 1 then
    TvgColorRec(Result).A := Trunc(TvgColorRec(C).A * AOpacity);
end;

function vgColor(R, G, B: Byte; A: Byte = $FF): TvgColor;
begin
  TvgColorRec(Result).R := R;
  TvgColorRec(Result).G := G;
  TvgColorRec(Result).B := B;
  TvgColorRec(Result).A := A;
end;

function vgChangeHSL(const C: TvgColor; dH, dS, dL: single): TvgColor;
var
  H, S, L: single;
  A: byte;
begin
  A := TvgColorRec(c).A;
  RGBtoHSL(C, H, S, L);
  H := H + dH;
  if H < 0 then H := 0;
  if H > 1 then H := 1;
  S := S + dS;
  if S < 0 then S := 0;
  if S > 1 then S := 1;
  L := L + dL;
  if L < 0 then L := 0;
  if L > 1 then L := 1;
  Result := HSLtoRGB(H, S, L);
  TvgColorRec(Result).A := A;
end;

function vgColorToStr(Value: TvgColor): string;
begin
  Result := '#' + IntToHex(Value, 8);
end;

function vgStrToColor(Value: string): TvgColor;
var
  i: integer;
begin
  if Value = #0 then
    Value := '$0'
  else
    if (Value <> '') and (Value[1] = '#') then
      Value[1] := '$'
    else
    begin
      for i := 0 to High(vgColorIdents) do
        if CompareText(Value, vgColorIdents[i].Name) = 0 then
        begin
          Value := vgColorIdents[i].Value;
          Value[1] := '$';
          Break;
        end;
    end;
  try
    Result := StrToInt(Value);
  except
  end;
end;

function vgFloatToStr(Value: single): string;
var
  S: char;
begin
  S := DecimalSeparator;
  try
    DecimalSeparator := '.';
    if Frac(Value) <> 0 then
      Result := Format('%.3f', [Value])
    else
      Result := IntToStr(Trunc(Value))
  finally
    DecimalSeparator := S;
  end;
end;

function vgStrToFloat(Value: string): single;
var
  S: char;
begin
  S := DecimalSeparator;
  try
    DecimalSeparator := '.';
    Result := StrToFloat(Value);
  finally
    DecimalSeparator := S;
  end;
end;

{$Q-}

function HSLtoRGB(H, S, L: Single): TvgColor;
var
  M1, M2: Single;
  R, G, B: Byte;

  function HueToColor(Hue: Single): Byte;
  var
    V: Double;
  begin
    Hue := Hue - Floor(Hue);

    if 6 * Hue < 1 then V := M1 + (M2 - M1) * Hue * 6
    else if 2 * Hue < 1 then V := M2
    else if 3 * Hue < 2 then V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else V := M1;
    Result := Round(255 * V);
  end;
begin
  if S = 0 then
  begin
    R := Round(255 * L);
    G := R;
    B := R;
  end
  else
  begin
    if L <= 0.5 then M2 := L * (1 + S)
    else M2 := L + S - L * S;
    M1 := 2 * L - M2;
    R := HueToColor(H + 1 / 3);
    G := HueToColor(H);
    B := HueToColor(H - 1 / 3)
  end;
  Result := vgColor(R, G, B);
end;

procedure RGBtoHSL(RGB: TvgColor; out H, S, L: single);
var
  R, G, B, D, Cmax, Cmin: Single;
begin
  R := TvgColorRec(RGB).R / 255;
  G := TvgColorRec(RGB).G / 255;
  B := TvgColorRec(RGB).B / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));
  L := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    H := 0;
    S := 0
  end
  else
  begin
    D := Cmax - Cmin;
    if L < 0.5 then S := D / (Cmax + Cmin)
    else S := D / (2 - Cmax - Cmin);
    if R = Cmax then H := (G - B) / D
    else
      if G = Cmax then H  := 2 + (B - R) /D
      else H := 4 + (R - G) / D;
    H := H / 6;
    if H < 0 then H := H + 1
  end;
end;

{ math }

const
  // to be used as descriptive indices
  X = 0;
  Y = 1;
  Z = 2;
  W = 3;

procedure vgSinCos(const Theta: single; var Sin, Cos: single);
var
  s, c : Extended;
begin
  Math.SinCos(Theta, s, c);
  {$HINTS OFF}
  Sin:=s; Cos:=c;
  {$HINTS ON}
end;

function vgRadToDeg(const Degrees: single): single;
begin
   Result:=Degrees * c180divPI;
end;

function vgDegToRad(const Degrees: single): single;
begin
   Result:=Degrees*cPIdiv180;
end;

function vgNormalizeAngle(const angle: Single) : Single;
begin
  Result := angle - Int(angle * cInv360) * c360;
  if Result < -c180 then
    Result := Result + c360;
end;

function vgPoint(X, Y: single): TvgPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

function vgScalePoint(P: TvgPoint; dx, dy: single): TvgPoint;
begin
  Result.X := P.X * dx;
  Result.Y := P.Y * dy;
end;

function vgRect(ALeft, ATop, ARight, ABottom: single): TvgRect;
begin
  Result.Left := ALeft;
  Result.Top := ATop;
  Result.Right := ARight;
  Result.Bottom := ABottom;
end;

function vgNormalizeRect(const Pts: array of TvgPoint): TvgRect;
var
  i: integer;
begin
  Result.Left := $F000;
  Result.Top := $F000;
  Result.Right := -$F000;
  Result.Bottom := -$F000;
  for i := 0 to High(Pts) do
  begin
    if Pts[i].X < Result.Left then
      Result.Left := Pts[i].X;
    if Pts[i].Y < Result.Top then
      Result.Top := Pts[i].Y;
    if Pts[i].X > Result.Right then
      Result.Right := Pts[i].X;
    if Pts[i].Y > Result.Bottom then
      Result.Bottom := Pts[i].Y;
  end;
end;

function vgNormalizeRect2(const ARect: TvgRect): TvgRect;
begin
  with ARect do
    Result := vgNormalizeRect([vgPoint(Left, Top), vgPoint(Right, Top), vgPoint(Right, Bottom), vgPoint(Left, Bottom)]);
end;

procedure vgOffsetRect(var R: TvgRect; const Dx, Dy: single);
begin
  R.Left := R.Left + Dx;
  R.Right := R.Right + Dx;
  R.Top := R.Top + Dy;
  R.Bottom := R.Bottom + Dy;
end;

procedure vgMultiplyRect(var R: TvgRect; const Dx, Dy: single);
begin
  R.Left := R.Left * Dx;
  R.Right := R.Right * Dx;
  R.Top := R.Top * Dy;
  R.Bottom := R.Bottom * Dy;
end;

procedure vgInflateRect(var R: TvgRect; const Dx, Dy: single);
begin
  R.Left := R.Left - Dx;
  R.Right := R.Right + Dx;
  R.Top := R.Top - Dy;
  R.Bottom := R.Bottom + Dy;
end;

function vgUnionRect(const ARect1, ARect2: TvgRect): TvgRect;
begin
  Result.Left := ARect1.Left;
  if ARect2.Left < Result.Left then
    Result.Left := ARect2.Left;

  Result.Top := ARect1.Top;
  if ARect2.Top < Result.Top then
    Result.Top := ARect2.Top;

  Result.Right := ARect1.Right;
  if ARect2.Right > Result.Right then
    Result.Right := ARect2.Right;

  Result.Bottom := ARect1.Bottom;
  if ARect2.Bottom > Result.Bottom then
    Result.Bottom := ARect2.Bottom;
end;

function vgRectWidth(const R: TvgRect): single;
begin
  Result := R.Right - R.Left;
end;

function vgRectHeight(const R: TvgRect): single;
begin
  Result := R.Bottom - R.Top;
end;

function vgRectCenter(var R: TvgRect; Bounds: TvgRect): TvgRect;
begin
  vgOffsetRect(R, -R.Left, -R.Top);
  vgOffsetRect(R, Trunc((vgRectWidth(Bounds) - vgRectWidth(R)) / 2), Trunc((vgRectHeight(Bounds) - vgRectHeight(R)) / 2));
  vgOffsetRect(R, Bounds.Left, Bounds.Top);

  Result := R;
end;

function vgFitRect(var R: TvgRect; BoundsRect: TvgRect): single;
var
  ratio: single;
begin
  Result := 1;
  if vgRectWidth(BoundsRect) * vgRectHeight(BoundsRect) = 0 then Exit;

  if (vgRectWidth(R) / vgRectWidth(BoundsRect)) > (vgRectHeight(R) / vgRectHeight(BoundsRect)) then
    ratio := vgRectWidth(R) / vgRectWidth(BoundsRect)
  else
    ratio := vgRectHeight(R) / vgRectHeight(BoundsRect);

  if ratio < 1 then
  begin
    R := vgRect(0, 0, vgRectWidth(R), vgRectHeight(R));
  end
  else
  begin
    R := vgRect(0, 0, vgRectWidth(R) / ratio, vgRectHeight(R) / ratio);
  end;

  Result := ratio;
  vgRectCenter(R, BoundsRect);
end;

function vgIsRectEmpty(Rect: TvgRect): boolean;
begin
  Result := (vgRectWidth(Rect) <= 0) or (vgRectHeight(Rect) <= 0);
end;

function vgIntersectRect(const Rect1, Rect2: TvgRect): boolean;
begin
  (*
    Assumes that:  x1 < x2, y1 < y2, x3 < x4, y3 < y4
  *)
  Result := (Rect1.Left <= Rect2.Right) and (Rect1.Right >= Rect2.Left) and (Rect1.Top <= Rect2.Bottom) and (Rect1.Bottom >= Rect2.Top);
end;

{$IFDEF FPC}
function Rect(const Left, Top, Right, Bottom: integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;
{$ENDIF}

function vgIntersectRect(var R: TvgRect; const Rect1, Rect2: TvgRect): boolean;
var
  fR1, fR2, fR: TRect;
begin
  fR1 := Rect(Round(Rect1.Left * $FFFF), Round(Rect1.Top * $FFFF), Round(Rect1.Right * $FFFF), Round(Rect1.Bottom * $FFFF));
  fR2 := Rect(Round(Rect2.Left * $FFFF), Round(Rect2.Top * $FFFF), Round(Rect2.Right * $FFFF), Round(Rect2.Bottom * $FFFF));
  Result := IntersectRect(fR, fR1, fR2);
  if Result then
    R := vgRect(fR.Left / $FFFF, fR.Top / $FFFF, fR.Right / $FFFF, fR.Bottom / $FFFF)
  else
    R := vgRect(Rect1.Left, Rect1.Top, Rect1.Left, Rect1.Top);
end;

function vgPtInRect(const P: TvgPoint; const Rect: TvgRect): boolean;
begin
  Result := (P.X >= Rect.Left) and (P.X <= Rect.Right) and (P.Y >= Rect.Top) and (P.Y <= Rect.Bottom);
end;

function vgRectToString(R: TvgRect): Ansistring;
begin
  Result := '(' + vgFloatToStr(R.Left) + ',' + vgFloatToStr(R.Top) + ',' + vgFloatToStr(R.Right) + ',' +
    vgFloatToStr(R.Bottom) + ')';
end;

function vgStringToRect(S: Ansistring): TvgRect;
begin
  try
    GetToken(S, ',()');
    Result.Left := vgStrToFloat(GetToken(S, ',()'));
    Result.Top := vgStrToFloat(GetToken(S, ',()'));
    Result.Right := vgStrToFloat(GetToken(S, ',()'));
    Result.Bottom := vgStrToFloat(GetToken(S, ',()'));
  except
    Result := vgRect(0, 0, 0, 0);
  end;
end;

function vgPointFromVector(const v: TvgVector): TvgPoint;
begin
  Result.x := v.x;
  Result.y := v.y;
end;

function vgPointToString(R: TvgPoint): Ansistring;
begin
  Result := '(' + vgFloatToStr(R.X) + ',' + vgFloatToStr(R.Y) + ')';
end;

function vgStringToPoint(S: Ansistring): TvgPoint;
begin
  try
    GetToken(S, ',()');
    Result.X := vgStrToFloat(GetToken(S, ',()'));
    Result.Y := vgStrToFloat(GetToken(S, ',()'));
  except
    Result := vgPoint(0, 0);
  end;
end;

function vgMatrixMultiply(const M1, M2: TvgMatrix): TvgMatrix;
begin
  Result.m11 := M1.m11 * M2.m11 + M1.m12 * M2.m21 + M1.m13 * M2.m31;
  Result.m12 := M1.m11 * M2.m12 + M1.m12 * M2.m22 + M1.m13 * M2.m32;
  Result.m13 := M1.m11 * M2.m13 + M1.m12 * M2.m23 + M1.m13 * M2.m33;
  Result.m21 := M1.m21 * M2.m11 + M1.m22 * M2.m21 + M1.m23 * M2.m31;
  Result.m22 := M1.m21 * M2.m12 + M1.m22 * M2.m22 + M1.m23 * M2.m32;
  Result.m23 := M1.m21 * M2.m13 + M1.m22 * M2.m23 + M1.m23 * M2.m33;
  Result.m31 := M1.m31 * M2.m11 + M1.m32 * M2.m21 + M1.m33 * M2.m31;
  Result.m32 := M1.m31 * M2.m12 + M1.m32 * M2.m22 + M1.m33 * M2.m32;
  Result.m33 := M1.m31 * M2.m13 + M1.m32 * M2.m23 + M1.m33 * M2.m33;
end;

function vgMatrixDeterminant(const M: TvgMatrix): single;
begin
  Result := M.M[X].V[X] * (M.M[Y].V[Y] * M.M[Z].V[Z] - M.M[Z].V[Y] * M.M[Y].V[Z])
          - M.M[X].V[Y] * (M.M[Y].V[X] * M.M[Z].V[Z] - M.M[Z].V[X] * M.M[Y].V[Z])
          + M.M[X].V[Z] * (M.M[Y].V[X] * M.M[Z].V[Y] - M.M[Z].V[X] * M.M[Y].V[Y]);
end;

procedure vgAdjointMatrix(var M: TvgMatrix);
var
   a1, a2, a3,
   b1, b2, b3,
   c1, c2, c3: Single;
begin
   a1:= M.M[X].V[X]; a2:= M.M[X].V[Y]; a3:= M.M[X].V[Z];
   b1:= M.M[Y].V[X]; b2:= M.M[Y].V[Y]; b3:= M.M[Y].V[Z];
   c1:= M.M[Z].V[X]; c2:= M.M[Z].V[Y]; c3:= M.M[Z].V[Z];
   M.M[X].V[X]:= (b2*c3-c2*b3);
   M.M[Y].V[X]:=-(b1*c3-c1*b3);
   M.M[Z].V[X]:= (b1*c2-c1*b2);

   M.M[X].V[Y]:=-(a2*c3-c2*a3);
   M.M[Y].V[Y]:= (a1*c3-c1*a3);
   M.M[Z].V[Y]:=-(a1*c2-c1*a2);

   M.M[X].V[Z]:= (a2*b3-b2*a3);
   M.M[Y].V[Z]:=-(a1*b3-b1*a3);
   M.M[Z].V[Z]:= (a1*b2-b1*a2);
end;

procedure vgScaleMatrix(var M: TvgMatrix; const factor: single);
var
   i : Integer;
begin
   for i:=0 to 2 do begin
      M.M[I].V[0]:=M.M[I].V[0] * Factor;
      M.M[I].V[1]:=M.M[I].V[1] * Factor;
      M.M[I].V[2]:=M.M[I].V[2] * Factor;
   end;
end;

procedure vgInvertMatrix(var M: TvgMatrix);
var
   det : Single;
begin
  det := vgMatrixDeterminant(M);
  if Abs(Det) < EPSILON then
     M := IdentityMatrix
  else
  begin
    vgAdjointMatrix(M);
    vgScaleMatrix(M, 1/det);
  end;
end;

function vgVector(const x, y: Single; const w: single = 1.0): TvgVector;
begin
  Result.X := x;
  Result.Y := y;
  Result.W := w;
end;

function vgVector(const P: TvgPoint; const w: single = 1.0): TvgVector;
begin
  Result.X := P.x;
  Result.Y := P.y;
  Result.W := w;
end;

function vgVectorTransform(const V: TvgVector; const M: TvgMatrix): TvgVector;
begin
  Result.V[X] := V.V[X] * M.M[X].V[X] + V.V[Y] * M.M[Y].V[X] + V.V[Z] * M.M[Z].V[X];
  Result.V[Y] := V.V[X] * M.M[X].V[Y] + V.V[Y] * M.M[Y].V[Y] + V.V[Z] * M.M[Z].V[Y];
  Result.V[2] := 1.0;
end;

function vgVectorAdd(const v1: TvgVector; const v2: TvgVector): TvgVector;
begin
   Result.v[0] := v1.v[0] + v2.v[0];
   Result.v[1] := v1.v[1] + v2.v[1];
   Result.W := 1.0;
end;

function vgVectorSubtract(const v1: TvgVector; const v2: TvgVector): TvgVector;
begin
   Result.v[0] := v1.v[0] - v2.v[0];
   Result.v[1] := v1.v[1] - v2.v[1];
   Result.W := 1.0;
end;

function vgVectorNorm(const v : TvgVector) : Single;
begin
   Result:=v.V[0]*v.V[0]+v.V[1]*v.V[1];
end;

function RSqrt(v : Single) : Single;
var
  R: double;
begin
  R := Abs(V);
  if (R > 0) then
    Result := 1 / Sqrt(R)
  else
    Result := 1;
end;

function vgVectorNormalize(const v: TvgVector): TvgVector;
var
   invLen : Single;
begin
   invLen := RSqrt(Abs(vgVectorNorm(v)));
   Result.v[0] := v.v[0]*invLen;
   Result.v[1] := v.v[1]*invLen;
   Result.v[2] := 0.0;
end;

function vgVectorScale(const v: TvgVector; factor : Single): TvgVector;
begin
   Result.v[0] := v.v[0] * factor;
   Result.v[1] := v.v[1] * factor;
   Result.W := 1;
end;

function vgVectorLength(const v : TvgVector) : Single;
begin
   Result:=Sqrt(vgVectornorm(v));
end;

function vgVectorDotProduct(const V1, V2 : TvgVector): Single;
begin
   Result:=V1.V[0]*V2.V[0]+V1.V[1]*V2.V[1];
end;

function vgVectorAngleCosine(const V1, V2: TvgVector): Single;
begin
  if (vgVectorLength(V1) <> 0) and (vgVectorLength(V2) <> 0) then
    Result := vgVectorDotProduct(V1, V2) / (vgVectorLength(V1) * vgVectorLength(V2))
  else
    Result := 0;
end;

function vgVectorCrossProductZ(const V1, V2: TvgVector): single;
begin
  // 3D Cross with Z = 0
  Result := v1.X*v2.Y - v1.Y*v2.X;
end;

function vgVectorCombine2(const V1, V2: TvgVector; const F1, F2: Single): TvgVector;
begin
   Result.V[X]:=(F1 * V1.V[X]) + (F2 * V2.V[X]);
   Result.V[Y]:=(F1 * V1.V[Y]) + (F2 * V2.V[Y]);
   Result.W := 1.0;
end;

function vgVectorReflect(const V, N: TvgVector): TvgVector;
begin
  Result := vgVectorCombine2(V, N, 1, -2 * vgVectorDotProduct(V, N));
end;

function vgVectorAngle(const V, N: TvgVector): single;
begin
  if vgVectorCrossProductZ(V, N) < 0 then
    Result := vgRadToDeg(ArcCos(vgVectorAngleCosine(V, N)))
  else
    Result := -vgRadToDeg(ArcCos(vgVectorAngleCosine(V, N)));
end;

function vgCreateRotationMatrix(angle: single): TvgMatrix;
var
  cosine, sine: single;
begin
  vgSinCos(angle, sine, cosine);

  Result.m11 := cosine;
  Result.m12 := sine;
  Result.m13 := 0;
  Result.m21 := -sine;
  Result.m22 := cosine;
  Result.m23 := 0;

  Result.m31 := 0;
  Result.m32 := 0;
  Result.m33 := 1;
end;

function vgInterpolateSingle(const start, stop, t: single): single;
begin
  Result := start + (stop - start) * t;
end;

function vgInterpolateRotation(start, stop, t : Single) : Single;
begin
{   start:=NormalizeAngle(start);
   stop:=NormalizeAngle(stop);
   d:=stop-start;
   if d>PI then begin
      // positive d, angle on opposite side, becomes negative i.e. changes direction
      d:=-d-c2PI;
   end else if d<-PI then begin
      // negative d, angle on opposite side, becomes positive i.e. changes direction
      d:=d+c2PI;
   end;
   Result:=start+d*t; }
  Result := vgInterpolateSingle(start, stop, t);
end;

function vgInterpolateColor(start, stop: TvgColor; t : single): TvgColor;
begin
  TvgColorRec(Result).A := TvgColorRec(start).A + Trunc((TvgColorRec(stop).A - TvgColorRec(start).A) * t);
  TvgColorRec(Result).R := TvgColorRec(start).R + Trunc((TvgColorRec(stop).R - TvgColorRec(start).R) * t);
  TvgColorRec(Result).G := TvgColorRec(start).G + Trunc((TvgColorRec(stop).G - TvgColorRec(start).G) * t);
  TvgColorRec(Result).B := TvgColorRec(start).B + Trunc((TvgColorRec(stop).B - TvgColorRec(start).B) * t);
end;

function vgAppendColor(start, stop: TvgColor): TvgColor;
begin
  if TvgColorRec(start).A + TvgColorRec(stop).A < $FF then
    TvgColorRec(Result).A := TvgColorRec(start).A + TvgColorRec(stop).A
  else
    TvgColorRec(Result).A := $FF;
  if TvgColorRec(start).R + TvgColorRec(stop).R < $FF then
    TvgColorRec(Result).R := TvgColorRec(start).R + TvgColorRec(stop).R
  else
    TvgColorRec(Result).R := $FF;
  if TvgColorRec(start).G + TvgColorRec(stop).G < $FF then
    TvgColorRec(Result).G := TvgColorRec(start).G + TvgColorRec(stop).G
  else
    TvgColorRec(Result).G := $FF;
  if TvgColorRec(start).B + TvgColorRec(stop).B < $FF then
    TvgColorRec(Result).B := TvgColorRec(start).B + TvgColorRec(stop).B
  else
    TvgColorRec(Result).B := $FF;
end;

function vgSubtractColor(start, stop: TvgColor): TvgColor;
begin
  if TvgColorRec(start).A - TvgColorRec(stop).A < $FF then
    TvgColorRec(Result).A := TvgColorRec(start).A - TvgColorRec(stop).A
  else
    TvgColorRec(Result).A := $FF;
  if TvgColorRec(start).R - TvgColorRec(stop).R < $FF then
    TvgColorRec(Result).R := TvgColorRec(start).R - TvgColorRec(stop).R
  else
    TvgColorRec(Result).R := $FF;
  if TvgColorRec(start).G - TvgColorRec(stop).G < $FF then
    TvgColorRec(Result).G := TvgColorRec(start).G - TvgColorRec(stop).G
  else
    TvgColorRec(Result).G := $FF;
  if TvgColorRec(start).B - TvgColorRec(stop).B < $FF then
    TvgColorRec(Result).B := TvgColorRec(start).B - TvgColorRec(stop).B
  else
    TvgColorRec(Result).B := $FF;
end;

{}
{$IFDEF WIN32}
type

  PBlendFunction = ^TBlendFunction;
  _BLENDFUNCTION = packed record
    BlendOp: BYTE;
    BlendFlags: BYTE;
    SourceConstantAlpha: BYTE;
    AlphaFormat: BYTE;
  end;
  TBlendFunction = _BLENDFUNCTION;
  BLENDFUNCTION = _BLENDFUNCTION;

const

  WS_EX_LAYERED = $00080000;
  LWA_COLORKEY = $00000001;
  LWA_ALPHA = $00000002;
  ULW_COLORKEY = $00000001;
  ULW_ALPHA = $00000002;
  ULW_OPAQUE = $00000004;

var
  SetLayeredWindowAttributes: function (hwnd: HWND; crKey: COLORREF; bAlpha: BYTE;
    dwFlags: DWORD): BOOL; stdcall;
  UpdateLayeredWindow: function (hWnd: HWND; hdcDst: HDC; pptDst: PPOINT;
    psize: PSIZE; hdcSrc: HDC; pptSrc: PPOINT; crKey: COLORREF;
    pblend: PBlendFunction; dwFlags: DWORD): BOOL; stdcall;
  PrintWindow: function(hwnd: HWND; hdcBlt: HDC; nFlags: DWORD): BOOL; stdcall;
{$ENDIF}

{$IFDEF DARWIN}
function WndEventHandler(inHandlerCallRef: EventHandlerCallRef;
                          inEvent: EventRef;
                          inUserData: Pointer): OSStatus; stdcall;
var
  myContext: CGContextRef;
  myRect: CGRect;
  rgnCode: WindowRegionCode;
  rgn: RgnHandle;
  bool: longbool;
begin
  Result := CallNextEventHandler(inHandlerCallRef, inEvent);
  Result := eventNotHandledErr;
  if GetEventClass(inEvent) = kEventClassControl then
  begin
    case GetEventKind(inEvent) of
      kEventControlDraw:
        begin
          GetEventParameter (inEvent,
                            kEventParamCGContextRef,
                            typeCGContextRef,
                            nil,
                            sizeof (CGContextRef),
                            nil,
                            @myContext);
          if myContext <> nil then
          begin
            myRect := CGRectFromRect(vgRect(0, 0, TvgCustomScene(inUserData).Parent.Width, TvgCustomScene(inUserData).Parent.Height));
            CGContextClearRect(myContext, myRect);
          end;
          Result := noErr;
        end;
    end;
  end;
  if GetEventClass(inEvent) = kEventClassWindow then
  begin
    case GetEventKind(inEvent) of
      kEventWindowGetRegion:
        begin
          TvgCustomScene(inUserData).Invalidate;
          // which region code is being queried?
{          GetEventParameter(inEvent, kEventParamWindowRegionCode, typeWindowRegionCode, nil, sizeof(rgnCode), nil, @rgnCode);
          // if it is the opaque region code then set the region to Empty and return noErr to stop the propagation
	  if (rgnCode = kWindowOpaqueRgn) then
	  begin
	    GetEventParameter(inEvent, kEventParamRgnHandle, typeQDRgnHandle, nil, sizeof(rgn), nil, @rgn);
            SetEmptyRgn(rgn);
            Result := noErr;
	  end;}
          Result := noErr;
        end;
    end;
  end;
end;

type
  byte8array = array [1..8] of byte;

function RegionToRectsCallback(message: UInt16; rgn: RgnHandle; const rect_: byte8array; data: Pointer): OSStatus; cdecl;
var
  R: TvgRect;
begin
  if (message = kQDRegionToRectsMsgParse) then
  begin
    {$ifdef FPC_BIG_ENDIAN}
    R := vgRect(rect_[4] or (rect_[3] shl 8), rect_[2] or (rect_[1] shl 8), rect_[8] or (rect_[7] shl 8), rect_[6] or (rect_[5] shl 8));
    {$else}
    R := vgRect(rect_[3] or (rect_[4] shl 8), rect_[1] or (rect_[2] shl 8), rect_[7] or (rect_[8] shl 8), rect_[5] or (rect_[6] shl 8));
    {$endif}
    SetLength(TvgCustomScene(data).FUpdateRects, Length(TvgCustomScene(data).FUpdateRects) + 1);
    TvgCustomScene(data).FUpdateRects[High(TvgCustomScene(data).FUpdateRects)] := R;
  end;
  Result := noErr;
end;

function CreateFileURLFromPasteboard(inPasteboard: PasteboardRef): TvgDragObject;
var
  inIndex: CFIndex;
  inCount: ItemCount;
	item: PasteboardItemID;
	fileURL: CFURLRef;
  fileURLData: CFDataRef;
	info: LSItemInfoRecord;
	uti: CFStringRef;
begin
  Fillchar(Result, sizeOf(Result), 0);

	if PasteboardGetItemCount(inPasteboard, inCount) <> noErr then Exit;
  SetLength(Result.Files, inCount);
  for inIndex := 1 to inCount do
  begin
  	if PasteboardGetItemIdentifier(inPasteboard, inIndex, item) <> noErr then Exit;
  	if PasteboardCopyItemFlavorData(inPasteboard, item, kUTTypeFileURL, fileURLData) <> noErr then Exit;

  	// create the file URL with the dragged data
  	fileURL := CFURLCreateAbsoluteURLWithBytes( kCFAllocatorDefault, CFDataGetBytePtr( fileURLData ), CFDataGetLength( fileURLData ), kCFStringEncodingMacRoman, nil, true);
    if fileURL <> nil then
    begin
      uti := CFURLCopyFileSystemPath(fileURL, kCFURLPOSIXPathStyle);
      Result.Files[inIndex - 1] := CFStringToStr(uti);
    	CFRelease(uti);
  		CFRelease(fileURL);
    end;
    CFRelease(fileURLData);

    if inIndex = 1 then
      Result.Data := Result.Files[inIndex - 1];
  end;
end;

function CtrlEventHandler(inHandlerCallRef: EventHandlerCallRef;
                          inEvent: EventRef;
                          inUserData: Pointer): OSStatus; stdcall;
var
  myContext: CGContextRef;
  myRect: CGRect;
  rgn: RgnHandle;
  bool: longbool;
  proc: RegionToRectsUPP;
  err: OSStatus;
  Part: ControlPartCode;
  drag: DragRef;
  pasteboard: PasteboardRef;
  str: string;
  mouseP: MacOSAll.Point;
  P: TvgPoint;
  NewTarget: TvgVisualObject;
  Data: TvgDragObject;
  Accept: boolean;
begin
  Result := CallNextEventHandler(inHandlerCallRef, inEvent);
  Result := eventNotHandledErr;
  if GetEventClass(inEvent) = kEventClassControl then
  begin
    case GetEventKind(inEvent) of
      kEventControlDraw:
        begin
          GetEventParameter (inEvent, kEventParamCGContextRef, typeCGContextRef, nil, sizeof (CGContextRef), nil, @myContext);
          GetEventParameter (inEvent, kEventParamRgnHandle, typeQDRgnHandle, nil, sizeof(rgn), nil, @rgn);
          if rgn <> nil then
          begin
            proc := NewRegionToRectsUPP(@RegionToRectsCallback);
            { clear rects }
            SetLength(TvgCustomScene(inUserData).FUpdateRects, 0);
            err := QDRegionToRects(rgn, kQDParseRegionFromBottomRight, proc, inUserData);
            DisposeRegionToRectsUPP(proc);
          end;
          { draw }
          if TvgCustomScene(inUserData).Canvas.FBuffered then
            TvgCustomScene(inUserData).Canvas.SceneCtx := myContext
          else
            TvgCustomScene(inUserData).Canvas.CtxRef := myContext;
{          if TvgCustomScene(inUserData).Transparency then
            CGContextClearRect(myContext, CGRectFromRect(vgRect(0, 0, TvgCustomScene(inUserData).Width, TvgCustomScene(inUserData).Height)));}
          TvgCustomScene(inUserData).Draw;
          if TvgCustomScene(inUserData).Canvas.FBuffered then
            TvgCustomScene(inUserData).Canvas.SceneCtx := nil
          else
            TvgCustomScene(inUserData).Canvas.CtxRef := nil;
          Result := noErr;
        end;
      kEventControlDragEnter:
        begin
           bool := true;
           SetEventParameter(inEvent, kEventParamControlWouldAcceptDrop, typeBoolean, sizeof(bool), @bool);
           Result := noErr;
        end;
      kEventControlDragWithin:
        begin
          GetEventParameter(inEvent, kEventParamDragRef, typeDragRef, nil, sizeof(DragRef), nil, @drag);
          if drag <> nil then
          begin
            GetDragPasteboard(drag, pasteboard);
            if pasteboard <> nil then
            begin
              if TvgCustomScene(inUserData).Root = nil then Exit;

              Data := CreateFileURLFromPasteboard(pasteboard);

              GetDragMouse(drag, mouseP, mouseP);
              with TvgCustomScene(inUserData).ScreenToClient(Point(mouseP.h, mouseP.v)) do
                P := vgPoint(X, Y);
              NewTarget := TvgCustomScene(inUserData).Root.Visual.FindTarget(P, Data);
              if (NewTarget <> FTarget) then
              begin
                if FTarget <> nil then
                   FTarget.DragLeave;
                FTarget := NewTarget;
                if FTarget <> nil then
                begin
                  FTarget.DragEnter(Data, P);
                end;
              end;
              if FTarget = nil then
                 Accept := false;
            end;
          end;
          Result := noErr;
        end;
      kEventControlDragLeave:
        begin
          if FTarget <> nil then
            FTarget.DragLeave;
          FTarget := nil;
          Result := noErr;
        end;
      kEventControlDragReceive:
        begin
          GetEventParameter(inEvent, kEventParamDragRef, typeDragRef, nil, sizeof(DragRef), nil, @drag);
          if drag <> nil then
          begin
            GetDragPasteboard(drag, pasteboard);
            if pasteboard <> nil then
            begin
              if TvgCustomScene(inUserData).Root = nil then Exit;

              Data := CreateFileURLFromPasteboard(pasteboard);
              if FTarget <> nil then
                FTarget.DragDrop(Data, P);
            end;
          end;
          FTarget := nil;
          Result := noErr;
        end;
    end;
  end;
end;

var
  EventKinds: array [0..20] of EventTypeSpec;
  WndEventHandlerUPP: EventHandlerUPP;
{$ENDIF}

{$IFDEF WIN32}
function WndCallback(Ahwnd: HWND; uMsg: UINT; wParam: WParam; lParam: LParam):LRESULT; stdcall;
{$IFDEF FPC}
var
  Win: TWinControl;
{$ENDIF}
begin
  {$IFDEF FPC}
  Win := FindControl(Ahwnd);
  if (Win is TvgCustomScene) then
  begin
    if not (csDestroying in Win.ComponentState) then
    begin
      if (uMsg = WM_PAINT) or (uMsg = WM_ADDUPDATERECT) then
      begin
        Result := Win.Perform(uMsg, wParam, lParam);
        exit;
      end;
    end;
    result := CallWindowProcW(@TvgCustomScene(Win).PrevWndProc, Ahwnd, uMsg, WParam, LParam);
    Exit;
  end;
  {$ENDIF}
  result := CallWindowProcW(@DefWindowProcW, Ahwnd, uMsg, WParam, LParam);
end;
{$ENDIF}

type

  TvgAlignInfo = record
    AlignList: TList;
    ControlIndex: Integer;
    Align: TvgAlign;
    Scratch: Integer;
  end;

{ TvgBounds }

constructor TvgBounds.Create(const ADefaultValue: TvgRect);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
  Rect := FDefaultValue;
end;

procedure TvgBounds.Assign(Source: TPersistent);
begin
  if Source is TvgBounds then
  begin
    Rect := TvgBounds(Source).Rect;
  end
  else
    inherited
end;

function TvgBounds.GetRect: TvgRect;
begin
  Result := vgRect(FLeft, FTop, FRight, FBottom);
end;

function TvgBounds.MarginRect(const R: TvgRect): TvgRect;
begin
  Result := vgRect(R.Left + FLeft, R.Top + FTop, R.Right - FRight, R.Bottom - FBottom);
end;

function TvgBounds.PaddinRect(const R: TvgRect): TvgRect;
begin
  Result := vgRect(R.Left - FLeft, R.Top - FTop, R.Right + FRight, R.Bottom + FBottom);
end;

function TvgBounds.Width: single;
begin
  Result := vgRectWidth(Rect);
end;

function TvgBounds.Height: single;
begin
  Result := vgRectHeight(Rect);
end;

function TvgBounds.MarginEmpty: boolean;
begin
  Result := (FLeft = 0) and (FTop = 0) and (FRight = 0) and (FBottom = 0);
end;

function TvgBounds.Empty: boolean;
begin
  Result := vgIsRectEmpty(Rect)
end;

procedure TvgBounds.SetBottom(const Value: single);
begin
  if FBottom <> Value then
  begin
    FBottom := Value;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TvgBounds.SetLeft(const Value: single);
begin
  if FLeft <> Value then
  begin
    FLeft := Value;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TvgBounds.SetRight(const Value: single);
begin
  if FRight <> Value then
  begin
    FRight := Value;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TvgBounds.SetTop(const Value: single);
begin
  if FTop <> Value then
  begin
    FTop := Value;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TvgBounds.SetRect(const Value: TvgRect);
begin
  if (FLeft <> Value.Left) or (FTop <> Value.Top) or (FRight <> Value.Right) or (FBottom <> Value.Bottom) then
  begin
    FLeft := Value.Left;
    FTop := Value.Top;
    FRight := Value.Right;
    FBottom := Value.Bottom;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TvgBounds.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Rect', ReadRect, WriteRect, (FLeft <> DefaultValue.Left) or (FTop <> DefaultValue.Top) or
    (FRight <> DefaultValue.Right) or (FBottom <> DefaultValue.Bottom));
end;

procedure TvgBounds.ReadRect(Reader: TReader);
begin
  Rect := vgStringToRect(Reader.ReadString);
end;

procedure TvgBounds.WriteRect(Writer: TWriter);
begin
  Writer.WriteString(vgRectToString(Rect));
end;

{ TvgPosition }

constructor TvgPosition.Create(const ADefaultValue: TvgPoint);
begin
  inherited Create;
  FDefaultValue := ADefaultValue;
  FX := FDefaultValue.X;
  FY := FDefaultValue.Y;
end;

procedure TvgPosition.Assign(Source: TPersistent);
begin
  if Source is TvgPosition then
  begin
    Point := TvgPosition(Source).Point;
  end
  else
    inherited
end;

function TvgPosition.Empty: boolean;
begin
  Result := (FX = 0) and (FY = 0);
end;

procedure TvgPosition.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Point', ReadPoint, WritePoint, (FX <> DefaultValue.X) or (FY <> DefaultValue.Y));
end;

procedure TvgPosition.ReadPoint(Reader: TReader);
begin
  Point := vgStringToPoint(Reader.ReadString);
end;

procedure TvgPosition.WritePoint(Writer: TWriter);
begin
  Writer.WriteString(vgPointToString(Point));
end;

function TvgPosition.GetPoint: TvgPoint;
begin
  Result := vgPoint(Fx, Fy);
end;

procedure TvgPosition.SetPoint(const Value: TvgPoint);
begin
  Fx := Value.X;
  Fy := Value.Y;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TvgPosition.SetX(const Value: single);
begin
  if FX <> Value then
  begin
    FX := Value;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TvgPosition.SetY(const Value: single);
begin
  if FY <> Value then
  begin
    FY := Value;
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

{ TvgTransform ================================================================}

constructor TvgTransform.Create;
begin
  inherited ;
  FMatrix := IdentityMatrix;
  FPosition := TvgPosition.Create(vgPoint(0, 0));
  FPosition.OnChange := MatrixChanged;
  FScale := TvgPosition.Create(vgPoint(1, 1));
  FScale.OnChange := MatrixChanged;
  FSkew := TvgPosition.Create(vgPoint(0, 0));
  FSkew.OnChange := MatrixChanged;
  FRotateCenter := TvgPosition.Create(vgPoint(0.5, 0.5));
  FRotateCenter.OnChange := MatrixChanged;
end;

destructor TvgTransform.Destroy;
begin
  FRotateCenter.Free;
  FScale.Free;
  FSkew.Free;
  FPosition.Free;
  inherited ;
end;

procedure TvgTransform.Assign(Source: TPersistent);
begin
  if Source is TvgTransform then
  begin
    FPosition.FX := TvgTransform(Source).Position.FX;
    FPosition.FY := TvgTransform(Source).Position.FY;
    FScale.FX := TvgTransform(Source).Scale.FX;
    FScale.FY := TvgTransform(Source).Scale.FY;
    FSkew.FX := TvgTransform(Source).Skew.FX;
    FSkew.FY := TvgTransform(Source).Skew.FY;
    FRotateCenter.FX := TvgTransform(Source).RotateCenter.FX;
    FRotateCenter.FY := TvgTransform(Source).RotateCenter.FY;
    MatrixChanged(Self);
  end
  else
    inherited
end;

procedure TvgTransform.MatrixChanged(Sender: TObject);
begin
  FMatrix := IdentityMatrix;
  FMatrix.m31 := FPosition.X;
  FMatrix.m32 := FPosition.Y;
  FMatrix.m13 := FSkew.X;
  FMatrix.m23 := FSkew.Y;
  FMatrix.m11 := FScale.X;
  FMatrix.m22 := FScale.Y;
  if FRotateAngle <> 0 then
  begin
{    M1 := IdentityMatrix;
    M1.m31 := -FRotateCenter.X * FWidth;
    M1.m32 := -FRotateCenter.Y * FHeight;
    M2 := IdentityMatrix;
    M2.m31 := FRotateCenter.X * FWidth;
    M2.m32 := FRotateCenter.Y * FHeight;
    RotMatrix := vgMatrixMultiply(M1, vgMatrixMultiply(vgCreateRotationMatrix(vgDegToRad(FRotateAngle)), M2));
    FMatrix := vgMatrixMultiply(RotMatrix, FMatrix); }
    FMatrix := vgMatrixMultiply(vgCreateRotationMatrix(vgDegToRad(FRotateAngle)), FMatrix);
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

procedure TvgTransform.SetRotateAngle(const Value: single);
begin
  if FRotateAngle <> Value then
  begin
    FRotateAngle := Value;
  end;
end;

{ TvgVisual ===================================================================}

constructor TvgVisual.Create;
begin
  inherited ;
end;

procedure TvgVisual.Assign(Source: TPersistent);
begin
  if Source is TvgVisual then
  begin
    VisualObject := TvgVisual(Source).VisualObject;
  end
  else
    inherited;
end;

destructor TvgVisual.Destroy;
begin
  if FVisualObject <> nil then
  begin
    FVisualObject.RemoveFreeNotify(Self);
    FVisualObject := nil;
  end;
  inherited;
end;

procedure TvgVisual.SetVisualObject(const Value: TvgVisualObject);
begin
  if FVisualObject <> Value then
  begin
    if FVisualObject <> nil then
      FVisualObject.RemoveFreeNotify(Self);
    FVisualObject := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
    if FVisualObject <> nil then
      FVisualObject.AddFreeNotify(Self);
  end;
end;

function TvgPosition.GetVector: TvgVector;
begin
  Result := vgVector(FX, FY);
end;

procedure TvgPosition.SetVector(const Value: TvgVector);
begin
  Point := vgPoint(Value.X, Value.Y);
end;

procedure TvgPosition.Reflect(const Normal: TvgVector);
begin
  Vector := vgVectorReflect(Vector, Normal);
end;

{ TvgGradientPoint }

procedure TvgGradientPoint.Assign(Source: TPersistent);
begin
  if Source is TvgGradientPoint then
  begin
    FColor := TvgGradientPoint(Source).FColor;
    FOffset := TvgGradientPoint(Source).FOffset;
  end
  else
    inherited;
end;

{$IFDEF FPC}
constructor TvgGradientPoint.Create(ACollection: TCollection);
{$ELSE}
constructor TvgGradientPoint.Create(Collection: TCollection);
{$ENDIF}
begin
  inherited Create({$IFDEF FPC}ACollection{$ELSE}Collection{$ENDIF});
end;

function TvgGradientPoint.GetColor: string;
begin
  Result := vgColorToStr(FColor);
end;

procedure TvgGradientPoint.SetColor(const Value: string);
begin
  FColor := vgStrToColor(Value);
end;

{ TvgGradientPoints }

function TvgGradientPoints.GetPoint(Index: integer): TvgGradientPoint;
begin
  Result := TvgGradientPoint(Items[Index]);
end;

{ TvgGradient }

constructor TvgGradient.Create;
begin
  inherited ;
  FStartPosition := TvgPosition.Create(vgPoint(0, 0));
  FStartPosition.OnChange := PositionChanged;
  FStopPosition := TvgPosition.Create(vgPoint(0, 1));
  FStopPosition.OnChange := PositionChanged;
  FRadialTransform := TvgTransform.Create;
  FRadialTransform.OnChanged := PositionChanged;
  FPoints := TvgGradientPoints.Create(TvgGradientPoint);
  with TvgGradientPoint(FPoints.Add) do
  begin
    IntColor := $FF000000;
  end;
  with TvgGradientPoint(FPoints.Add) do
  begin
    IntColor := $FFFFFFFF;
    Offset := 1;
  end;
end;

procedure TvgGradient.Assign(Source: TPersistent);
var
  SaveChanged: TNotifyEvent;
begin
  if Source is TvgGradient then
  begin
    SaveChanged := FOnChanged;
    FOnChanged := nil;
    FPoints.Assign(TvgGradient(Source).FPoints);
    FStyle := TvgGradient(Source).Style;
    if FStyle = vgLinearGradient then
    begin
      FStopPosition.Assign(TvgGradient(Source).StopPosition);
      FStartPosition.Assign(TvgGradient(Source).StartPosition);
    end
    else
    begin
      FRadialTransform.Assign(TvgGradient(Source).RadialTransform);
    end;
    FOnChanged := SaveChanged;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end
  else
    inherited;
end;

destructor TvgGradient.Destroy;
begin
  FStartPosition.Free;
  FStopPosition.Free;
  FRadialTransform.Free;
  FPoints.Free;
  inherited;
end;

procedure TvgGradient.Change;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TvgGradient.InterpolateColor(Offset: single): TvgColor;
var
  i: integer;
begin
  if FPoints.Count > 1 then
  begin
    if Offset < 0 then
      Offset := 0;
    if Offset > 1 then
      Offset := 1;
    if Offset < FPoints[0].Offset then
    begin
      Result := Points[0].IntColor;
      Exit;
    end;
    if Offset > FPoints[FPoints.Count - 1].Offset then
    begin
      Result := FPoints[FPoints.Count - 1].IntColor;
      Exit;
    end;
    for i := 0 to FPoints.Count - 2 do
    begin
      if (Offset < Points[i].Offset) then Continue;
      if Points[i + 1].Offset - Points[i].Offset <= 0 then
        Result := Points[i].IntColor
      else
        if (i = FPoints.Count - 2) and (Offset > Points[Points.Count - 1].Offset) then // last
          Result := Points[Points.Count - 1].IntColor
        else
          Result := vgInterpolateColor(Points[i].IntColor, Points[i + 1].IntColor, (Offset - Points[i].Offset) / (Points[i + 1].Offset - Points[i].Offset));
    end;
  end
  else
    Result := 0;
end;

procedure TvgGradient.PositionChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TvgGradient.isLinearStored: Boolean;
begin
  Result := FStyle = vgLinearGradient;
end;

function TvgGradient.isRadialStored: Boolean;
begin
  Result := FStyle = vgRadialGradient;
end;

procedure TvgGradient.SetRadialTransform(const Value: TvgTransform);
begin
  FRadialTransform.Assign(Value);
end;

procedure TvgGradient.SetStartPosition(const Value: TvgPosition);
begin
  FStartPosition.Assign(Value);
end;

procedure TvgGradient.SetStopPosition(const Value: TvgPosition);
begin
  FStopPosition.Assign(Value);
end;

procedure TvgGradient.SetColor(const Value: string);
begin
  if FPoints.Count > 0 then
    Points[0].Color := Value;
end;

procedure TvgGradient.SetColor1(const Value: string);
begin
  if FPoints.Count > 1 then
    Points[1].Color := Value;
end;

procedure TvgGradient.SetStyle(const Value: TvgGradientStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

{ TvgBrushResource ==========================================================}

destructor TvgBrushResource.Destroy;
begin
  if FResource <> nil then
  begin
    FResource.RemoveFreeNotify(Self);
    FResource := nil;
  end;
  inherited;
end;

procedure TvgBrushResource.Assign(Source: TPersistent);
begin
  if Source is TvgBrushResource then
  begin
    Resource := TvgBrushResource(Source).Resource;
  end
  else
    inherited;
end;

procedure TvgBrushResource.SetResource(const Value: TvgBrushObject);
begin
  if FResource <> Value then
  begin
    if FResource <> nil then
      FResource.RemoveFreeNotify(Self);
    FResource := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
    if FResource <> nil then
      FResource.AddFreeNotify(Self);
  end;
end;

function TvgBrushResource.GetBrush: TvgBrush;
begin
  if FResource <> nil then
  begin
    Result := TvgBrushObject(FResource).Brush;
  end
  else
    Result := nil;
end;

{ TvgBrushBitmap }

constructor TvgBrushBitmap.Create;
begin
  inherited Create;
  FBitmap := TvgBitmap.Create(0, 0);
end;

destructor TvgBrushBitmap.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TvgBrushBitmap.Assign(Source: TPersistent);
begin
  if Source is TvgBrushBitmap then
  begin
    FWrapMode := TvgBrushBitmap(Source).FWrapMode;
    FBitmap.Assign(TvgBrushBitmap(Source).Bitmap);
    if Assigned(FOnChanged) then FOnChanged(Self);
  end
  else
    inherited;
end;

procedure TvgBrushBitmap.SetWrapMode(const Value: TvgWrapMode);
begin
  if FWrapMode <> Value then
  begin
    FWrapMode := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

{ TvgBrush ===================================================================}

constructor TvgBrush.Create;
begin
  inherited Create;
  FDefaultStyle := ADefaultStyle;
  FDefaultColor := ADefaultColor;
  FColor := ADefaultColor;
  FStyle := FDefaultStyle;
  FGradient := TvgGradient.Create;
  FGradient.OnChanged := GradientChanged;
  FVisual := TvgVisual.Create;
  FVisual.OnChanged := VisualChanged;
  FResource := TvgBrushResource.Create;
  FResource.OnChanged := ResourceChanged;
  FBitmap := TvgBrushBitmap.Create;
  FBitmap.OnChanged := BitmapChanged;
end;

destructor TvgBrush.Destroy;
begin
  FBitmap.Free;
  FVisual.Free;
  FResource.Free;
  FGradient.Free;
  inherited;
end;

procedure TvgBrush.Assign(Source: TPersistent);
var
  SaveChange: TNotifyEvent;
begin
  if Source is TvgBrush then
  begin
    SaveChange := FOnChanged;
    FOnChanged := nil;
    FDefaultStyle := (Source as TvgBrush).FDefaultStyle;
    FDefaultColor := (Source as TvgBrush).FDefaultColor;
    FColor := (Source as TvgBrush).SolidColor;
    FStyle := (Source as TvgBrush).Style;
    case FStyle of
      vgBrushGradient: FGradient.Assign((Source as TvgBrush).Gradient);
      vgBrushResource: FResource.Assign((Source as TvgBrush).Resource);
      vgBrushVisual: FVisual.Assign((Source as TvgBrush).Visual);
      vgBrushBitmap: FBitmap.Assign((Source as TvgBrush).Bitmap);
    end;
    FOnChanged := SaveChange;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end
  else
    inherited;
end;

procedure TvgBrush.GradientChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TvgBrush.VisualChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TvgBrush.ResourceChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

procedure TvgBrush.BitmapChanged(Sender: TObject);
begin
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

function TvgBrush.isBitmapStored: Boolean;
begin
  Result := (FStyle = vgBrushBitmap);
end;

function TvgBrush.isColorStored: Boolean;
begin
  Result := (FStyle = vgBrushSolid) and (FColor <> FDefaultColor);
end;

function TvgBrush.isGradientStored: Boolean;
begin
  Result := FStyle = vgBrushGradient;
end;

function TvgBrush.isStyleStored: Boolean;
begin
  Result := FStyle <> FDefaultStyle;
end;

function TvgBrush.isVisualStored: Boolean;
begin
  Result := FStyle = vgBrushVisual;
end;

function TvgBrush.isResourceStored: Boolean;
begin
  Result := FStyle = vgBrushResource;
end;

procedure TvgBrush.SetResource(const Value: TvgBrushResource);
begin
  FResource := Value;
end;

procedure TvgBrush.SetGradient(const Value: TvgGradient);
begin
  FGradient := Value;
end;

procedure TvgBrush.SetVisual(const Value: TvgVisual);
begin
  FVisual := Value;
end;

function TvgBrush.GetColor: string;
begin
  Result := vgColorToStr(FColor);
end;

procedure TvgBrush.SetColor(const Value: string);
begin
  SolidColor := vgStrToColor(Value);
  if FStyle = vgBrushGradient then
    FGradient.Color := Value;
end;

function TvgBrush.GetSolidColor: TvgColor;
begin
  Result := FColor;
  if (Style = vgBrushResource) and (Resource <> nil) and (Resource.Brush <> nil) then
    Result := Resource.Brush.SolidColor;
end;

procedure TvgBrush.SetSolidColor(const Value: TvgColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

procedure TvgBrush.SetStyle(const Value: TvgBrushStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

{ TvgFont =====================================================================}

constructor TvgFont.Create;
begin
  inherited ;
  FSize := 11;
  FFamily := 'Tahoma';
end;

destructor TvgFont.Destroy;
begin
  inherited;
end;

procedure TvgFont.Assign(Source: TPersistent);
begin
  if Source is TvgFont then
  begin
    FFamily := (Source as TvgFont).Family;
    FSize := (Source as TvgFont).Size;
    FStyle := (Source as TvgFont).Style;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end
  else
    inherited;
end;

function TvgFont.isSizeStored: Boolean;
begin
  Result := FSize <> 11;
end;

function TvgFont.isFamilyStored: Boolean;
begin
  Result := FFamily <> 'Tahoma';
end;

procedure TvgFont.SetFamily(const Value: string);
begin
  if FFamily <> Value then
  begin
    FFamily := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

procedure TvgFont.SetSize(const Value: single);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

procedure TvgFont.SetStyle(const Value: TvgFontStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    if Assigned(FOnChanged) then FOnChanged(Self);
  end;
end;

class function TvgFilter.GetFileTypes: string;
begin
  Result := '';
end;

class function TvgFilter.GetImageSize(const AFileName: string): TvgPoint;
begin
  Result := vgPoint(0, 0);
end;

{ TvgBitmap ===================================================================}

constructor TvgBitmap.Create(const AWidth, AHeight: integer; const APremulAlpha: boolean = true);
begin
  inherited Create;
  FWidth := AWidth;
  FHeight := AHeight;
  Recreate;
end;

constructor TvgBitmap.CreateFromStream(const AStream: TStream);
begin
  inherited Create;
  LoadFromStream(AStream);
end;

destructor TvgBitmap.Destroy;
begin
  if Assigned(FCanvas) then
    FreeAndNil(FCanvas);
  if Assigned(FOnDestroyHandle) then
    FOnDestroyHandle(Self);
  Handle := 0;
  if FBits <> nil then
    FreeMem(FBits, FWidth * FHeight * SizeOf(TvgColor));
  inherited;
end;

procedure TvgBitmap.SetSize(const AWidth, AHeight: integer);
begin
  if (FWidth <> AWidth) or (FHeight <> AHeight) then
  begin
    if FBits <> nil then
      FreeMem(FBits, FWidth * FHeight * SizeOf(TvgColor));
    FBits := nil;
    FWidth := AWidth;
    FHeight := AHeight;
    Recreate;
  end;
end;

procedure TvgBitmap.Recreate;
begin
  if Assigned(FOnBitmapDestroy) then
    FOnBitmapDestroy(Self);
  if Assigned(FOnDestroyHandle) then
    FOnDestroyHandle(Self);
  FHandle := 0;
  if Assigned(FCanvas) then
    FreeAndNil(FCanvas);
  FCanvas := nil;
  if FBits <> nil then
    FreeMem(FBits);
  { not empty - we can't create empty bitmap }
  if FWidth < 1 then FWidth := 1;
  if FHeight < 1 then FHeight := 1;
  { create and clear }
  GetMem(FBits, FWidth * FHeight * SizeOf(TvgColor));
  Clear(0);
  FNeedUpdate := true;
  if Assigned(FOnBitmapCreate) then
    FOnBitmapCreate(Self);
end;

procedure TvgBitmap.Clear(const AColor: TvgColor);
begin
  if FBits <> nil then
  begin
    FillLongword(FBits, FWidth * FHeight, vgPremultyAlpha(AColor));
    FNeedUpdate := true;
  end;
end;

procedure TvgBitmap.ClearRect(const ARect: TvgRect;
  const AColor: TvgColor);
var
  R: TRect;
begin
  if FBits <> nil then
  begin
    R := Rect(Trunc(ARect.Left), Trunc(ARect.Top), Trunc(ARect.Right) - 1, Trunc(ARect.Bottom) - 1);
    if R.Left < 0 then R.Left := 0;
    if R.Top < 0 then R.Top := 0;
    if R.Right > FWidth - 1 then R.Right := FWidth - 1;
    if R.Bottom > FHeight - 1 then R.Bottom := FHeight - 1;
    if R.Bottom < R.Top then R.Bottom := R.Top;
    if R.Right < R.Left then R.Right := R.Left;
    if (R.Right < 0) or (R.Top < 0) or (R.Left > FWidth - 1) or (R.Top > FHeight - 1) then Exit;
    FillLongwordRect(FBits, FWidth, FHeight, R.Left, R.Top, R.Right, R.Bottom, vgPremultyAlpha(AColor));
    FNeedUpdate := true;
  end;
end;

function TvgBitmap.GetPixels(x, y: integer): TvgColor;
begin
  if (x >= 0) and (y >= 0) and (x < FWidth) and (y < FHeight) and (FBits <> nil) then
    Result := FBits[x + (y * FHeight)]
  else
    Result := 0;
end;

procedure TvgBitmap.BitmapChanged;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TvgBitmap.DrawGraphic(const Graphic: TGraphic; const DstRect: TRect);
var
  Bitmap: TBitmap;
  SL: PvgColorArray;
  i, j: integer;
begin
  {$IFNDEF FPC}
  { Create DIB copy }
  Bitmap := TBitmap.Create;
  try
    Bitmap.HandleType := bmDIB;
    Bitmap.PixelFormat := pf32bit;
    Bitmap.Width := FWidth;
    Bitmap.Height := FHeight;
    Bitmap.Canvas.Brush.Color := RGB(255, 0, 255);
    Bitmap.Canvas.StretchDraw(DstRect, Graphic);
    { Copy to bitmap }
    for j := 0 to FHeight - 1 do
    begin
      SL := Bitmap.Scanline[j];
      for i := 0 to FWidth - 1 do
        if (TvgColorRec(SL[i]).R = $FF) and (TvgColorRec(SL[i]).G = 0) and (TvgColorRec(SL[i]).B = $FF) then
          Continue
        else
          FBits[i + (j * Width)] := SL[i];
    end;
  finally
    Bitmap.Free;
  end;
  {$ENDIF}
end;

procedure TvgBitmap.Assign(Source: TPersistent);
var
  SLine: PvgColorArray;
  SLine24: PvgColor24Array;
  i, j: integer;
begin
  if Source is TvgBitmap then
  begin
    SetSize(TvgBitmap(Source).Width, TvgBitmap(Source).Height);
    MoveLongword(TvgBitmap(Source).FBits, FBits, Width * Height);
    FNeedUpdate := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end
  else
  {$IFNDEF FPC}
    if (Source is TBitmap) and ((Source as TBitmap).PixelFormat = pf32bit) and ((Source as TBitmap).HandleType = bmDIB) then
    with (Source as TBitmap) do
    begin
      Self.SetSize(Width, Height);
      for j := 0 to Height - 1 do
      begin
        SLine := Scanline[j];
        for i := 0 to Width - 1 do
        begin
          if Transparent and (SLine^[i] = ColorToRGB(TransparentColor)) then
            FBits[i + (j * Width)] := 0
          else
            FBits[i + (j * Width)] := SLine^[i];
        end;
      end;
      FNeedUpdate := true;
      if Assigned(FOnChange) then FOnChange(Self);
    end
    else
    if (Source is TBitmap) and ((Source as TBitmap).PixelFormat = pf24bit) then
    with (Source as TBitmap) do
    begin
      Self.SetSize(Width, Height);
      for j := 0 to Height - 1 do
      begin
        SLine24 := Scanline[j];
        for i := 0 to Width - 1 do
        begin
          if Transparent and (RGB(SLine24^[i].R, SLine24^[i].G, SLine24^[i].B) = ColorToRGB(TransparentColor)) then
            FBits[i + (j * Width)] := 0
          else
            FBits[i + (j * Width)] := RGB(SLine24^[i].R, SLine24^[i].G, SLine24^[i].B);
        end;
      end;
      FNeedUpdate := true;
      if Assigned(FOnChange) then FOnChange(Self);
    end
    else
      if Source is TGraphic then
      begin
        SetSize(TGraphic(Source).Width, TGraphic(Source).Height);
        if FBits = nil then Exit;
        DrawGraphic(TGraphic(Source), Rect(0, 0, FWidth, FHeight));
        FillAlpha(FBits, FWidth * FHeight, $FF);
        FNeedUpdate := true;
        if Assigned(FOnChange) then FOnChange(Self);
      end
      else
        if Source is TPicture then
        begin
          with TPicture(Source) do
          begin
            // icons, metafiles etc...
            Self.SetSize(TPicture(Source).Graphic.Width, TPicture(Source).Graphic.Height);
            if FBits = nil then Exit;
            DrawGraphic(TPicture(Source).Graphic, Rect(0, 0, FWidth, FHeight));
            FillAlpha(FBits, FWidth * FHeight, $FF);
            FNeedUpdate := true;
            if Assigned(FOnChange) then FOnChange(Self);
          end;
        end
        else
  {$ENDIF}
  { inherited }
          inherited;
end;

procedure TvgBitmap.AssignTo(Dest: TPersistent);
var
  i: integer;
  B: TBitmap;
begin
  {$IFNDEF FPC}
  if Dest is TPicture then
  begin
    B := TBitmap.Create;
    B.HandleType := bmDIB;
    B.PixelFormat := pf32bit;
    B.Width := FWidth;
    B.Height := FHeight;
    for i := 0 to FHeight - 1 do
      System.Move(Scanline[i]^, B.Scanline[i]^, Width * 4);
    TPicture(Dest).Assign(B);
    B.Free;
  end
  else
  if Dest is TBitmap then
  begin
    TBitmap(Dest).HandleType := bmDIB;
    TBitmap(Dest).PixelFormat := pf32bit;
    TBitmap(Dest).Width := FWidth;
    TBitmap(Dest).Height := FHeight;
    for i := 0 to FHeight - 1 do
      System.Move(Scanline[i]^, TBitmap(Dest).Scanline[i]^, Width * 4);
  end
  else
  {$ENDIF}
    inherited ;
end;

procedure TvgBitmap.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('PNG', ReadBitmap, WriteBitmap, FWidth * FHeight > 0);
end;

procedure TvgBitmap.ReadBitmap(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TvgBitmap.WriteBitmap(Stream: TStream);
begin
  SaveToStream(Stream);
end;

procedure TvgBitmap.DoLoaded(Sender: TObject);
begin
  if Assigned(FOnThreadLoaded) then
    FOnThreadLoaded(Self);
end;

procedure TvgBitmap.Rotate(const Angle: single);
var
  temp: TvgBitmap;
  M, M2: TvgMatrix;
  Pts: array [1..4] of TvgPoint;
  R: TvgRect;
begin
  if Angle = 0 then Exit;

  M := IdentityMatrix;
  M.m31 := -FWidth / 2;
  M.m32 := -FHeight / 2;
  M := vgMatrixMultiply(M, vgCreateRotationMatrix(vgDegToRad(Angle)));
  { calc new size }
  Pts[1] := vgPointFromVector(vgVectorTransform(vgVector(0, 0), M));
  Pts[2] := vgPointFromVector(vgVectorTransform(vgVector(FWidth, 0), M));
  Pts[3] := vgPointFromVector(vgVectorTransform(vgVector(FWidth, FHeight), M));
  Pts[4] := vgPointFromVector(vgVectorTransform(vgVector(0, FHeight), M));
  R := vgNormalizeRect(Pts);
  { translate }
  M2 := IdentityMatrix;
  M2.m31 := vgRectWidth(R) / 2;
  M2.m32 := vgRectHeight(R) / 2;
  M := vgMatrixMultiply(M, M2);
  { rotate }
  temp := TvgBitmap.Create(Trunc(vgRectWidth(R)), Trunc(vgRectHeight(R)));
  temp.Clear(0);
  temp.Canvas.SetMatrix(M);
  temp.Canvas.DrawBitmap(Self, vgRect(0, 0, FWidth, FHeight), vgRect(0, 0, FWidth, FHeight), 1);
  Assign(temp);
  temp.Free;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TvgBitmap.FlipHorizontal;
var
  i: integer;
  tmp: PvgColorArray;
begin
  GetMem(tmp, Width * 4);
  for i := 0 to (Height - 1) div 2 do
  begin
    System.Move(Scanline[Height - 1 - i][0], tmp[0], Width * 4);
    System.Move(Scanline[i][0], Scanline[Height - 1 - i][0], Width * 4);
    System.Move(tmp[0], Scanline[i][0], Width * 4);
  end;
  if Assigned(FOnChange) then FOnChange(Self);
  FreeMem(tmp, Width * 4);
end;

procedure TvgBitmap.FlipVertical;
var
  i, j: integer;
  tmp: TvgColor;
begin
  for j := 0 to Height - 1 do
    for i := 0 to (Width - 1) div 2 do
    begin
      tmp := Scanline[j][Width - 1 - i];
      Scanline[j][Width - 1 - i] := Scanline[j][i];
      Scanline[j][i] := tmp;
    end;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TvgBitmap.InvertAlpha;
var
  i, j: integer;
  Bits: PvgColorRecArray;
begin
  Bits := PvgColorRecArray(Startline);
  for j := 0 to Height - 1 do
    for i := 0 to Width - 1 do
    begin
      Bits[i + (j * Width)].Color := vgUnpremultyAlpha(Bits[i + (j * Width)].Color);
      TvgColorRec(Bits[(i) + ((j) * Width)]).A := $FF - TvgColorRec(Bits[(i) + ((j) * Width)]).A;
      Bits[i + (j * Width)].Color := vgPremultyAlpha(Bits[i + (j * Width)].Color);
    end;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TvgBitmap.FillColor(const Color: TvgColor);
var
  Bits: PvgColorRecArray;
  i, j: integer;
  a: byte;
begin
  Bits := PvgColorRecArray(Startline);
  for j := 0 to Height - 1 do
  begin
    for i := 0 to Width - 1 do
    begin
      {$ifdef FPC_BIG_ENDIAN}
      a := TvgColorRec(Bits[(i) + ((j) * Width)]).Color and $FF;
      {$else}
      a := TvgColorRec(Bits[(i) + ((j) * Width)]).A;
      {$endif}
      if a > 0 then
      begin
        bits[(i) + ((j) * Width)].Color := vgPremultyAlpha(vgOpacity(Color, a / $FF));
        {$ifdef FPC_BIG_ENDIAN}
        ReverseBytes(@Bits[(i) + ((j) * Width)].Color, 4);
        {$endif}
      end;
    end
  end;
  if Assigned(FOnChange) then FOnChange(Self);
  FNeedUpdate := true;
end;

function TvgBitmap.CreateMask: PByteArray;
var
  a: byte;
  Bits: PvgColorRecArray;
  i, j: integer;
begin
  GetMem(Result, Width * Height);
  FillChar(Result^, Width * Height, 0);
  Bits := PvgColorRecArray(Startline);
  for j := 0 to Height - 1 do
  begin
    for i := 0 to Width - 1 do
    begin
      {$ifdef FPC_BIG_ENDIAN}
      a := TvgColorRec(Bits[(i) + ((j) * Width)]).Color and $FF;
      {$else}
      a := TvgColorRec(Bits[(i) + ((j) * Width)]).A;
      {$endif}
      if a > 0 then
      begin
        Result[i + (j * Width)] := a;
      end;
    end
  end;
end;

procedure TvgBitmap.ApplyMask(const Mask: PByteArray; const DstX: integer = 0; const DstY: integer = 0);
var
  Bits: PvgColorRecArray;
  i, j: integer;
begin
  Bits := PvgColorRecArray(Startline);
  for j := 0 to Height - 1 do
  begin
    for i := 0 to Width - 1 do
    begin
      if (i - DstX < 0) or (i - DstX > Width - 1) or (j - DstY < 0) or (j - DstY > Height - 1) then Continue;

      if mask[i - DstX + ((j - DstY) * Width)] > 0 then
      begin
        Bits[i + (j * Width)].Color := vgPremultyAlpha(vgOpacity(vgUnpremultyAlpha(Bits[i + (j * Width)].Color), ($FF - mask[i - DstX + ((j - DstY) * Width)]) / $FF))
      end;
      {$ifdef FPC_BIG_ENDIAN}
      ReverseBytes(@Bits[(i) + ((j) * Width)].Color, 4);
      {$endif}
    end
  end;
  if Assigned(FOnChange) then FOnChange(Self);
  FNeedUpdate := true;
end;

function TvgBitmap.CreateThumbnail(const Width, Height: integer): TvgBitmap;
begin
  Result := TvgBitmap.Create(Width, Height);
  Result.Canvas.DrawThumbnail(Self, Width, Height);
end;

procedure TvgBitmap.LoadFromFile(const AFileName: string; const Rotate: single = 0);
var
  Filter: TvgFilter;
begin
  Filter := DefaultFilterClass.Create;
  if Filter.LoadFromFile(AFileName, Rotate, Self) then
    if Assigned(FOnChange) then FOnChange(Self);
  Filter.Free;
end;

procedure TvgBitmap.LoadThumbnailFromFile(const AFileName: string; const AFitWidth, AFitHeight: single;
  const UseEmbedded: boolean = true);
var
  Filter: TvgFilter;
begin
  Filter := DefaultFilterClass.Create;
  if Filter.LoadThumbnailFromFile(AFileName, AFitWidth, AFitHeight, UseEmbedded, Self) then
    if Assigned(FOnChange) then FOnChange(Self);
  Filter.Free;
end;

procedure TvgBitmap.SaveToFile(const AFileName: string; const Params: string = '');
var
  Filter: TvgFilter;
begin
  Filter := DefaultFilterClass.Create;
  Filter.SaveToFile(AFileName, Self, Params);
  Filter.Free;
end;

procedure TvgBitmap.LoadFromStream(const AStream: TStream);
var
  Filter: TvgFilter;
begin
  Filter := DefaultFilterClass.Create;
  if Filter.LoadFromStream(AStream, Self) then
    if Assigned(FOnChange) then FOnChange(Self);
  Filter.Free;
end;

procedure TvgBitmap.SaveToStream(const AStream: TStream);
var
  i: integer;
  hasAlpha: boolean;
  Filter: TvgFilter;
begin
  { check alpha }
  hasAlpha := false;
  for i := 0 to FWidth * FHeight - 1 do
    if FBits[i] and $FF000000 <> $FF000000 then
    begin
      hasAlpha := true;
      Break;
    end;

  if hasAlpha then
  begin
    Filter := DefaultFilterClass.Create;
    Filter.SaveToStream(AStream, Self, 'png');
    Filter.Free;
  end
  else
  begin
    Filter := DefaultFilterClass.Create;
    Filter.SaveToStream(AStream, Self, 'jpeg', 'quality=100');
    Filter.Free;
  end;
end;

procedure TvgBitmap.SetHeight(const Value: integer);
begin
  if FHeight <> Value then
  begin
    FHeight := Value;
    Recreate;
  end;
end;

procedure TvgBitmap.SetWidth(const Value: integer);
begin
  if FWidth <> Value then
  begin
    FWidth := Value;
    Recreate;
  end;
end;

function TvgBitmap.GetCanvas: TvgCanvas;
begin
  if FCanvas = nil then
  begin
    FCanvas := DefaultCanvasClass.CreateFromBitmap(Self);
    FCanvas.SetMatrix(IdentityMatrix);
  end;
  Result := FCanvas;
end;

function TvgBitmap.GetScanline(y: integer): PvgColorArray;
begin
  Result := DefaultCanvasClass.GetBitmapScanline(Self, y);
end;

{ TvgPath =====================================================================}

constructor TvgPathData.Create;
begin
  inherited Create;
end;

destructor TvgPathData.Destroy;
begin
  if (Handle <> 0) and Assigned(FOnDestroyHandle) then
    FOnDestroyHandle(Self);
  Handle := 0;
  inherited;
end;

procedure TvgPathData.Assign(Source: TPersistent);
begin
  if Source is TvgPathData then
  begin
    SetLength(PathData, Length(TvgPathData(Source).PathData));
    System.Move(TvgPathData(Source).PathData[0], PathData[0], SizeOf(TvgPathPoint) * Length(PathData));
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end
  else
    inherited
end;

procedure TvgPathData.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Path', ReadPath, WritePath, Length(PathData) > 0);
end;

procedure TvgPathData.ReadPath(Stream: TStream);
var
  S: longint;
begin
  Stream.Read(S, SizeOf(S));
  SetLength(PathData, S);
  if S > 0 then
  begin
    Stream.Read(PathData[0], S * SizeOf(TvgPathPoint));
  end;
end;

procedure TvgPathData.WritePath(Stream: TStream);
var
  S: longint;
begin
  S := Length(PathData);
  Stream.Write(S, SizeOf(S));
  if S > 0 then
    Stream.Write(PathData[0], S * SizeOf(TvgPathPoint));
end;

function TvgPathData.LastPoint: TvgPoint;
begin
  if Length(PathData) > 0 then
    Result := PathData[High(PathData)].Point
  else
    Result := vgPoint(0, 0);
end;

procedure TvgPathData.MoveTo(const P: TvgPoint);
begin
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointMoveTo;
  PathData[High(PathData)].Point := P;
end;

procedure TvgPathData.MoveToRel(const P: TvgPoint);
begin
  with LastPoint do
  begin
    SetLength(PathData, Length(PathData) + 1);
    PathData[High(PathData)].Kind := vgPathPointMoveTo;
    PathData[High(PathData)].Point := vgPoint(x + P.x, y + P.y);
  end;
end;

procedure TvgPathData.LineTo(const P: TvgPoint);
begin
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointLineTo;
  PathData[High(PathData)].Point := P;
end;

procedure TvgPathData.LineToRel(const P: TvgPoint);
begin
  with LastPoint do
  begin
    SetLength(PathData, Length(PathData) + 1);
    PathData[High(PathData)].Kind := vgPathPointLineTo;
    PathData[High(PathData)].Point := vgPoint(x + P.x, y + P.y);
  end;
end;

procedure TvgPathData.HLineTo(const x: single);
begin
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointLineTo;
  PathData[High(PathData)].Point := vgPoint(x, PathData[High(PathData) - 1].Point.y);
end;

procedure TvgPathData.HLineToRel(const x: single);
var
  LP: TvgPoint;
begin
  LP := LastPoint;
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointLineTo;
  PathData[High(PathData)].Point := vgPoint(LP.x + x, LP.y);
end;

procedure TvgPathData.VLineTo(const y: single);
begin
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointLineTo;
  PathData[High(PathData)].Point := vgPoint(PathData[High(PathData) - 1].Point.x, y);
end;

procedure TvgPathData.VLineToRel(const y: single);
var
  LP: TvgPoint;
begin
  LP := LastPoint;
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointLineTo;
  PathData[High(PathData)].Point := vgPoint(LP.x, LP.y + y);
end;

procedure TvgPathData.CurveTo(const ControlPoint1, ControlPoint2, EndPoint: TvgPoint);
begin
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointCurveTo;
  PathData[High(PathData)].Point := ControlPoint1;
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointCurveTo;
  PathData[High(PathData)].Point := ControlPoint2;
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointCurveTo;
  PathData[High(PathData)].Point := EndPoint;
end;

procedure TvgPathData.CurveToRel(const ControlPoint1, ControlPoint2, EndPoint: TvgPoint);
begin
  with LastPoint do
  begin
    SetLength(PathData, Length(PathData) + 1);
    PathData[High(PathData)].Kind := vgPathPointCurveTo;
    PathData[High(PathData)].Point := vgPoint(x + ControlPoint1.x, y + ControlPoint1.y);
    SetLength(PathData, Length(PathData) + 1);
    PathData[High(PathData)].Kind := vgPathPointCurveTo;
    PathData[High(PathData)].Point := vgPoint(x + ControlPoint2.x, y + ControlPoint2.y);;
    SetLength(PathData, Length(PathData) + 1);
    PathData[High(PathData)].Kind := vgPathPointCurveTo;
    PathData[High(PathData)].Point := vgPoint(x + EndPoint.x, y + EndPoint.y);;
  end;
end;

procedure TvgPathData.SmoothCurveTo(const ControlPoint2, EndPoint: TvgPoint);
var
  ControlPoint1: TvgPoint;
begin
  if Length(PathData) > 2 then
  begin
    ControlPoint1.x := LastPoint.x + (LastPoint.x - PathData[High(PathData)].Point.x);
    ControlPoint1.y := LastPoint.y + (LastPoint.y - PathData[High(PathData)].Point.y);
  end
  else
    ControlPoint1 := ControlPoint2;
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointCurveTo;
  PathData[High(PathData)].Point := ControlPoint1;
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointCurveTo;
  PathData[High(PathData)].Point := ControlPoint2;
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointCurveTo;
  PathData[High(PathData)].Point := EndPoint;
end;

procedure TvgPathData.SmoothCurveToRel(const ControlPoint2, EndPoint: TvgPoint);
var
  ControlPoint1: TvgPoint;
begin
  if Length(PathData) > 2 then
  begin
    ControlPoint1.x := LastPoint.x + (LastPoint.x - PathData[High(PathData)].Point.x);
    ControlPoint1.y := LastPoint.y + (LastPoint.y - PathData[High(PathData)].Point.y);
  end
  else
    ControlPoint1 := ControlPoint2;
  with LastPoint do
  begin
    SetLength(PathData, Length(PathData) + 1);
    PathData[High(PathData)].Kind := vgPathPointCurveTo;
    PathData[High(PathData)].Point := vgPoint(ControlPoint1.x, ControlPoint1.y);
    SetLength(PathData, Length(PathData) + 1);
    PathData[High(PathData)].Kind := vgPathPointCurveTo;
    PathData[High(PathData)].Point := vgPoint(x + ControlPoint2.x, y + ControlPoint2.y);;
    SetLength(PathData, Length(PathData) + 1);
    PathData[High(PathData)].Kind := vgPathPointCurveTo;
    PathData[High(PathData)].Point := vgPoint(x + EndPoint.x, y + EndPoint.y);;
  end;
end;

procedure TvgPathData.ClosePath;
begin
  SetLength(PathData, Length(PathData) + 1);
  PathData[High(PathData)].Kind := vgPathPointClose;
end;

procedure TvgPathData.Clear;
begin
  SetLength(PathData, 0);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TvgPathData.GetBounds: TvgRect;
var
  i: integer;
begin
  if Length(PathData) = 0 then
  begin
    Result := vgRect(0, 0, 0, 0);
    Exit;
  end;
  Result := vgRect($FFFF, $FFFF, -$FFFF, -$FFFF);
  for i := 0 to High(PathData) do
  begin
    if PathData[i].Kind = vgPathPointClose then Continue;

    if PathData[i].Point.X < Result.Left then Result.Left := PathData[i].Point.X;
    if PathData[i].Point.X > Result.Right then Result.Right := PathData[i].Point.X;
    if PathData[i].Point.Y < Result.Top then Result.Top := PathData[i].Point.Y;
    if PathData[i].Point.Y > Result.Bottom then Result.Bottom := PathData[i].Point.Y;
  end;
end;

procedure TvgPathData.Scale(const scaleX, scaleY: single);
var
  i: integer;
begin
  if Length(PathData) > 0 then
  begin
    for i := 0 to High(PathData) do
      case PathData[i].Kind of
        vgPathPointMoveTo, vgPathPointLineTo, vgPathPointCurveTo:
          begin
            PathData[i].Point.x := PathData[i].Point.x * scaleX;
            PathData[i].Point.y := PathData[i].Point.y * scaleY;
          end;
        vgPathPointClose:
          begin
          end;
      end;
  end;
end;

procedure TvgPathData.Offset(const dX, dY: single);
var
  i: integer;
begin
  if Length(PathData) > 0 then
  begin
    for i := 0 to High(PathData) do
      case PathData[i].Kind of
        vgPathPointMoveTo, vgPathPointLineTo, vgPathPointCurveTo:
          begin
            PathData[i].Point.x := PathData[i].Point.x + dX;
            PathData[i].Point.y := PathData[i].Point.y + dY;
          end;
        vgPathPointClose:
          begin
          end;
      end;
  end;
end;

procedure TvgPathData.ApplyMatrix(const M: TvgMatrix);
var
  i: integer;
begin
  if Length(PathData) > 0 then
  begin
    for i := 0 to High(PathData) do
      case PathData[i].Kind of
        vgPathPointMoveTo, vgPathPointLineTo, vgPathPointCurveTo:
          begin
            with vgVectorTransform(vgVector(PathData[i].Point), M) do
              PathData[i].Point := vgPoint(x, y);
          end;
        vgPathPointClose:
          begin
          end;
      end;
  end;
end;

procedure TvgPathData.Flatten(const Flatness: single = 0.25);

  procedure CalculateBezierCoefficients(const Bezier: TvgCubicBezier; out ax,bx,cx,ay,by,cy: single);
  begin
    cx := 3.0 * (Bezier[1].x - Bezier[0].x);
    cy := 3.0 * (Bezier[1].y - Bezier[0].y);
    bx := 3.0 * (Bezier[2].x - Bezier[1].x) - cx;
    by := 3.0 * (Bezier[2].y - Bezier[1].y) - cy;
    ax := Bezier[3].x - Bezier[0].x - cx - bx;
    ay := Bezier[3].y - Bezier[0].y - cy - by;
  end;

  function PointOnBezier(const StartPoint: TvgPoint; const ax,bx,cx,ay,by,cy,T: single): TvgPoint;
  var
    tSqr  : single;
    tCube : single;
  begin
    tSqr     := t * t;
    tCube    := tSqr * t;
    Result.x := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.x;
    Result.y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.y;
  end;

  function CreateBezier(const Bezier: TvgCubicBezier; const PointCount: integer): TvgPolygon;
  var
    ax : single;
    bx : single;
    cx : single;
    ay : single;
    by : single;
    cy : single;
    dT : single;
    T  : single;
    i  : Integer;
  begin
    if PointCount = 0 then exit;
    dT := 1.0 / (1.0 * PointCount - 1.0);
    T  := 0.0;
    SetLength(Result, PointCount);
    CalculateBezierCoefficients(Bezier, ax, bx, cx, ay, by, cy);
    for i := 0 to PointCount - 1 do
    begin
      Result[i] := PointOnBezier(Bezier[0], ax, bx, cx, ay, by, cy, T);
      T := T + dT;
    end;
  end;

var
  i, j: integer;
  BPts: TvgPolygon;
  B: TvgCubicBezier;
  Len: single;
  SegCount: integer;
  OldPathData: array of TvgPathPoint;
  CurPoint: TvgPoint;
  f, s: single;
  Bounds, R: TvgRect;
begin
  { scale }
  if Length(PathData) > 0 then
  begin
    Bounds := GetBounds;
    R := Bounds;
    vgFitRect(R, vgRect(0, 0, 100, 100));
    s := MinFloat(vgRectWidth(Bounds) / 100, vgRectHeight(Bounds) / 100);
    f := Flatness * s;
    if f < 0.25 then f := 0.25;

    { copy data }
    SetLength(OldPathData, Length(PathData));
    System.Move(PathData[0], OldPathData[0], Length(PathData) * SizeOf(PathData[0]));
    SetLength(PathData, 0);

    i := 0;
    while i < Length(OldPathData) do
    begin
      case OldPathData[i].Kind of
        vgPathPointMoveTo:
          begin
            MoveTo(OldPathData[i].Point);
            CurPoint := OldPathData[i].Point;
          end;
        vgPathPointLineTo:
          begin
            LineTo(OldPathData[i].Point);
            CurPoint := OldPathData[i].Point;
          end;
        vgPathPointCurveTo:
          begin
            B[0] := CurPoint;
            B[1] := OldPathData[i].Point;
            Inc(i);
            B[2] := OldPathData[i].Point;
            Inc(i);
            B[3] := OldPathData[i].Point;
            Len := vgVectorLength(vgVectorSubtract(vgVector(B[1]), vgVector(B[3])));
            SegCount := round(Len / f);
            if SegCount < 2 then
              SegCount := 2;
            BPts := CreateBezier(B, SegCount);
            for j := 0 to High(BPts) do
            begin
              LineTo(BPts[j]);
            end;
            CurPoint := OldPathData[i].Point;
          end;
        vgPathPointClose:
          begin
            ClosePath;
          end;
      end;
      Inc(i);
    end;
    if Assigned(FOnChanged) then
      FOnChanged(Self);
  end;
end;

function TvgPathData.FlattenToPolygon(var Polygon: TvgPolygon; const Flatness: single = 0.25): TvgPoint;

  procedure CalculateBezierCoefficients(const Bezier: TvgCubicBezier; out ax,bx,cx,ay,by,cy: single);
  begin
    cx := 3.0 * (Bezier[1].x - Bezier[0].x);
    cy := 3.0 * (Bezier[1].y - Bezier[0].y);
    bx := 3.0 * (Bezier[2].x - Bezier[1].x) - cx;
    by := 3.0 * (Bezier[2].y - Bezier[1].y) - cy;
    ax := Bezier[3].x - Bezier[0].x - cx - bx;
    ay := Bezier[3].y - Bezier[0].y - cy - by;
  end;

  function PointOnBezier(const StartPoint: TvgPoint; const ax,bx,cx,ay,by,cy,T: single): TvgPoint;
  var
    tSqr  : single;
    tCube : single;
  begin
    tSqr     := t * t;
    tCube    := tSqr * t;
    Result.x := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.x;
    Result.y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.y;
  end;

  function CreateBezier(const Bezier: TvgCubicBezier; const PointCount: integer): TvgPolygon;
  var
    ax : single;
    bx : single;
    cx : single;
    ay : single;
    by : single;
    cy : single;
    dT : single;
    T  : single;
    i  : Integer;
  begin
    if PointCount = 0 then exit;
    dT := 1.0 / (1.0 * PointCount - 1.0);
    T  := 0.0;
    SetLength(Result, PointCount);
    CalculateBezierCoefficients(Bezier, ax, bx, cx, ay, by, cy);
    for i := 0 to PointCount - 1 do
    begin
      Result[i] := PointOnBezier(Bezier[0], ax, bx, cx, ay, by, cy, T);
      T := T + dT;
    end;
  end;

var
  i, j: integer;
  BPts: TvgPolygon;
  B: TvgCubicBezier;
  SP, CurPoint: TvgPoint;
  Len: single;
  SegCount: integer;
  f, s: single;
  Bounds, R: TvgRect;
begin
  Result := vgPoint(0, 0);
  SetLength(Polygon, 0);
  if Length(PathData) > 0 then
  begin
    Bounds := GetBounds;
    R := Bounds;
//    s := vgFitRect(R, vgRect(0, 0, 100, 100));
    s := MinFloat(vgRectWidth(Bounds) / 100, vgRectHeight(Bounds) / 100);
    f := Flatness * s;
    if f < 0.25 then f := 0.25;

    i := 0;
    while i < Length(PathData) do
    begin
      case PathData[i].Kind of
        vgPathPointMoveTo:
          begin
            SetLength(Polygon, Length(Polygon) + 1);
            Polygon[High(Polygon)] := PathData[i].Point;
            CurPoint := PathData[i].Point;
            SP := CurPoint;
          end;
        vgPathPointLineTo:
          begin
            SetLength(Polygon, Length(Polygon) + 1);
            Polygon[High(Polygon)] := PathData[i].Point;
            CurPoint := PathData[i].Point;
          end;
        vgPathPointCurveTo:
          begin
            B[0] := CurPoint;
            B[1] := PathData[i].Point;
            Inc(i);
            B[2] := PathData[i].Point;
            Inc(i);
            B[3] := PathData[i].Point;
            Len := vgVectorLength(vgVectorSubtract(vgVector(B[1]), vgVector(B[3])));
            SegCount := round(Len / f);
            if SegCount < 2 then
              SegCount := 2;
            BPts := CreateBezier(B, SegCount);
            for j := 0 to High(BPts) do
            begin
              SetLength(Polygon, Length(Polygon) + 1);
              Polygon[High(Polygon)] := BPts[j];
            end;
            CurPoint := PathData[i].Point;
          end;
        vgPathPointClose:
          begin
            SetLength(Polygon, Length(Polygon) + 1);
            Polygon[High(Polygon)] := SP;
            SetLength(Polygon, Length(Polygon) + 1);
            Polygon[High(Polygon)] := ClosePolygon;
          end;
      end;
      Inc(i);
    end;
    with GetBounds do
      Result := vgPoint(Abs(Right - Left), Abs(Bottom - Top));
  end;
end;

procedure TvgPathData.AddEllipse(const ARect: TvgRect);
const
  Kappa: single = 0.5522847498;
var
  cx, cy: single;
  px, py: single;
begin
  cx := (ARect.Left + ARect.Right) / 2;
  cy := (ARect.Top + ARect.Bottom) / 2;
  px := Kappa * (vgRectWidth(ARect) / 2);
  py := Kappa * (vgRectHeight(ARect) / 2);
  MoveTo(vgPoint(ARect.Left, cy));
  CurveTo(vgPoint(ARect.Left, cy - py), vgPoint(cx - px, ARect.Top), vgPoint(cx, ARect.Top));
  CurveTo(vgPoint(cx + px, ARect.Top), vgPoint(ARect.Right, cy - py), vgPoint(ARect.Right, cy));
  CurveTo(vgPoint(ARect.Right, cy + py), vgPoint(cx + px, ARect.Bottom), vgPoint(cx, ARect.Bottom));
  CurveTo(vgPoint(cx - px, ARect.Bottom), vgPoint(ARect.Left, cy + py), vgPoint(ARect.Left, cy));
end;

procedure TvgPathData.AddRectangle(const ARect: TvgRect; const xRadius, yRadius: single; const ACorners: TvgCorners;
      const ACornerType: TvgCornerType = vgCornerRound);
var
  R: TvgRect;
  x1, x2, y1, y2: single;
begin
  R := ARect;
  x1 := xRadius;
  if vgRectWidth(R) - (x1 * 2) < 0 then
    x1 := (xRadius * (vgRectWidth(R) / (x1 * 2)));
  x2 := x1 / 2;
  y1 := yRadius;
  if vgRectHeight(R) - (y1 * 2) < 0 then
    y1 := (yRadius * (vgRectHeight(R) / (y1 * 2)));
  y2 := y1 / 2;

  MoveTo(vgPoint(R.Left, R.Top + y1));
  if vgCornerTopLeft in ACorners then
  begin
    case ACornerType of
      // vgCornetRound - default
      vgCornerBevel: LineTo(vgPoint(R.Left + x1, R.Top));
      vgCornerInnerRound: CurveTo(vgPoint(R.Left + x2, R.Top + y1), vgPoint(R.Left + x1, R.Top + y2),
        vgPoint(R.Left + x1, R.Top));
      vgCornerInnerLine:
        begin
          LineTo(vgPoint(R.Left + x2, R.Top + y1));
          LineTo(vgPoint(R.Left + x1, R.Top + y2));
          LineTo(vgPoint(R.Left + x1, R.Top));
        end;
    else
      CurveTo(vgPoint(R.Left, R.Top + (y2)), vgPoint(R.Left + x2, R.Top), vgPoint(R.Left + x1, R.Top))
    end;
  end
  else
  begin
    LineTo(vgPoint(R.Left, R.Top));
    LineTo(vgPoint(R.Left + x1, R.Top));
  end;
  LineTo(vgPoint(R.Right - x1, R.Top));
  if vgCornerTopRight in ACorners then
  begin
    case ACornerType of
      // vgCornetRound - default
      vgCornerBevel: LineTo(vgPoint(R.Right, R.Top + y1));
      vgCornerInnerRound: CurveTo(vgPoint(R.Right - x1, R.Top + y2), vgPoint(R.Right - x2, R.Top + y1),
        vgPoint(R.Right, R.Top + y1));
      vgCornerInnerLine:
        begin
          LineTo(vgPoint(R.Right - x1, R.Top + y2));
          LineTo(vgPoint(R.Right - x2, R.Top + y1));
          LineTo(vgPoint(R.Right, R.Top + y1));
        end;
    else
      CurveTo(vgPoint(R.Right - x2, R.Top), vgPoint(R.Right, R.Top + (y2)), vgPoint(R.Right, R.Top + y1))
    end;
  end
  else
  begin
    LineTo(vgPoint(R.Right, R.Top));
    LineTo(vgPoint(R.Right, R.Top + y1));
  end;
  LineTo(vgPoint(R.Right, R.Bottom - y1));
  if vgCornerBottomRight in ACorners then
  begin
    case ACornerType of
      // vgCornetRound - default
      vgCornerBevel: LineTo(vgPoint(R.Right - x1, R.Bottom));
      vgCornerInnerRound: CurveTo(vgPoint(R.Right - x2, R.Bottom - y1), vgPoint(R.Right - x1, R.Bottom - y2),
        vgPoint(R.Right - x1, R.Bottom));
      vgCornerInnerLine:
        begin
          LineTo(vgPoint(R.Right - x2, R.Bottom - y1));
          LineTo(vgPoint(R.Right - x1, R.Bottom - y2));
          LineTo(vgPoint(R.Right - x1, R.Bottom));
        end;
    else
      CurveTo(vgPoint(R.Right, R.Bottom - (y2)), vgPoint(R.Right - x2, R.Bottom), vgPoint(R.Right - x1, R.Bottom))
    end;
  end
  else
  begin
    LineTo(vgPoint(R.Right, R.Bottom));
    LineTo(vgPoint(R.Right - x1, R.Bottom));
  end;
  LineTo(vgPoint(R.Left + x1, R.Bottom));
  if vgCornerBottomLeft in ACorners then
  begin
    case ACornerType of
      // vgCornetRound - default
      vgCornerBevel: LineTo(vgPoint(R.Left, R.Bottom - y1));
      vgCornerInnerRound: CurveTo(vgPoint(R.Left + x1, R.Bottom - y2), vgPoint(R.Left + x2, R.Bottom - y1),
        vgPoint(R.Left, R.Bottom - y1));
      vgCornerInnerLine:
        begin
          LineTo(vgPoint(R.Left + x1, R.Bottom - y2));
          LineTo(vgPoint(R.Left + x2, R.Bottom - y1));
          LineTo(vgPoint(R.Left, R.Bottom - y1));
        end;
    else
      CurveTo(vgPoint(R.Left + x2, R.Bottom), vgPoint(R.Left, R.Bottom - (y2)), vgPoint(R.Left, R.Bottom - y1))
    end;
  end
  else
  begin
    LineTo(vgPoint(R.Left, R.Bottom));
    LineTo(vgPoint(R.Left, R.Bottom - y1));
  end;
  ClosePath;
end;

procedure DrawArcWithBezier(Path: TvgPathData; CenterX, CenterY, RadiusX, RadiusY, StartAngle, SweepRange: single; UseMoveTo: boolean);
var
  Coord: array[0..3] of TvgPoint;
  pts: array[0..3] of TvgPoint;
  a, b, c, x, y: Double;
  ss, cc: Double;
  i: Integer;
begin
  if SweepRange = 0 then
  begin
    if UseMoveTo then
      Path.MoveTo(vgPoint(CenterX + RadiusX * cos(StartAngle), CenterY - RadiusY * sin(StartAngle)));
    Path.LineTo(vgPoint(CenterX + RadiusX * cos(StartAngle), CenterY - RadiusY * sin(StartAngle)));
    Exit;
  end;
  b := sin(SweepRange / 2);
  c := cos(SweepRange / 2);
  a := 1 - c;
  x := a * 4.0 / 3.0;
  y := b - x * c / b;
  ss := sin(StartAngle + SweepRange / 2);
  cc := cos(StartAngle + SweepRange / 2);
  Coord[0] := vgPoint(c, -b);
  Coord[1] := vgPoint(c + x, -y);
  Coord[2] := vgPoint(c + x, y);
  Coord[3] := vgPoint(c, b);
  for i := 0 to 3 do
  begin
    pts[i] := vgPoint(CenterX + RadiusX * (Coord[i].x * cc - Coord[i].y * ss), Centery + RadiusY * (Coord[i].x * ss + Coord[i].y * cc));
  end;
  if UseMoveTo then
    Path.MoveTo(pts[0]);
  Path.CurveTo(pts[1], pts[2], pts[3]);
end;

procedure TvgPathData.AddArc(const Center, Radius: TvgPoint; StartAngle, SweepAngle: single);
const
  bezier_arc_angle_epsilon = 0.01;
var
  UseMoveTo: boolean;
  i: integer;
  f: single;
  total_sweep ,
  local_sweep ,
  prev_sweep  : single;
  done: boolean;
begin
  StartAngle := vgDegToRad(StartAngle);
  SweepAngle := vgDegToRad(SweepAngle);

  i := trunc(StartAngle / (2.0 * cPi));
  f := StartAngle - (i * 2.0 * cPi);

  StartAngle := f;

  if SweepAngle >= 2.0 * cPi then
    SweepAngle := 2.0 * cPi;
  if SweepAngle <= -2.0 * cPi then
    SweepAngle:=-2.0 * cPi;

  if Abs(SweepAngle ) < 1e-10 then
  begin
    exit;
  end;

  total_sweep := 0.0;
//  local_sweep := 0.0;

  done := false;
  UseMoveTo := true;
  repeat
    if SweepAngle < 0.0 then
    begin
      prev_sweep := total_sweep;
      local_sweep:= -cPi * 0.5;
      total_sweep:= total_sweep - (cPi * 0.5 );
      if total_sweep <= SweepAngle + bezier_arc_angle_epsilon  then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := true;
      end;
    end
    else
    begin
      prev_sweep := total_sweep;
      local_sweep:= cPi * 0.5;
      total_sweep:= total_sweep + (pi * 0.5 );
      if total_sweep >= SweepAngle - bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done:=true;
      end;
    end;
    DrawArcWithBezier(Self, Center.x, Center.y, Radius.x, Radius.y, StartAngle, local_sweep, UseMoveTo);
    UseMoveTo := false;
    StartAngle := StartAngle + local_sweep;
  until done;
end;

procedure TvgPathData.AddArcSvgPart(const Center, Radius: TvgPoint; StartAngle, SweepAngle: single);
const
  bezier_arc_angle_epsilon = 0.01;
var
  UseMoveTo: boolean;
  i: integer;
  f: single;
  total_sweep ,
  local_sweep ,
  prev_sweep  : single;
  done: boolean;
begin
  StartAngle := vgDegToRad(StartAngle);
  SweepAngle := vgDegToRad(SweepAngle);

  i := trunc(StartAngle / (2.0 * cPi));
  f := StartAngle - (i * 2.0 * cPi);

  StartAngle := f;

  if SweepAngle >= 2.0 * cPi then
    SweepAngle := 2.0 * cPi;
  if SweepAngle <= -2.0 * cPi then
    SweepAngle:=-2.0 * cPi;

  if Abs(SweepAngle ) < 1e-10 then
  begin
    exit;
  end;

  total_sweep := 0.0;
//  local_sweep := 0.0;

  done := false;
  UseMoveTo := false;
  repeat
    if SweepAngle < 0.0 then
    begin
      prev_sweep := total_sweep;
      local_sweep:= -cPi * 0.5;
      total_sweep:= total_sweep - (cPi * 0.5 );
      if total_sweep <= SweepAngle + bezier_arc_angle_epsilon  then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done := true;
      end;
    end
    else
    begin
      prev_sweep := total_sweep;
      local_sweep:= cPi * 0.5;
      total_sweep:= total_sweep + (pi * 0.5 );
      if total_sweep >= SweepAngle - bezier_arc_angle_epsilon then
      begin
        local_sweep := SweepAngle - prev_sweep;
        done:=true;
      end;
    end;
    DrawArcWithBezier(Self, Center.x, Center.y, Radius.x, Radius.y, StartAngle, local_sweep, UseMoveTo);
    UseMoveTo := false;
    StartAngle := StartAngle + local_sweep;
  until done;
end;

procedure TvgPathData.AddArcSvg(const P1, Radius: TvgPoint; Angle: single; const LargeFlag, SweepFlag: boolean; const P2: TvgPoint);
var
  i: integer;
  m_radii_ok: boolean;
  v , p , n , sq ,
  rx ,ry ,
  x0 ,y0 ,
  x1 ,y1 ,
  x2 ,y2 ,
  cx ,cy ,
  ux ,uy ,
  vx ,vy ,

  dx2 ,dy2 ,
  prx ,pry ,
  px1 ,py1 ,
  cx1 ,cy1 ,
  sx2 ,sy2 ,

  sign ,coef ,

  radii_check ,
  start_angle ,
  sweep_angle ,

  cos_a ,sin_a: single;
  tm: TvgMatrix;
  len: integer;
begin
  rx := Radius.X;
  ry := Radius.Y;
  x0 := P1.X;
  y0 := P1.Y;
  x2 := P2.X;
  y2 := P2.Y;
  angle := vgDegToRad(Angle);

  m_radii_ok := true;

  if rx < 0.0 then
   rx:=-rx;

  if ry < 0.0 then
   ry:=-rx;

  // Calculate the middle point between
  // the current and the final points
  dx2 := (x0 - x2 ) / 2.0;
  dy2 := (y0 - y2 ) / 2.0;

  // Convert angle from degrees to radians
  cos_a := cos(angle);
  sin_a := sin(angle);

  // Calculate (x1, y1)
  x1 := cos_a * dx2 + sin_a * dy2;
  y1 := -sin_a * dx2 + cos_a * dy2;

  // Ensure radii are large enough
  prx :=rx * rx;
  pry :=ry * ry;
  px1 :=x1 * x1;
  py1 :=y1 * y1;

  // Check that radii are large enough
  radii_check:=px1 / prx + py1 / pry;

  if radii_check > 1.0 then
  begin
    rx :=sqrt(radii_check ) * rx;
    ry :=sqrt(radii_check ) * ry;
    prx:=rx * rx;
    pry:=ry * ry;

    if radii_check > 10.0 then
      m_radii_ok:=false;

  end;

  // Calculate (cx1, cy1)
  if LargeFlag = SweepFlag then
    sign:=-1.0
  else
    sign:=1.0;

  sq:=(prx * pry - prx * py1 - pry * px1 ) / (prx * py1 + pry * px1 );

  if sq < 0 then
    coef:=sign * sqrt(0 )
  else
    coef:=sign * sqrt(sq );

  cx1:=coef *  ((rx * y1 ) / ry );
  cy1:=coef * -((ry * x1 ) / rx );

  // Calculate (cx, cy) from (cx1, cy1)
  sx2:=(x0 + x2 ) / 2.0;
  sy2:=(y0 + y2 ) / 2.0;
  cx := sx2 + (cos_a * cx1 - sin_a * cy1 );
  cy := sy2 + (sin_a * cx1 + cos_a * cy1 );

  // Calculate the start_angle (angle1) and the sweep_angle (dangle)
  ux:= (x1 - cx1 ) / rx;
  uy:= (y1 - cy1 ) / ry;
  vx:=(-x1 - cx1 ) / rx;
  vy:=(-y1 - cy1 ) / ry;

  // Calculate the angle start
  n:=sqrt(ux * ux + uy * uy );
  p:=ux; // (1 * ux ) + (0 * uy )

  if uy < 0 then
    sign:=-1.0
  else
    sign:=1.0;

  v:=p / n;

  if v < -1.0 then
    v:=-1.0;

  if v > 1.0 then
    v:=1.0;

  start_angle:=sign * ArcCos(v );

  // Calculate the sweep angle
  n:=sqrt((ux * ux + uy * uy ) * (vx * vx + vy * vy ) );
  p:=ux * vx + uy * vy;

  if ux * vy - uy * vx < 0 then
    sign:=-1.0
  else
    sign:=1.0;

   v:=p / n;

  if v < -1.0 then
    v:=-1.0;

  if v > 1.0 then
    v:=1.0;

  sweep_angle:=sign * ArcCos(v );

  if (not SweepFlag ) and (sweep_angle > 0 ) then
    sweep_angle:=sweep_angle - pi * 2.0
  else
    if SweepFlag and (sweep_angle < 0 ) then
      sweep_angle:=sweep_angle + pi * 2.0;

  len := Length(PathData);
  AddArcSvgPart(vgPoint(0, 0), vgPoint(rx, ry), vgRadToDeg(start_angle), vgRadToDeg(sweep_angle));

  tm := IdentityMatrix;
  tm.m31 := cx;
  tm.m32 := cy;
  tm := vgMatrixMultiply(vgCreateRotationMatrix(Angle), tm);

  i := len;
  while i < Length(PathData) do
  begin
    with vgVectorTransform(vgVector(PathData[i].Point), tm) do
      PathData[i].Point := vgPoint(x, y);
    inc(i);
  end;
end;

function TvgPathData.IsEmpty: boolean;
begin
  Result := Length(PathData) = 0;
end;

function TvgPathData.GetPathString: AnsiString;
var
  i: integer;
begin
  Result := '';
  i := 0;
  while i < Length(PathData) do
  begin
    case PathData[i].Kind of
      vgPathPointMoveTo:
        Result := Result + 'M ' + vgFloatToStr(PathData[i].Point.X) + ',' + vgFloatToStr(PathData[i].Point.Y) + ' ';
      vgPathPointLineTo:
        Result := Result + 'L ' + vgFloatToStr(PathData[i].Point.X) + ',' + vgFloatToStr(PathData[i].Point.Y) + ' ';
      vgPathPointCurveTo:
        begin
          Result := Result + 'C ' + vgFloatToStr(PathData[i].Point.X) + ',' + vgFloatToStr(PathData[i].Point.Y) + ' ' +
            vgFloatToStr(PathData[i + 1].Point.X) + ',' + vgFloatToStr(PathData[i + 1].Point.Y) + ' ' +
            vgFloatToStr(PathData[i + 2].Point.X) + ',' + vgFloatToStr(PathData[i + 2].Point.Y) + ' ';
          Inc(i, 2);
        end;
      vgPathPointClose:
        Result := Result + 'Z ';
    end;
    Inc(i);
  end;
end;

function GetTok(const S: AnsiString; var Pos: integer): AnsiString;
var
  i: integer;
begin
  Result := '';
  if Pos > Length(S) then Exit;
  while (Pos <= Length(S)) and (S[Pos] in [' ']) do
    Inc(Pos);
  for i := Pos to Length(S) do
  begin
    if System.Pos(S[i], 'zmlchvsqtaZMLCHVSQTA') = 0 then Break;
    Result := Result + S[i];
  end;
  Pos := i;
end;

function GetNum(const S: AnsiString; var Pos: integer): AnsiString;
var
  i: integer;
begin
  Result := '';
  if Pos > Length(S) then Exit;
  while (Pos <= Length(S)) and (S[Pos] in [' ']) do
    Inc(Pos);
  for i := Pos to Length(S) do
  begin
    if (System.Pos(S[i], '0123456789.') = 0) and not ((i = Pos) and (S[i] = '-')) then Break;
    Result := Result + S[i];
  end;
  while S[Pos] in [' '] do
    Inc(Pos);
  Pos := i;
end;

function GetPoint(const S: AnsiString; var Pos: integer): TvgPoint;
var
//  i: integer;
  x, y: AnsiString;
begin
  Result := vgPoint(0, 0);
  if Pos > Length(S) then Exit;
  while (Pos <= Length(S)) and (S[Pos] in [',', ' ']) do
    Inc(Pos);
  x := GetNum(S, Pos);
  while (Pos <= Length(S)) and (S[Pos] in [',', ' ']) do
    Inc(Pos);
  y := GetNum(S, Pos);
  while (Pos <= Length(S)) and (S[Pos] in [',', ' ']) do
    Inc(Pos);

  Result := vgPoint(vgStrToFloat(x), vgStrToFloat(y));
end;

procedure TvgPathData.SetPathString(const Value: AnsiString);
const
  TokSep = ' '#10#13;
  TokStop = '0123456789-';
  NumSep = ' ,'#10#13;
  NumStop = 'zmlchvsqtaZMLCHVSQTA';
var
  S, toks: AnsiString;
  tok: AnsiChar;
  R, CP1, CP2: TvgPoint;
  angle: single;
  large, sweet: boolean;
  lastlen, pos, i: integer;
begin
  { remove #10#13 }
  for i := 1 to Length(Value) do
  begin
    if Value[i] in [#9,#10,#13] then Continue;
    S := S + Value[i];
  end;
  { }
  SetLength(PathData, 0);
  pos := 1;
  while S <> '' do
  begin
    lastlen := pos;
    toks := GetTok(S, pos);
    while toks <> '' do
    begin
      tok := toks[1];
      Delete(toks, 1, 1);
      try
        if (tok in ['z', 'Z']) then
        begin
          ClosePath;
        end;
        if (tok in ['M']) then
        begin
          MoveTo(GetPoint(S, pos));
          while (S <> '') and (S[1] in ['0','1','2','3','4','5','6','7','8','9','-']) do
          begin
            { next points }
            MoveTo(GetPoint(S, pos));
          end;
        end;
        if (tok in ['m']) then
        begin
          MoveToRel(GetPoint(S, pos));
          while (S <> '') and (S[1] in ['0','1','2','3','4','5','6','7','8','9','-']) do
          begin
            { next points }
            MoveToRel(GetPoint(S, pos));
          end;
        end;
        if (tok = 'L') then
        begin
          LineTo(GetPoint(S, pos));
          while (S <> '') and (S[1] in ['0','1','2','3','4','5','6','7','8','9','-']) do
          begin
            { next points }
            LineTo(GetPoint(S, pos));
          end;
        end;
        if (tok = 'l') then
        begin
          LineToRel(GetPoint(S, pos));
          while (S <> '') and (S[1] in ['0','1','2','3','4','5','6','7','8','9','-']) do
          begin
            { next points }
            LineToRel(GetPoint(S, pos));
          end;
        end;
        if (tok = 'C') then
        begin
          CP1 := GetPoint(S, pos);
          CP2 := GetPoint(S, pos);
          CurveTo(CP1, CP2, GetPoint(S, pos));
          while (S <> '') and (S[1] in ['0','1','2','3','4','5','6','7','8','9','-']) do
          begin
            { next points }
            CP1 := GetPoint(S, pos);
            CP2 := GetPoint(S, pos);
            CurveTo(CP1, CP2,
              GetPoint(S, pos)
            );
          end;
        end;
        if (tok = 'c') then
        begin
          CP1 := GetPoint(S, pos);
          CP2 := GetPoint(S, pos);
          CurveToRel(CP1, CP2, GetPoint(S, pos));
          while (S <> '') and (S[1] in ['0','1','2','3','4','5','6','7','8','9','-']) do
          begin
            { next points }
            CP1 := GetPoint(S, pos);
            CP2 := GetPoint(S, pos);
            CurveToRel(CP1, CP2, GetPoint(S, pos));
          end;
        end;
        if (tok = 'S') then
        begin
          CP2 := GetPoint(S, pos);
          SmoothCurveTo(CP2, GetPoint(S, pos));
          while (S <> '') and (S[1] in ['0','1','2','3','4','5','6','7','8','9','-']) do
          begin
            { next points }
            CP2 := GetPoint(S, pos);
            SmoothCurveTo(CP2, GetPoint(S, pos));
          end;
        end;
        if (tok = 's') then
        begin
          CP2 := GetPoint(S, pos);
          SmoothCurveToRel(CP2, GetPoint(S, pos));
          while (S <> '') and (S[1] in ['0','1','2','3','4','5','6','7','8','9','-']) do
          begin
            { next points }
            CP2 := GetPoint(S, pos);
            SmoothCurveToRel(CP2, GetPoint(S, pos));
          end;
        end;
        if (tok = 'H') then
        begin
          // skip horizontal line
          HLineTo(vgStrToFloat(GetNum(S, Pos)));
        end;
        if (tok = 'h') then
        begin
          // skip horizontal line
          HLineToRel(vgStrToFloat(GetNum(S, Pos)));
        end;
        if (tok = 'V') then
        begin
          // skip vertical line
          VLineTo(vgStrToFloat(GetNum(S, Pos)));
        end;
        if (tok = 'v') then
        begin
          // skip vertical line
          VLineToRel(vgStrToFloat(GetNum(S, Pos)));
        end;
        if (tok = 'Q') then
        begin
          // skip quadratic bezier
          GetPoint(S, pos);
          GetPoint(S, pos);
        end;
        if (tok = 'q') then
        begin
          // skip quadratic bezier
          GetPoint(S, pos);
          GetPoint(S, pos);
        end;
        if (tok = 'T') then
        begin
          // skip show qudratic bezier
          GetPoint(S, pos);
        end;
        if (tok = 't') then
        begin
          // skip show qudratic bezier
          GetPoint(S, pos);
        end;
        if (tok = 'A') then
        begin
          // arc
          if Length(PathData) > 0 then
            CP1 := PathData[High(PathData)].Point
          else
            CP1 := vgPoint(0, 0);
          R := GetPoint(S, pos);
          angle := vgStrToFloat(GetNum(S, Pos));
          with GetPoint(S, pos) do
          begin
            large := X = 1;
            sweet := Y = 1;
          end;
          CP2 := GetPoint(S, pos);
          AddArcSvg(CP1, R, angle, large, sweet, CP2);
        end;
        if (tok = 'a') then
        begin
          // arc rel
          if Length(PathData) > 0 then
            CP1 := PathData[High(PathData)].Point
          else
            CP1 := vgPoint(0, 0);
          R := GetPoint(S, pos);
          angle := vgStrToFloat(GetNum(S, Pos));
          with GetPoint(S, pos) do
          begin
            large := X = 1;
            sweet := Y = 1;
          end;
          CP2 := GetPoint(S, pos);
          CP2.x := CP1.x + CP2.x;
          CP2.y := CP1.y + CP2.y;
          AddArcSvg(CP1, R, angle, large, sweet, CP2);
        end;
      except
      end;
    end;
    if lastlen = pos then
    begin
      Pos :=0;
      Break;
    end;
  end;
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{ TvgCanvas ===================================================================}

constructor TvgCanvas.Create(const AWidth, AHeight: integer);
begin
  inherited Create;
  FStroke := TvgBrush.Create(vgBrushSolid, $FF000000);
  FFill := TvgBrush.Create(vgBrushSolid, $FFFFFFFF);
  FFont := TvgFont.Create;
  FFont.OnChanged := FontChanged;
  ResizeBuffer(AWidth, AHeight);
end;

constructor TvgCanvas.CreateFromBitmap(const ABitmap: TvgBitmap);
begin
  inherited Create;
  FWidth := ABitmap.Width;
  FHeight := ABitmap.Height;
  FStroke := TvgBrush.Create(vgBrushSolid, $FF000000);
  FFill := TvgBrush.Create(vgBrushSolid, $FFFFFFFF);
  FFont := TvgFont.Create;
  FFont.OnChanged := FontChanged;
end;

destructor TvgCanvas.Destroy;
var
  i: integer;
begin
  if Length(FSaveData) > 0 then
  begin
    for i := 0 to High(FSaveData) do
    begin
      if FSaveData[i].Index = $FFFFFFFF then Continue;
      FSaveData[i].Stroke.Free;
      FSaveData[i].Fill.Free;
      FSaveData[i].Font.Free;
    end;
    SetLength(FSaveData, 0);
  end;
  FFont.Free;
  FStroke.Free;
  FFill.Free;
  FreeBuffer;
  inherited;
end;

procedure TvgCanvas.FreeBuffer;
begin
{$IFDEF LINUX}
  if FBufferBits <> nil then
    System.FreeMem(FBufferBits);
  if FBuffered then
  begin
    cairo_destroy(cr);
    cairo_surface_destroy(sr);
  end;
  cr := nil;
{$ENDIF}
{$IFDEF WIN32}
  if FBuffered then
  begin
    if FBufferHandle = 0 then Exit;
    if FBufferDC <> 0 then DeleteDC(FBufferDC);
    FBufferDC := 0;
    if FBufferHandle <> 0 then DeleteObject(FBufferHandle);
    FBufferHandle := 0;
  end;
{$ENDIF}
{$IFDEF DARWIN}
  if FBuffered then
  begin
    if CtxRef <> nil then
      CGContextRelease(CtxRef);
  end
  else
  begin
  end;
  if FBufferBits <> nil then
    System.FreeMem(FBufferBits);
{$ENDIF}
  FBufferBits := nil;
end;

procedure TvgCanvas.ResizeBuffer(const AWidth, AHeight: integer);
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then Exit;
  FreeBuffer;
  FWidth := AWidth;
  FHeight := AHeight;
  if FWidth <= 0 then FWidth := 1;
  if FHeight <= 0 then FHeight := 1;
  FResized := true;

  if FWidth * FHeight = 0 then Exit;
{$IFDEF WIN32}
  if FBuffered then
  begin
    { Initialization }
    with FBitmapInfo.bmiHeader do
    begin
      biSize := SizeOf(TBitmapInfoHeader);
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
      biWidth := AWidth;
      if biWidth <= 0 then biWidth := 1;
      biHeight := -AHeight;
      if biHeight >= 0 then biHeight := -1;
    end;

    { Create new DIB }
    FBufferHandle := CreateDIBSection(0, FBitmapInfo, DIB_RGB_COLORS, Pointer(FBufferBits), 0, 0);
    if FBufferBits = nil then
      raise Exception.Create('Can''t allocate the DIB handle');

    FBufferDC := CreateCompatibleDC(0);
    if FBufferDC = 0 then
    begin
      DeleteObject(FBufferHandle);
      FBufferDC := 0;
      FBufferBits := nil;
      raise Exception.Create('Can''t create compatible DC');
    end;

    if SelectObject(FBufferDC, FBufferHandle) = 0 then
    begin
      DeleteDC(FBufferDC);
      DeleteObject(FBufferHandle);
      FBufferDC := 0;
      FBufferHandle := 0;
      FBufferBits := nil;
      raise Exception.Create('Can''t select an object into DC');
    end;
  end;
{$ENDIF}
{$IFDEF DARWIN}
  if FBuffered then
  begin
    GetMem(FBufferBits, FWidth * FHeight * 4);
    CtxRef := CGBitmapContextCreate(FBufferBits, FWidth, FHeight, 8,
      FWidth * 4, CGColorSpaceCreateDeviceRGB, kCGImageAlphaPremultipliedLast);
    CGContextTranslateCTM(CtxRef, 0, FHeight);
    CGContextScaleCTM(CtxRef, 1, -1);
  end
  else
  begin
    GetMem(FBufferBits, 4);
  end;
{$ENDIF}
{$IFDEF LINUX}
  if FBuffered then
  begin
    GetMem(FBufferBits, FWidth * FHeight * 4);
    sr := cairo_image_surface_create_for_data(FBufferBits, CAIRO_FORMAT_ARGB32,
      FWidth, FHeight, FWidth * 4);
    cr := cairo_create(sr);
  end
  else
  begin
    GetMem(FBufferBits, 4);
  end;
{$ENDIF}
end;

class function TvgCanvas.GetBitmapScanline(Bitmap: TvgBitmap;
  y: integer): PvgColorArray;
begin
  Result := nil;
end;

procedure TvgCanvas.SaveToStream(S: TStream);
var
  B: TvgBitmap;
begin
  if FBuffered then
  begin
    B := TvgBitmap.Create(FWidth, FHeight);
    MoveLongword(FBufferBits, B.StartLine, FWidth * FHeight);
    B.SaveToStream(S);
    B.Free;
  end;
end;

procedure TvgCanvas.SaveToBits(Bits: Pointer);
begin
  if FBuffered then
    MoveLongword(FBufferBits, Bits, FWidth * FHeight);
end;

function TvgCanvas.BeginScene: boolean;
begin
  Result := true;
end;

procedure TvgCanvas.EndScene;
begin
end;

procedure TvgCanvas.FlushBuffer(const X, Y: integer; const DC: Cardinal);
{$IFDEF DARWIN}
var
  CGR: CGRect;
  ImgRef: CGImageRef;
{$ENDIF}
begin
{$IFDEF WIN32}
  if FBufferHandle = 0 then Exit;
  Windows.BitBlt(DC, X, Y, FWidth, FHeight, FBufferDC, 0, 0, SRCCOPY);
{$ENDIF}
{$IFDEF DARWIN}
  if FBuffered then
  begin
    if (CtxRef <> nil) and (SceneCtx <> nil) then
    begin
      CGR.origin.x := X;
      CGR.origin.y := -Y + FHeight;
      CGR.size.width := FWidth;
      CGR.size.height := FHeight;
      ImgRef := CGBitmapContextCreateImage(CtxRef);
      CGContextDrawImage(SceneCtx, CGR, ImgRef);
      CFRelease(ImgRef);
    end;
  end;
{$ENDIF}
end;

procedure TvgCanvas.FlushBufferRect(const X, Y: integer; const DC: Cardinal;
  const ARect: TvgRect);
var
{$IFDEF DARWIN}
  NewR: TvgRect;
  R, SubR: CGRect;
  SubImgRef, ImgRef: CGImageRef;
{$ELSE}
  R: TRect;
{$ENDIF}
begin
{$IFDEF WIN32}
  if FBufferHandle = 0 then Exit;
  R := Rect(trunc(ARect.left), trunc(ARect.top), trunc(ARect.right) + 1, trunc(ARect.bottom) + 1);
  with R do
  begin
    Windows.BitBlt(DC, X + R.left, Y + R.top, R.right - R.left, R.bottom - R.top, FBufferDC, R.left, R.top, SRCCOPY);
  end;
{$ENDIF}
{$IFDEF DARWIN}
  if FBuffered and (SceneCtx <> nil) and (CtxRef <> nil) then
  begin
    vgIntersectRect(NewR, ARect, vgRect(0, 0, FWidth, FHeight));
    R.origin.x := X + NewR.Left;
    R.origin.y := -(Y + NewR.Bottom);
    R.size.width := vgRectWidth(NewR);
    R.size.height := vgRectHeight(NewR);
    SubR.origin.x := NewR.Left;
    SubR.origin.y := NewR.Top;
    SubR.size.width := vgRectWidth(NewR);
    SubR.size.height := vgRectHeight(NewR);

    ImgRef := CGBitmapContextCreateImage(CtxRef);
    SubImgRef := CGImageCreateWithImageInRect(ImgRef, SubR);
    if SubImgRef <> nil then
    begin
      CGContextSaveGState(SceneCtx);
      CGContextScaleCTM(SceneCtx, 1, -1);
      CGContextDrawImage(SceneCtx, R, SubImgRef);
      CFRelease(SubImgRef);
      CGContextRestoreGState(SceneCtx);
    end;
    CFRelease(ImgRef);
  end;
{$ENDIF}
end;

procedure TvgCanvas.Clear(const Color: cardinal);
begin
  {$IFDEF LINUX}
  if FBufferBits = nil then Exit;
  if not FBuffered then
  begin
    with TvgColorRec(Color) do
      cairo_set_source_rgba(cr, R / $FF, G / $FF, B / $FF, A / $FF);
    cairo_paint(cr);
  end
  else
    FillLongword(FBufferBits, FWidth * FHeight, Color);
  {$ENDIF}
  {$IFDEF WIN32}
  if FBufferBits = nil then Exit;
  if not FBuffered then
  begin
  end
  else
    FillLongword(FBufferBits, FWidth * FHeight, Color);
  {$ENDIF}
  {$IFDEF DARWIN}
  if not FBuffered then
  begin
    CGContextClearRect(CtxRef, CGRectFromRect(vgRect(0, 0, FWidth, FHeight)));
  end
  else
    FillLongword(FBufferBits, FWidth * FHeight, Color);
  {$ENDIF}
end;

procedure TvgCanvas.ClearRect(const ARect: TvgRect; const AColor: TvgColor);
var
  R: TRect;
begin
  if FBufferBits = nil then Exit;
  R := Rect(Trunc(ARect.Left), Trunc(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  if R.Left < 0 then R.Left := 0;
  if R.Top < 0 then R.Top := 0;
  if R.Top < 0 then R.Top := 0;
  if R.Right > FWidth - 1 then R.Right := FWidth - 1;
  if R.Bottom > FHeight - 1 then R.Bottom := FHeight - 1;
  if R.Bottom < R.Top then R.Bottom := R.Top;
  if R.Right < R.Left then R.Right := R.Left;
  if (R.Right < 0) or (R.Top < 0) or (R.Left > FWidth - 1) or (R.Top > FHeight - 1) then Exit;
  {$IFDEF LINUX}
  if not FBuffered then
  begin
    with TvgColorRec(AColor) do
      cairo_set_source_rgba(cr, R / $FF, G / $FF, B / $FF, A / $FF);
    cairo_rectangle(cr, R.Left, R.Top, R.right - R.left, R.bottom - R.top);
    cairo_fill(cr);
  end
  else
    FillLongwordRect(FBufferBits, FWidth, FHeight, R.Left, R.Top, R.Right, R.Bottom, AColor);
  {$ENDIF}
  {$IFDEF DARWIN}
  if not FBuffered then
  begin
    CGContextClearRect(CtxRef, CGRectFromRect(ARect));
  end
  else
    FillLongwordRect(FBufferBits, FWidth, FHeight, R.Left, R.Top, R.Right, R.Bottom, AColor);
  {$ENDIF}
  {$IFDEF WIN32}
  FillLongwordRect(FBufferBits, FWidth, FHeight, R.Left, R.Top, R.Right, R.Bottom, AColor);
  {$ENDIF}
end;

procedure TvgCanvas.SetMatrix(const M: TvgMatrix);
begin
  FMatrix := M;
end;

procedure TvgCanvas.MultyMatrix(const M: TvgMatrix);
begin
end;

procedure TvgCanvas.FontChanged(Sender: TObject);
begin
end;

function TvgCanvas.TextHeight(const AText: WideString): single;
var
  R: TvgRect;
begin
  R := vgRect(0, 0, 10000, 10000);
  MeasureText(R, R, AText, false, vgTextAlignNear, vgTextAlignCenter);
  Result := vgRectHeight(R);
end;

function TvgCanvas.TextWidth(const AText: WideString): single;
var
  R: TvgRect;
begin
  R := vgRect(0, 0, 10000, 20);
  MeasureText(R, R, AText, false, vgTextAlignNear, vgTextAlignCenter);
  Result := vgRectWidth(R);
end;

procedure TvgCanvas.DrawArc(const Center, Radius: TvgPoint; StartAngle, SweepAngle: single; const AOpacity: single);
var
  P: TvgPathData;
begin
  P := TvgPathData.Create;
  P.AddArc(Center, Radius, StartAngle, SweepAngle);
  DrawPath(P, P.Getbounds, AOpacity);
  P.Free;
end;

procedure TvgCanvas.SetStrokeDash(const Value: TvgStrokeDash);
begin
  if Value <> FStrokeDash then
  begin
    FStrokeDash := Value;
    case FStrokeDash of
      vgDashSolid:
        begin
          FDashOffset := 0;
          SetLength(FDash, 0);
        end;
      vgDashDash:
        begin
          FDashOffset := 0;
          SetLength(FDash, 2);
          FDash[0] := 1 * 3;
          FDash[1] := 1;
        end;
      vgDashDot:
        begin
          FDashOffset := 0;
          SetLength(FDash, 2);
          FDash[0] := 1;
          FDash[1] := 1;
        end;
      vgDashDashDot:
        begin
          FDashOffset := 0;
          SetLength(FDash, 4);
          FDash[0] := 1 * 3;
          FDash[1] := 1;
          FDash[2] := 1;
          FDash[3] := 1;
        end;
      vgDashDashDotDot:
        begin
          FDashOffset := 0;
          SetLength(FDash, 6);
          FDash[0] := 1 * 3;
          FDash[1] := 1;
          FDash[2] := 1;
          FDash[3] := 1;
          FDash[4] := 1;
          FDash[5] := 1;
        end;
      vgDashCustom: ;
    else
      FDashOffset := 0;
      SetLength(FDash, 0);
    end;
  end;
end;

procedure TvgCanvas.SetCustomDash(Dash: array of single; Offset: single);
var
  i: integer;
begin
  FStrokeDash := vgDashCustom;
  SetLength(FDash, Length(Dash));
  for i := 0 to High(Dash) do
    FDash[i] := Dash[i];
  FDashOffset := Offset;
end;

procedure TvgCanvas.FillPolygon(const Points: TvgPolygon; const AOpacity: single);
var
  i: integer;
  Path: TvgPathData;
begin
  Path := TvgPathData.Create;
  SetLength(Path.PathData, Length(Points));
  for i := 0 to High(Points) do
    with Path.PathData[i] do
    begin
      Kind := vgPathPointLineTo;
      if i = 0 then
        Kind := vgPathPointMoveTo;
      if (i > 0) and (Path.PathData[i - 1].Kind = vgPathPointClose) then
        Kind := vgPathPointMoveTo;
      if (Points[i].x = ClosePolygon.x) and (Points[i].y = ClosePolygon.y) then
      begin
        Kind := vgPathPointClose;
        Continue;
      end;
      if i = High(Points) then
        Kind := vgPathPointClose;
      Point := Points[i];
    end;
  FillPath(Path, Path.GetBounds, AOpacity);
  Path.Free;
end;

procedure TvgCanvas.DrawPolygon(const Points: TvgPolygon; const AOpacity: single);
var
  i: integer;
  Path: TvgPathData;
begin
  Path := TvgPathData.Create;
  SetLength(Path.PathData, Length(Points));
  for i := 0 to High(Points) do
    with Path.PathData[i] do
    begin
      Kind := vgPathPointLineTo;
      if (i = 0)  then
        Kind := vgPathPointMoveTo;
      if (i > 0) and (Path.PathData[i - 1].Kind = vgPathPointClose) then
        Kind := vgPathPointMoveTo;
      if (Points[i].x = ClosePolygon.x) and (Points[i].y = ClosePolygon.y) then
      begin
        Kind := vgPathPointClose;
        Continue;
      end;
      Point := Points[i];
    end;
  DrawPath(Path, Path.GetBounds, AOpacity);
  Path.Free;
end;

{ TvgAnimation ===================================================================}

type

  TAniThread = class(TTimer)
  private
    FAniList: TList;
    FStartTime, FTime, FDeltaTime: single;
    FAverStepTime, FStepTime, FMinStepTime: single;
    procedure OneStep;
    procedure DoSyncTimer(Sender: TObject);
  protected
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TAniThread }

constructor TAniThread.Create;
begin
  inherited Create(nil);
  Interval := Trunc(1000 / 40);
  OnTimer := DoSyncTimer;

  FAniList := TList.Create;
  FStartTime := {$IFDEF WIN32}timeGetTime;{$else}GetTickCount / 1000; {$endif}

  FMinStepTime := 0.004; // 30 fps
  FStepTime := FMinStepTime;
  FAverStepTime := FMinStepTime;
end;

destructor TAniThread.Destroy;
begin
  FAniList.Free;
  inherited ;
end;

procedure TAniThread.DoSyncTimer(Sender: TObject);
begin
  OneStep;
end;

procedure TAniThread.OneStep;
var
  i: integer;
  NewTime: single;
begin
  NewTime := {$IFDEF WIN32}(timeGetTime - FStartTime) / 1000;{$else}(GetTickCount / 1000) - FStartTime; {$endif}
  FDeltaTime := NewTime - FTime;
  FloatCount := FDeltaTime;
  FTime := NewTime;
  if FAniList.Count > 0 then
  begin
    for i := FAniList.Count - 1 downto 0 do
      if TvgAnimation(FAniList[i]).FRunning then
        TvgAnimation(FAniList[i]).ProcessTick(FTime, FDeltaTime);
  end;
end;

constructor TvgAnimation.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := false;
  Duration := 0.2;
end;

destructor TvgAnimation.Destroy;
begin
  if aniThread <> nil then
  begin
    TAniThread(aniThread).FAniList.Remove(Self);
  end;
  inherited;
end;

procedure TvgAnimation.Loaded;
begin
  inherited ;
  if not(Assigned(FScene) and (FScene.GetDesignTime)) and Enabled then
    Start;
end;

procedure TvgAnimation.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if not (Assigned(Scene) and Scene.GetDesignTime) and not (csLoading in ComponentState) then
    begin
      if FEnabled then
        Start
      else
        Stop;
    end;
  end;
end;

function TvgAnimation.NormalizedTime: single;
begin
  if FDuration > 0 then
  begin
    case FInterpolation of
      vgInterpolationLinear: Result:= vgInterpolateLinear(FTime, 0, 1, FDuration);
      vgInterpolationQuadratic: Result:= vgInterpolateQuad(FTime, 0, 1, FDuration, FAnimationType);
      vgInterpolationCubic: Result:= vgInterpolateCubic(FTime, 0, 1, FDuration, FAnimationType);
      vgInterpolationQuartic: Result:= vgInterpolateQuart(FTime, 0, 1, FDuration, FAnimationType);
      vgInterpolationQuintic: Result:= vgInterpolateQuint(FTime, 0, 1, FDuration, FAnimationType);
      vgInterpolationSinusoidal: Result:= vgInterpolateSine(FTime, 0, 1, FDuration, FAnimationType);
      vgInterpolationExponential: Result:= vgInterpolateExpo(FTime, 0, 1, FDuration, FAnimationType);
      vgInterpolationCircular: Result:= vgInterpolateCirc(FTime, 0, 1, FDuration, FAnimationType);
      vgInterpolationElastic: Result:= vgInterpolateElastic(FTime, 0, 1, FDuration, 0, 0, FAnimationType);
      vgInterpolationBack: Result:= vgInterpolateBack(FTime, 0, 1, FDuration, 0, FAnimationType);
      vgInterpolationBounce: Result:= vgInterpolateBounce(FTime, 0, 1, FDuration, FAnimationType);
    end;
  end
  else
    Result := 0;
end;

procedure TvgAnimation.ProcessAnimation;
begin
end;

procedure TvgAnimation.ProcessTick(time, deltaTime: single);
begin
  inherited;
  if Assigned(FScene) and (FScene.GetDesignTime) then Exit;
  if csDestroying in ComponentState then Exit;

  if (Parent <> nil) and (Parent.IsVisual) and (not TvgVisualObject(Parent).Visible) then
    Stop;

  if not FRunning then Exit;
  if FPause then Exit;

  if FInverse then
    FTime := FTime - deltaTime
  else
    FTime := FTime + deltaTime;
  if FTime >= FDuration then
  begin
    FTime := FDuration;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := true;
        FTime := FDuration;
      end
      else
        FTime := 0;
    end
    else
      FRunning := false;
  end
  else
  if FTime <= 0 then
  begin
    FTime := 0;
    if FLoop then
    begin
      if FAutoReverse then
      begin
        FInverse := false;
        FTime := 0;
      end
      else
        FTime := FDuration;
    end
    else
      FRunning := false;
  end;

  ProcessAnimation;

  if (FScene <> nil) then
    if not FRunning then
    begin
      if aniThread <> nil then
        TAniThread(aniThread).FAniList.Remove(Self);
      if Assigned(FOnFinish) then FOnFinish(Self);
    end;
end;

procedure TvgAnimation.Start;
begin
  if (Parent <> nil) and (Parent.IsVisual) and (not TvgVisualObject(Parent).Visible) then Exit;
  if (Abs(FDuration) < 0.001) or (FScene = nil) or (Assigned(FScene) and (FScene.GetDesignTime)) then
  begin
    { imediatly animation }
    if FInverse then
    begin
      FTime := 0;
      FDuration := 1;
    end
    else
    begin
      FTime := 1;
      FDuration := 1;
    end;
    FRunning := true;
    ProcessAnimation;
    FRunning := false;
    FTime := 0;
    FDuration := 0;
    if Assigned(FOnFinish) then FOnFinish(Self);
    FEnabled := false;
  end
  else
  begin
    FRunning := true;
    if FInverse then
      FTime := FDuration
    else
      FTime := 0;
    ProcessAnimation;

    if (FScene <> nil) then
    begin
      if aniThread = nil then
        aniThread := TAniThread.Create;

      TAniThread(aniThread).FAniList.Add(Self);
    end;
    FEnabled := true;
  end;
end;

procedure TvgAnimation.Stop;
begin
  if not FRunning then Exit;

  if aniThread <> nil then
  begin
    TAniThread(aniThread).FAniList.Remove(Self);
  end;

  if FInverse then
    FTime := 0
  else
    FTime := FDuration;
  ProcessAnimation;
  FRunning := false;
  FEnabled := false;
  if Assigned(FOnFinish) then FOnFinish(Self);
end;

procedure TvgAnimation.StartTrigger(AInstance: TvgObject; ATrigger: string);
var
  StartValue: boolean;
  Line, Setter, Prop, Value: AnsiString;
begin
  if FTrigger = '' then Exit;
  if AInstance = nil then Exit;
  if Pos(LowerCase(ATrigger), LowerCase(FTrigger)) = 0 then Exit;

  Line := FTrigger;
  Setter := GetToken(Line, ';');
  StartValue := false;
  while Setter <> '' do
  begin
    Prop := GetToken(Setter, '=');
    Value := Setter;
    if GetPropInfo(AInstance, Prop, [{$IFDEF FPC}tkBool{$ELSE}tkEnumeration{$ENDIF}]) <> nil then
    begin
      {$IFDEF FPC}
      StartValue := false;
      if (CompareText(Value, 'true') = 0) and (GetOrdProp(AInstance, Prop) > 0) then
        StartValue := true;
      if (CompareText(Value, 'false') = 0) and (GetOrdProp(AInstance, Prop) = 0) then
        StartValue := true;
      {$ELSE}
      StartValue := CompareText(GetEnumProp(AInstance, Prop), Value) = 0;
      {$ENDIF}
      if not StartValue then Exit;
    end;
    Setter := GetToken(Line, ';');
  end;
  if StartValue then
    Start;
end;

{ TvgEffect ===================================================================}

constructor TvgEffect.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := true;
end;

destructor TvgEffect.Destroy;
begin
  inherited;
end;

function TvgEffect.GetOffset: TvgPoint;          
begin

end;

function TvgEffect.GetRect(const ARect: TvgRect): TvgRect;
begin
  Result := ARect;
end;

procedure TvgEffect.ApplyTrigger(AInstance: TvgObject; ATrigger: string);
var
  StartValue: boolean;
  Line, Setter, Prop, Value: AnsiString;
begin
  if FTrigger = '' then Exit;
  if AInstance = nil then Exit;
  if Pos(LowerCase(ATrigger), LowerCase(FTrigger)) = 0 then Exit;

  Line := FTrigger;
  Setter := GetToken(Line, ';');
  StartValue := false;
  while Setter <> '' do
  begin
    Prop := GetToken(Setter, '=');
    Value := Setter;
    if GetPropInfo(AInstance, Prop, [{$IFDEF FPC}tkBool{$ELSE}tkEnumeration{$ENDIF}]) <> nil then
    begin
      {$IFDEF FPC}
      StartValue := false;
      if (CompareText(Value, 'true') = 0) and (GetOrdProp(AInstance, Prop) > 0) then
        StartValue := true;
      if (CompareText(Value, 'false') = 0) and (GetOrdProp(AInstance, Prop) = 0) then
        StartValue := true;
      {$ELSE}
      StartValue := CompareText(GetEnumProp(AInstance, Prop), Value) = 0;
      {$ENDIF}
    end;
    Setter := GetToken(Line, ';');
  end;
  Enabled := StartValue;
end;

procedure TvgEffect.UpdateParentEffects;
var
  SaveEnabled: boolean;
begin
  if not (csLoading in ComponentState) then
    if (Parent <> nil) and (Parent.isVisual) then
    begin
      TvgVisualObject(Parent).FUpdateEffects := true;
      TvgVisualObject(Parent).FRecalcUpdateRect := true;
      // update if enabled = false (erase effect )
      SaveEnabled := FEnabled;
      FEnabled := true;
      TvgVisualObject(Parent).Repaint;
      FEnabled := SaveEnabled;        
    end;
end;

procedure TvgEffect.ProcessEffect(Canvas: TvgCanvas;
  const Visual: TvgBitmap; const Data: single);
begin
end;

procedure TvgEffect.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled then
    begin
      if (Parent <> nil) and (Parent.isVisual) then
        TvgVisualObject(Parent).FRecalcHasEffect := true;
    end;
    UpdateParentEffects;
  end;
end;

{ TvgObject ==================================================================}

constructor TvgObject.Create(AOwner: TComponent);
begin
  inherited;
  AddResource(Self);
  FStored := true;
  FIsVisual := Self is TvgVisualObject;
  FRecalcHasEffect := true;
end;

destructor TvgObject.Destroy;
var
  i: integer;
begin
  if FScene <> nil then
  begin
    FScene.Notification(Self, opRemove);
  end;
  { NotifList }
  if FNotifyList <> nil then
  begin
    for i := 0 to FNotifyList.Count - 1 do
    begin
      if (TObject(FNotifyList[i]) is TvgVisual) and (TvgVisual(FNotifyList[i]).VisualObject = Self) then
        TvgVisual(FNotifyList[i]).FVisualObject := nil;
      if (TObject(FNotifyList[i]) is TvgBrushResource) and (TvgBrushResource(FNotifyList[i]).Resource = Self) then
        TvgBrushResource(FNotifyList[i]).FResource := nil;
    end;
    FreeAndNil(FNotifyList);
  end;
  { Free notify }
  i := ResourceList.IndexOf(Self);
  if i >= 0 then
  begin
    ResourceList.Delete(i);
    for i := 0 to ResourceList.Count - 1 do
      TvgObject(ResourceList[i]).FreeNotify(Self);
  end;
  if FParent <> nil then
    FParent.RemoveObject(Self)
  else
    if FScene <> nil then
      FScene.RemoveObject(Self);
  FScene := nil;
  DeleteChildren;
  FScene := nil;
  inherited;
end;

function TvgObject.ItemClass: string;
begin
  Result := '';
end;

procedure TvgObject.FreeNotify(AObject: TvgObject);
begin
end;

procedure TvgObject.AddFreeNotify(const AObject: TObject);
begin
  if FNotifyList = nil then
    FNotifyList := TList.Create;
  if FNotifyList.IndexOf(AObject) < 0 then
    FNotifyList.Add(AObject);
end;

procedure TvgObject.RemoveFreeNotify(const AObject: TObject);
begin
  if FNotifyList <> nil then
    FNotifyList.Remove(AObject);
end;

procedure TvgObject.ReaderSetName(Reader: TReader; Component: TComponent;
  var Name: string);
begin
  Name := '';
end;

procedure TvgObject.ReaderError(Reader: TReader; const Message: string;
  var Handled: Boolean);
begin
  Handled := true;
end;

procedure TvgObject.LoadFromStream(const AStream: TStream);
var
  SaveName: string;
  BinStream: TStream;
begin
  { store }
  BinStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(AStream, BinStream);
    BinStream.Position := 0;
    SaveName := Name;
    Name := '';
    BinStream.ReadComponent(Self);
    Name := SaveName;
  finally
    BinStream.Free;
  end;
end;

procedure TvgObject.SaveToStream(const Stream: TStream);
var
  SaveName: string;
  BinStream: TStream;
begin
  { store }
  BinStream := TMemoryStream.Create;
  try
    SaveName := Name;
    Name := '';
    BinStream.WriteComponent(Self);
    Name := SaveName;
    BinStream.Position := 0;
    ObjectBinaryToText(BinStream, Stream);
  finally
    BinStream.Free;
  end;
end;

procedure TvgObject.LoadFromBinStream(const AStream: TStream);
var
  R: TReader;
begin
  R := TReader.Create(AStream, 1024);
  R.OnSetName := ReaderSetName;
  R.OnError := ReaderError;
  R.ReadRootComponent(Self);
  R.Free;
end;

procedure TvgObject.SaveToBinStream(const AStream: TStream);
var
  SaveName: string;
begin
  { store }
  SaveName := Name;
  Name := '';
  AStream.WriteComponent(Self);
  Name := SaveName;
end;

function TvgObject.Clone(const AOwner: TComponent): TvgObject;
var
  S: TStream;
  R: TReader;
  SaveName: string;
begin
  S := TMemoryStream.Create;
  try
    { store }
    SaveName := Name;
    Name := '';
    S.WriteComponent(Self);
    Name := SaveName;
    S.Position := 0;
    { load }
    R := TReader.Create(S, 1024);
    R.OnSetName := ReaderSetName;
    Result := TvgObjectClass(ClassType).Create(AOwner);
    if Result <> nil then
    begin
      R.ReadRootComponent(Result);
    end;
    R.Free;
  finally
    S.Free;
  end;
end;

procedure TvgObject.CloneChildFromStream(AStream: TStream);
var
  i: integer;
  Obj: TvgObject;
begin
  Obj := CreateObjectFromStream(Self, AStream);
  if (Obj <> nil) and (Obj.FChildren <> nil) and (Obj.FChildren.Count > 0) then
  begin
    { delete self childs }
    DeleteChildren;
    { copy parent }
    for i := 0 to Obj.FChildren.Count - 1 do
    begin
      if TvgObject(Obj.FChildren[0]).isVisual then
        TvgVisualObject(Obj.FChildren[0]).Locked := true;
      TvgObject(Obj.FChildren[0]).Stored := false;
      TvgObject(Obj.FChildren[0]).Parent := Self;
    end;
    { realign to new size }
    if Obj.isVisual and (isVisual) then
    begin
      TvgVisualObject(Self).FLastWidth := TvgVisualObject(Obj).Width;
      TvgVisualObject(Self).FLastHeight := TvgVisualObject(Obj).Height;
      TvgVisualObject(Self).Realign;
    end;
//    Obj.Free;
  end;
end;

procedure TvgVisualObject.SetLocked(const Value: boolean);
begin
  FLocked := Value;
{  if (FChildren <> nil) and (FChildren.Count > 0) then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]).isVisual then
        TvgVisualObject(FChildren[i]).Locked := Value;}
end;

function TvgObject.GetVisual: TvgVisualObject;
begin
  if IsVisual then
    Result := TvgVisualObject(Self)
  else
    Result := nil;
end;

function TvgObject.GetScene: IvgScene;
begin
  if FScene <> nil then
    Result := FScene
  else
    if FParent <> nil then
    begin
      FScene := FParent.FScene;
      Result := FScene;
    end
    else
      Result := nil;
end;

function TvgObject.HasClipParent: TvgVisualObject;
var
  i: integer;
begin
  Result := nil;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if (TvgObject(FChildren[i]) is TvgVisualObject) and (TvgVisualObject(FChildren[i]).ClipParent) then
      begin
        Result := TvgVisualObject(FChildren[i]);
        Exit;
      end;
end;

function TvgVisualObject.GetEffectsRect: TvgRect;
var
  i: integer;
begin
  Result := LocalRect;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if (TvgObject(FChildren[i]) is TvgEffect) and (TvgEffect(FChildren[i]).Enabled) then
        Result := vgUnionRect(Result, TvgEffect(FChildren[i]).GetRect(LocalRect));
    end;
end;

function TvgObject.HasEffect: boolean;
var
  i: integer;
begin
  if FRecalcHasEffect then
  begin
    FRecalcHasEffect := false;
    FHasEffect := false;
    if FDisableEffect then Exit;
    if FChildren <> nil then
      for i := 0 to FChildren.Count - 1 do
      begin
        if (TvgObject(FChildren[i]) is TvgEffect) and ((TvgEffect(FChildren[i]).Enabled) or (TvgEffect(FChildren[i]).Trigger <> '')) then
        begin
          FHasEffect := true;
          Break;
        end;
      end;
  end;
  Result := FHasEffect;
end;

function TvgObject.HasDisablePaintEffect: boolean;
var
  i: integer;
begin
  Result := false;
  if FDisableEffect then Exit;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if (TvgObject(FChildren[i]) is TvgEffect) and (TvgEffect(FChildren[i]).Enabled) and (TvgEffect(FChildren[i]).DisablePaint) then
      begin
        Result := true;
        Exit;
      end;
end;

function TvgObject.HasAfterPaintEffect: boolean;
var
  i: integer;
begin
  Result := false;
  if FDisableEffect then Exit;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if (TvgObject(FChildren[i]) is TvgEffect) and (TvgEffect(FChildren[i]).Enabled) and (TvgEffect(FChildren[i]).AfterPaint) then
      begin
        Result := true;
        Exit;
      end;
end;

{ Property animation }

procedure TvgObject.AnimateColor(const APropertyName, NewValue: string;
  Duration: single = 0.2; AType: TvgAnimationType = vgAnimationIn; AInterpolation: TvgInterpolationType = vgInterpolationLinear);
var
  A: TvgColorAnimation;
begin
  A := TvgColorAnimation.Create(Self);
  A.Parent := Self;
  A.AnimationType := AType;
  A.Interpolation := AInterpolation;
  A.OnFinish := DoAniFinished;
  A.Duration := Duration;
  A.PropertyName := APropertyName;
  A.StartFromCurrent := true;
  A.StopValue := NewValue;
  A.Start;
end;

procedure TvgObject.AnimateFloat(const APropertyName: string;
  const NewValue: single; Duration: single = 0.2;
  AType: TvgAnimationType = vgAnimationIn; AInterpolation: TvgInterpolationType = vgInterpolationLinear);
var
  A: TvgFloatAnimation;
begin
  A := TvgFloatAnimation.Create(Self);
  A.Parent := Self;
  A.AnimationType := AType;
  A.Interpolation := AInterpolation;
  A.OnFinish := DoAniFinished;
  A.Duration := Duration;
  A.PropertyName := APropertyName;
  A.StartFromCurrent := true;
  A.StopValue := NewValue;
  A.Start;
end;

procedure TvgObject.AnimateFloatWait(const APropertyName: string;
  const NewValue: single; Duration: single = 0.2;
  AType: TvgAnimationType = vgAnimationIn; AInterpolation: TvgInterpolationType = vgInterpolationLinear);
var
  A: TvgFloatAnimation;
begin
  A := TvgFloatAnimation.Create(Self);
  A.Parent := Self;
  A.AnimationType := AType;
  A.Interpolation := AInterpolation;
  A.Duration := Duration;
  A.PropertyName := APropertyName;
  A.StartFromCurrent := true;
  A.StopValue := NewValue;
  A.Start;
  while A.FRunning do
  begin
    Application.ProcessMessages;
    Sleep(0);
  end;
  A.Free;
end;

procedure TvgObject.DoAniFinished(Sender: TObject);
begin
  TvgAnimation(Sender).Free;
end;

{ Animations }

procedure TvgObject.StartAnimation(const AName: WideString);
var
  i: integer;
  E: TvgAnimation;
begin
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if TvgObject(FChildren[i]) is TvgAnimation then
        if CompareText(TvgAnimation(FChildren[i]).Name, AName) = 0 then
        begin
          E := TvgAnimation(FChildren[i]);
          E.Start;
        end;
    end;
end;

procedure TvgObject.StopAnimation(const AName: WideString);
var
  i: integer;
  E: TvgAnimation;
begin
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]) is TvgAnimation then
        if CompareText(TvgAnimation(FChildren[i]).Name, AName) = 0 then
        begin
          E := TvgAnimation(FChildren[i]);
          E.Stop;
        end;
end;

procedure TvgObject.StartTriggerAnimation(AInstance: TvgObject; ATrigger: string);
var
  i: integer;
begin
  StopTriggerAnimation(AInstance);
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if TvgObject(FChildren[i]) is TvgAnimation then
        TvgAnimation(FChildren[i]).StartTrigger(AInstance, ATrigger);
      { locked objects }
      if TvgObject(FChildren[i]).isVisual and TvgVisualObject(FChildren[i]).Locked and not TvgVisualObject(FChildren[i]).HitTest then
      begin
        TvgObject(FChildren[i]).StartTriggerAnimation(AInstance, ATrigger);
      end;
    end;
end;

procedure TvgObject.StartTriggerAnimationWait(AInstance: TvgObject; ATrigger: string);
var
  i: integer;
begin
  StopTriggerAnimation(AInstance);
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if TvgObject(FChildren[i]) is TvgAnimation then
      begin
        TvgAnimation(FChildren[i]).StartTrigger(AInstance, ATrigger);
        while TvgAnimation(FChildren[i]).Running do
        begin
          Application.ProcessMessages;
          Sleep(0);
        end;
      end;
      { locked objects }
      if TvgObject(FChildren[i]).isVisual and TvgVisualObject(FChildren[i]).Locked and not TvgVisualObject(FChildren[i]).HitTest then
      begin
        TvgObject(FChildren[i]).StartTriggerAnimationWait(AInstance, ATrigger);
      end;
    end;
end;

procedure TvgObject.StopTriggerAnimation(AInstance: TvgObject);
var
  i: integer;
  E: TvgAnimation;
begin
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if TvgObject(FChildren[i]) is TvgAnimation then
        if TvgAnimation(FChildren[i]).Trigger <> '' then
        begin
          E := TvgAnimation(FChildren[i]);
          E.Stop;
        end;
      { locked objects }
      if TvgObject(FChildren[i]).isVisual and TvgVisualObject(FChildren[i]).Locked and not TvgVisualObject(FChildren[i]).HitTest then
      begin
        TvgObject(FChildren[i]).StopTriggerAnimation(AInstance);
      end;
    end;
end;

procedure TvgObject.ApplyTriggerEffect(AInstance: TvgObject; ATrigger: string);
var
  i: integer;
begin
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if TvgObject(FChildren[i]) is TvgEffect then
        TvgEffect(FChildren[i]).ApplyTrigger(AInstance, ATrigger);
      { locked objects }
      if TvgObject(FChildren[i]).isVisual and TvgVisualObject(FChildren[i]).Locked and not TvgVisualObject(FChildren[i]).HitTest then
      begin
        TvgObject(FChildren[i]).ApplyTriggerEffect(AInstance, ATrigger);
      end;
    end;
end;

{ VCL }

procedure TvgObject.SetNewScene(AScene: IvgScene);
var
  i: integer;
begin
  FScene := AScene;
  if (FChildren <> nil) and (FChildren.Count > 0) then
    for i := 0 to FChildren.Count - 1 do
      TvgObject(FChildren[i]).SetNewScene(FScene);
end;

procedure TvgObject.UpdateChildScene;
var
  i: integer;
begin
  if (FScene = nil) and (FParent <> nil) then
    FScene := FParent.FScene;
  if (FChildren <> nil) and (FChildren.Count > 0) then
    for i := 0 to FChildren.Count - 1 do
      TvgObject(FChildren[i]).UpdateChildScene;
end;

procedure TvgObject.ChangeParent;
begin
end;

procedure TvgObject.SetParent(const Value: TvgObject);
begin
  if Parent <> Value then
  begin
    if FParent <> nil then
      FParent.RemoveObject(Self)
    else
      if FScene <> nil then
        FScene.RemoveObject(Self);
    FParent := Value;
    if FParent <> nil then
    begin
      FParent.AddObject(Self);
      ChangeParent;
    end;
  end;
end;

function TvgObject.GetChild(Index: integer): TvgObject;
begin
  if (FChildren <> nil) and (Index < FChildren.Count) then
    Result := TvgObject(FChildren[Index])
  else
    Result := nil;
end;

function TvgObject.GetChildrenCount: integer;
begin
  if (FChildren <> nil) then
    Result := FChildren.Count
  else
    Result := 0;
end;

procedure TvgObject.SetParentComponent(Value: TComponent);
var
  SI: IvgScene;
begin
  inherited ;
  if FParent <> nil then
    FParent.RemoveObject(Self);

  if (Value <> nil) and (Value is TvgObject) then
  begin
    TvgObject(Value).AddObject(Self);
  end
  else
    if (Value <> nil) and (Value is TvgCustomScene) then
    begin
      TvgCustomScene(Value).AddObject(Self);
    end
    else
    if (THackComponent(Value).QueryInterface(IvgScene, SI) = 0) and (Assigned(SI)) then
    begin
      SI.AddObject(Self);
    end
end;

procedure TvgObject.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i, j: Integer;
begin
  inherited;
  if (Self is TvgContent) then Exit;

  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if (TvgObject(FChildren[i]) is TvgContent) and (TvgContent(FChildren[i]).FChildren <> nil) then
      begin
        for j := 0 to TvgContent(FChildren[i]).FChildren.Count - 1 do
          if TvgObject(TvgContent(FChildren[i]).FChildren[j]).Stored then
            Proc(TComponent(TvgContent(FChildren[i]).FChildren[j]));
      end;
      if TvgObject(FChildren[i]).Stored then
      begin
        Proc(TComponent(FChildren[i]));
      end;
    end;
end;

function TvgObject.GetParentComponent: TComponent;
begin
  if (FParent <> nil) and (FParent is TvgContent) then
    Result := TvgContent(FParent).Parent
  else
  if (FParent <> nil) and (FParent is TvgComboListBox)  then
    Result := TvgComboListBox(FParent).Parent
  else
    Result := FParent;
  if (Result = nil) and (FScene <> nil) then
    Result := FScene.GetComponent;
end;

function TvgObject.HasParent: Boolean;
begin
  Result := true;
end;

{ binding }

function TvgObject.GetData: Variant;
begin
  Result := Name;
end;

procedure TvgObject.SetData(const Value: Variant);
begin
  // nothing
end;

function TvgObject.GetBinding(Index: string): Variant;
var
  Obj: TvgObject;
begin
  Obj := FindBinding(Index);
  if Obj <> nil then
    Result := Obj.Data
  else
    Result := '';
end;

procedure TvgObject.SetBinding(Index: string; const Value: Variant);
var
  Obj: TvgObject;
begin
  Obj := FindBinding(Index);
  if Obj <> nil then
    Obj.Data := Value;
end;

function TvgObject.FindBinding(const ABinding: string): TvgObject;
var
  i: integer;
begin
  Result := nil;
  if (FChildren <> nil) and (FChildren.Count > 0) then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      if CompareText(TvgObject(FChildren[i]).BindingName, ABinding) = 0 then
      begin
        Result := TvgObject(FChildren[i]);
        Exit;
      end;
      Result := TvgObject(FChildren[i]).FindBinding(ABinding);
      if Result <> nil then Exit;
    end;
  end;
end;

procedure TvgObject.SetBindingName(const Value: string);
begin
  if FBindingName <> Value then
  begin
    FBindingName := Value;
  end;
end;

{  }

function TvgObject.FindResource(const AResource: string): TvgObject;
var
  i: integer;
begin
  Result := nil;
  if (FChildren <> nil) and (FChildren.Count > 0) then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      if CompareText(TvgObject(FChildren[i]).ResourceName, AResource) = 0 then
      begin
        Result := TvgObject(FChildren[i]);
        Exit;
      end;
      Result := TvgObject(FChildren[i]).FindResource(AResource);
      if Result <> nil then Exit;
    end;
  end;
end;

procedure TvgObject.SetResourceName(const Value: string);
begin
  if FResourceName <> Value then
  begin
    FResourceName := Value;
  end;
end;

procedure TvgObject.SetStored(const Value: boolean);
var
  i: integer;
begin
  if FStored <> Value then
  begin
    FStored := Value;
    if (FChildren <> nil) and (FChildren.Count > 0) then
    begin
      for i := 0 to FChildren.Count - 1 do
      begin
        TvgObject(FChildren[i]).Stored := Value;
      end;
    end;
  end;
end;

procedure TvgObject.UpdateResource;
var
  i: integer;
begin
  if csLoading in ComponentState then Exit;
  if csDestroying in ComponentState then Exit;

  if (Self is TvgControl) then
    TvgControl(Self).Resource := TvgControl(Self).FResource;
  if (FChildren <> nil) and (FChildren.Count > 0) then
  begin
    for i := 0 to FChildren.Count - 1 do
    begin
      TvgObject(FChildren[i]).UpdateResource;
    end;
  end;
end;

procedure TvgObject.DeleteChildren;
var
  Child: TvgObject;
begin
  if Assigned(FChildren) then
  begin
    while FChildren.Count > 0 do
    begin
      Child := TvgObject(FChildren[0]);
      FChildren.Delete(0);
      Child.FParent := nil;
      Child.Free;
    end;
    FreeAndNil(FChildren);
  end;
end;

procedure TvgObject.AddObjectsToList(const AList: TList);
var
  i: integer;
begin
  AList.Add(Self);
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      TvgObject(FChildren[i]).AddObjectsToList(AList);
end;

procedure TvgObject.AddObject(AObject: TvgObject);
begin
  if FChildren = nil then
    FChildren := TvgObjectList.Create;
  FChildren.Add(AObject);
  AObject.FParent := Self;
  AObject.FScene := FScene;
  AObject.UpdateChildScene;
  AObject.ChangeParent;
  FRecalcHasEffect := true;
  if (isVisual) and not (csLoading in ComponentState) then
  begin
    TvgVisualObject(Self).RecalcUpdateRect;
    if HasEffect then
    begin
      TvgVisualObject(Self).FUpdateEffects := true;
      TvgVisualObject(Self).Repaint;
    end;
    if AObject.isVisual and (TvgVisualObject(AObject).Align <> vaNone) then
      TvgVisualObject(Self).Realign;
  end;
  if AObject.IsVisual and not (csLoading in ComponentState) then
  begin
    TvgVisualObject(AObject).RecalcOpacity;
  end;
end;

procedure TvgObject.RemoveObject(AObject: TvgObject);
begin
  AObject.FParent := nil;
  AObject.FScene := nil;
  if FChildren <> nil then
    FChildren.Remove(AObject);
  FRecalcHasEffect := true;
end;

procedure TvgObject.BringToFront;
begin
  if (Parent <> nil) and (Parent.FChildren <> nil) then
  begin
    Parent.FChildren.Remove(Self);
    Parent.FChildren.Add(Self);
    if isVisual then
      TvgVisualObject(Self).Repaint;
  end;
end;

procedure TvgObject.SendToBack;
begin
  if (Parent <> nil) and (Parent.FChildren <> nil) then
  begin
    Parent.FChildren.Remove(Self);
    Parent.FChildren.Insert(0, Self);
    if Parent.IsVisual then
      TvgVisualObject(Parent).Realign;
  end;
end;

{ TvgVisualObject ==================================================================}

constructor TvgVisualObject.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := true;
  FRecalcEnabled := true;
  FOpacity := 1;
  FLocalMatrix := IdentityMatrix;
  FPosition := TvgPosition.Create(vgPoint(0, 0));
  FPosition.OnChange := MatrixChanged;
  FScale := TvgPosition.Create(vgPoint(1, 1));
  FScale.OnChange := MatrixChanged;
  FSkew := TvgPosition.Create(vgPoint(0, 0));
  FSkew.OnChange := MatrixChanged;
  FRotateCenter := TvgPosition.Create(vgPoint(0.5, 0.5));
  FRotateCenter.OnChange := MatrixChanged;
  FMargins := TvgBounds.Create(vgRect(0, 0, 0, 0));
  FMargins.OnChange := MarginsChanged;
  FPadding := TvgBounds.Create(vgRect(0, 0, 0, 0));
  FPadding.OnChange := PaddingChanged;
  FWidth := 50;
  FLastWidth := FWidth;
  FHeight := 50;
  FLastHeight := FHeight;
  FVisible := true;
  FHitTest := true;
  FRecalcAbsolute := true;
  FRecalcOpacity := true;
  FUpdateEffects := true;
  FRecalcUpdateRect := true;
  FCanFocused := false;
  FCanClipped := true;
end;

destructor TvgVisualObject.Destroy;
begin
  if FEffectBitmap <> nil then
    FreeAndNil(FEffectBitmap);
  FMargins.Free;
  FPadding.Free;
  FRotateCenter.Free;
  FScale.Free;
  FSkew.Free;
  FPosition.Free;
  inherited;
end;

procedure TvgVisualObject.Loaded;
begin
  inherited;
  MatrixChanged(Self);
  if (FChildren <> nil) and (FChildren.Count > 0) then
    Realign;
end;

procedure TvgVisualObject.Notification(AComponent: TComponent;
      Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FPopupMenu) then
    FPopupMenu := nil;
end;

{ matrix }

procedure TvgVisualObject.MatrixChanged(Sender: TObject);
var
  RotMatrix: TvgMatrix;
  M1, M2: TvgMatrix;
begin
  if (FScene <> nil) and (not FScene.GetDisableUpdate) and (not FInPaintTo) then
    Repaint;
  FLocalMatrix := IdentityMatrix;
  FLocalMatrix.m31 := FPosition.X;
  FLocalMatrix.m32 := FPosition.Y;
  FLocalMatrix.m11 := FScale.X;
  FLocalMatrix.m22 := FScale.Y;
  if FRotateAngle <> 0 then
  begin
    M1 := IdentityMatrix;
    M1.m31 := -FRotateCenter.X * FWidth{ * FScale.X};
    M1.m32 := -FRotateCenter.Y * FHeight{ * FScale.Y};
    M2 := IdentityMatrix;
    M2.m31 := FRotateCenter.X * FWidth{ * FScale.X};
    M2.m32 := FRotateCenter.Y * FHeight{ * FScale.Y};
    RotMatrix := vgMatrixMultiply(M1, vgMatrixMultiply(vgCreateRotationMatrix(vgDegToRad(FRotateAngle)), M2));
    FLocalMatrix := vgMatrixMultiply(RotMatrix, FLocalMatrix);
  end;
  RecalcAbsolute;
  RecalcUpdateRect;
  if (FScene <> nil) and (not FScene.GetDisableUpdate) and (not FInPaintTo) then
    Repaint;
end;

procedure TvgVisualObject.RecalcUpdateRect;
var
  i: integer;
begin
  if FRecalcUpdateRect then Exit;
  FRecalcUpdateRect := true;
  if (Parent <> nil) and (Parent.IsVisual) then
    Parent.Visual.FRecalcUpdateRect := true;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if not TvgObject(FChildren[i]).isVisual then Continue;
      TvgVisualObject(FChildren[i]).RecalcUpdateRect;
    end;
end;

function TvgVisualObject.GetUpdateRect: TvgRect;
var
  R: TvgRect;
  P: TvgObject;
  i: integer;
begin
  if FRecalcUpdateRect then
  begin
    FRecalcUpdateRect := false;
    FUpdateRect := AbsoluteRect;
    if (FScene <> nil) and not (FScene.GetDisableUpdate) then
    begin
      if not (csLoading in ComponentState) then
      begin
        P := Parent;
        while (P <> nil) and (P.IsVisual) do
        begin
          if TvgVisualObject(P).ClipChildren then
            vgIntersectRect(FUpdateRect, FUpdateRect, TvgVisualObject(P).UpdateRect);
          P := P.Parent;
        end;
        { design }
        if (FScene <> nil) and (Self = FScene.GetSelected) then
        begin
          vgInflateRect(FUpdateRect, (GripSize) + 1, (GripSize) + 1);
          FUpdateRect.Top := FUpdateRect.Top - RotSize - GripSize;
        end;
        if (FScene <> nil) and (FScene.GetDesignPlaceObject = Self) then
        begin
          vgInflateRect(FUpdateRect, 1, 1);
          FUpdateRect.Top := FUpdateRect.Top - 20;
          if vgRectWidth(FUpdateRect) < 160 then
            FUpdateRect.Right := FUpdateRect.Left + 160;
        end;
        { Effects }
        if HasEffect and not ClipChildren then
        begin
          R := GetEffectsRect;
          with R do
            R := vgNormalizeRect([LocaltoAbsolute(vgPoint(Left, Top)), LocaltoAbsolute(vgPoint(Right, Top)),
              LocaltoAbsolute(vgPoint(Right, Bottom)), LocaltoAbsolute(vgPoint(Left, Bottom))]);
          FUpdateRect := vgUnionRect(FUpdateRect, R);
        end;
        { Children }
        if not ClipChildren and (FChildren <> nil) then
        begin
          for i := 0 to FChildren.Count - 1 do
          begin
            if not TvgObject(FChildren[i]).IsVisual then Continue;
            R := TvgVisualObject(FChildren[i]).UpdateRect;
            FUpdateRect := vgUnionRect(FUpdateRect, R);
          end;
        end;
      end;
    end;
  end;
  Result := FUpdateRect;
end;

function TvgVisualObject.GetChildrenRect: TvgRect;
var
  i: integer;
begin
  Result := AbsoluteRect;
  { children }
  if not ClipChildren and (FChildren <> nil) then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]).IsVisual and (TvgVisualObject(FChildren[i]).Visible) then
        Result := vgUnionRect(Result, TvgVisualObject(FChildren[i]).GetChildrenRect);
end;

function TvgVisualObject.GetAbsoluteWidth: single;
var
  V: TvgVector;
begin
  V := LocalToAbsoluteVector(vgVector(Width, Height));
  Result := V.X;
end;

function TvgVisualObject.GetAbsoluteHeight: single;
var
  V: TvgVector;
begin
  V := LocalToAbsoluteVector(vgVector(Width, Height));
  Result := V.Y;
end;

function TvgVisualObject.GetAbsoluteScale: TvgPoint;
var
  P: TvgObject;
begin
  Result := Scale.Point;
  P := Parent;
  while P <> nil do
  begin
    if P.isVisual then
    begin
      Result.X := Result.X * P.Visual.Scale.X;
      Result.Y := Result.Y * P.Visual.Scale.Y;
    end;
    P := P.Parent;
  end;
end;

function TvgVisualObject.GetChildrenMatrix: TvgMatrix;
begin
  Result := IdentityMatrix;
end;

function TvgVisualObject.GetAbsoluteMatrix: TvgMatrix;
begin
  if FRecalcAbsolute then
  begin
    if (FParent <> nil) and (FParent is TvgVisualObject) then
    begin
      FAbsoluteMatrix := vgMatrixMultiply(vgMatrixMultiply(FLocalMatrix, TvgVisualObject(FParent).GetChildrenMatrix), TvgVisualObject(FParent).AbsoluteMatrix);
    end
    else
      FAbsoluteMatrix := FLocalMatrix;

    Result := FAbsoluteMatrix;
    FRecalcAbsolute := false;
    if not FInPaintTo then
      FUpdateEffects := true;
    if (FScene <> nil) and (not FScene.GetDisableUpdate) and (not FInPaintTo) then
      Repaint;
  end
  else
  begin
    Result := FAbsoluteMatrix;
  end;
end;

function TvgVisualObject.GetInvertAbsoluteMatrix: TvgMatrix;
begin
  Result := AbsoluteMatrix;
  vgInvertMatrix(Result);
end;

procedure TvgVisualObject.RecalcAbsoluteNow;
var
  i: integer;
  Child: TvgVisualObject;
begin
  AbsoluteMatrix; // recalc
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if not TvgObject(FChildren[i]).isVisual then Continue;
      Child := TvgVisualObject(FChildren[i]);
      TvgVisualObject(Child).RecalcAbsoluteNow;
    end;
end;

procedure TvgVisualObject.RecalcAbsolute;
var
  i: integer;
  Child: TvgVisualObject;
begin
  FRecalcAbsolute := true;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if not TvgObject(FChildren[i]).isVisual then Continue;
      Child := TvgVisualObject(FChildren[i]);
      TvgVisualObject(Child).RecalcAbsolute;
    end;
end;

function TvgVisualObject.AbsoluteToLocalVector(P: TvgVector): TvgVector;
begin
  P.W := 0;
  Result := vgVectorTransform(P, InvertAbsoluteMatrix);
end;

function TvgVisualObject.LocalToAbsoluteVector(P: TvgVector): TvgVector;
begin
  P.W := 0;
  Result := vgVectorTransform(P, AbsoluteMatrix);
end;

function TvgVisualObject.AbsoluteToLocal(P: TvgPoint): TvgPoint;
var
  V: TvgVector;
begin
  V.X := P.X;
  V.Y := P.Y;
  V.W := 1;
  V := vgVectorTransform(V, InvertAbsoluteMatrix);
  Result.X := V.X;
  Result.Y := V.Y;
end;

function TvgVisualObject.LocalToAbsolute(P: TvgPoint): TvgPoint;
var
  V: TvgVector;
begin
  V.X := P.X;
  V.Y := P.Y;
  V.W := 1;
  V := vgVectorTransform(V, AbsoluteMatrix);
  Result := vgPoint(V.X, V.Y);
end;

{ Opacity }

function TvgVisualObject.GetAbsoluteOpacity: single;
begin
  if FRecalcOpacity then
  begin
    if (FParent <> nil) and (FParent is TvgVisualObject) then
      FAbsoluteOpacity := FOpacity * TvgVisualObject(FParent).AbsoluteOpacity
    else
      FAbsoluteOpacity := FOpacity;

    if not AbsoluteEnabled and (FScene <> nil) and ((FScene.GetRoot <> Self) and (FScene.GetRoot <> Parent)) then
      FAbsoluteOpacity := FAbsoluteOpacity * 0.8;

    Result := FAbsoluteOpacity;

    FRecalcOpacity := false;
  end
  else
  begin
    Result := FAbsoluteOpacity;
  end;
end;

procedure TvgVisualObject.RecalcOpacity;
var
  i: integer;
  Child: TvgVisualObject;
begin
  FRecalcOpacity := true;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
    begin
      if not TvgObject(FChildren[i]).isVisual then Continue;
      Child := TvgVisualObject(FChildren[i]);
      TvgVisualObject(Child).RecalcOpacity;
    end;
end;

{ methods }

function TvgVisualObject.pointInObject(X, Y: single): boolean;
var
  P: TvgPoint;
begin
  Result := false;
  P := AbsoluteToLocal(vgPoint(X, Y));
  if (P.X > 0) and (P.X < Width) and
     (P.Y > 0) and (P.Y < Height) then
  begin
    Result := true;
  end;
end;

function TvgVisualObject.CheckHitTest(const AHitTest: boolean): boolean;
begin
  Result := FHitTest;
  if ((Scene <> nil) and (Scene.GetDesignTime)) then
    Result := true;
  if (((Scene <> nil) and (Scene.GetDesignTime))) and FLocked then
    Result := false;
  if (((Scene <> nil) and (Scene.GetDesignTime))) and FDesignHide then
    Result := false;
end;

function TvgVisualObject.FindTarget(const APoint: TvgPoint; const Data: TvgDragObject): TvgVisualObject;
var
  i: integer;
  Obj, NewObj: TvgVisualObject;
  Accept: boolean;
begin
  if not Visible and not (Assigned(FScene) and (FScene.GetDesignTime)) then
  begin
    Result := nil;
    Exit;
  end;
  if (Self is TvgControl) and not TvgControl(Self).AbsoluteEnabled and not (Assigned(FScene) and (FScene.GetDesignTime)) then
  begin
    Result := nil;
    Exit;
  end;

  if FChildren <> nil then
    for i := FChildren.Count - 1 downto 0 do
    begin
      if not TvgObject(FChildren[i]).IsVisual then Continue;
      Obj := TvgVisualObject(FChildren[i]);
      if (not Obj.Visible) and not (((FScene <> nil) and FScene.GetDesignTime)) then Continue;
      if FDesignHide and (((FScene <> nil) and FScene.GetDesignTime)) then Continue;
      if ClipChildren and not PointInObject(APoint.X, APoint.Y) then Continue;

      NewObj := Obj.FindTarget(APoint, Data);
      if NewObj <> nil then
      begin
        Result := NewObj;
        Exit;
      end;
    end;

  Result := nil;
  Accept := false;
  DragOver(Data, APoint, Accept);
  if PointInObject(APoint.X, APoint.Y) and CheckHitTest(HitTest) and (Accept) then
    Result := Self;
end;

function TvgVisualObject.ObjectByPoint(X, Y: single): TvgVisualObject;
var
  i: integer;
  Obj, NewObj: TvgVisualObject;
begin
  if not Visible and not (Assigned(FScene) and (FScene.GetDesignTime)) then
  begin
    Result := nil;
    Exit;
  end;
  if (Self is TvgControl) and not TvgControl(Self).AbsoluteEnabled and not (Assigned(FScene) and (FScene.GetDesignTime)) then
  begin
    Result := nil;
    Exit;
  end;

  if FChildren <> nil then
    for i := FChildren.Count - 1 downto 0 do
    begin
      if not TvgObject(FChildren[i]).IsVisual then Continue;
      Obj := TvgVisualObject(FChildren[i]);
      if (not Obj.Visible) and not (((FScene <> nil) and FScene.GetDesignTime)) then Continue;
      if FDesignHide and (((FScene <> nil) and FScene.GetDesignTime)) then Continue;
      if ClipChildren and not PointInObject(X, Y) then Continue;

      NewObj := Obj.ObjectByPoint(X, Y);
      if NewObj <> nil then
      begin
        Result := NewObj;
        Exit;
      end;
    end;

  Result := nil;
  if PointInObject(X, Y) and CheckHitTest(HitTest) then
    Result := Self;
end;

function TvgVisualObject.GetCanvas: TvgCanvas;
begin
  if FTempCanvas <> nil then
    Result := FTempCanvas
  else
    if FScene <> nil then
      Result := FScene.GetCanvas
    else
      Result := nil;
end;

procedure TvgVisualObject.BeforePaint;
begin
end;

procedure TvgVisualObject.ApplyResource;
var
  i: integer;
begin
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]).isVisual then
        TvgVisualObject(FChildren[i]).ApplyResource;
end;

procedure TvgVisualObject.Paint;
begin
end;

procedure TvgVisualObject.AfterPaint;
begin
end;

procedure TvgVisualObject.SetInPaintTo(value: boolean);
var
  i: integer;
begin
  FInPaintTo := value;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]).isVisual then
        TvgVisualObject(FChildren[i]).SetInPaintTo(value);
end;

procedure TvgVisualObject.PaintTo(const ACanvas: TvgCanvas; const ARect: TvgRect; const AParent: TvgObject = nil);
var
  SaveIndex: integer;
  SaveTempCanvas: TvgCanvas;
  SaveDisableAlign: boolean;
  SavePos: TvgPoint;
  SaveScale: TvgPoint;
  SaveParent: TvgObject;
  SaveRotate: single;
begin
  if FScene = nil then Exit;
  if Width * Height = 0 then Exit;

  FScene.SetDisableUpdate(true);
  SaveDisableAlign := FDisableAlign;
  FDisableAlign := true;
  SetInPaintTo(true);
  try
    SaveTempCanvas := TempCanvas;
    TempCanvas := ACanvas;
    SaveIndex := TempCanvas.SaveCanvas;
    { save }
    SavePos := Position.Point;
    SaveScale := Scale.Point;
    SaveParent := FParent;
    SaveRotate := RotateAngle;
    FParent := AParent;
    FPosition.FX := ARect.Left;
    FPosition.FY := ARect.Top;
    FScale.FX := vgRectWidth(ARect) / Width;
    FScale.FY := vgRectHeight(ARect) / Height;
    FRotateAngle := 0;
    MatrixChanged(Self);

    { paint }
    TempCanvas.SetMatrix(AbsoluteMatrix);
    BeforePaint;
    Paint;
    AfterPaint;
    PaintChildren;

    { restore }
    FRotateAngle := SaveRotate;
    FPosition.FX := SavePos.X;
    FPosition.FY := SavePos.Y;
    FScale.FX := SaveScale.X;
    FScale.FY := SaveScale.Y;
    FParent := SaveParent;
    MatrixChanged(Self);
    RecalcUpdateRect;
    RecalcAbsoluteNow;
    RecalcOpacity;
    RecalcEnabled;
  finally
    SetInPaintTo(false);
    FDisableAlign := SaveDisableAlign;
    TempCanvas.RestoreCanvas(SaveIndex);
    TempCanvas := SaveTempCanvas;
    FScene.SetDisableUpdate(false);
  end;
end;

procedure TvgVisualObject.UpdateEffects;
var
  P: TvgObject;
begin
  if HasEffect then
  begin
    FUpdateEffects := true;
    P := Parent;
    while P <> nil do
    begin
      if P.IsVisual then
        P.Visual.UpdateEffects;
      P := P.Parent;
    end;
  end;
end;

procedure TvgVisualObject.ApplyEffect;
var
  i, State, State2: integer;
  M: TvgMatrix;
  R: TvgRect;
  Effect: TvgEffect;
  EffectRect: TvgRect;
begin
  if FChildren = nil then Exit;
  if FScene = nil then Exit;
  if FDisableEffect then Exit;
  if not HasEffect then Exit;

  State := Canvas.SaveCanvas;
  if not FUpdateEffects and not HasDisablePaintEffect then
  begin
    if FEffectBitmap <> nil then
    begin
      for i := 0 to FChildren.Count - 1 do
      if (TvgObject(FChildren[i]) is TvgEffect) and (TvgEffect(FChildren[i]).Enabled) then
      begin
        Effect := TvgEffect(FChildren[i]);
        EffectRect := Effect.GetRect(vgRect(0, 0, Width, Height));
        Canvas.DrawBitmap(FEffectBitmap, vgRect(0, 0, FEffectBitmap.Width, FEffectBitmap.Height), EffectRect, AbsoluteOpacity, RotateAngle = 0);
      end;
    end;
  end
  else
  begin
    for i := 0 to FChildren.Count - 1 do
    if (TvgObject(FChildren[i]) is TvgEffect) and (TvgEffect(FChildren[i]).Enabled) then
    begin
      Effect := TvgEffect(FChildren[i]);
      EffectRect := Effect.GetRect(vgRect(0, 0, Width, Height));
      with GetAbsoluteScale do
        vgMultiplyRect(EffectRect, X, Y);
      if FEffectBitmap = nil then
      begin
        { create }
        FEffectBitmap := TvgBitmap.Create(trunc(vgRectWidth(EffectRect)), trunc(vgRectHeight(EffectRect)));
      end
      else
        if (FEffectBitmap.Width <> trunc(vgRectWidth(EffectRect))) or
           (FEffectBitmap.Height <> trunc(vgRectHeight(EffectRect))) then
        begin
          { resize }
          FEffectBitmap.SetSize(trunc(vgRectWidth(EffectRect)), trunc(vgRectHeight(EffectRect)));
          FEffectBitmap.Clear(0);
        end
        else
          FEffectBitmap.Clear(0);
      { Paint Self }
      State2 := FEffectBitmap.Canvas.SaveCanvas;
      M := IdentityMatrix;
      FEffectBitmap.Canvas.SetMatrix(M);
      R := vgRect(Effect.GetOffset.X, Effect.GetOffset.Y, (Effect.GetOffset.X + Width), (Effect.GetOffset.Y + Height));
      with GetAbsoluteScale do
        vgMultiplyRect(R, X, Y);

      PaintTo(FEffectBitmap.Canvas, R);

      FEffectBitmap.Canvas.RestoreCanvas(State2);
      { apply effects }
      with GetAbsoluteScale do
      begin
        Effect.ProcessEffect(FEffectBitmap.Canvas, FEffectBitmap, X);
        { draw effectBitmap }
        vgMultiplyRect(EffectRect, 1 / X, 1 / Y);
      end;
      Canvas.DrawBitmap(FEffectBitmap, vgRect(0, 0, FEffectBitmap.Width, FEffectBitmap.Height), EffectRect, AbsoluteOpacity, RotateAngle = 0);
    end;
    FUpdateEffects := false;
  end;
  Canvas.RestoreCanvas(State);
end;

procedure TvgVisualObject.PaintChildren;
var
  i, j: integer;
  R: TvgRect;
  State, State3: cardinal;
  ClipParentObject: TvgVisualObject;
  AllowPaint: boolean;
begin
  if FScene = nil then Exit;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]).IsVisual and
         ((TvgVisualObject(FChildren[i]).Visible) or
          (not TvgVisualObject(FChildren[i]).Visible and (Assigned(FScene) and (FScene.GetDesignTime)) and not TvgVisualObject(FChildren[i]).Locked)) then
        with TvgVisualObject(FChildren[i]) do
        begin
          if (((Scene <> nil) and (Scene.GetDesignTime))) and FDesignHide then Continue;

          FScene := Self.FScene;
          if Self.ClipChildren and not vgIntersectRect(Self.UpdateRect, UpdateRect) then Continue;
          if (Abs(Scale.X) < 0.01) or (Abs(Scale.Y) < 0.01) then Continue;

          AllowPaint := false;
          if (Assigned(FScene) and (FScene.GetDesignTime)) or FInPaintTo then
            AllowPaint := true;
          if not AllowPaint then
          begin
            R := vgUnionRect(GetChildrenRect, UpdateRect);
            for j := 0 to FScene.GetUpdateRectsCount - 1 do
              if vgIntersectRect(FScene.GetUpdateRect(j), R) then
              begin
                AllowPaint := true;
                Break;
              end;
          end;

          if AllowPaint then
          begin
            if Self.FClipChildren and CanClipped then
            begin
              State := Canvas.SaveCanvas;
              Canvas.SetMatrix(Self.AbsoluteMatrix);
              Canvas.IntersectClipRect(Self.ClipRect);
            end
            else
              State := 0;
            Canvas.SetMatrix(AbsoluteMatrix);
            BeforePaint;
            if not HasAfterPaintEffect then
              ApplyEffect;
            Canvas.SetMatrix(AbsoluteMatrix);
            if not HasDisablePaintEffect then
            begin
              ClipParentObject := HasClipParent;
              if ClipParentObject <> nil then
              begin
                State3 := Canvas.SaveCanvas;

                Canvas.SetMatrix(ClipParentObject.AbsoluteMatrix);
                Canvas.ExcludeClipRect(ClipParentObject.LocalRect);
                Canvas.SetMatrix(AbsoluteMatrix);

                Paint;
                Canvas.RestoreCanvas(State3);
                PaintChildren;
                if Assigned(FOnPaint) then
                begin
                  Canvas.SetMatrix(AbsoluteMatrix);
                  FOnPaint(TvgVisualObject(Self.FChildren[i]), Canvas, LocalRect);
                end;
              end
              else
              begin
                Paint;
                PaintChildren;
                if Assigned(FOnPaint) then
                begin
                  Canvas.SetMatrix(AbsoluteMatrix);
                  FOnPaint(TvgVisualObject(Self.FChildren[i]), Canvas, LocalRect);
                end;
              end;
            end;
            AfterPaint;
            // design selection
            if (vgDesigner <> nil) and (Assigned(Self.Scene)) and (Self.GetOwner <> nil) and
               (Self.FChildren[i] <> Scene.GetSelected) and (vgDesigner.IsSelected(Self.Scene.GetOwner, TvgVisualObject(Self.FChildren[i]))) then
            begin
              Canvas.SetMatrix(AbsoluteMatrix);
              Canvas.Stroke.Style := vgBrushSolid;
              Canvas.Stroke.SolidColor := $B200CC5A;
              Canvas.StrokeCap := vgCapFlat;
              Canvas.StrokeJoin := vgJoinMiter;
              Canvas.StrokeDash := vgDashDash;
              Canvas.StrokeThickness := 1;
              R := vgRect(0, 0, Width, Height);
              vgInflateRect(R, -1, -1);
              Canvas.DrawRect(R, 1, 1, AllCorners, 1);
              Canvas.StrokeDash := vgDashSolid;
            end;
            // drag highlight
            if IsDragOver and not DragDisableHighlight then
            begin
              Canvas.SetMatrix(AbsoluteMatrix);
              Canvas.Stroke.Style := vgBrushSolid;
              Canvas.Stroke.SolidColor := $B2005ACC;
              Canvas.StrokeCap := vgCapFlat;
              Canvas.StrokeJoin := vgJoinMiter;
              Canvas.StrokeDash := vgDashSolid;
              Canvas.StrokeThickness := 3;
              R := vgRect(0, 0, Width, Height);
              vgInflateRect(R, -1, -1);
              Canvas.DrawRect(R, 1, 1, AllCorners, 1);
              Canvas.StrokeDash := vgDashSolid;
            end;
            if State > 0 then
            begin
              Canvas.RestoreCanvas(State);
            end;
          end;
          if HasAfterPaintEffect then
          begin
            Canvas.SetMatrix(AbsoluteMatrix);
            ApplyEffect;
          end;
          {$IFDEF BOUNDS}
          State3 := Canvas.SaveCanvas;
          Canvas.ResetClipRect;
          Canvas.SetMatrix(IdentityMatrix);
          Canvas.Stroke.Style := vgBrushSolid;
          Canvas.Stroke.Color := '#FF00FF00';
          Canvas.StrokeThickness := 1;
          R := AbsoluteRect;
          Canvas.DrawRect(R, 0, 0, Allcorners, 0.5);
          Canvas.RestoreCanvas(State3);
          {$ENDIF}
        end;
end;

function TvgVisualObject.CheckParentVisible: boolean;
var
  P: TvgObject;
begin
  P := Self;
  Result := false;
  while P <> nil do
  begin
    if P.IsVisual and not TvgVisualObject(P).Visible then Exit;
    P := P.Parent;
  end;
  Result := true;
end;

procedure TvgVisualObject.Repaint;
begin
  if not Visible and (FScene <> nil) and (not FScene.GetDesignTime) then Exit;
  if FScene = nil then Exit;
  if (((Scene <> nil) and (Scene.GetDesignTime))) and FDesignHide then Exit;;
  if FScene.GetDisableUpdate then Exit;
  if not (((Scene <> nil) and (Scene.GetDesignTime))) and not CheckParentVisible then Exit;;
  FScene.AddUpdateRect(UpdateRect);
end;


procedure TvgVisualObject.Lock;
var
  i: integer;
begin
  Locked := true;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]).isVisual then
        TvgVisualObject(FChildren[i]).Lock;
end;

{ bounds }

function TvgVisualObject.GetLocalRect: TvgRect;
begin
  Result := vgRect(0, 0, FWidth, FHeight);
end;

function TvgVisualObject.GetAbsoluteRect: TvgRect;
begin
  Result := vgNormalizeRect([LocalToAbsolute(vgPoint(0, 0)), LocalToAbsolute(vgPoint(Width, 0)),
    LocalToAbsolute(vgPoint(Width, Height)), LocalToAbsolute(vgPoint(0, Height))]);
end;

function TvgVisualObject.GetClipRect: TvgRect;
begin
  Result := vgRect(0, 0, Width, Height);
end;

function TvgVisualObject.GetBoundsRect: TvgRect;
begin
  Result := vgRect(0, 0, Width, Height);
end;

function TvgVisualObject.GetParentedRect: TvgRect;
begin
  Result := vgRect(0, 0, Width, Height);
  vgOffsetRect(Result, Position.X, Position.Y);
end;

procedure TvgVisualObject.SetBoundsRect(const Value: TvgRect);
var
  P: TvgPoint;
begin
  Repaint;
  FWidth := Value.Right - Value.Left;
  FHeight := Value.Bottom - Value.Top;
  P := LocalToAbsolute(vgPoint(Value.Left + FRotateCenter.X * FWidth, Value.Top + FRotateCenter.Y * FHeight));
  if (Parent <> nil) and (Parent.isVisual) then
    P := TvgVisualObject(Parent).AbsoluteToLocal(P);
  FPosition.FX := P.X - FScale.X * FRotateCenter.X * FWidth;
  FPosition.FY := P.Y - FScale.Y * FRotateCenter.Y * FHeight;
  if (FWidth < 0) then
  begin
    FWidth := Abs(FWidth);
    FScale.X := -FScale.X;
  end;
  if (FHeight < 0) then
  begin
    FHeight := Abs(FHeight);
    FScale.Y := -FScale.Y;
  end;
  MatrixChanged(Self);
  Realign;
end;

{ }

procedure TvgVisualObject.PaddingChanged(Sender: TObject);
begin
  if (FParent <> nil) and (FParent.isVisual) then
    TvgVisualObject(FParent).Realign;
end;

procedure TvgVisualObject.MarginsChanged(Sender: TObject);
begin
  Realign;
end;

procedure TvgVisualObject.DesignInsert;
begin

end;

procedure TvgVisualObject.DesignSelect;
begin

end;

procedure TvgVisualObject.DesignClick;
begin

end;

procedure TvgVisualObject.DisableAlign;
begin
  FDisableAlign := true;
end;

procedure TvgVisualObject.EnableAlign;
begin
  FDisableAlign := false;
end;

procedure TvgVisualObject.Realign;
var
  i: integer;
  R: TvgRect;
  AlignList: TList;

  function InsertBefore(C1, C2: TvgVisualObject; AAlign: TvgAlign): Boolean;
  begin
    Result := False;
    case AAlign of
      vaTop: Result := C1.Position.Y < C2.Position.Y;
      vaBottom: Result := (C1.Position.Y + C1.Height) >= (C2.Position.Y + C2.Height);
      vaLeft, vaMostLeft: Result := C1.Position.X < C2.Position.X;
      vaRight, vaMostRight: Result := (C1.Position.X + C1.Width) >= (C2.Position.X + C2.Width);
    end;
  end;

  procedure DoPosition(Control: TvgVisualObject; AAlign: TvgAlign; AlignInfo: TvgAlignInfo);
  var
    NewLeft, NewTop, NewWidth, NewHeight: single;
    cR, mR: TvgRect;
    fitScale: single;
  begin
    if not Control.Visible then Exit;
    with R do
    begin
      NewWidth := Right - Left;
      if (NewWidth < 0) or (AAlign in [vaLeft, vaRight, vaVertical, vaMostLeft, vaMostRight, vaTopLeft, vaTopRight, vaBottomLeft, vaBottomRight]) then
        NewWidth := Control.Width + Control.Padding.Left + Control.Padding.Right;
      NewHeight := Bottom - Top;
      if (NewHeight < 0) or (AAlign in [vaTop, vaBottom, vaHorizontal, vaTopLeft, vaTopRight, vaBottomLeft, vaBottomRight]) then
        NewHeight := Control.Height + Control.Padding.Top + Control.Padding.Bottom;
      NewLeft := Left;
      NewTop := Top;
      if (AAlign in [vaVertical]) then
        NewLeft := Control.Position.X + Control.Padding.Left;
      if (AAlign in [vaHorizontal]) then
        NewTop := Control.Position.Y + Control.Padding.Top;
      case AAlign of
        vaTop:
          Top := Top + NewHeight;
        vaBottom:
          begin
            Bottom := Bottom - NewHeight;
            NewTop := Bottom;
          end;
        vaLeft, vaMostLeft:
          Left := Left + NewWidth;
        vaRight, vaMostRight:
          begin
            Right := Right - NewWidth;
            NewLeft := Right;
          end;
        vaContents:
          begin
            NewLeft := 0;
            NewTop := 0;
            NewWidth := Width;
            NewHeight := Height;
            Control.FPosition.FX := NewLeft + Control.Padding.Left;
            Control.FPosition.FY := NewTop + Control.Padding.Top;
            Control.FWidth := NewWidth - Control.Padding.Left - Control.Padding.Right;
            Control.FHeight := NewHeight - Control.Padding.Top - Control.Padding.Bottom;
            Control.MatrixChanged(Self);
            Control.Realign;
            Exit;
          end;
        vaFit:
          begin
            mR := vgRect(Margins.Left, Margins.Top, Width - Margins.Right, Height - Margins.Bottom);
            cR := vgRect(Control.FPosition.FX - Control.Padding.Left, Control.FPosition.FY - Control.Padding.Top,
              Control.FPosition.FX + Control.FWidth + Control.Padding.Right,
              Control.FPosition.FY + Control.FHeight + Control.Padding.Bottom);
            fitScale := vgFitRect(cR, mR);
            if fitScale < 1 then
            begin
              cR.Left := cR.Left / fitScale;
              cR.Right := cR.Right / fitScale;
              cR.Top := cR.Top / fitScale;
              cR.Bottom := cR.Bottom / fitScale;
              vgRectCenter(cR, mR);
              NewLeft := cR.Left;
              NewTop := cR.Top;
              NewWidth := cR.Right - cR.Left;
              NewHeight := cR.Bottom - cR.Top;
            end
            else
            begin
              NewLeft := cR.Left;
              NewTop := cR.Top;
              NewWidth := cR.Right - cR.Left;
              NewHeight := cR.Bottom - cR.Top;
            end;
            Control.FPosition.FX := NewLeft + Control.Padding.Left;
            Control.FPosition.FY := NewTop + Control.Padding.Top;
            Control.FWidth := NewWidth - Control.Padding.Left - Control.Padding.Right;
            Control.FHeight := NewHeight - Control.Padding.Top - Control.Padding.Bottom;
            Control.MatrixChanged(Self);
            Control.Realign;
            Exit;
          end;
        vaCenter:
          begin
            NewLeft := Left + Trunc((NewWidth - (Control.Width + Control.Padding.Left + Control.Padding.Right)) / 2);
            NewWidth := (Control.Width + Control.Padding.Left + Control.Padding.Right);
            NewTop := Top + Trunc((NewHeight - (Control.Height + Control.Padding.Top + Control.Padding.Bottom)) / 2);
            NewHeight := (Control.Height + Control.Padding.Top + Control.Padding.Bottom);
          end;
        vaHorzCenter:
          begin
            NewLeft := Left + Trunc((NewWidth - (Control.Width + Control.Padding.Left + Control.Padding.Right)) / 2);
            NewWidth := (Control.Width + Control.Padding.Left + Control.Padding.Right);
          end;
        vaVertCenter:
          begin
            NewTop := Top + Trunc((NewHeight - (Control.Height + Control.Padding.Top + Control.Padding.Bottom)) / 2);
            NewHeight := (Control.Height + Control.Padding.Top + Control.Padding.Bottom);
          end;
        vaTopRight:
          begin
            Control.Position.X := Control.Position.X + (FWidth - FLastWidth);
            Exit;
          end;
        vaBottomLeft:
          begin
            Control.Position.Y := Control.FPosition.Y + (FHeight - FLastHeight);
            Exit;
          end;
        vaBottomRight:
          begin
            Control.Position.SetPoint(vgPoint(Control.Position.X + (FWidth - FLastWidth), Control.FPosition.Y + (FHeight - FLastHeight)));
            Exit;
          end;
      end;
    end;

    if (AALign = vaScale) then
    begin
      if (FLastWidth > 0) and (FLastHeight > 0) and (FWidth > 0) and (FHeight > 0) then
      begin
        Control.FPosition.FX := Control.FPosition.X * (FWidth / FLastWidth);
        Control.FPosition.FY := Control.FPosition.Y * (FHeight / FLastHeight);
        Control.FWidth := Control.FWidth * (FWidth / FLastWidth);
        Control.FHeight := Control.FHeight * (FHeight / FLastHeight);
        Control.MatrixChanged(Self);
        Control.Realign;
      end;
      Exit;
    end
    else
    begin
      Control.FPosition.FX := NewLeft + Control.Padding.Left;
      Control.FPosition.FY := NewTop + Control.Padding.Top;
      if (Control.FWidth <> NewWidth - Control.Padding.Left - Control.Padding.Right) or
         (Control.FHeight <> NewHeight - Control.Padding.Top - Control.Padding.Bottom) then
      begin
        Control.FWidth := NewWidth - Control.Padding.Left - Control.Padding.Right;
        Control.FHeight := NewHeight - Control.Padding.Top - Control.Padding.Bottom;
        Control.Realign;
      end;
      Control.MatrixChanged(Self);
    end;

    { Adjust client rect if control didn't resize as we expected }
    if (Control.Width + Control.Padding.Left + Control.Padding.Right <> NewWidth) or
       (Control.Height + Control.Padding.Top + Control.Padding.Bottom <> NewHeight) then
      with R do
        case AAlign of
          vaTop:
            Top := Top - (NewHeight - (Control.Height + Control.Padding.Left + Control.Padding.Right));
          vaBottom:
            Bottom := Bottom + (NewHeight - (Control.Height + Control.Padding.Top + Control.Padding.Bottom));
          vaLeft:
            Left := Left - (NewWidth - (Control.Width + Control.Padding.Left + Control.Padding.Right));
          vaRight:
            Right := Right + (NewWidth - (Control.Width + Control.Padding.Top + Control.Padding.Bottom));
          vaClient:
            begin
              Right := Right + NewWidth - (Control.Width + Control.Padding.Left + Control.Padding.Right);
              Bottom := Bottom + NewHeight - (Control.Height + Control.Padding.Top + Control.Padding.Bottom);
            end;
        end;
  end;

  procedure DoAlign(AAlign: TvgAlign);
  var
    I, J: Integer;
    Control: TvgVisualObject;
    AlignInfo: TvgAlignInfo;
  begin
    AlignList.Clear;
    for I := 0 to FChildren.Count - 1 do
    begin
      if not TvgObject(FChildren[i]).isVisual then Continue;
      Control := TvgVisualObject(FChildren[i]);
      if (Control.Align = AAlign) and ((AAlign = vaNone) or (Control.Visible) or
         (Assigned(FScene) and (FScene.GetDesignTime))) then
      begin
        J := 0;
        while (J < AlignList.Count) and not InsertBefore(Control, TvgVisualObject(AlignList[J]), AAlign) do Inc(J);
        AlignList.Insert(J, Control);
      end;
    end;
    for I := 0 to AlignList.Count - 1 do
    begin
      AlignInfo.AlignList := AlignList;
      AlignInfo.ControlIndex := I;
      AlignInfo.Align := AAlign;
      DoPosition(TvgVisualObject(AlignList[I]), AAlign, AlignInfo);
    end;
  end;

{  function AlignWork: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := ControlCount - 1 downto 0 do
      if (Controls[I].Align <> alNone) or
        (Controls[I].Anchors <> [akLeft, akTop]) then Exit;
    Result := False;
  end;}

const
  AllowAlign: boolean = true;
  NeedAlign: boolean = false;
begin
  if csDestroying in ComponentState then Exit;
  if ((FWidth > -2) and (FWidth < 2)) or ((FHeight > -2) and (FHeight < 2)) then Exit;
  if FDisableAlign then Exit;
  if csLoading in ComponentState then
  begin
    FLastWidth := FWidth;
    FLastHeight := FHeight;
    Exit;
  end;
  if FChildren = nil then Exit;
  if FChildren.Count = 0 then Exit;

  FDisableAlign := true;
  try
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]).isVisual and (TvgVisualObject(FChildren[i]).Align <> vaNone) then
      begin
        NeedAlign := true;
        Break;
      end;

    if NeedAlign then
    begin
      R := vgRect(0, 0, FWidth, FHeight);
      R := FMargins.MarginRect(R);
      if AllowAlign then
      begin
        AlignList := TList.Create;
        try
          DoAlign(vaMostLeft);
          DoAlign(vaMostRight);
          DoAlign(vaTop);
          DoAlign(vaBottom);
          DoAlign(vaLeft);
          DoAlign(vaRight);
          DoAlign(vaClient);
          DoAlign(vaHorizontal);
          DoAlign(vaVertical);
          DoAlign(vaContents);
          DoAlign(vaCenter);
          DoAlign(vaHorzCenter);
          DoAlign(vaVertCenter);
          DoAlign(vaScale);
          DoAlign(vaFit);
          // Move anchored controls
          // DoAlign(vaTopLeft); nothing to move
          DoAlign(vaTopRight);
          DoAlign(vaBottomLeft);
          DoAlign(vaBottomRight);
        finally
          AlignList.Free;
        end;
      end;
      if (FLastWidth <> FWidth) or (FLastHeight <> FHeight) then
      begin
        FUpdateEffects := true;
      end;
      FLastWidth := FWidth;
      FLastHeight := FHeight;
      Repaint;
    end;
  finally
    FDisableAlign := false;
  end;
end;

{ events }

procedure TvgVisualObject.KeyDown(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  P: TPoint;
  VP: TvgPoint;
begin
  if (Key = VK_APPS) then
  begin
    VP := LocalToAbsolute(vgPoint(Width / 2, Height / 2));
    P := Point(Trunc(VP.X), Trunc(VP.Y));
    if Scene.GetComponent is TControl then
      P := TControl(Scene.GetComponent).ClientToScreen(P);
    ContextMenu(vgPoint(P.X, P.Y));
  end
  else
    if Assigned(FOnKeyDown) then
      FOnKeyDown(Key, KeyChar, Shift);
end;

procedure TvgVisualObject.KeyUp(var Key: Word; var KeyChar: System.WideChar;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Key, KeyChar, Shift);
end;

procedure TvgVisualObject.Capture;
begin
  if (FScene <> nil) then
  begin
    FScene.SetCaptured(Self);
  end;
end;

procedure TvgVisualObject.ReleaseCapture;
begin
  if (FScene <> nil) and (FScene.GetCaptured = Self) then
  begin
    FScene.SetCaptured(nil);
  end;
end;

procedure TvgVisualObject.MouseEnter;
begin
  FIsMouseOver := true;
  StartTriggerAnimation(Self, 'IsMouseOver');
  ApplyTriggerEffect(Self, 'IsMouseOver');
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TvgVisualObject.MouseLeave;
begin
  FIsMouseOver := false;
  StartTriggerAnimation(Self, 'IsMouseOver');
  ApplyTriggerEffect(Self, 'IsMouseOver');
  if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TvgVisualObject.EnterFocus;
begin
  if not CanFocused then Exit;

  FIsFocused := true;
  Repaint;
  StartTriggerAnimation(Self, 'IsFocused');
  ApplyTriggerEffect(Self, 'IsFocused');
end;

procedure TvgVisualObject.KillFocus;
begin
  if not CanFocused then Exit;

  FIsFocused := false;
  Repaint;
  StartTriggerAnimation(Self, 'IsFocused');
  ApplyTriggerEffect(Self, 'IsFocused');
end;

procedure TvgVisualObject.SetFocus;
begin
  if not CanFocused then Exit;
  FScene.SetFocused(Self);
end;

procedure TvgVisualObject.ContextMenu(const ScreenPosition: TvgPoint);
begin
  if FPopupMenu <> nil then
  begin
    FPopupMenu.PopupComponent := Self;
    FPopupMenu.Popup(round(ScreenPosition.X), round(ScreenPosition.Y));
  end;
end;

procedure TvgVisualObject.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TvgVisualObject.DblClick;
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TvgVisualObject.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
var
  P: TPoint;
  VP: TvgPoint;
begin
  if (FDragMode = vgDragAutomatic) and Assigned(FScene) then
  begin
    FScene.BeginVCLDrag(Self);
    Exit;
  end;
  if (ssDouble in Shift) and (((FScene <> nil) and (FScene.GetDesignTime))) then
    FScene.SetDesignRoot(Self);
  if CanFocused and not FIsFocused and (FScene <> nil) and (FScene.GetFocused <> Self) then
    SetFocus;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
  if (Button = mbRight) then
  begin
    VP := LocalToAbsolute(vgPoint(X, Y));
    P := Point(Trunc(VP.X), Trunc(VP.Y));
    P := Scene.ClientToScreen(P);
    ContextMenu(vgPoint(P.X, P.Y));
    Exit;
  end;
  if FAutoCapture then
    Capture;
  if (ssDouble in Shift) then
  begin
    DblClick;
    FDoubleClick := true;
  end
  else
  if Button = mbLeft then
  begin
    FPressed := true;
  end;
end;

procedure TvgVisualObject.MouseMove(Shift: TShiftState; X, Y, Dx, Dy: single);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y, Dx, Dy);
end;

procedure TvgVisualObject.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: single);
begin
  ReleaseCapture;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
  if FPressed and not (FDoubleClick) and vgPtInRect(vgPoint(X, Y), LocalRect) then
  begin
    FPressed := false;
    Click;
  end;
  FDoubleClick := false;
end;

procedure TvgVisualObject.MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean);
begin
  if Assigned(FOnMouseWheel) then
    if FScene <> nil then
      FOnMouseWheel(Self, Shift, WheelDelta, FScene.GetMousePos, Handled)
    else
      FOnMouseWheel(Self, Shift, WheelDelta, vgPoint(0, 0), Handled);
end;

procedure TvgVisualObject.DragEnter(const Data: TvgDragObject; const Point: TvgPoint);
begin
  FIsDragOver := true;
  Repaint;
  StartTriggerAnimation(Self, 'IsDragOver');
  ApplyTriggerEffect(Self, 'IsDragOver');
  if Assigned(OnDragEnter) then
    OnDragEnter(Self, Data, Point);
end;

procedure TvgVisualObject.DragLeave;
begin
  FIsDragOver := false;
  Repaint;
  StartTriggerAnimation(Self, 'IsDragOver');
  ApplyTriggerEffect(Self, 'IsDragOver');
  if Assigned(OnDragLeave) then
    OnDragLeave(Self);
end;

procedure TvgVisualObject.DragOver(const Data: TvgDragObject; const Point: TvgPoint; var Accept: Boolean);
begin
  if Assigned(OnDragOver) then
    OnDragOver(Self, Data, Point, Accept);
end;

procedure TvgVisualObject.DragDrop(const Data: TvgDragObject; const Point: TvgPoint);
begin
  FIsDragOver := false;
  Repaint;
  StartTriggerAnimation(Self, 'IsDragOver');
  ApplyTriggerEffect(Self, 'IsDragOver');
  if Assigned(OnDragDrop) then
    OnDragDrop(Self, Data, Point);
end;

{ controls }

procedure TvgVisualObject.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    RecalcEnabled;
    RecalcOpacity;
    Repaint;
  end;
end;

function TvgVisualObject.GetAbsoluteEnabled: boolean;
begin
  if FRecalcEnabled then
  begin
    if (FParent <> nil) and (FParent is TvgVisualObject) and (not TvgVisualObject(FParent).AbsoluteEnabled) then
      FAbsoluteEnabled := false
    else
      FAbsoluteEnabled := FEnabled;

    Result := FAbsoluteEnabled;
    FRecalcEnabled := false;
  end
  else
  begin
    Result := FAbsoluteEnabled;
  end;
end;

procedure TvgVisualObject.RecalcEnabled;
var
  i: integer;
begin
  FRecalcEnabled := true;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]) is TvgVisualObject then
        TvgVisualObject(FChildren[i]).RecalcEnabled;
end;

{ properties }

procedure TvgVisualObject.SetTempCanvas(const Value: TvgCanvas);
var
  i: integer;
begin
  FTempCanvas := Value;
  if (FChildren <> nil) and (FChildren.Count > 0) then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]).isVisual then
        TvgVisualObject(FChildren[i]).TempCanvas := Value;
end;

procedure TvgVisualObject.SetHitTest(const Value: boolean);
begin
  FHitTest := Value;
end;

procedure TvgVisualObject.SetClipChildren(const Value: boolean);
begin
  if FClipChildren <> Value then
  begin
    FClipChildren := Value;
    Repaint;
  end;
end;

procedure TvgVisualObject.SetAlign(const Value: TvgAlign);
begin
  if FAlign <> Value then
  begin
    FAlign := Value;
    if (FParent <> nil) and (TvgObject(FParent).IsVisual) then
    begin
      TvgVisualObject(FParent).Realign;
    end;
  end;
end;

procedure TvgVisualObject.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    if FVisible then
    begin
      if FScene <> nil then
        FScene.AddUpdateRect(UpdateRect);
    end;
    FVisible := Value;
    if FVisible and not (csLoading in ComponentState) then
    begin
      if (Parent <> nil) and (Parent.IsVisual) and (Align <> vaNone) then
        TvgVisualObject(Parent).Realign;
    end;
    if FVisible then
    begin
      if FScene <> nil then
        FScene.AddUpdateRect(UpdateRect);
      StartTriggerAnimation(Self, 'IsVisible');
    end;
  end;
end;

procedure TvgVisualObject.SetRotateAngle(const Value: single);
begin
  if FRotateAngle <> Value then
  begin
    FRotateAngle := vgNormalizeAngle(Value);
    MatrixChanged(Self);
  end;
end;

procedure TvgVisualObject.SetBounds(X, Y, AWidth, AHeight: single);
var
  SizeChanged: boolean;
begin
  Repaint;
  SizeChanged := false;
  if (FHeight <> AHeight) then
  begin
    FHeight := AHeight;
    if (FHeight < 0) and (((FScene <> nil) and (FScene.GetDesignTime))) then
    begin
      FHeight := Abs(FHeight);
      FScale.Y := -FScale.Y;
    end;
    SizeChanged := true;
  end;
  if (FWidth <> AWidth) then
  begin
    FWidth := AWidth;
    if (FWidth < 0) and (((FScene <> nil) and (FScene.GetDesignTime))) then
    begin
      FWidth := Abs(FWidth);
      FScale.X := -FScale.X;
    end;
    SizeChanged := true;
  end;

  if (X <> FPosition.X) or (Y <> FPosition.Y) then
  begin
    FPosition.FX := X;
    FPosition.FY := Y;
    MatrixChanged(Self);
  end;

  if not (csLoading in ComponentState) and (SizeChanged) then
  begin
    if (Parent <> nil) and (Parent.IsVisual) and (Align <> vaNone) then
    begin
      TvgVisualObject(Parent).Realign;
    end;
    if (FChildren <> nil) then
      Realign;
  end;
end;

procedure TvgVisualObject.SetSizeWithoutChange(AWidth, AHeight: single);
begin
  FWidth := AWidth;
  FLastWidth := AWidth;
  FHeight := AHeight;
  FLastHeight := AHeight;
end;

procedure TvgVisualObject.SetHeight(const Value: single);
begin
  if FHeight <> Value then
  begin
    Repaint;
    FHeight := Value;
    if (FHeight < 0) and (((FScene <> nil) and (FScene.GetDesignTime))) then
    begin
      FHeight := Abs(FHeight);
      FScale.Y := -FScale.Y;
    end;
    if not (csLoading in ComponentState) and (FScene <> nil) then
    begin
      RecalcUpdateRect;
      if (Parent <> nil) and (Parent.IsVisual) and (Align <> vaNone) then
      begin
        TvgVisualObject(Parent).Realign;
      end;
      if (FChildren <> nil) then
        Realign;
    end;
  end;
end;

procedure TvgVisualObject.SetWidth(const Value: single);
begin
  if FWidth <> Value then
  begin
    Repaint;
    FWidth := Value;
    if (FWidth < 0) and (((FScene <> nil) and (FScene.GetDesignTime))) then
    begin
      FWidth := Abs(FWidth);
      FScale.X := -FScale.X;
    end;
    if not (csLoading in ComponentState) and (FScene <> nil) then
    begin
      RecalcUpdateRect;
      if (Parent <> nil) and (Parent.IsVisual) and (Align <> vaNone) then
      begin
        TvgVisualObject(Parent).Realign;
      end;
      if (FChildren <> nil) then
        Realign;
    end;
  end;
end;

function TvgVisualObject.isOpacityStored: Boolean;
begin
  Result := FOpacity <> 1;
end;

procedure TvgVisualObject.SetOpacity(const Value: single);
begin
  if FOpacity <> Value then
  begin
    FOpacity := Value;
    if FOpacity < 0 then FOpacity := 0;
    if FOpacity > 1 then FOpacity := 1;
    RecalcOpacity;
    Repaint;
  end;
end;

procedure TvgVisualObject.UpdateDesignHide(const Value: boolean);
var
  i: integer;
begin
  FDesignHide := Value;
  for i := 0 to ChildrenCount - 1 do
  begin
    if Children[i].IsVisual then
      Children[i].Visual.UpdateDesignHide(Value);
  end;
end;

procedure TvgVisualObject.SetDesignHide(const Value: boolean);
begin
  if FDesignHide <> Value then
  begin
    FDesignHide := Value;
    if (((FScene <> nil) and FScene.GetDesignTime)) then
      Repaint;
  end;
end;

procedure TvgVisualObject.SetCursor(const Value: TCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
  end;
end;

{ TvgBrushObject ============================================================}

constructor TvgBrushObject.Create(AOwner: TComponent);
begin
  inherited;
  FBrush := TvgBrush.Create(vgBrushSolid, $FFFFFFFF);
end;

destructor TvgBrushObject.Destroy;
begin
  FreeAndNil(FBrush);
  inherited;
end;

procedure TvgBrushObject.SetName(const NewName: TComponentName);
begin
  inherited;
  if FResourceName = '' then
    FResourceName := Name;
end;

{ TvgControl ===================================================================}

constructor TvgControl.Create(AOwner: TComponent);
begin
  inherited;
  FNeedResource := true;
end;

destructor TvgControl.Destroy;
begin
  inherited;
end;

procedure TvgControl.ApplyResource;
var
  Obj: TvgObject;
  ResourceObject: TvgVisualObject;
  S: TStream;
  StyleName: string;
  SaveUpdate: boolean;
begin
  inherited ;
  if FNeedResource then
  begin
    FNeedResource := false;
    ResourceObject := nil;
    try
      if (FResource <> '') then
      begin
        { style }
        Obj := nil;
        if Assigned(FScene) and (FScene.GetStyle <> nil) and (FScene.GetStyle.FRoot <> nil) then
          Obj := TvgVisualObject(FScene.GetStyle.FRoot.FindResource(FResource));
        if Obj = nil then
          if DefaultStyles <> nil then
            Obj := TvgVisualObject(DefaultStyles.FindResource(FResource));
        if Obj = nil then
          Obj := vg_scene.FindResource(FResource);
        if (Obj <> nil) and (Obj.isVisual) then
        begin
          ResourceObject := TvgVisualObject(Obj.Clone(Self));
          ResourceObject.ResourceName := '';
        end;
      end;

      if ResourceObject = nil then
      begin
        { default }
        if DefaultStyles = nil then
        begin
          { load default styles }
          {$IFDEF FPC}
          if LazarusResources.Find('default', 'VGSTYLE') <> nil then
          begin
            S := TLazarusResourceStream.Create('default', PChar('VGSTYLE'));
            DefaultStyles := CreateObjectFromStream(nil, S);
            S.Free;
          end;
          {$ELSE}
          if Windows.FindResource(HInstance, PChar('defaultvgstyle'), RT_RCDATA) <> 0 then
          begin
            S := TResourceStream.Create(HInstance, 'defaultvgstyle', RT_RCDATA);
            DefaultStyles := CreateObjectFromStream(nil, S);
            S.Free;
          end;
          {$ENDIF}
        end;
        if Assigned(FScene) and (FScene.GetStyle <> nil) and (FScene.GetStyle.FRoot <> nil) then
        begin
          if FResource <> '' then
          begin
            StyleName := FResource;
            ResourceObject := TvgVisualObject(FScene.GetStyle.FRoot.FindResource(StyleName));
            if ResourceObject <> nil then
              ResourceObject := TvgVisualObject(ResourceObject.Clone(Self));
          end;
          if ResourceObject = nil then
          begin
            StyleName := ClassName + 'style';
            Delete(StyleName, 1, 3);
            ResourceObject := TvgVisualObject(FScene.GetStyle.FRoot.FindResource(StyleName));
            if ResourceObject <> nil then
            begin
              ResourceObject := TvgVisualObject(ResourceObject.Clone(Self));
            end;
          end;
        end;
        if (ResourceObject = nil) and (DefaultStyles <> nil) then
        begin
          if FResource <> '' then
          begin
            StyleName := FResource;
            ResourceObject := TvgVisualObject(DefaultStyles.FindResource(StyleName));
            if ResourceObject <> nil then
              ResourceObject := TvgVisualObject(ResourceObject.Clone(Self));
          end;
          if ResourceObject = nil then
          begin
            StyleName := ClassName + 'style';
            Delete(StyleName, 1, 3);
            ResourceObject := TvgVisualObject(DefaultStyles.FindResource(StyleName));
            if ResourceObject <> nil then
              ResourceObject := TvgVisualObject(ResourceObject.Clone(Self));
          end;
        end;
      end;
      if ResourceObject <> nil then
      begin
        if (FScene <> nil) and (csLoading in ComponentState) then
        begin
          SaveUpdate := FScene.GetDisableUpdate;
          FScene.SetDisableUpdate(true);
        end;
        if FResourceLink <> nil then
        begin
          FResourceLink.Free;
          FResourceLink := nil;
        end;
        ResourceObject.FAlign := vaContents;
        ResourceObject.DesignHide := false;
        AddObject(ResourceObject);
        { bring to front }
        FChildren.Remove(ResourceObject);
        FChildren.Insert(0, ResourceObject);
        { }
        ResourceObject.Stored := false;
        ResourceObject.Lock;
        FResourceLink := ResourceObject;
        ApplyStyle;
        if (FScene <> nil) and (csLoading in ComponentState) then
        begin
          FScene.SetDisableUpdate(SaveUpdate);
        end;
        FUpdateEffects := true;
      end;
    except
    end;
  end;
end;

procedure TvgControl.ApplyStyle;
begin
end;

procedure TvgControl.BeforePaint;
begin
  inherited;
  ApplyResource;
end;

procedure TvgControl.Paint;
begin
  inherited ;
end;

procedure TvgControl.SetResource(const Value: string);
begin
  FResource := Value;
  FNeedResource := true;
  if not (csLoading in ComponentState) then
  begin
    ApplyResource;
  end;
end;

{ TvgBackground }

constructor TvgBackground.Create(AOwner: TComponent);
begin
  inherited;
  FFill := TvgBrush.Create(vgBrushNone, $FFE0E0E0);
  FFill.OnChanged := FillChanged;
end;

destructor TvgBackground.Destroy;
begin
  FFill.Free;
  inherited;
end;

procedure TvgBackground.FillChanged(Sender: TObject);
begin
  Repaint;
end;

procedure TvgBackground.Paint;
begin
  if FFill.Style <> vgBrushNone then
  begin
    Canvas.Fill.Assign(FFill);
    Canvas.FillRect(LocalRect, 0, 0, AllCorners, AbsoluteOpacity);
  end
end;

procedure TvgBackground.PaintChildren;
begin
  if FFill.Style <> vgBrushNone then
  begin
    if FResourceLink <> nil then
      FResourceLink.Visual.FVisible := false;
    inherited;
    if FResourceLink <> nil then
      FResourceLink.Visual.FVisible := true;
  end
  else
  if (Parent = nil) and (FScene <> nil) and (FScene.GetRoot = Self) and (FScene.GetTransparency) then
  begin
    if FResourceLink <> nil then
      FResourceLink.Visual.FVisible := false;
    inherited;
    if FResourceLink <> nil then
      FResourceLink.Visual.FVisible := true;
  end
  else
    inherited;
end;

procedure TvgBackground.SetFill(const Value: TvgBrush);
begin
  FFill.Assign(Value);
end;

{ TvgContent ==================================================================}

constructor TvgContent.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TvgContent.Destroy;
begin
  inherited;
end;

function TvgContent.GetParentComponent: TComponent;
begin
  Result := inherited GetParentComponent;
end;

procedure TvgContent.Realign;
begin
  if not FDisableAlign then
  begin
    FDisableAlign := true;
    if (Parent <> nil) and (Parent.IsVisual) then
      TvgVisualObject(Parent).Realign;
    FDisableAlign := false;
  end;
  inherited;
end;

procedure TvgContent.Paint;
begin
  inherited;
end;

{ TvgFrame ====================================================================}

constructor TvgFrame.Create(AOwner: TComponent);
begin
  inherited;
  if Assigned(FScene) and (FScene.GetDesignTime) then
    FBuffer := TvgBitmap.Create(Round(Width), Round(Height));
end;

destructor TvgFrame.Destroy;
begin
  if not (Assigned(FScene) and (FScene.GetDesignTime)) then
    if (FSceneObject <> nil) and (FChildren <> nil) and (FChildren.Count > 0) then
    begin
      FSceneObject.AddObject(TvgObject(FChildren[0]));
      FSceneObject.FOpenInFrame := nil;
      if FSceneObject.FCloneFrame <> nil then
        FreeAndNil(FSceneObject.FCloneFrame);
    end;
  if FBuffer <> nil then
    FBuffer.Free;
  inherited;
end;

procedure TvgFrame.Loaded;
begin
  inherited ;
end;

procedure TvgFrame.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TvgFrame.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FSceneObject) then
    SceneObject := nil;
end;

procedure TvgFrame.Paint;
var
  Obj: TvgObject;
  CloneFrame: TForm;
begin
  if FNeedClone then
  begin
    FNeedClone := false;
    if not (Assigned(FScene) and (FScene.GetDesignTime)) and (FSceneObject <> nil) then
    begin
      { clone Owner first - if already opened }
      if (FSceneObject <> nil) and (FSceneObject.FOpenInFrame <> nil) then
      begin
        Application.CreateForm(TComponentClass(FSceneObject.Owner.ClassType), CloneFrame);
        if CloneFrame <> nil then
        begin
          FSceneObject := TvgCustomScene(CloneFrame.FindComponent(FSceneObject.Name));
          if FSceneObject <> nil then
          begin
            FSceneObject.FCloneFrame := CloneFrame;
          end;
        end
        else
          FSceneObject := nil;
      end;
      { open }
      if (FSceneObject <> nil) and (FSceneObject.FOpenInFrame = nil) and (FSceneObject.Root <> nil) then
      begin
        FSceneObject.FOpenInFrame := Self;
        Obj := FSceneObject.Root;
        Obj.Parent := Self;
        if Obj.isVisual then
        begin
          TvgVisualObject(Obj).Align := vaClient;
          Realign;
        end;
      end;
    end;
  end;
  if (FScene <> nil) and (Assigned(FScene) and (FScene.GetDesignTime)) and (SceneObject <> nil) and (SceneObject.Root <> nil) then
  begin
{    Obj := SceneObject.Root;
    if Obj.isVisual then
    begin
      if (FBuffer.Width <> Trunc(Width)) or (FBuffer.Height <> Trunc(Height)) then
        FBuffer.SetSize(Trunc(Width), Trunc(Height));
      SaveScene := TvgVisualObject(Obj).FScene;
      TvgVisualObject(Obj).SetNewScene(FScene);
      TvgVisualObject(Obj).Width := Width;
      TvgVisualObject(Obj).Height := Height;
      TvgVisualObject(Obj).UpdateChildScene;
      TvgVisualObject(Obj).RecalcAbsolute;
      TvgVisualObject(Obj).PaintTo(FBuffer.Canvas, LocalRect);
      Canvas.SetMatrix(AbsoluteMatrix);
      Canvas.DrawBitmap(FBuffer, vgRect(0, 0, FBuffer.Width, FBuffer.Height),
        vgRect(0, 0, FBuffer.Width - 1, FBuffer.Height - 1), 1);
      TvgVisualObject(Obj).SetNewScene(SaveScene);
    end;}
  end;
end;

procedure TvgFrame.SetSceneObject(const Value: TvgCustomScene);
var
  Obj: TvgObject;
begin
  if FSceneObject <> Value then
  begin
    if Value = FScene.GetComponent then Exit;

    if not (Assigned(FScene) and (FScene.GetDesignTime)) and (FSceneObject <> nil) then
    begin
      if (FSceneObject <> nil) and (FChildren <> nil) and (FChildren.Count > 0) then
      begin
        FSceneObject.AddObject(TvgObject(FChildren[0]));
        FSceneObject.FOpenInFrame := nil;
        if FSceneObject.FCloneFrame <> nil then
          FreeAndNil(FSceneObject.FCloneFrame);
      end;
    end;

    FSceneObject := Value;

    if not (Assigned(FScene) and (FScene.GetDesignTime)) and (FSceneObject <> nil) then
    begin
      { clone Owner first - if already opened }
      if (FSceneObject <> nil) and (FSceneObject.FOpenInFrame <> nil) then
      begin
        FNeedClone := true;
        Exit;
      end;
      { open }
      if (FSceneObject <> nil) and (FSceneObject.FOpenInFrame = nil) and (FSceneObject.Root <> nil) then
      begin
        FSceneObject.FOpenInFrame := Self;
        Obj := FSceneObject.Root;
        Obj.Parent := Self;
        if Obj.isVisual then
        begin
          TvgVisualObject(Obj).Align := vaClient;
          Realign;
        end;
      end;
    end;
    if Assigned(FScene) and (FScene.GetDesignTime) then
      Repaint;
  end;
end;

{ TvgDesigner =================================================================}

procedure TvgDesigner.CallDesignSelect(AObject: TObject);
var
  i: integer;
begin
  if FScenes <> nil then
    for i := 0 to FScenes.Count - 1 do
    begin
      IvgScene(FScenes[i]).DoDesignSelect(AObject);
      IvgScene(FScenes[i]).AddUpdateRect(vgRect(0, 0, 1000, 1000));
    end;
end;

procedure TvgDesigner.AddScene(const Scene: IvgScene);
begin
  if FScenes = nil then
    FScenes := TList.Create;
  if FScenes.IndexOf(Pointer(Scene)) < 0 then
    FScenes.Add(Pointer(Scene));
end;

procedure TvgDesigner.RemoveScene(const Scene: IvgScene);
begin
  FScenes.Remove(Pointer(Scene));
  if FScenes.Count = 0 then
    FreeAndNil(FScenes);
end;

{ TvgCustomScene ==============================================================}

constructor TvgCustomScene.Create(AOwner: TComponent);
begin
  inherited;
  ShowHint := true;
  FDesignTime := csDesigning in ComponentState;
  ControlStyle := ControlStyle + [csAcceptsControls];
  OnDragOver := DoDragOver;
  OnDragDrop := DoDragDrop;
  ControlStyle := ControlStyle + [csCaptureMouse, csOpaque, csDoubleClicks];
  FSnapToLines := true;
  FAlignRoot := true;
  FCanvas := DefaultCanvasClass.Create(Width, Height);
  {$IFDEF DARWINBUFFER}
  FCanvas.FBuffered := true;
  {$ENDIF}
  FFill := TvgBrush.Create(vgBrushNone, $FF000000);
  FFill.OnChanged := FillChanged;
  DesignPopupEnabled := true;
  FSnapGridSize := 1;
  Width := 100;
  Height := 100;
  if vgDesigner <> nil then
    vgDesigner.AddScene(Self);
  vgSceneCount := vgSceneCount + 1;
end;

destructor TvgCustomScene.Destroy;
begin
  if vgDesigner <> nil then
    vgDesigner.RemoveScene(Self);
  vgSceneCount := vgSceneCount - 1;
  if vgSceneCount = 0 then
  begin
    if aniThread <> nil then
    begin
      aniThread.Free;
    end;
    aniThread := nil;
  end;
  if FOpenInFrame <> nil then
    FOpenInFrame.SceneObject := nil;
  DeleteChildren;
  if FChildren <> nil then
    FreeAndNil(FChildren);
  FreeAndNil(FFill);
  FreeAndNil(FCanvas);
  inherited;
end;

procedure TvgCustomScene.CreateHandle;
{$IFDEF LINUX}
var
  screen: PGdkScreen;
  colormap: PGdkColormap;
{$ENDIF}
begin
  inherited;
  {$IFDEF WIN32}
  if IsWin2kUp then
    SetWindowLongW(Handle, GWL_WNDPROC, GetWindowLong(Handle, GWL_WNDPROC));
  Canvas.SceneWnd := Handle;
  {$ENDIF}
  {$IFDEF LINUX}
  Canvas.Widget := GetFixedWidget(pgtkwidget(Handle));
  {$ENDIF}
  Canvas.ResizeBuffer(Width, Height);
  Canvas.SetTextRendering(FTextRendering);
  RealignRoot;
  AddUpdateRect(vgRect(0, 0, Width, Height));
  { Add Hook }
  if FTransparency and not (GetDesignTime) then
  begin
    if Parent is TCustomForm then
    begin
      {$IFDEF LINUX}
      screen := gtk_widget_get_screen(pgtkwidget(Parent.Handle));
      colormap := gdk_screen_get_rgba_colormap(screen);
      if colormap <> nil then
        gtk_widget_set_colormap(pgtkwidget(Parent.Handle), colormap);
      { Set Paint by App }
      gtk_widget_set_app_paintable(pgtkwidget(Parent.Handle), true);
      {$ENDIF}
      {$IFDEF KS_WIN}
      SetWindowLong(Parent.Handle, GWL_EXSTYLE, GetWindowLong(Parent.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
      UpdateLayer;
      AddUpdateRect(vgRect(0, 0, $FFFF, $FFFF));
      {$ENDIF}
      {$IFDEF DARWIN}
      WndEventHandlerUPP := NewEventHandlerUPP(EventHandlerProcPtr(Pointer(@WndEventHandler)));
      EventKinds[0].eventClass := kEventClassControl;
      EventKinds[0].eventKind := kEventControlDraw;
      InstallEventHandler(GetControlEventTarget(TCarbonWindow(TCustomForm(Parent).Handle).Widget), WndEventHandlerUPP, 1, @EventKinds[0], Self, nil);
      EventKinds[0].eventClass := kEventClassWindow;
      EventKinds[0].eventKind := kEventWindowGetRegion;
      InstallEventHandler(GetWindowEventTarget(TCarbonWindow(TCustomForm(Parent).Handle).Window), WndEventHandlerUPP, 1, @EventKinds[0], Self, nil);
      SetWindowAlpha(TCarbonWindow(TCustomForm(Parent).Handle).Window, 0.999);
      ReshapeCustomWindow(TCarbonWindow(TCustomForm(Parent).Handle).Window);
      {$ENDIF}
    end
  end;
  {$IFDEF DARWIN}
  SetControlDragTrackingEnabled(TCarbonWidget(Handle).Widget, true);
  if Parent is TCustomForm then
    SetAutomaticControlDragTrackingEnabledForWindow(TCarbonWindow(TCustomForm(Parent).Handle).Window, true);
  WndEventHandlerUPP := NewEventHandlerUPP(EventHandlerProcPtr(Pointer(@CtrlEventHandler)));
  EventKinds[0].eventClass := kEventClassControl;
  EventKinds[0].eventKind := kEventControlDraw;
  EventKinds[1].eventClass := kEventClassControl;
  EventKinds[1].eventKind := kEventControlDragEnter;
  EventKinds[2].eventClass := kEventClassControl;
  EventKinds[2].eventKind := kEventControlDragReceive;
  EventKinds[3].eventClass := kEventClassControl;
  EventKinds[3].eventKind := kEventControlDragWithin;
  EventKinds[4].eventClass := kEventClassControl;
  EventKinds[4].eventKind := kEventControlDragLeave;
  InstallEventHandler(GetControlEventTarget(TCarbonWidget(Handle).Widget), WndEventHandlerUPP, 5, @EventKinds[0], Self, nil);
  {$ELSE}
  {$ENDIF}
  {$IFDEF WINDOWS} // fpc lazarus
  PrevWndProc := Windows.WNDPROC(SetWindowLongW(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback)));
  {$ENDIF}
end;

{$IFDEF WIN32}

procedure TvgCustomScene.CreateWnd;
begin
  inherited ;
  {$IFNDEF FPC}
  RegisterDragDrop(Handle, Self);
  {$ENDIF}
end;

procedure TvgCustomScene.DestroyWnd;
begin
  {$ifdef win32}
  if Canvas <> nil then
    Canvas.SceneWnd := 0;
  {$endif}
  {$IFNDEF FPC}
  if HandleAllocated then
    RevokeDragDrop(Handle);
  {$ENDIF}
  {$IFDEF WINDOWS} // fpc lazarus
  if PtrInt(SetWindowLongW(Self.Handle, GWL_WNDPROC, PtrInt(@WndCallback))) = PtrInt(@WndCallback) then
    SetWindowLong(Self.Handle, GWL_WNDPROC, PtrInt(@PrevWndProc));
  {$ENDIF}
  inherited ;
end;

{$ENDIF}

procedure TvgCustomScene.Loaded;
begin
  inherited;
  FLoadCursor := Cursor;
  if FSnapToLines then
    FSnapToGrid := false;
  FShowTimer := TTimer.Create(Self);
  FShowTimer.Interval := 1;
  FShowTimer.OnTimer := DoShowTimer;
end;

procedure TvgCustomScene.CreateEmbedded(const AWidth, AHeight: integer; AOnFlush: TNotifyEvent);
begin
  FOnFlush := AOnFlush;
  Width := AWidth;
  Height := AHeight;
  Canvas.ResizeBuffer(AWidth, AHeight);
  Canvas.SetTextRendering(FTextRendering);
  RealignRoot;
  AddUpdateRect(vgRect(0, 0, AWidth, AHeight));
end;

procedure TvgCustomScene.EmbeddedMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseDown(Button, Shift, X, Y);
end;

procedure TvgCustomScene.EmbeddedMouseMove(Shift: TShiftState; X, Y: Integer);
begin
  MouseMove(Shift, X, Y);
end;

procedure TvgCustomScene.EmbeddedMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MouseUp(Button, Shift, X, Y);
end;

function TvgCustomScene.EmbeddedMouseWheel(Shift: TShiftState; WheelDelta: Integer): Boolean;
var
  MousePos: TPoint;
begin
  Result := DoMouseWheel(Shift, WheelDelta, MousePos);
end;

procedure TvgCustomScene.EmbeddedKeyUp(var Key: Word; var Char: System.WideChar; Shift: TShiftState);
begin
  UnicodeKeyUp(Key, Char, Shift);
end;

procedure TvgCustomScene.EmbeddedKeyDown(var Key: Word; var Char: System.WideChar; Shift: TShiftState);
begin
  UnicodeKeyDown(Key, Char, Shift);
end;

procedure TvgCustomScene.DoShowTimer(Sender: TObject);
begin
  FShowTimer.Enabled := false;
  {$IFDEF DARWIN}
  HIViewSetNeedsDisplay(HiViewRef(TCarbonWidget(Handle).Widget), true);
  {$ENDIF}
  {$IFDEF WIN32}
  AddUpdateRect(vgRect(0, 0, $FFFF, $FFFF));
  {$ENDIF}
  FShowTimer.Free;
end;

procedure TvgCustomScene.UpdateBuffer;
begin
  {$IFDEF DARWIN}
  HIViewSetNeedsDisplay(HiViewRef(TCarbonWidget(Handle).Widget), true);
  {$ENDIF}
  {$IFDEF WIN32}
  SetLength(FUpdateRects, 1);
  FUpdateRects[0] := vgRect(0, 0, Width, Height);
  Draw;
  {$ENDIF}
end;

procedure TvgCustomScene.UpdateLayer;
{$IFDEF KS_WIN}
var
  Blend: TBLENDFUNCTION;
  Origin, Size, BitmapOrigin: Windows.TPoint;
  i, j: integer;
  SaveBits: PvgColorRecArray;
{$ENDIF}
begin
{$IFDEF KS_WIN}
  if (GetDesignTime) then Exit;
  if not (Owner is TWinControl) then Exit;
  if Parent.Handle = 0 then Exit;

  Origin := Point(Parent.Left + Left, Parent.Top + Top);
  Size := Point(Width, Height);
  { Update }
  with Blend do
  begin
    BlendOp := AC_SRC_OVER;
    AlphaFormat := $01; //AC_SRC_ALPHA;
    BlendFlags := 0;
    SourceConstantAlpha := $FF;
  end;
  BitmapOrigin := Point(0, 0);

  { VCL }
  if ControlCount > 0 then
  begin
    // save alpha
    GetMem(SaveBits, Width * Height * 4);
    MoveLongword(Canvas.FBufferBits, SaveBits, Width * Height);
    // paint
    PaintControls(Canvas.FBufferDC, nil);
    // restore alpha
    for j := 0 to Height - 1 do
      for i := 0 to Width - 1 do
        PvgColorRecArray(Canvas.FBufferBits)[i + (j * Width)].A := SaveBits[i + (j * Width)].A;
    FreeMem(SaveBits, Width * Height * 4);
  end;

  UpdateLayeredWindow(Parent.Handle, 0, @Origin, @Size, Canvas.FBufferDC, @BitmapOrigin, $00000000, @Blend, ULW_ALPHA);
{$ENDIF}
end;

procedure TvgCustomScene.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  inherited;
  if FChildren <> nil then
    for i := 0 to FChildren.Count - 1 do
      if TvgObject(FChildren[i]).Stored then
        Proc(FChildren[i]);
end;

procedure TvgCustomScene.AddUpdateRect(R: TvgRect);
{$IFDEF WIN32}
var
  WR: TRect;
{$ENDIF}
{$IFDEF LINUX}
var
  GR: TGdkRectangle;
{$ENDIF}
begin
  if FDisableUpdate then Exit;
  if csLoading in ComponentState then Exit;
  if not Assigned(FOnFlush) and not HandleAllocated then Exit;
  if csDestroying in ComponentState then Exit;
  if (Canvas.FBuffered) and (Canvas.FBufferBits = nil) then Exit;

  R := vgRect(Trunc(R.Left), Trunc(R.Top), Trunc(R.Right) + 1, Trunc(R.Bottom) + 1);
  if not vgIntersectRect(R, vgRect(0, 0, Width, Height)) then Exit;

  if Assigned(FOnFlush) then
  begin
    { not use WM_PAINT in embedded mode }
    {$IFDEF WIN32}
    SetLength(FUpdateRects, Length(FUpdateRects) + 1);
    FUpdateRects[High(FUpdateRects)] := R;
    PostMessage(Handle, WM_ADDUPDATERECT, 0, 0); // only for layered
    {$ENDIF}
    Exit;
  end;
{$IFDEF LINUX}
   GR.x := Trunc(R.Left);
   GR.y := Trunc(R.Top);
   GR.width := Trunc(vgRectWidth(R));
   GR.height := Trunc(vgRectHeight(R));
   SetLength(FUpdateRects, Length(FUpdateRects) + 1);
   FUpdateRects[High(FUpdateRects)] := R;
   if HandleAllocated and GDK_IS_WINDOW(pgtkwidget(Handle)^.window) then
     gdk_window_invalidate_rect(pgtkwidget(Handle)^.window, @GR, true);
{$ENDIF}
{$IFDEF DARWIN}
   HIViewSetNeedsDisplayInRect(HiViewRef(TCarbonWidget(Handle).Widget), CGRectFromRect(R), true);
{$ENDIF}
{$IFDEF WIN32}
   if Transparency and (Parent is TCustomForm) and not FDEsigntime then
   begin
     SetLength(FUpdateRects, Length(FUpdateRects) + 1);
     FUpdateRects[High(FUpdateRects)] := R;
     PostMessage(Handle, WM_ADDUPDATERECT, 0, 0); // only for layered
   end
   else
   begin
     WR := Rect(Trunc(R.Left), Trunc(R.Top), Trunc(R.Right), Trunc(R.Bottom));
     Windows.InvalidateRect(Handle, @WR, false);
   end;
{$ENDIF}
end;

{$IFDEF WIN32}
procedure GetControls(X, Y, W, H: Integer;
                      Control: TCustomControl; Dest: TCanvas);
var
  I, Count, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
  Ctrl: TControl;
begin
  Count := Control.ControlCount;
  DC := Dest.Handle;
  SelfR := Bounds(0, 0, W, H);
  // Copy images of controls
  for I := 0 to Count - 1 do
  begin
    Ctrl := Control.Controls[I];
    if (Ctrl <> nil) and (Ctrl is TCustomControl)
    then
      begin
        with Ctrl do
        begin
          CtlR := Bounds(X + Left, Y + Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
          begin
            SaveIndex := SaveDC(DC);
            SetViewportOrgEx(DC, Left + X, Top + Y, nil);
            IntersectClipRect(DC, 0, 0, Width, Height);
            Perform(WM_PAINT, DC, 0);
            RestoreDC(DC, SaveIndex);
            if TCustomControl(Ctrl).ControlCount <> 0
            then
              GetControls(Left + X, Top + Y, W, H,
              TCustomControl(Ctrl), Dest);
          end;
       end;
    end;
  end;
end;

procedure DrawParentImage(Control: TControl; DestDC: HDC);
var
  I, Count, X, Y, SaveIndex: Integer;
  Dest: TCanvas;
  R, SelfR, CtlR: TRect;
  Ctrl: TControl;
begin
  if Control.Parent = nil then Exit;
  Count := Control.Parent.ControlCount;
  Dest := TCanvas.Create;
  try
    Dest.Handle := DestDC;
    SelfR := Bounds(Control.Left, Control.Top, Control.Width, Control.Height);
    X := -Control.Left; Y := -Control.Top;
    // Copy parent control image
    if Control.Parent is TForm then
    begin
        SaveIndex := SaveDC(DestDC);
        SetViewportOrgEx(DestDC, X, Y, nil);
        IntersectClipRect(DestDC, 0, 0, Control.Parent.ClientWidth,
           Control.Parent.ClientHeight);
        if (Control.Parent is TForm) and
            (TForm(Control.Parent).FormStyle = fsMDIForm)
         then
           begin
             SendMessage(TForm(Control.Parent).ClientHandle, WM_ERASEBKGND, DestDC, 0);
           end
        else
          SendMessage(Control.Parent.Handle, WM_ERASEBKGND, DestDC, 0);
        RestoreDC(DestDC, SaveIndex);
      end
    else
      begin
        SaveIndex := SaveDC(DestDC);
        SetViewportOrgEx(DestDC, X, Y, nil);
        IntersectClipRect(DestDC, 0, 0, Control.Parent.ClientWidth,
           Control.Parent.ClientHeight);
        TParentControl(Control.Parent).Perform(WM_ERASEBKGND, DestDC, 0);
        TParentControl(Control.Parent).Perform(WM_PAINT, DestDC, 0);
        RestoreDC(DestDC, SaveIndex);
      end;

    // Copy images of controls
    for I := 0 to Count - 1 do
    begin
      Ctrl := Control.Parent.Controls[I];
      if Ctrl = Control then Break;
      if (Ctrl <> nil) and
         ((Ctrl is TGraphicControl) or (Ctrl is TCustomControl))
      then
        with Ctrl do
        begin
          CtlR := Bounds(Left, Top, Width, Height);
          if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
          begin
            SaveIndex := SaveDC(DestDC);
            SetViewportOrgEx(DestDC, Left + X, Top + Y, nil);
            IntersectClipRect(DestDC, 0, 0, Width, Height);
            Perform(WM_PAINT, DestDC, 0);
            RestoreDC(DestDC, SaveIndex);
            if Ctrl is TCustomControl then
              GetControls(Left + X, Top + Y,
                Control.Width, Control.Height,
                TCustomControl(Ctrl), Dest);
          end;
       end;
    end;
  finally
    Dest.Handle := 0;
    Dest.Free;
  end;
end;
{$ENDIF}

procedure TvgCustomScene.Draw;
var
  i, j: integer;
  R: TvgRect;
  Rgn, NRgn: Cardinal;
  ScaleMatrix: TvgMatrix;
begin
  if not (FDesignTime) and (FOpenInFrame <> nil) then Exit;
  if FDrawing then Exit;
  if Length(FUpdateRects) > 0 then
  begin
    FDrawing := true;
    try
      { Split rects if rects too more }
      if (Length(FUpdateRects) > 20) then
      begin
        for i := 1 to High(FUpdateRects) do
          FUpdateRects[0] := vgUnionRect(FUpdateRects[0], FUpdateRects[i]);
        SetLength(FUpdateRects, 1);
      end;
      { draw back }
      if Canvas.BeginScene then
      begin
        Canvas.ResetClipRect;
        ScaleMatrix := IdentityMatrix;
        Canvas.SetMatrix(ScaleMatrix);
        Canvas.SetClipRects(FUpdateRects);

        if FTransparency and not (Parent is TCustomForm) then
        begin
          if FFill.Style = vgBrushNone then
          begin
            { apply cliprgn }
            {$IFDEF WIN32}
            j := SaveDC(Canvas.FBufferDC);
            with FUpdateRects[0] do
              Rgn := CreateRectRgn(trunc(left), trunc(top), trunc(right), trunc(bottom));
            for i := 1 to High(FUpdateRects) do
            begin
              with FUpdateRects[i] do
                NRgn := CreateRectRgn(trunc(left), trunc(top), trunc(right), trunc(bottom));
              CombineRgn(Rgn, Rgn, NRgn, RGN_OR);
              DeleteObject(NRgn);
            end;
            SelectClipRgn(Canvas.FBufferDC, Rgn);
            DrawParentImage(Self, Canvas.FBufferDC);
            RestoreDC(Canvas.FBufferDC, j);
            {$ENDIF}
          end
          else
          begin
            Canvas.Fill.Assign(FFill);
            Canvas.FillRect(vgRect(-1, -1, Width + 1, Height + 1), 0, 0, AllCorners, 1);
          end;
        end
        else
        begin
          if (FFill.Style = vgBrushNone) or ((FFill.SolidColor and $FF000000 = 0) and (FFill.Style = vgBrushSolid)) then
          begin
            for i := 0 to High(FUpdateRects) do
            begin
              if FTransparency then
                Canvas.ClearRect(FUpdateRects[i], 0)
              else
                Canvas.ClearRect(FUpdateRects[i], FFill.SolidColor and $FFFFFF);
            end;
          end
          else
          begin
            Canvas.Fill.Assign(FFill);
            Canvas.FillRect(vgRect(-1, -1, Width + 1, Height + 1), 0, 0, AllCorners, 1);
          end;
        end;
        { reset }
        Canvas.StrokeThickness := 1;
        Canvas.StrokeCap := vgCapFlat;
        Canvas.StrokeJoin := vgJoinMiter;
        Canvas.StrokeDash := vgDashSolid;
        Canvas.Stroke.Style := vgBrushSolid;
        Canvas.Fill.Style := vgBrushSolid;
        if FChildren <> nil then
          for i := 0 to FChildren.Count - 1 do
          begin
            if not (TObject(FChildren[i]) is TvgVisualObject) then Continue;
            if not TvgVisualObject(FChildren[i]).Visible then Continue;

            ScaleMatrix := IdentityMatrix;
            for j := 0 to High(FUpdateRects) do
              if vgIntersectRect(FUpdateRects[j], TvgVisualObject(FChildren[i]).UpdateRect) then
              begin
                Canvas.SetMatrix(vgMatrixMultiply(ScaleMatrix, TvgVisualObject(FChildren[i]).AbsoluteMatrix));
                TvgVisualObject(FChildren[i]).BeforePaint;
                TvgVisualObject(FChildren[i]).Paint;
                TvgVisualObject(FChildren[i]).AfterPaint;
                TvgVisualObject(FChildren[i]).PaintChildren;
                if Assigned(TvgVisualObject(FChildren[i]).FOnPaint) then
                begin
                  Canvas.SetMatrix(vgMatrixMultiply(ScaleMatrix, TvgVisualObject(FChildren[i]).AbsoluteMatrix));
                  TvgVisualObject(FChildren[i]).FOnPaint(TvgVisualObject(FChildren[i]), Canvas, TvgVisualObject(FChildren[i]).LocalRect);
                end;
                Break;
              end;
          end;

        { grid }
        if FSnapGridShow and (FSnapGridSize <> 0) then
        begin
          ScaleMatrix := IdentityMatrix;
          Canvas.SetMatrix(ScaleMatrix);
          Canvas.Stroke.Style := vgBrushSolid;
          Canvas.StrokeThickness := 1;
  {        for i := Trunc((-FDesignScroll.X) / (FSnapGridSize)) - 1 to Trunc((-FDesignScroll.X + Width) / (FSnapGridSize)) + 1 do
          begin
            if i mod 5 = 0 then
              Canvas.Stroke.SolidColor := $50505050
            else
              Canvas.Stroke.SolidColor := $50303030;
            Canvas.DrawLine(vgPoint(i * FSnapGridSize + 0.5, -FDesignScroll.Y + 0.5), vgPoint(i * FSnapGridSize + 0.5, -FDesignScroll.Y + Height + 0.5), 1);
          end;
          for j := Trunc((-FDesignScroll.Y) / (FSnapGridSize)) - 1 to Trunc((-FDesignScroll.Y + Height) / (FSnapGridSize)) + 1 do
          begin
            if j mod 5 = 0 then
              Canvas.Stroke.SolidColor := $50505050
            else
              Canvas.Stroke.SolidColor := $50303030;
            Canvas.DrawLine(vgPoint(-FDesignScroll.X + 0.5, j * FSnapGridSize + 0.5), vgPoint(-FDesignScroll.X + Width + 0.5, j * FSnapGridSize + 0.5), 1)
          end;}
        end;
        { design }
        if (FSelected <> nil) and not FSelected.DisableDesignResize then
        begin
          Canvas.Fill.Style := vgBrushSolid;
          Canvas.Fill.SolidColor := $FFFFFFFF;
          Canvas.StrokeThickness := 1;
          Canvas.Stroke.Style := vgBrushSolid;
          Canvas.Stroke.SolidColor := $FF1072C5;
          ScaleMatrix := vg_scene.IdentityMatrix;
          Canvas.SetMatrix(vgMatrixMultiply(ScaleMatrix, FSelected.AbsoluteMatrix));
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.StrokeDash := vgDashDash;
          Canvas.DrawRect(R, 0, 0, AllCorners, 1);
          Canvas.StrokeDash := vgDashSolid;
          begin
          { rotate }
          if FRotateHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.DrawLine(vgPoint((R.Left + R.Right) / 2, R.Top), vgPoint((R.Left + R.Right) / 2, R.Top - RotSize), 1);
          Canvas.Fillellipse(vgRect((R.Left + R.Right) / 2 - (GripSize), R.Top - RotSize - (GripSize),
            (R.Left + R.Right) / 2 +(GripSize), R.Top - RotSize + (GripSize)), Opaque);
          Canvas.DrawEllipse(vgRect((R.Left + R.Right) / 2 - (GripSize), R.Top - RotSize - (GripSize),
            (R.Left + R.Right) / 2 +(GripSize), R.Top - RotSize + (GripSize)), Opaque);
          { angles }
          if FLeftTopHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.Fillellipse(vgRect(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize)), Opaque);
          Canvas.DrawEllipse(vgRect(R.Left - (GripSize), R.Top - (GripSize), R.Left + (GripSize), R.Top + (GripSize)), Opaque);

          if FRightTopHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.Fillellipse(vgRect(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize)), Opaque);
          Canvas.DrawEllipse(vgRect(R.Right - (GripSize), R.Top - (GripSize), R.Right + (GripSize), R.Top + (GripSize)), Opaque);

          if FLeftBottomHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.Fillellipse(vgRect(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize)), Opaque);
          Canvas.DrawEllipse(vgRect(R.Left - (GripSize), R.Bottom - (GripSize), R.Left + (GripSize), R.Bottom + (GripSize)), Opaque);

          if FRightBottomHot then
            Canvas.Fill.SolidColor := $FFFF0000
          else
            Canvas.Fill.SolidColor := $FFFFFFFF;
          R := FSelected.BoundsRect;
          vgInflateRect(R, -0.5, -0.5);
          Canvas.FillEllipse(vgRect(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize)), Opaque);
          Canvas.DrawEllipse(vgRect(R.Right - (GripSize), R.Bottom - (GripSize), R.Right + (GripSize), R.Bottom + (GripSize)), Opaque);
          { lines }
          if FSelected.Width > GripSize * 4 then
          begin
            if FTopHot then
              Canvas.Fill.SolidColor := $FFFF0000
            else
              Canvas.Fill.SolidColor := $FFFFFFFF;
            R := FSelected.BoundsRect;
            vgInflateRect(R, -0.5, -0.5);
            Canvas.FillRect(vgRect(R.Left + vgRectWidth(R)/2 - (GripSize), R.Top - (GripSize), R.Left + vgRectWidth(R)/2 + (GripSize), R.Top + (GripSize)), 0, 0, [], Opaque);
            Canvas.DrawRect(vgRect(R.Left + vgRectWidth(R)/2 - (GripSize), R.Top - (GripSize), R.Left + vgRectWidth(R)/2 + (GripSize), R.Top + (GripSize)), 0, 0, [], Opaque);
            if FBottomHot then
              Canvas.Fill.SolidColor := $FFFF0000
            else
              Canvas.Fill.SolidColor := $FFFFFFFF;
            R := FSelected.BoundsRect;
            vgInflateRect(R, -0.5, -0.5);
            Canvas.FillRect(vgRect(R.Left + vgRectWidth(R)/2 - (GripSize), R.Bottom - (GripSize), R.Left + vgRectWidth(R)/2 + (GripSize), R.Bottom + (GripSize)), 0, 0, [], Opaque);
            Canvas.DrawRect(vgRect(R.Left + vgRectWidth(R)/2 - (GripSize), R.Bottom - (GripSize), R.Left + vgRectWidth(R)/2 + (GripSize), R.Bottom + (GripSize)), 0, 0, [], Opaque);
          end;
          if FSelected.Height > GripSize * 4 then
          begin
            if FLeftHot then
              Canvas.Fill.SolidColor := $FFFF0000
            else
              Canvas.Fill.SolidColor := $FFFFFFFF;
            R := FSelected.BoundsRect;
            vgInflateRect(R, -0.5, -0.5);
            Canvas.FillRect(vgRect(R.Left - (GripSize), R.Top + vgRectHeight(R)/2 - (GripSize), R.Left + (GripSize), R.Top + vgRectHeight(R)/2 + (GripSize)), 0, 0, [], Opaque);
            Canvas.DrawRect(vgRect(R.Left - (GripSize), R.Top + vgRectHeight(R)/2 - (GripSize), R.Left + (GripSize), R.Top + vgRectHeight(R)/2 + (GripSize)), 0, 0, [], Opaque);
            if FRightHot then
              Canvas.Fill.SolidColor := $FFFF0000
            else
              Canvas.Fill.SolidColor := $FFFFFFFF;
            R := FSelected.BoundsRect;
            vgInflateRect(R, -0.5, -0.5);
            Canvas.FillRect(vgRect(R.Right - (GripSize), R.Top + vgRectHeight(R)/2 - (GripSize), R.Right + (GripSize), R.Top + vgRectHeight(R)/2 + (GripSize)), 0, 0, [], Opaque);
            Canvas.DrawRect(vgRect(R.Right - (GripSize), R.Top + vgRectHeight(R)/2 - (GripSize), R.Right + (GripSize), R.Top + vgRectHeight(R)/2 + (GripSize)), 0, 0, [], Opaque);
          end;
          { grid lines }
          if (FMoving or FLeftTop or FRightTop or FLeftBottom or FRightBottom or FTop or FBottom or FLeft or FRight) and
             (Length(FDesignGridLines) > 0) and (FSelected.Parent <> nil) and (FSelected.Parent.IsVisual) then
          begin
            ScaleMatrix := vg_scene.IdentityMatrix;
            Canvas.SetMatrix(vgMatrixMultiply(ScaleMatrix, TvgVisualObject(FSelected.Parent).AbsoluteMatrix));
            Canvas.StrokeDash := vgDashDash;
            for i := 0 to High(FDesignGridLines) do
            begin
              if (FDesignGridLines[i].Position.Y + round(FDesignGridLines[i].Height / 2)) = (FSelected.Position.Y + round(FSelected.Height / 2)) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, Trunc(FSelected.Position.Y + (FSelected.Height / 2)) + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + 0.5, Trunc(FSelected.Position.Y + (FSelected.Height / 2)) + 0.5), 1);
              end;
              if (FDesignGridLines[i].Position.X + round(FDesignGridLines[i].Width / 2)) = (FSelected.Position.X + round(FSelected.Width / 2)) then
              begin
                Canvas.DrawLine(vgPoint(Trunc(FSelected.Position.X + (FSelected.Width / 2)) + 0.5, FSelected.Position.Y + 0.5),
                  vgPoint(Trunc(FDesignGridLines[i].Position.X + (FDesignGridLines[i].Width / 2)) + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.X = FDesignGridLines[i].Position.X) or (FSelected.Position.Y = FDesignGridLines[i].Position.Y) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, FSelected.Position.Y + 0.5), vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.X + FSelected.Width = FDesignGridLines[i].Position.X) then
              begin
                Canvas.DrawLine(vgPoint(FDesignGridLines[i].Position.X + 0.5, FSelected.Position.Y + 0.5), vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.Y + FSelected.Height = FDesignGridLines[i].Position.Y) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, FDesignGridLines[i].Position.Y + 0.5), vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.X = FDesignGridLines[i].Position.X + FDesignGridLines[i].Width) then
              begin
                Canvas.DrawLine(vgPoint(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 0.5, FSelected.Position.Y + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.Y = FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 0.5), 1);
              end;
              if (FSelected.Position.X + FSelected.Width = FDesignGridLines[i].Position.X + FDesignGridLines[i].Width) then
              begin
                Canvas.DrawLine(vgPoint(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 0.5, FSelected.Position.Y + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 0.5, FDesignGridLines[i].Position.Y + 0.5), 1);
              end;
              if (FSelected.Position.Y + FSelected.Height = FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height) then
              begin
                Canvas.DrawLine(vgPoint(FSelected.Position.X + 0.5, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 0.5),
                  vgPoint(FDesignGridLines[i].Position.X + 0.5, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 0.5), 1);
              end;
            end;
            Canvas.StrokeDash := vgDashSolid;
          end;
          end;
          { place message }
          if FDesignPlaceObject <> nil then
          begin
            ScaleMatrix := vg_scene.IdentityMatrix;
            Canvas.SetMatrix(ScaleMatrix);

            R := FDesignPlaceObject.AbsoluteRect;
            Canvas.Stroke.SolidColor := $FF5B91DE;
            Canvas.DrawRect(R, 0, 0, AllCorners, 1);
            Canvas.Font.Family := 'Tahoma';
            Canvas.Font.Style := vgFontRegular;
            Canvas.Font.Size := 9;
            R.Bottom := R.Top;
            R.Top := R.Bottom - 19;
            R.Right := R.Left + 160;
            Canvas.Fill.SolidColor := $FF5B91DE;
            Canvas.FillRect(R, 0, 0, AllCorners, 1);
            Canvas.Fill.SolidColor := $FFFFFFFF;
            vgInflateRect(R, -2, -2);
            if FDesignPlaceObject.Name <> '' then
              Canvas.FillText(R, R, 'ALT-drag to place into [' + FDesignPlaceObject.Name + ']', false, 1, vgTextAlignNear, vgTextAlignCenter)
            else
              Canvas.FillText(R, R, 'ALT-drag to place into [' + FDesignPlaceObject.ClassName + ']', false, 1, vgTextAlignNear, vgTextAlignCenter);
          end;
        end;
        { design modes }
        if FDesignTime then
        begin
          ScaleMatrix := IdentityMatrix;
          Canvas.SetMatrix(ScaleMatrix);

          Canvas.Stroke.SolidColor := $FF5B91DE;
          Canvas.Font.Family := 'Tahoma';
          Canvas.Font.Style := vgFontRegular;
          Canvas.Font.Size := 9;
          R := vgRect(0, 0, 200, 17);
        end;
        { debug }
        if ShowUpdateRects then
        with Canvas do
        begin
          ResetClipRect;
          ScaleMatrix := IdentityMatrix;
          Canvas.SetMatrix(ScaleMatrix);
          Stroke.Style := vgBrushSolid;
          Stroke.Color := '#9000FF00';
          StrokeThickness := 1;
          Fill.Style := vgBrushNone;
          for i := 0 to High(FUpdateRects) do
          begin
            R := FUpdateRects[i];
            DrawRect(FUpdateRects[i], 0, 0, AllCorners, 0.5);
          end;
        end;
        {$IFDEF UPDATERECT}
        if not ShowUpdateRects then
        with Canvas do
        begin
          ResetClipRect;
          ScaleMatrix := IdentityMatrix;
          Canvas.SetMatrix(ScaleMatrix);
          Stroke.Style := vgBrushSolid;
          Stroke.Color := '#90FF0000';
          StrokeThickness := 1;
          Fill.Style := vgBrushNone;
          for i := 0 to High(FUpdateRects) do
          begin
            R := FUpdateRects[i];
            DrawRect(FUpdateRects[i], 0, 0, AllCorners, 0.5);
          end;
        end;
        {$ENDIF}
        Canvas.EndScene;
      end;

      if Assigned(OnFlush) then
        OnFlush(Self)
      else
      begin
        { buffer }
        {$IFDEF WIN32}
        if (not FTransparency) or (FTransparency and not (Parent is TCustomForm)) or (FDesignTime) then
        {$ENDIF}
        begin
          for i := 0 to High(FUpdateRects) do
          begin
            R := FUpdateRects[i];
            {$IFDEF WIN32}
            Canvas.FlushBufferRect(0, 0, Canvas.SceneDC, FUpdateRects[i]);
            {$ELSE}
            Canvas.FlushBufferRect(0, 0, 0, R);
            {$ENDIF}
          end;
        end;
        { Transparancy }
        {$IFDEF KS_WIN}
        if FTransparency and (Parent is TCustomForm) then
          UpdateLayer;
        {$ENDIF}
      end;
    finally
      setLength(FUpdateRects, 0);
      FDrawing := false;
    end;
  end;
end;

{ Drag and Drop }

type
  THackControl = class(TCustomControl);

procedure TvgCustomScene.BeginVCLDrag(Source: TObject);
begin
  VCLDragSource := TCustomControl.Create(Self);
  VCLDragSource.Parent := Self;
  VCLDragSource.Tag := Integer(Source);
  VCLDragSource.BeginDrag(true, -1);
  THackControl(VCLDragSource).OnEndDrag := EndDragEvent;
end;

procedure TvgCustomScene.EndDragEvent(Sender, Target: TObject; X, Y: Integer);
begin
  VCLDragSource.Tag := 0;
end;

procedure TvgCustomScene.DoDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
var
  P: TvgPoint;
  NewTarget: TvgVisualObject;
  Data: TvgDragObject;
begin
  Accept := false;
  if Root = nil then Exit;

  FillChar(Data, SizeOf(Data), 0);
  if (VCLDragSource <> nil) and (VCLDragSource.Tag <> 0) then
    Data.Source := TvgObject(VCLDragSource.Tag)
  else
    Data.Source := Source;

  P := vgPoint(X, Y);
  NewTarget := Root.Visual.FindTarget(P, Data);

  if (VCLDragSource <> nil) and (NewTarget = TvgObject(VCLDragSource.Tag)) then
  begin
    Accept := false;
    Exit;
  end;

  if FTarget <> nil then
  begin
    FTarget.DragOver(Data, P, Accept);
  end;
  if (NewTarget <> FTarget) then
  begin
    if FTarget <> nil then
      FTarget.DragLeave;
    FTarget := NewTarget;
    if FTarget <> nil then
    begin
      FTarget.DragEnter(Data, P);
    end;
  end;
  if FTarget = nil then
    Accept := false;
end;

procedure TvgCustomScene.DoDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  Data: TvgDragObject;
begin
  if FTarget <> nil then
  begin
    FillChar(Data, SizeOf(Data), 0);
    if (VCLDragSource <> nil) and (VCLDragSource.Tag <> 0) then
      Data.Source := TvgObject(VCLDragSource.Tag)
    else
      Data.Source := Source;
    FTarget.DragDrop(Data, vgPoint(X, Y));
  end;
  if (VCLDragSource <> nil) then
    VCLDragSource.Tag := 0;
  FTarget := nil;
end;

{$IFDEF WIN32}

const
  IID_IDropTargetHelper: TGUID = (
    D1:$4657278b; D2:$411b; D3:$11d2; D4:($83,$9a,$00,$c0,$4f,$d9,$18,$d0));
  SID_IDropTargetHelper = '{4657278B-411B-11d2-839A-00C04FD918D0}';
  CLSID_DragDropHelper: TGUID = (
    D1:$4657278a; D2:$411b; D3:$11d2; D4:($83,$9a,$00,$c0,$4f,$d9,$18,$d0));

type
  {_$EXTERNALSYM IDropTargetHelper}
  IDropTargetHelper = interface(IUnknown)
    [SID_IDropTargetHelper]
    function DragEnter(hwndTarget: HWND; const DataObj: IDataObject;
      var pt: TPoint; dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function DragOver(var pt: TPoint; dwEffect: longInt): HResult; stdcall;
    function Drop(const DataObj: IDataObject; var pt: TPoint;
      dwEffect: longInt): HResult; stdcall;
    function Show(Show: BOOL): HResult; stdcall;
  end;

var
  FDropTargetHelper: IDropTargetHelper;
  FDataObj: IDataObject;

function TvgCustomScene.GetDataObject: TvgDragObject;
var
  formatEtc: TFORMATETC;
  stgMedium: TSTGMEDIUM;
  str: wideString;
  drop: HDrop;
  i, numFiles: integer;
  buffer : array[0..MAX_PATH] of widechar;
begin
  FillChar(Result, SizeOf(Result), 0);
  if not Assigned(FDataObj) then Exit;
  // get file name first
  with formatEtc do
  begin
    cfFormat := CF_HDROP;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  { Get the data }
  str := '';
  if FDataObj.GetData(formatEtc, stgMedium) = S_OK then
  begin
    try
      {Lock the global memory handle to get a pointer to the data}
      drop := HDrop(GlobalLock(stgMedium.hGlobal));
      { Replace Text }
      numFiles := DragQueryFile(drop, $FFFFFFFF, nil, 0);
      SetLength(Result.Files, numFiles);
      for i := 0 to numFiles - 1 do
      begin
        DragQueryFileW(drop, i, @buffer, sizeof(buffer));
        Result.Files[i] := buffer;
        if i = 0 then
          Result.Data := Result.Files[0];
      end;
    finally
      {Finished with the pointer}
      GlobalUnlock(stgMedium.hGlobal);
      {Free the memory}
      ReleaseStgMedium({$IFDEF FPC}@{$ENDIF}stgMedium);
    end;
  end
  else
  begin
    // get text
    formatEtc.cfFormat := CF_UNICODETEXT;
    if FDataObj.GetData(formatEtc, stgMedium) = S_OK then
    begin
      try
        {Lock the global memory handle to get a pointer to the data}
        str := PWideChar(GlobalLock(stgMedium.hGlobal));
        Result.Data := str;
      finally
        {Finished with the pointer}
        GlobalUnlock(stgMedium.hGlobal);
        {Free the memory}
        ReleaseStgMedium({$IFDEF FPC}@{$ENDIF}stgMedium);
      end;
    end
  end;
end;

function TvgCustomScene.DragEnter(const dataObj: IDataObject; grfKeyState: Longint;
      pt: TPoint; var dwEffect: Longint): HResult;
begin
  try
    if (Root = nil) and not (Root.IsVisual) then
    begin
      dwEffect := DROPEFFECT_NONE;
      Result := E_UNEXPECTED;
      Exit;
    end;
    FDataObj := dataObj;
    Result := S_OK;
    dwEffect := DROPEFFECT_NONE;
    if (Succeeded(CoCreateInstance(CLSID_DragDropHelper, nil, CLSCTX_INPROC_SERVER,
      IDropTargetHelper, FDropTargetHelper))) and
      (FDropTargetHelper <> nil) then
    begin
      if (Failed(FDropTargetHelper.DragEnter(Handle, DataObj, pt, dwEffect))) then
        FDropTargetHelper := nil;
    end;
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TvgCustomScene.DragOver(grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult;
var
  P: TvgPoint;
  NewTarget: TvgVisualObject;
begin
  try
    dwEffect := DROPEFFECT_NONE;
    Result := S_OK;
    with ScreenToClient(pt) do
      P := vgPoint(X, Y);
    NewTarget := Root.Visual.FindTarget(P, GetDataObject);
    if (NewTarget <> FTarget) then
    begin
      if FTarget <> nil then
        FTarget.DragLeave;
      FTarget := NewTarget;
      if FTarget <> nil then
      begin
        FTarget.DragEnter(GetDataObject, P);
      end;
    end;
    if NewTarget <> nil then
      dwEffect := DROPEFFECT_LINK;
    if FDropTargetHelper <> nil then
      FDropTargetHelper.DragOver(pt, dwEffect);
  except
    dwEffect := DROPEFFECT_NONE;
    Result := E_UNEXPECTED;
  end;
end;

function TvgCustomScene.DragLeave: HResult;
begin
  if FTarget <> nil then
    FTarget.DragLeave;
  if (FDropTargetHelper <> nil) then
    FDropTargetHelper.DragLeave;
  FTarget := nil;
  FDropTargetHelper := nil;
  FDataObj := nil;
  Result := S_OK;
end;

function TvgCustomScene.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HResult;
var
  P: TvgPoint;
begin
  try
    if (dataObj = nil) then Exit;
    if FTarget <> nil then
    begin
      with ScreenToClient(pt) do
        P := vgPoint(X, Y);
      FTarget.DragDrop(GetDataObject, P);
    end;
    if (FDropTargetHelper <> nil) then
      FDropTargetHelper.Drop(DataObj, pt, dwEffect)
  finally
    FDataObj := nil;
    FDropTargetHelper := nil;
  end;
end;

procedure TvgCustomScene.WMAddUpdateRect(var Msg: TMessage);
 procedure ProcessUpdateMessages;
 var
   Msg: TMsg;
 begin
   while PeekMessage(Msg, Handle, WM_ADDUPDATERECT, WM_ADDUPDATERECT, PM_REMOVE) do
   begin
     if Msg.message = WM_QUIT then
     begin
       { Repost WM_QUIT messages }
       PostQuitMessage(Msg.WParam);
       Break;
     end;
   end;
 end;
begin
  ProcessUpdateMessages;
  Draw;
end;

{$ENDIF}

{$IFDEF LINUX}
procedure TvgCustomScene.EraseBackground(DC: HDC);
begin
end;
{$ENDIF}

procedure TvgCustomScene.Paint;
begin
  {$IFDEF LINUX}
  Canvas.cr := gdk_cairo_create(TGtkDeviceContext(inherited Canvas.Handle).Drawable);

  if FTransparency then
  begin
    cairo_set_operator(Canvas.cr, CAIRO_OPERATOR_SOURCE);
    cairo_set_source_rgba(Canvas.cr, 0, 0, 0, 0);
    cairo_paint(Canvas.cr);
    cairo_set_operator(Canvas.cr, CAIRO_OPERATOR_OVER);
  end;
//  if Length(FUpdateRects) = 0 then
  begin
    // update all area
    SetLength(FUpdateRects, 1);
    FUpdateRects[0] := vgRect(0, 0, Width, Height);
  end;
  Draw;
  cairo_destroy(Canvas.cr);
  Canvas.cr := nil;
  {$ENDIF}
end;

function TvgCustomScene.ObjectByPoint(X, Y: single): TvgVisualObject;
var
  i: integer;
  Obj, NewObj: TvgObject;
begin
  Result := nil;
  for i := Count - 1 downto 0 do
  begin
    Obj := Children[i];
    if not (Obj is TvgVisualObject) then Exit;
    if not TvgVisualObject(Obj).Visible and not (FDesignTime) then Continue;

    NewObj := TvgVisualObject(Obj).ObjectByPoint(X, Y);
    if NewObj <> nil then
    begin
      Result := TvgVisualObject(NewObj);
      Exit;
    end;
  end;
end;

procedure TvgCustomScene.CMShowingChanged(var Message: {$IFDEF FPC} TLMessage {$ELSE} TMessage {$ENDIF});
begin
  inherited ;
  AddUpdateRect(vgRect(0, 0, Width, Height));
end;

procedure TvgCustomScene.CMDesignHitTest(var Msg: {$IFDEF FPC} TLMMouse {$ELSE} TWMMouse {$ENDIF});
var
  Obj: TvgVisualObject;
  P: TvgPoint;
begin
  inherited ;

  if (FChildren = nil) or (FChildren.Count = 0) then
  begin
    Msg.Result := 1;
    Exit;
  end;

  P := vgPoint(Msg.XPos, Msg.YPos);
  if (FMoving or FLeftTop or FRightTop or FLeftBottom or FRightBottom) then
  begin
    Msg.Result := 1;
    Exit;
  end;
  Obj := ObjectByPoint(P.X, P.Y);
  if Obj = nil then
  begin
    if (FSelected <> nil) and (vgPtInRect(vgPoint(P.X, P.Y), FSelected.AbsoluteRect)) then
      Msg.Result := 1
    else
      Msg.Result := 0
  end
  else
  begin
    Msg.Result := 1;
  end;
end;

procedure TvgCustomScene.CMHintShow(var Message: {$IFDEF FPC} TLMessage {$ELSE} TMessage {$ENDIF});
var
  Obj: TvgVisualObject;
begin
  inherited ;
  with TCMHintShow(Message).HintInfo^ do
  begin
    Obj := ObjectByPoint(CursorPos.X, CursorPos.Y);
    if (Obj <> nil) and (Obj.ShowHint) then
    begin
      HintStr := Obj.Hint;
      with Obj.AbsoluteRect do
        CursorRect := Rect(Trunc(Left), Trunc(Top), Trunc(Right), Trunc(Bottom));
    end
    else
    begin
      HintStr := '';
    end;
  end;
end;

{$IFDEF WIN32}
procedure TvgCustomScene.CMMouseLeave(var Message: TMessage);
begin
  inherited ;
  if FHovered <> nil then
  begin
    FHovered.MouseInObject := false;
    FHovered.MouseLeave;
    FHovered := nil;
  end;
end;

type
  PRgnRects = ^TRgnRects;
  TRgnRects = array [0..0] of TRect;

procedure TvgCustomScene.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
var
  rgnStatus: integer;
  rgn: HRgn;
begin
  if (Msg.DC <> 0) and (Canvas <> nil) then
  begin
    rgn := CreateRectRgn(0, 0, 1, 1);
    rgnStatus := GetUpdateRgn(Handle, rgn, false);
    if (rgnStatus = 1) then
    begin
      Canvas.FlushBuffer(0, 0, Msg.DC);
    end;
    DeleteObject(rgn);
  end;
  Msg.Result := 1;
end;

procedure TvgCustomScene.WMPaint(var Msg: TWMPaint);
var
  i, rgnStatus: integer;
  rgn: HRgn;
  rgnSize: integer;
  rgnData: PRgnData;
  R: windows.TRect;
  DC: cardinal;
begin
//  if FDrawing then Exit;
  rgn := CreateRectRgn(0, 0, 1, 1);
  rgnStatus := GetUpdateRgn(Handle, rgn, false);
  if (rgnStatus = 2) or (rgnStatus = 3) then
  begin
    rgnSize := GetRegionData(rgn, $FFFF, nil);
    if rgnSize > 0 then
    begin
      GetMem(rgnData, rgnSize);
      rgnSize := GetRegionData(rgn, rgnSize, rgnData);
      if rgnSize = rgnSize then
      begin
        SetLength(FUpdateRects, rgnData.rdh.nCount);
        for i := 0 to rgnData.rdh.nCount - 1 do
        begin
          R := PRgnRects(@rgnData.buffer[0])[i];
          with R do
            FUpdateRects[i] := vgRect(left, top, right, bottom);
        end;
      end;
      FreeMem(rgnData, rgnSize);

      DC := GetDC(Handle);
      Canvas.SceneDC := DC;
      Draw;
      Canvas.SceneDC := 0;
      ReleaseDC(Handle, DC);

      {$IFDEF FPC}
      Msg.result := CallWindowProcW(@PrevWndProc, Handle, Msg.Msg, TMessage(Msg).WParam, TMessage(Msg).LParam);
      {$ELSE}
      inherited ;
      {$ENDIF}
    end
    else
    begin
      {$IFDEF FPC}
      Msg.result := CallWindowProcW(@PrevWndProc, Handle, Msg.Msg, TMessage(Msg).WParam, TMessage(Msg).LParam);
      {$ELSE}
      inherited ;
      {$ENDIF}
    end;
  end
  else
  begin
    {$IFDEF FPC}
    Msg.result := CallWindowProcW(@PrevWndProc, Handle, Msg.Msg, TMessage(Msg).WParam, TMessage(Msg).LParam);
    {$ELSE}
    inherited ;
    {$ENDIF}
  end;
  DeleteObject(rgn);
end;

{$IFNDEF FPC}

procedure TvgCustomScene.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited ;
  Msg.Result := DLGC_WANTTAB or dlgc_WantArrows or DLGC_WANTCHARS;
end;

procedure TvgCustomScene.WMKeyDown(var Message: TWMKeyDown);
var
  ShiftState: TShiftState;
  Ch: WideChar;
  NewKey: word;
begin
  with Message do
  begin
    Ch := #0;
    NewKey := Message.CharCode;
    ShiftState := KeyDataToShiftState(KeyData);
    UnicodeKeyDown(NewKey, Ch, ShiftState);
  end;
  inherited ;
end;

procedure TvgCustomScene.WMKeyUp(var Message: TWMKeyUp);
var
  ShiftState: TShiftState;
  Ch: WideChar;
  NewKey: word;
begin
  with Message do
  begin
    Ch := #0;
    NewKey := Message.CharCode;
    ShiftState := KeyDataToShiftState(KeyData);
    UnicodeKeyUp(NewKey, Ch, ShiftState);
  end;
  inherited ;
end;

procedure TvgCustomScene.WMChar(var Message: TWMChar);
var
  ShiftState: TShiftState;
  Ch: WideChar;
  NewKey: word;
begin
  with Message do
  begin
    Ch := Widechar(CharCode);
    ShiftState := KeyDataToShiftState(KeyData);
    case Ch of
      #3: // ctrl+c
        begin
          Ch := 'c';
          UnicodeKeyDown(NewKey, Ch, ShiftState);
          UnicodeKeyUp(NewKey, Ch, ShiftState);
        end;
      #$16: // ctrl+v
        begin
          Ch := 'v';
          UnicodeKeyDown(NewKey, Ch, ShiftState);
          UnicodeKeyUp(NewKey, Ch, ShiftState);
        end;
      #$18: // ctrl+x
        begin
          Ch := 'x';
          UnicodeKeyDown(NewKey, Ch, ShiftState);
          UnicodeKeyUp(NewKey, Ch, ShiftState);
        end;
      #$1A: // ctrl+z
        begin
          Ch := 'z';
          UnicodeKeyDown(NewKey, Ch, ShiftState);
          UnicodeKeyUp(NewKey, Ch, ShiftState);
        end;
      #13:
        begin
        end;
      else
        NewKey := 0;
        UnicodeKeyDown(NewKey, Ch, ShiftState);
    end;
  end;
  inherited ;
end;

procedure AssignToLogFont(var LogFont: TLogFont; Font: TvgFont);
var
  ppi: Integer;
  b: TBitmap;
begin
  FillChar(LogFont, sizeof(LogFont), 0);
  b := TBitmap.Create;
  b.Width := 1;
  b.Height := 1;
  b.Canvas.Font.Name := Font.Family;
  with LogFont do
  begin
    ppi := b.Canvas.Font.PixelsPerInch;
    lfHeight := -MulDiv(trunc(Font.size), ppi, 72);
    if Font.Style in [vgFontBoldItalic, vgFontBold] then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(Font.Style in [vgFontBoldItalic, vgFontItalic]);
    lfUnderline := 0;
    lfStrikeOut := 0;
    StrPCopy(lfFaceName, b.Canvas.Font.Name);
    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfPitchAndFamily := DEFAULT_PITCH;
  end;
  b.Free;
end;

procedure TvgCustomScene.WMImeStartComposition(var Message: TMessage);
var
  IMC: HIMC;
  LogFont: TLogFont;
  CF: TCompositionForm;
begin
  Message.Result := 1;
  inherited;
  IMC := ImmGetContext(Handle);
  if IMC <> 0 then
  begin
    if (Focused <> nil) and (Focused is TvgTextControl) then
      AssignToLogFont(LogFont, TvgTextControl(Focused).Font);
    ImmSetCompositionFont(IMC, @LogFont);
    CF.dwStyle := CFS_RECT;
    CF.rcArea  := ClientRect;
    if Focused <> nil then
    begin
      with Focused.LocalToAbsolute(vgPoint(0, 0)) do
        CF.ptCurrentPos := Point(trunc(x), trunc(y));
      if Focused is TvgTextBox then
      begin
        CF.ptCurrentPos.X := CF.ptCurrentPos.X + Trunc(TvgTextBox(Focused).ContentRect.Left);
        CF.ptCurrentPos.Y := CF.ptCurrentPos.Y + Trunc(TvgTextBox(Focused).ContentRect.Top);
        CF.ptCurrentPos.X := CF.ptCurrentPos.X + Trunc(TvgTextBox(Focused).GetCharX(TvgTextBox(Focused).CaretPosition));
      end;
      if Focused is TvgMemo then
      begin
        with TvgMemo(Focused).GetPositionPoint(TvgMemo(Focused).CaretPosition) do
        begin
          CF.ptCurrentPos.X := CF.ptCurrentPos.X + Trunc(x);
          CF.ptCurrentPos.Y := CF.ptCurrentPos.Y + Trunc(y);
        end;
      end;
    end;
    if (GetKeyboardLayout(0) and $FFFF)= $0412 then begin// Special support for Korean IME
      CF.rcArea.TopLeft := CF.ptCurrentPos;
      OffsetRect(CF.rcArea,0,1);
    end;
    ImmSetCompositionWindow(IMC, @CF);
    ImmReleaseContext(Handle, IMC);
  end;
end;

procedure TvgCustomScene.WMImeComposition(var Message: TMessage);
var
  i: integer;
  IMC: HIMC;
  s: WideString;
  Size: Integer;
  key: word;
  char: widechar;
begin
  if (Message.LParam and GCS_RESULTSTR) <> 0 then
  begin
    IMC := ImmGetContext(Handle);
    if IMC<>0 then
    begin
      begin
        try
          Size := ImmGetCompositionStringW(IMC, GCS_RESULTSTR, nil, 0);
          SetLength(s, Size div 2);
          FillChar(PChar(s)^, Size, 0);
          ImmGetCompositionStringW(IMC, GCS_RESULTSTR, PWideChar(s), Size);
        finally
          ImmReleaseContext(Handle, IMC);
        end;
      end;
      for i := 1 to Length(s) do
      begin
        key := 0;
        char := s[i];
        UnicodeKeyDown(key, char, []);
      end;
      Message.Result := 0;
    end;
    if (GetKeyboardLayout(0) and $FFFF)= $0412 then // Special support for Korean IME
      PostMessage(Handle, WM_IME_STARTCOMPOSITION,0,0);
  end
  else
    inherited;
end;

{$ENDIF}

{$ENDIF}

procedure TvgCustomScene.UnicodeKeyDown(var Key: Word; var Char: System.WideChar;
  Shift: TShiftState);
var
  List: TList;
  i, CurIdx: integer;
  Found: boolean;
  O: TComponent;
begin
  { modal }
  if (Key = VK_ESCAPE) then
  begin
    O := Owner;
    while O <> nil do
    begin
      if (O is TCustomForm) then
      begin
        TCustomForm(O).ModalResult := mrCancel;
        Break;
      end;
      O := O.Owner;
    end;
    Key := 0;
    Exit;
  end;
  { change focus }
  if (Key = VK_TAB) and (Root <> nil) then
  begin
    Key := 0;
    List := TList.Create;
    Root.AddObjectsToList(List);
    if FFocused <> nil then
      CurIdx := List.IndexOf(FFocused) + 1
    else
      CurIdx := 0;

    Found := false;
    { first search in last part of list }
    if (List.Count > 2) and (CurIdx < List.Count) then
      for i := CurIdx to List.Count - 1 do
        if TvgObject(List[i]).isVisual and (TvgVisualObject(List[i]).CheckParentVisible) and (TvgVisualObject(List[i]).CanFocused) then
        begin
          TvgVisualObject(List[i]).SetFocus;
          Found := true;
          Break;
        end;
    { second search in last part of list }
    if not Found then
      if CurIdx > 0 then
        for i := 0 to CurIdx - 1 do
          if TvgObject(List[i]).isVisual and (TvgVisualObject(List[i]).CheckParentVisible) and (TvgVisualObject(List[i]).CanFocused) then
          begin
            TvgVisualObject(List[i]).SetFocus;
            Break;
          end;
    List.Free;
    Exit;
  end;
  { focused handler }
  if FFocused <> nil then
  begin
    FFocused.KeyDown(Key, Char, Shift);
  end;
end;

procedure TvgCustomScene.UnicodeKeyUp(var Key: Word; var Char: System.WideChar;
  Shift: TShiftState);
begin
  if FDesignTime or (FDesignTime) then
  begin
    if (Key = VK_DELETE) and (FSelected <> nil) then
    begin
      FSelected.Free;
      FSelected := nil;
    end;
  end;
  { focused handler }
  if FFocused <> nil then
  begin
    FFocused.KeyUp(Key, Char, Shift);
  end;
end;

procedure TvgCustomScene.DeleteChildren;
var
  Child: TvgObject;
begin
  if Assigned(FChildren) then
  begin
    while FChildren.Count > 0 do
    begin
      Child := TvgObject(FChildren[0]);
      FChildren.Delete(0);
      Child.FParent := nil;
      Child.FScene := nil;
      Child.Free;
    end;
    FreeAndNil(FChildren);
  end;
end;

procedure TvgCustomScene.AddObject(AObject: TvgObject);
begin
  if AObject = nil then Exit;
  if AObject.Parent <> nil then
    AObject.Parent := nil;
  if FChildren = nil then
    FChildren := TList.Create;
  FChildren.Add(AObject);
  AObject.FScene := Self;
  if AObject.IsVisual and not (csDestroying in ComponentState) then
    TvgVisualObject(AObject).Repaint;
end;

procedure TvgCustomScene.RemoveObject(AObject: TvgObject);
begin
  if FChildren <> nil then
  begin
    Invalidate;
    AObject.FScene := nil;
    Notification(AObject, opRemove);
    FChildren.Remove(AObject);
  end;
end;

function TvgCustomScene.GetCount: integer;
begin
  if FChildren <> nil then
    Result := FChildren.Count
  else
    Result := 0;
end;

function TvgCustomScene.GetChildrenObject(Index: integer): TvgObject;
begin
  if FChildren <> nil then
    Result := TvgObject(FChildren[Index])
  else
    Result := nil;
end;

procedure TvgCustomScene.SetChildren(Index: integer; const Value: TvgObject);
begin
end;

procedure TvgCustomScene.BeginDrag;
var
  P: TPoint;
begin
  FDragging := true;

  GetCursorPos(P);
  P := TWinControl(Owner).ScreenToClient(P);
  FDownPos := vgPoint(P.X, P.Y);
  MouseCapture;
end;

procedure TvgCustomScene.BeginResize;
var
  P: TPoint;
begin
  FResizing := true;
  GetCursorPos(P);
  P := TWinControl(Owner).ScreenToClient(P);
  FDownPos := vgPoint(P.X, P.Y);
  FResizeSize := Point(TWinControl(Owner).Width, TWinControl(Owner).Height);
  MouseCapture;
end;

function TvgCustomScene.SnapToGridValue(Value: single): single;
begin
  if (DesignSnapToGrid) and (DesignSnapGridSize <> 0) then
    Result := Trunc(Value / DesignSnapGridSize) * DesignSnapGridSize
  else
    Result := Value;
end;

procedure TvgCustomScene.AddUpdateRectsFromGridLines;
  procedure IntAddUpdateRect(const R: TvgRect);
  var
    i: integer;
  begin
    for i := 0 to High(FUpdateRects) do
      with FUpdateRects[i] do
        if (R.Left = Left) and (R.Top = Top) and (R.Right = Right) and (R.Bottom = Bottom) then
        begin
          Exit;
        end;
    AddUpdateRect(R);
  end;
var
  i: integer;
begin
  { Add grip lines }
  if FDesignTime and (FSelected <> nil) and not FSelected.DisableDesignResize and
     (FMoving or FLeftTop or FRightTop or FLeftBottom or FRightBottom or FTop or FBottom or FLeft or FRight) and
     (Length(FDesignGridLines) > 0) and (FSelected.Parent <> nil) and (FSelected.Parent.IsVisual) then
  begin
    for i := 0 to High(FDesignGridLines) do
    begin
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FSelected.Position.Y + (FSelected.Height / 2) - 1,
        FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + (FDesignGridLines[i].Height / 2) + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X + (FSelected.Width / 2) - 1, FSelected.Position.Y - 1,
        FDesignGridLines[i].Position.X + (FDesignGridLines[i].Width / 2) + 1, FDesignGridLines[i].Position.Y + 1)));

      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FSelected.Position.Y - 1, FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FDesignGridLines[i].Position.X - 1, FSelected.Position.Y - 1, FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FDesignGridLines[i].Position.Y - 1, FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width - 1, FSelected.Position.Y - 1,
          FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height - 1,
          FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FDesignGridLines[i].Position.X + FDesignGridLines[i].Width - 1, FSelected.Position.Y - 1,
          FDesignGridLines[i].Position.X + FDesignGridLines[i].Width + 1, FDesignGridLines[i].Position.Y + 1)));
      IntAddUpdateRect(vgNormalizeRect2(vgRect(FSelected.Position.X - 1, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height - 1,
          FDesignGridLines[i].Position.X + 1, FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height + 1)));
    end; 
  end;
end;

procedure TvgCustomScene.SnapToGridLines(AllowChangePosition: boolean);
  procedure AddGridLine(const Obj: TvgVisualObject);
  var
    i: integer;
  begin
    for i := 0 to High(FDesignGridLines) do
      if FDesignGridLines[i] = Obj then Exit;
    SetLength(FDesignGridLines, Length(FDesignGridLines) + 1);
    FDesignGridLines[High(FDesignGridLines)] := Obj;
  end;
const
  SnapLineSize = 2;
var
  i: integer;
begin
  if (DesignSnapToLines) and (FSelected.Parent <> nil) then
    for i := 0 to FSelected.Parent.FChildren.Count - 1 do
    begin
      if TvgObject(FSelected.Parent.FChildren[i]) = FSelected then Continue;
      if not TvgObject(FSelected.Parent.FChildren[i]).isVisual then Continue;
      with TvgVisualObject(FSelected.Parent.FChildren[i]) do
      begin
        if (Abs((Position.Y + round(Height / 2)) - (FSelected.Position.Y + round(FSelected.Height / 2))) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + ((Position.Y + round(Height / 2)) - (FSelected.Position.Y + round(FSelected.Height / 2)));
            FSelected.Position.Y := (Position.Y + round(Height / 2) - round(FSelected.Height / 2));
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
        if (Abs((Position.X + round(Width / 2)) - (FSelected.Position.X + round(FSelected.Width / 2))) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + ((Position.X + round(Width / 2)) - (FSelected.Position.X + round(FSelected.Width / 2)));
            FSelected.Position.X := (Position.X + round(Width / 2) - round(FSelected.Width / 2));
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
        if (Abs(Position.X - FSelected.Position.X) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + (Position.X - FSelected.Position.X);
            FSelected.Position.X := Position.X;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
        if (Abs(Position.Y - FSelected.Position.Y) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + (Position.Y - FSelected.Position.Y);
            FSelected.Position.Y := Position.Y;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
        if (Abs(Position.X - (FSelected.Position.X + FSelected.Width)) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + (Position.X - (FSelected.Position.X + FSelected.Width));
            FSelected.Position.X := Position.X - FSelected.Width;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
        if (Abs(Position.Y - (FSelected.Position.Y + FSelected.Height)) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + (Position.Y - (FSelected.Position.Y + FSelected.Height));
            FSelected.Position.Y := Position.Y - FSelected.Height;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
        if (Abs((Position.X + Width) - FSelected.Position.X) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + ((Position.X + Width) - FSelected.Position.X);
            FSelected.Position.X := Position.X + Width;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
        if (Abs((Position.Y + Height) - FSelected.Position.Y) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + ((Position.Y + Height) - FSelected.Position.Y);
            FSelected.Position.Y := Position.Y + Height;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
        if (Abs((Position.X + Width) - (FSelected.Position.X + FSelected.Width)) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.X := FMousePos.X + ((Position.X + Width) - (FSelected.Position.X + FSelected.Width));
            FSelected.Position.X := Position.X + Width - FSelected.Width;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
        if (Abs((Position.Y + Height) - (FSelected.Position.Y + FSelected.Height)) < SnapLineSize) then
        begin
          if AllowChangePosition then
          begin
            FMousePos.Y := FMousePos.Y + ((Position.Y + Height) - (FSelected.Position.Y + FSelected.Height));
            FSelected.Position.Y := Position.Y + Height - FSelected.Height;
          end;
          AddGridLine(TvgVisualObject(FSelected.Parent.FChildren[i]));
          Continue;
        end;
      end;
    end;
  AddUpdateRectsFromGridLines;
end;

function TvgCustomScene.SnapPointToGridLines(const APoint: TvgPoint): TvgPoint;
var
  i: integer;
begin
  Result := APoint;
  if not DesignSnapToLines then Exit;
  if FSelected = nil then Exit;
  if FSelected.Parent = nil then Exit;
  SnapToGridLines(false);
  if Length(FDesignGridLines) > 0 then
  begin
    Result := FSelected.LocalToAbsolute(APoint);
    Result := TvgVisualObject(FSelected.Parent).AbsoluteToLocal(Result);
    for i := 0 to High(FDesignGridLines) do
    begin
      if Abs(Result.X - FDesignGridLines[i].Position.X) < (4) then
        Result.X := FDesignGridLines[i].Position.X;
      if Abs(Result.Y - FDesignGridLines[i].Position.Y) < (4) then
        Result.Y := FDesignGridLines[i].Position.Y;
      if Abs(Result.X - (FDesignGridLines[i].Position.X + FDesignGridLines[i].Width)) < (4) then
        Result.X := FDesignGridLines[i].Position.X + FDesignGridLines[i].Width;
      if Abs(Result.Y - (FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height)) < (4) then
        Result.Y := FDesignGridLines[i].Position.Y + FDesignGridLines[i].Height;
    end;
    Result := TvgVisualObject(FSelected.Parent).LocalToAbsolute(Result);
    Result := FSelected.AbsolutetoLocal(Result);
  end;
end;

procedure TvgCustomScene.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  P: TvgPoint;
  R: TvgRect;
  Obj: TvgVisualObject;
//  InsertObject: TvgObject;
  SG: IvgSizeGrip;
  i: integer;
begin
  inherited;
  if not (FDesignTime) and not Assigned(FOnFlush) and not (csDesigning in ComponentState) then
    SetFocus;
  { translate coord }
  FUnsnapMousePos := vgPoint(x, y);
  FMousePos := vgPoint(SnapToGridValue(x), SnapToGridValue(y));
  FDownPos := FMousePos;
  SetLength(FDesignGridLines, 0);
  { design }
  if FDesignTime then
  begin
    { Create root }
    if ((FChildren = nil) or (FChildren.Count = 0)) then
    begin
      Obj := TvgBackground.Create(Owner);
      if vgDesigner <> nil then
        Obj.Name := vgDesigner.UniqueName(Owner, 'Root');
      AddObject(Obj);
      RealignRoot;
    end;
    { Popup }
    if Button = mbRight then
      OpenDesignPopup;
    { Resize }
    if (FSelected <> nil) and (not FSelected.DisableDesignResize) then
    begin
      P := FSelected.AbsoluteToLocal(vgPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      R := vgRect(FSelected.Width / 2 - (GripSize), - RotSize - (GripSize),
          (FSelected.Width) / 2 +(GripSize), - RotSize + (GripSize));
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FRotate := true;
        FMoving := false;
        Exit;
      end;
      P := FSelected.AbsoluteToLocal(vgPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      R := vgRect(-GripSize, -GripSize, GripSize, GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FLeftTop := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right -GripSize, -GripSize, R.Right + GripSize, GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FRightTop := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(-GripSize, R.Bottom - GripSize, GripSize, R.Bottom + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FLeftBottom := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right - GripSize, R.Bottom - GripSize, R.Right + GripSize, R.Bottom + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FRightBottom := true;
        FMoving := false;
        Exit;
      end;

      R := FSelected.BoundsRect;
      R := vgRect(vgRectWidth(R)/2 - GripSize, -GripSize, vgRectWidth(R)/2 + GripSize, GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FTop := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(vgRectWidth(R)/2 - GripSize, R.Bottom - GripSize, vgRectWidth(R)/2 + GripSize, R.Bottom + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FBottom := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(-GripSize, vgRectHeight(R)/2 - GripSize, GripSize, vgRectHeight(R)/2 + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FLeft := true;
        FMoving := false;
        Exit;
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right-GripSize, vgRectHeight(R)/2 - GripSize, R.Right + GripSize, vgRectHeight(R)/2 + GripSize);
      if vgPtInRect(P, R) then
      begin
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, FSelected, []);
        if Assigned(FDesignChangeSelection) then
          FDesignChangeSelection(Self);
        FRight := true;
        FMoving := false;
        Exit;
      end;
    end;
    { Change Selected }
    Obj := ObjectByPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y);
    if (Obj <> nil) and (Obj = FSelected) and (ssDouble in Shift) then
    begin
      Obj.DesignClick;
    end;
    if (Obj <> nil) then
    begin
      if (ssCtrl in Shift) and (Obj <> Selected) then
      begin
        { check is exists }
        for i := 0 to High(FSelection) do
          if FSelection[i] = Obj then
          begin
            FSelection[i] := Selected;
            Obj := nil;
          end;
        if Obj <> nil then
        begin
          SetLength(FSelection, Length(FSelection) + 1);
          FSelection[High(FSelection)] := Obj;
        end;
        if vgDesigner <> nil then
          vgDesigner.SelectObject(Owner, Selected, FSelection);
        Exit;
      end;
      SetLength(FSelection, 0);
      if FSelected <> nil then
      begin
        FSelected.RecalcUpdateRect;
        FSelected.Repaint;
      end;
      FSelected := Obj;
      FSelected.DesignSelect;
      { Select in IDE }
      if vgDesigner <> nil then
        vgDesigner.SelectObject(Owner, Obj, []);
      if Assigned(FDesignChangeSelection) then
        FDesignChangeSelection(Self);
      { }
      FSelected.RecalcUpdateRect;
      FSelected.Repaint;

      if (Obj = FSelected) then
        FMoving := true;
    end;
    Exit;
  end;
  { event }
  if not FDesignTime then
  begin
    Obj := TvgVisualObject(ObjectByPoint(FMousePos.X, FMousePos.Y));
    if (Obj <> nil) then
    begin
      if (Obj.QueryInterface(IvgSizeGrip, SG) = 0) and (Assigned(SG)) then
      begin
        BeginResize;
      end
      else
      begin
        P := Obj.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
        Obj.MouseDown(Button, Shift, P.X, P.Y);
      end;
    end
    else
      if AllowDrag then
        BeginDrag;
  end;
end;

procedure TvgCustomScene.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  R: TvgRect;
  P, P1: TvgPoint;
  Obj: TvgVisualObject;
  SG: IvgSizeGrip;
  NewCursor: TCursor;
begin
  inherited ;
  NewCursor := FLoadCursor;
  { drag }
  if FDragging then
  begin
    with TWinControl(Owner) do
      SetBounds(Round(Left + (X - FDownPos.X)), Round(Top + (Y - FDownPos.Y)), Width, Height);
    Exit;
  end;
  if FResizing then
  begin
    FResizeSize.X := Round(FResizeSize.X + (X - FUnsnapMousePos.X));
    FResizeSize.Y := Round(FResizeSize.Y + (Y - FUnsnapMousePos.Y));
    with TWinControl(Owner) do
      SetBounds(Left, Top, FResizeSize.X, FResizeSize.Y);
    Cursor := crSizeNWSE;
    FUnsnapMousePos := vgPoint(x, y);
    Exit;
  end;
  { translate coord }
  FMousePos := vgPoint(SnapToGridValue(x), SnapToGridValue(y));
  FUnsnapMousePos := vgPoint(x, y);
  { design }
  if FDesignTime then
  begin
    { change cursor }
    if (FSelected <> nil) then
    begin
      P := FSelected.AbsoluteToLocal(vgPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      R := vgRect(FSelected.Width / 2 - (GripSize), - RotSize - (GripSize),
        (FSelected.Width) / 2 +(GripSize), - RotSize + (GripSize));
      if FRotateHot <> vgPtInRect(P, R) then
      begin
        FRotateHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      P := FSelected.AbsoluteToLocal(vgPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      R := vgRect(-GripSize, -GripSize, GripSize, GripSize);
      if FLeftTopHot <> vgPtInRect(P, R) then
      begin
        FLeftTopHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right -GripSize, -GripSize, R.Right + GripSize, GripSize);
      if FRightTopHot <> vgPtInRect(P, R) then
      begin
        FRightTopHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(-GripSize, R.Bottom - GripSize, GripSize, R.Bottom + GripSize);
      if FLeftBottomHot <> vgPtInRect(P, R) then
      begin
        FLeftBottomHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right - GripSize, R.Bottom - GripSize, R.Right + GripSize, R.Bottom + GripSize);
      if FRightBottomHot <> vgPtInRect(P, R) then
      begin
        FRightBottomHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;

      R := FSelected.BoundsRect;
      R := vgRect(vgRectWidth(R)/2 - GripSize, -GripSize, vgRectWidth(R)/2 + GripSize, GripSize);
      if FTopHot <> vgPtInRect(P, R) then
      begin
        FTopHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(vgRectWidth(R)/2 - GripSize, R.Bottom - GripSize, vgRectWidth(R)/2 + GripSize, R.Bottom + GripSize);
      if FBottomHot <> vgPtInRect(P, R) then
      begin
        FBottomHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(-GripSize, vgRectHeight(R)/2 - GripSize, GripSize, vgRectHeight(R)/2 + GripSize);
      if FLeftHot <> vgPtInRect(P, R) then
      begin
        FLeftHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
      R := FSelected.BoundsRect;
      R := vgRect(R.Right-GripSize, vgRectHeight(R)/2 - GripSize, R.Right + GripSize, vgRectHeight(R)/2 + GripSize);
      if FRightHot <> vgPtInRect(P, R) then
      begin
        FRightHot := vgPtInRect(P, R);
        R.TopLeft := FSelected.LocaltoAbsolute(R.TopLeft);
        R.BottomRight := FSelected.LocaltoAbsolute(R.BottomRight);
        vgInflateRect(R, 2, 2);
        AddUpdateRect(R);
      end;
    end;
    { resize and move }
    if (ssLeft in Shift) and (FSelected <> nil) and (FMoving) then
    begin
      P := vgPoint(FUnsnapMousePos.X - FDownPos.X, FUnsnapMousePos.Y - FDownPos.Y);
      if (FSelected.Parent <> nil) and (FSelected.Parent.IsVisual) then
      begin
        with TvgVisualObject(FSelected.Parent).AbsoluteToLocalVector(vgVector(P.X, P.Y)) do
          P := vgPoint(X, Y);
      end
      else
      begin
        with FSelected.AbsoluteToLocalVector(vgVector(P.X, P.Y)) do
          P := vgPoint(X, Y);
      end;
      FSelected.Position.X := SnapToGridValue(FSelected.Position.X + P.X);
      FSelected.Position.Y := SnapToGridValue(FSelected.Position.Y + P.Y);
      { lines grid }
      SnapToGridLines(true);
      { check place }
      FSelected.FLocked := true;
      Obj := TvgVisualObject(ObjectByPoint(FUnsnapMousePos.X, FUnsnapMousePos.Y));
      FSelected.FLocked := false;
      { select }
      if (Obj <> nil) and (Obj <> FSelected.Parent) and (Obj <> FSelected) then
      begin
        if FDesignPlaceObject <> nil then
          FDesignPlaceObject.Repaint;
        FDesignPlaceObject := Obj;
        if FDesignPlaceObject <> nil then
        begin
          FDesignPlaceObject.RecalcUpdateRect;
          FDesignPlaceObject.Repaint;
        end;
        if (ssAlt in Shift) then
        begin
          P := FSelected.LocalToAbsolute(vgPoint(0, 0));
          FSelected.Parent := FDesignPlaceObject;
          P := FDesignPlaceObject.AbsoluteToLocal(P);
          FSelected.Position.X := P.X;
          FSelected.Position.Y := P.Y;
        end;
      end
      else
      begin
        if FDesignPlaceObject <> nil then
          FDesignPlaceObject.Repaint;
        FDesignPlaceObject := nil;
      end;
    end;
    if (ssLeft in Shift) and (FSelected <> nil) then
    begin
      if (ssLeft in Shift) and (FSelected <> nil) and (FRotate) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P1 := FSelected.AbsoluteToLocal(FDownPos);
        if vgVectorCrossProductZ(vgVector(P.X - (FSelected.Width / 2), P.Y - (FSelected.Height / 2)),
          vgVector(P1.X - (FSelected.Width / 2), P1.Y - (FSelected.Height / 2))) < 0
        then
          FSelected.RotateAngle := FSelected.RotateAngle + vgRadToDeg(ArcCos(vgVectorAngleCosine(vgVector(P.X - (FSelected.Width / 2), P.Y - (FSelected.Height / 2)),
            vgVector(P1.X - (FSelected.Width / 2), P1.Y - (FSelected.Height / 2)))))
        else
          FSelected.RotateAngle := FSelected.RotateAngle - vgRadToDeg(ArcCos(vgVectorAngleCosine(vgVector(P.X - (FSelected.Width / 2), P.Y - (FSelected.Height / 2)),
            vgVector(P1.X - (FSelected.Width / 2), P1.Y - (FSelected.Height / 2)))));
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FLeftTop) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(P.X, P.Y,
          R.Right, R.Bottom);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FRightTop) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, P.Y,
          P.X, R.Bottom);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FLeftBottom) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(P.X, R.Top,
          R.Right, P.Y);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FRightBottom) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, R.Top, P.X, P.Y);
      end;

      if (ssLeft in Shift) and (FSelected <> nil) and (FTop) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, P.Y, R.Right, R.Bottom);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FBottom) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, R.Top, R.Right, P.Y);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FLeft) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(P.X, R.Top, R.Right, R.Bottom);
      end;
      if (ssLeft in Shift) and (FSelected <> nil) and (FRight) then
      begin
        R := FSelected.BoundsRect;
        P := FSelected.AbsoluteToLocal(FMousePos);
        P := SnapPointToGridLines(P);
        FSelected.BoundsRect := vgRect(R.Left, R.Top, P.X, R.Bottom);
      end;
      FDownPos := FMousePos;
    end;
    Exit;
  end;
  { event }
  if not FDesignTime then
  begin
    if (FCaptured <> nil) then
    begin
      P := FCaptured.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
      FCaptured.MouseMove(Shift, P.X, P.Y, 0, 0);
      Exit;
    end;
    Obj := TvgVisualObject(ObjectByPoint(FMousePos.X, FMousePos.Y));
    if (Obj <> nil) then
    begin
      if ((Obj.QueryInterface(IvgSizeGrip, SG) = 0) and Assigned(SG)) then
        NewCursor := crSizeNWSE
      else
        NewCursor := Obj.Cursor;

      if (Obj <> FHovered) then
      begin
        if FHovered <> nil then
        begin
          FHovered.MouseInObject := false;
          FHovered.MouseLeave;
        end;
        FHovered := Obj;
        FHovered.MouseInObject := true;
        FHovered.MouseEnter;
      end;

      P := Obj.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
      Obj.MouseMove(Shift, P.X, P.Y, 0, 0);
    end
    else
    begin
      if FHovered <> nil then
      begin
        FHovered.MouseInObject := false;
        FHovered.MouseLeave;
        FHovered := nil;
      end;
    end;
  end;
  Cursor := NewCursor;
  FDownPos := FMousePos;
end;

procedure TvgCustomScene.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  P: TvgPoint;
  Obj: TvgVisualObject;
begin
  inherited;
  { design }
  if FDesignPlaceObject <> nil then
    FDesignPlaceObject.Repaint;
  FDesignPlaceObject := nil;
  AddUpdateRectsFromGridLines;
  SetLength(FDesignGridLines, 0);
  if (FSelected <> nil) and FMoving then
  begin
    if (FSelected.Parent <> nil) and (TvgObject(FSelected.Parent).IsVisual) then
      TvgVisualObject(FSelected.Parent).Realign;
    if (vgDesigner <> nil) then
      vgDesigner.Modified(Owner);
    if FSelected.Parent = nil then
      RealignRoot;
  end;
  if (FSelected <> nil) and (FLeftTop or FLeftBottom or FLeftBottom or FRightBottom) then
  begin
    if (FSelected.Parent <> nil) and (TvgObject(FSelected.Parent).IsVisual) then
      TvgVisualObject(FSelected.Parent).Realign;
    if (vgDesigner <> nil) then
      vgDesigner.Modified(Owner);
    if FSelected.Parent = nil then
      RealignRoot;
  end;
  FMoving := false;
  FLeftTop := false;
  FLeftBottom := false;
  FRightTop := false;
  FRightBottom := false;
  FTop := false;
  FBottom := false;
  FLeft := false;
  FRight := false;
  FRotate := false;
  { drag }
  if FDragging then
  begin
    FDragging := false;
//    ReleaseCapture;
  end;
  if FResizing then
  begin
    FResizing := false;
//    ReleaseCapture;
  end;
  { event }
  if not FDesignTime then
  begin
    if (FCaptured <> nil) then
    begin
      P := FCaptured.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
      FCaptured.MouseUp(Button, Shift, P.X, P.Y);
      Exit;
    end;
    Obj := TvgVisualObject(ObjectByPoint(FMousePos.X, FMousePos.Y));
    if (Obj <> nil) then
    begin
      P := Obj.AbsoluteToLocal(vgPoint(FMousePos.X, FMousePos.Y));
      Obj.MouseUp(Button, Shift, P.X, P.Y);
    end;
  end;
end;

function TvgCustomScene.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
   MousePos: TPoint): Boolean;
var
  Obj: TvgVisualObject;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  { event }
  if not FDesignTime then
  begin
    {$IFDEF DARWIN}
//    WheelDelta := WheelDelta * 40;
    {$ENDIF}
    if (FCaptured <> nil) then
    begin
      FCaptured.MouseWheel(Shift, WheelDelta, Result);
      Exit;
    end;
    Obj := TvgVisualObject(ObjectByPoint(FMousePos.X, FMousePos.Y));
    while (Obj <> nil) do
    begin
      Obj.MouseWheel(Shift, WheelDelta, Result);
      if Result then Break;
      if (Obj.Parent <> nil) and (Obj.Parent.IsVisual) then
        Obj := TvgVisualObject(Obj.Parent)
      else
        Obj := nil;
    end;
  end;
end;

{$IFDEF FPC}
procedure TvgCustomScene.KeyDown(var Key: Word; Shift: TShiftState);
var
  List: TList;
  i, CurIdx: integer;
  Found: boolean;
  K: Word;
  Ch, Char: System.WideChar;
begin
  inherited;
  {$IFDEF FPC}
  FShift := Shift;
  if ssMeta in Shift then
  begin
    K := 0;
    case Key of
      67: // ctrl+c
      begin
        Ch := 'c';
        UnicodeKeyDown(K, Ch, [ssCtrl]);
        UnicodeKeyUp(K, Ch, [ssCtrl]);
        Exit;
      end;
      86: // ctrl+v
      begin
        Ch := 'v';
        UnicodeKeyDown(K, Ch, [ssCtrl]);
        UnicodeKeyUp(K, Ch, [ssCtrl]);
        Exit;
      end;
      88: // ctrl+x
      begin
        Ch := 'x';
        UnicodeKeyDown(K, Ch, [ssCtrl]);
        UnicodeKeyUp(K, Ch, [ssCtrl]);
        Exit;
      end;
      99: // ctrl+z
      begin
        Ch := 'z';
        UnicodeKeyDown(K, Ch, [ssCtrl]);
        UnicodeKeyUp(K, Ch, [ssCtrl]);
        Exit;
      end;
    end;
  end;
  { change focus }
  if (Key = VK_TAB) and (Root <> nil) then
  begin
    Key := 0;
    List := TList.Create;
    Root.AddObjectsToList(List);
    if FFocused <> nil then
      CurIdx := List.IndexOf(FFocused) + 1
    else
      CurIdx := 0;

    Found := false;
    { first search in last part of list }
    if (List.Count > 2) and (CurIdx < List.Count) then
      for i := CurIdx to List.Count - 1 do
        if TvgObject(List[i]).isVisual and (TvgVisualObject(List[i]).CanFocused) then
        begin
          TvgVisualObject(List[i]).SetFocus;
          Found := true;
          Break;
        end;
    { second search in last part of list }
    if not Found then
      if CurIdx > 0 then
        for i := 0 to CurIdx - 1 do
          if TvgObject(List[i]).isVisual and (TvgVisualObject(List[i]).CanFocused) then
          begin
            TvgVisualObject(List[i]).SetFocus;
            Break;
          end;
    List.Free;
    Exit;
  end;
  { focused handler }
  if FFocused <> nil then
  begin
    Char := #0;
    FFocused.KeyDown(Key, Char, Shift);
  end;
  {$ENDIF}
end;

procedure TvgCustomScene.UTF8KeyPress(var UTF8Key: TUTF8Char);
var
  S: System.WideChar;
  K: word;
  C: System.WideChar;
begin
  K := 0;
  C := UTF8ToUTF16(UTF8Key)[1];
  case C of
        #3: // ctrl+c
          begin
            C := 'c';
            UnicodeKeyDown(K, C, FShift);
            UnicodeKeyUp(K, C, FShift);
          end;
        #$16: // ctrl+v
          begin
            C := 'v';
            UnicodeKeyDown(K, C, FShift);
            UnicodeKeyUp(K, C, FShift);
          end;
        #$18: // ctrl+x
          begin
            C := 'x';
            UnicodeKeyDown(K, C, FShift);
            UnicodeKeyUp(K, C, FShift);
          end;
        #$1A: // ctrl+z
          begin
            C := 'z';
            UnicodeKeyDown(K, C, FShift);
            UnicodeKeyUp(K, C, FShift);
          end;
  else
    UnicodeKeyDown(K, C, FShift);
    UnicodeKeyUp(K, C, FShift);
  end;
  UTF8Key := '';
end;

procedure TvgCustomScene.KeyPress(var Key: char);
begin
  inherited KeyPress(Key);
end;

procedure TvgCustomScene.KeyUp(var Key: Word; Shift: TShiftState);
var
  Char: system.WideChar;
begin
  inherited;
  FShift := [];
  if FDesignTime then
  begin
    if (Key = VK_DELETE) and (FSelected <> nil) then
    begin
      FSelected.Free;
      FSelected := nil;
    end;
  end;
  { focused handler }
  if FFocused <> nil then
  begin
    Char := #0;
    FFocused.KeyUp(Key, char, Shift);
  end;
end;
{$ENDIF}

procedure TvgCustomScene.EmptyObjects;
begin
  FCaptured := nil;
  FSelected := nil;
  FHovered := nil;
  FFocused := nil;
  FDesignPlaceObject := nil;
end;

procedure TvgCustomScene.RealignRoot;
begin
  if (FChildren <> nil) and (FChildren.Count > 0) and (TvgObject(FChildren[0]).isVisual) then
    with TvgVisualObject(FChildren[0]) do
    begin
      Position.X := 0;
      Position.Y := 0;
      FWidth := Self.Width / Scale.X;
      FHeight := Self.Height / Scale.Y;
      RecalcAbsolute;
      RecalcUpdateRect;
      Realign;
    end;
end;

procedure TvgCustomScene.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if (Canvas <> nil) and (HandleAllocated) then
  begin
    Canvas.ResizeBuffer(AWidth, AHeight);
    Canvas.SetTextRendering(FTextRendering);
    if AlignRoot then
      RealignRoot;
  end;
end;

procedure TvgCustomScene.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FCaptured) then
    FCaptured := nil;
  if (Operation = opRemove) and (AComponent = FSelected) then
    FSelected := nil;
  if (Operation = opRemove) and (AComponent = FHovered) then
    FHovered := nil;
  if (Operation = opRemove) and (AComponent = FFocused) then
    FFocused := nil;
  if (Operation = opRemove) and (AComponent = FDesignPlaceObject) then
    FDesignPlaceObject := nil;
  if (Operation = opRemove) and (AComponent = FStyle) then
    Style := nil;
end;

procedure TvgCustomScene.SetFill(const Value: TvgBrush);
begin
  FFill := Value;
end;

procedure TvgCustomScene.FillChanged(Sender: TObject);
begin
  SetLength(FUpdateRects, 0);
  AddUpdateRect(vgRect(0, 0, Width, Height));
end;

procedure TvgCustomScene.SetSnapGridShow(const Value: boolean);
begin
  if FSnapGridShow <> Value then
  begin
    FSnapGridShow := Value;
    SetLength(FUpdateRects, 0);
    AddUpdateRect(vgRect(0, 0, Width, Height));
  end;
end;

procedure TvgCustomScene.SetSnapGridSize(const Value: single);
begin
  if FSnapGridSize <> Value then
  begin
    FSnapGridSize := Value;
    if FSnapGridSize < 0.01 then
      FSnapGridSize := 0.01;
    if FsnapGridShow then
      Repaint;
  end;
end;

procedure TvgCustomScene.InsertObject(const ClassName: string);
var
  P: TPoint;
  Obj: TvgObject;
  OldSel: TvgVisualObject;
  InsertPos: TvgPoint;
begin
  if GetClass(ClassName) <> nil then
  begin
    if GetClass(ClassName).InheritsFrom(TvgObject) then
    begin
      if FSelected <> nil then
      begin
        try
          Obj := TvgObjectClass(GetClass(ClassName)).Create(Owner);
          OldSel := FSelected;
          FSelected.AddObject(Obj);
          if vgDesigner <> nil then
            Obj.Name := vgDesigner.UniqueName(Owner, Obj.ClassName);
          if vgDesigner <> nil then
          begin
            vgDesigner.SelectObject(Owner, Obj, []);
            if Assigned(FDesignChangeSelection) then
              FDesignChangeSelection(Self);
            vgDesigner.Modified(Owner);
          end;
          if Obj.IsVisual then
          begin
            if GetPropInfo(Obj.ClassInfo, 'Text', [tkString, tkLString, tkWString]) <> nil then
              SetStrProp(Obj, 'Text', Copy(Obj.ClassName, 4, Length(Obj.ClassName)));
            if (Owner is TWinControl) and (FPopupPos.X > 0) then
            begin
              if Pos('Item', Obj.ClassName) = 0 then
              begin
                P := ScreenToClient(FPopupPos);
                InsertPos := OldSel.AbsoluteToLocal(vgPoint(P.X, P.Y));
                TvgVisualObject(Obj).Position.X := InsertPos.X;
                TvgVisualObject(Obj).Position.Y := InsertPos.Y;
              end;
            end;
            FPopupPos := Point(-1, -1);
            FSelected := TvgVisualObject(Obj);
            FSelected.DesignSelect;
            FSelected.DesignInsert;
          end;
        except
        end;
      end
      else
        if (FChildren = nil) or (FChildren.Count = 0) then
        begin
          { insert root object }
          try
            Obj := TvgObjectClass(GetClass(ClassName)).Create(Owner);
            if vgDesigner <> nil then
              Obj.Name := vgDesigner.UniqueName(Owner, Obj.ClassName);
            AddObject(Obj);
            if vgDesigner <> nil then
            begin
              vgDesigner.SelectObject(Owner, Obj, []);
              if Assigned(FDesignChangeSelection) then
                FDesignChangeSelection(Self);
              vgDesigner.Modified(Owner);
            end;
            if Obj.IsVisual then
            begin
              if (Owner is TWinControl) and (FPopupPos.X > 0) then
              begin
                P := TWinControl(Owner).ScreenToClient(FPopupPos);
                InsertPos := vgPoint(P.X, P.Y);
                TvgVisualObject(Obj).Position.X := P.X;
                TvgVisualObject(Obj).Position.Y := P.Y;
              end;
              FPopupPos := Point(-1, -1);
              FSelected := TvgVisualObject(Obj);
              FSelected.DesignSelect;
            end;
          except
          end;
        end;
    end
    else
    begin
      FInsertObject := ClassName;
    end;
  end
  else
    FInsertObject := '';
end;

procedure TvgCustomScene.Resize;
begin
  inherited;
end;

procedure TvgCustomScene.popupDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; Selected: Boolean);
var
  C: TvgCanvas;
  R: TvgRect;
  W, H: integer;
begin
  {$IFNDEF FPC}
  W := ARect.Right - ARect.Left;
  H := ARect.Bottom - ARect.Top;

  C := DefaultCanvasClass.Create(W, H);
  C.Clear($FF000000 or ColorToRGB(clMenu));

  if TMenuItem(Sender).Tag = $FF then
  begin
{    RName := 'PNG_Tvg' + StripHotkey(TMenuItem(Sender).Caption);
    if Windows.FindResource(HInstance, PChar(RName), RT_RCDATA) <> 0 then
    begin
      S := TResourceStream.Create(HInstance, RName, RT_RCDATA);
      B := TvgBitmap.CreateFromStream(S);
      S.Free;
    end
    else
    begin
      S := TResourceStream.Create(HInstance, 'PNG_Default', RT_RCDATA);
      B := TvgBitmap.CreateFromStream(S);
      S.Free;
    end;
    R := vgRect(2, 2, H - 2, H - 2);
    C.DrawBitmap(B, vgRect(0, 0, B.Width, B.Height), R, 1);
    B.Free;}
    R := vgRect(H + 2, 2, W - 2, H - 2);
    C.Fill.Color := '#FF000000';
    C.FillText(R, R, '  ' + StripHotkey(TMenuItem(Sender).Caption), false, 1, vgTextAlignNear);
  end
  else
  begin
    R := vgRect(12, 2, W - 2, H - 2);
    C.Fill.Color := '#FF000000';
    C.FillText(R, R, StripHotkey(TMenuItem(Sender).Caption), false, 1, vgTextAlignNear);
  end;

  R := vgRect(2, 2, W - 2, H - 2);

  if Selected then
  begin
    vgInflateRect(R, 1, 1);
    C.Fill.Color := '#50808080';
    C.FillRect(R, 5, 5, AllCorners, 1);
    C.Stroke.Color := '#FF404040';
    C.StrokeThickness := 1;
    C.Stroke.Style := vgBrushSolid;
    C.DrawRect(R, 5, 5, AllCorners, 1);
  end;

  C.FlushBuffer(ARect.Left, ARect.Top, ACanvas.Handle);

  C.Free;
  {$ENDIF}
end;

procedure TvgCustomScene.popupMeasureItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
begin
  {$IFNDEF FPC}
  Width := 136;
  if TMenuItem(Sender).Tag = $FF then
    Height := 36
  else
    Height := 21;
  {$ENDIF}
end;

procedure TvgCustomScene.doDesignPopupDesignHide(Sender: TObject);
begin
  if (FSelected <> nil) and (FSelected <> Root) then
  begin
    FSelected.FDesignHide := not FSelected.FDesignHide;
    if FSelected.FDesignHide and (FSelected.Parent.IsVisual) then
    begin
      FSelected := TvgVisualObject(FSelected.Parent); 
    end;
    AddUpdateRect(vgRect(0, 0, Width, Height));
  end;
end;

procedure TvgCustomScene.doDesignPopupDel(Sender: TObject);
var
  Obj: TvgVisualObject;
begin
  if (FSelected <> nil) and (FSelected <> Root) then
  begin
    Obj := FSelected;
    if (Obj.Parent <> nil) and (Obj.Parent.IsVisual) then
    begin
      FSelected := TvgVisualObject(Obj.Parent);
      FSelected.DesignSelect;
    end
    else
    begin
      FSelected := TvgVisualObject(Root);
      if FSelected <> nil then
        FSelected.DesignSelect;
    end;
    Obj.Free;
  end;
end;

procedure TvgCustomScene.doDesignPopupAddItem(Sender: TObject);
begin
  if FSelected <> nil then
  begin
    InsertObject(FSelected.ItemClass);
  end;
end;

{$IFDEF FPC}
function StripHotkey(S: string): string;
begin
  Result := S;
end;
{$ENDIF}

procedure TvgCustomScene.doDesignPopupAdd(Sender: TObject);
var
  S: string;
begin
  S := StripHotkey(TMenuItem(Sender).Caption);
  if (S <> '') then
    S := 'Tvg' + S;
  InsertObject(S);
end;

procedure TvgCustomScene.doDesignPopupReorder(Sender: TObject);
begin
  if FSelected = nil then Exit;

  if StripHotkey(TMenuItem(Sender).Caption) = 'Bring to front' then
    FSelected.BringToFront;
  if StripHotkey(TMenuItem(Sender).Caption) = 'Send to back' then
    FSelected.SendToBack;
end;

procedure TvgCustomScene.doDesignPopupGrid(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    1: FSnapGridShow := TMenuItem(Sender).Checked;
    2:
      begin
        FSnapToGrid := TMenuItem(Sender).Checked;
        FSnapToLines := not FSnapToGrid;
      end;
    3:
      begin
        FSnapToLines := TMenuItem(Sender).Checked;
        FSnapToGrid := not FSnapToLines;
      end;
    4:
      begin
        FSnapToLines := false;
        FSnapToGrid := false;
      end;
  end;
  SetLength(FUpdateRects, 0);
  AddUpdateRect(vgRect(0, 0, Width, Height));
end;

var
  Clipboard: AnsiString;
  
procedure TvgCustomScene.doDesignPopupCopy(Sender: TObject);
var
  S: TStringStream;
begin
  if FSelected <> nil then
  begin
    S := TStringStream.Create('');
    FSelected.SaveToStream(S);
    Clipboard := S.DataString;
    S.Free;
  end;
end;

procedure TvgCustomScene.doDesignPopupPaste(Sender: TObject);
var
  S: TStringStream;
  Result: TvgObject;
begin
  if Clipboard <> '' then
  begin
    S := TStringStream.Create(Clipboard);
    Result := CreateObjectFromStream(Owner, S);
    if FSelected <> nil then
      Result.Parent := FSelected
    else
      Result.Parent := Root;
    if vgDesigner <> nil then
      Result.Name := vgDesigner.UniqueName(Owner, Result.ClassName);
    S.Free;
  end;
end;

procedure TvgCustomScene.doDesignPopupLoadFromFile(Sender: TObject);
var
  S: TStream;
  Result: TvgObject;
  Open: TOpenDialog;
begin
  Open := TOpenDialog.Create(nil);
  Open.Filter := 'VGScene Files|*.vgscene;*.vg';
  if Open.Execute then
  begin
    S := TFileStream.Create(Open.FileName, fmOpenRead);
    Result := CreateObjectFromStream(Owner, S);
    if FSelected <> nil then
      Result.Parent := FSelected
    else
      Result.Parent := Root;
    if vgDesigner <> nil then
      Result.Name := vgDesigner.UniqueName(Owner, Result.ClassName);
    S.Free;
  end;
  Open.Free;
end;

procedure TvgCustomScene.OpenDesignPopup;
var
  i, j: integer;
  S: string;
  OItem, SItem, Item: TMenuItem;
  CatList: TStringList;
begin
  if not DesignPopupEnabled then Exit;
  if FDesignPopup <> nil then
  begin
    FDesignPopup.Free;
    FDesignPopup := nil;
  end;
  if FDesignPopup = nil then
  begin
    FDesignPopup := TPopupMenu.Create(nil);
    {$IFNDEF FPC}
//    FDesignPopup.OwnerDraw := true;
    {$ENDIF}
    if ObjectList <> nil then
    begin
      { item }
      if (FSelected <> nil) and (FSelected.ItemClass <> '') then
      begin
        S := FSelected.ItemClass;
        if Pos('Tvg', S) = 1 then
          Delete(S, 1, 3);
        Item := NewItem('Add ' + S, 0, false, true, doDesignPopupAddItem, 0, '');
        {$IFNDEF FPC}
        Item.OnMeasureItem := popupMeasureItem;
        Item.OnDrawItem := popupDrawItem;
        {$ENDIF}
        FDesignPopup.Items.Add(Item);
      end;
      { add }
      Item := NewItem('Add object', 0, false, true, nil, 0, '');
      {$IFNDEF FPC}
      Item.OnMeasureItem := popupMeasureItem;
      Item.OnDrawItem := popupDrawItem;
      {$ENDIF}
      { add categories }
      for i := 0 to ObjectList.Count - 1 do
      begin
        if Item.Find(ObjectList[i]) = nil then
        begin
          SItem := NewItem(ObjectList[i], 0, false, true, nil, 0, '');
          {$IFNDEF FPC}
          SItem.OnMeasureItem := popupMeasureItem;
          SItem.OnDrawItem := popupDrawItem;
          {$ENDIF}
          Item.Add(SItem);
        end;
      end;
      { add controls to list and sort }
      CatList := TStringList.Create;
      CatList.Sorted := true;
      for i := 0 to ObjectList.Count - 1 do
      begin
        S := TvgObjectClass(ObjectList.Objects[i]).ClassName;
        if Pos('Tvg', S) = 1 then
          Delete(S, 1, 3);
        CatList.Add(S);
      end;
      { add objects }
      for i := 0 to CatList.Count - 1 do
      begin
        OItem := NewItem(CatList[i], 0, false, true, doDesignPopupAdd, 0, '');
        {$IFNDEF FPC}
        OItem.OnMeasureItem := popupMeasureItem;
        OItem.OnDrawItem := popupDrawItem;
        {$ENDIF}
        OItem.Tag := $FF;
        for j := 0 to ObjectList.Count - 1 do
        begin
          S := TvgObjectClass(ObjectList.Objects[j]).ClassName;
          if Pos('Tvg', S) = 1 then
            Delete(S, 1, 3);
          if S = CatList[i] then
          begin
            SItem := Item.Find(ObjectList[j]);
            Break;
          end;
        end;
        if SItem <> nil then
          SItem.Add(OItem);
      end;
      CatList.Free;
      FDesignPopup.Items.Add(Item);
      { Delete }
      Item := NewItem('Delete object', 0, false, true, doDesignPopupDel, 0, '');
      {$IFNDEF FPC}
      Item.OnMeasureItem := popupMeasureItem;
      Item.OnDrawItem := popupDrawItem;
      {$ENDIF}
      FDesignPopup.Items.Add(Item);
      { Delete }
      Item := NewItem('Load From File...', 0, false, true, doDesignPopupLoadFromFile, 0, '');
      {$IFNDEF FPC}
      Item.OnMeasureItem := popupMeasureItem;
      Item.OnDrawItem := popupDrawItem;
      {$ENDIF}
      FDesignPopup.Items.Add(Item);
      { Design Hide }
      if FSelected <> nil then
      begin
        Item := NewItem('Hide in Design-time', 0, FSelected.FDesignHide, true, doDesignPopupDesignHide, 0, '');
        {$IFDEF KS_COMPILER6_UP}
        Item.AutoCheck := true;
        {$ENDIF}
        {$IFNDEF FPC}
        Item.OnMeasureItem := popupMeasureItem;
        Item.OnDrawItem := popupDrawItem;
        {$ENDIF}
        FDesignPopup.Items.Add(Item);
      end;
      { Reorder }
      Item := NewItem('Order', 0, false, true, nil, 0, '');
      {$IFNDEF FPC}
      Item.OnMeasureItem := popupMeasureItem;
      Item.OnDrawItem := popupDrawItem;
      {$ENDIF}
      SItem := NewItem('Bring to front', 0, false, true, doDesignPopupReorder, 0, '');
      {$IFNDEF FPC}
      SItem.OnMeasureItem := popupMeasureItem;
      SItem.OnDrawItem := popupDrawItem;
      {$ENDIF}
      Item.Add(SItem);
      SItem := NewItem('Send to back', 0, false, true, doDesignPopupReorder, 0, '');
      {$IFNDEF FPC}
      SItem.OnMeasureItem := popupMeasureItem;
      SItem.OnDrawItem := popupDrawItem;
      {$ENDIF}
      Item.Add(SItem);
      FDesignPopup.Items.Add(Item);
      { Edit }
      Item := NewItem('Edit', 0, false, true, nil, 0, '');
      {$IFNDEF FPC}
      Item.OnMeasureItem := popupMeasureItem;
      Item.OnDrawItem := popupDrawItem;
      {$ENDIF}
      SItem := NewItem('Copy to clipboard', 0, false, true, doDesignPopupCopy, 0, '');
      {$IFNDEF FPC}
      SItem.OnMeasureItem := popupMeasureItem;
      SItem.OnDrawItem := popupDrawItem;
      {$ENDIF}
      Item.Add(SItem);
      SItem := NewItem('Paste from clipboard', 0, false, true, doDesignPopupPaste, 0, '');
      {$IFNDEF FPC}
      SItem.OnMeasureItem := popupMeasureItem;
      SItem.OnDrawItem := popupDrawItem;
      {$ENDIF}
      Item.Add(SItem);
      FDesignPopup.Items.Add(Item);
      { Grid }
      Item := NewItem('Grid', 0, false, true, nil, 0, '');
      {$IFNDEF FPC}
      Item.OnMeasureItem := popupMeasureItem;
      Item.OnDrawItem := popupDrawItem;
      {$ENDIF}
      SItem := NewItem('Show snap grid', 0, FSnapGridShow, true, doDesignPopupGrid, 0, '');
      {$IFNDEF FPC}
      SItem.OnMeasureItem := popupMeasureItem;
      SItem.OnDrawItem := popupDrawItem;
      {$ENDIF}
      SItem.Tag := 1;
      {$IFDEF KS_DELPHI6_UP}
      SItem.AutoCheck := true;
      {$ENDIF}
      Item.Add(SItem);
      SItem := NewItem('Snap to grid', 0, FSnapToGrid, true, doDesignPopupGrid, 0, '');
      {$IFNDEF FPC}
      SItem.OnMeasureItem := popupMeasureItem;
      SItem.OnDrawItem := popupDrawItem;
      {$ENDIF}
      SItem.Tag := 2;
      SItem.RadioItem := true;
      {$IFDEF KS_DELPHI6_UP}
      SItem.AutoCheck := true;
      {$ENDIF}
      Item.Add(SItem);
      SItem := NewItem('Snap to lines', 0, FSnapToLines, true, doDesignPopupGrid, 0, '');
      {$IFNDEF FPC}
      SItem.OnMeasureItem := popupMeasureItem;
      SItem.OnDrawItem := popupDrawItem;
      {$ENDIF}
      SItem.Tag := 3;
      SItem.RadioItem := true;
      {$IFDEF KS_DELPHI6_UP}
      SItem.AutoCheck := true;
      {$ENDIF}
      Item.Add(SItem);
      SItem := NewItem('Snap disabled', 0, not FSnapToLines or not FSnapToLines, true, doDesignPopupGrid, 0, '');
      {$IFNDEF FPC}
      SItem.OnMeasureItem := popupMeasureItem;
      SItem.OnDrawItem := popupDrawItem;
      {$ENDIF}
      SItem.Tag := 4;
      SItem.RadioItem := true;
      {$IFDEF KS_DELPHI6_UP}
      SItem.AutoCheck := true;
      {$ENDIF}
      Item.Add(SItem);
      FDesignPopup.Items.Add(Item);
    end;
  end;
  GetCursorPos(FPopupPos);
  FDesignPopup.Popup(FPopupPos.X , FPopupPos.Y);
end;

procedure TvgCustomScene.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('DesignSnapGridShow', ReadDesignSnapGridShow, WriteDesignSnapGridShow, true);
  Filer.DefineProperty('DesignSnapToGrid', ReadDesignSnapToGrid, WriteDesignSnapToGrid, true);
  Filer.DefineProperty('DesignSnapToLines', ReadDesignSnapToLines, WriteDesignSnapToLines, true);
end;

procedure TvgCustomScene.ReadDesignSnapGridShow(Reader: TReader);
begin
  FSnapGridShow := Reader.ReadBoolean;
  if not (FDesignTime) then
    FSnapGridShow := false;
end;

procedure TvgCustomScene.ReadDesignSnapToGrid(Reader: TReader);
begin
  FSnapToGrid := Reader.ReadBoolean;
end;

procedure TvgCustomScene.ReadDesignSnapToLines(Reader: TReader);
begin
  FSnapToLines := Reader.ReadBoolean;
end;

procedure TvgCustomScene.WriteDesignSnapGridShow(Writer: TWriter);
begin
  Writer.WriteBoolean(FSnapGridShow);
end;

procedure TvgCustomScene.WriteDesignSnapToGrid(Writer: TWriter);
begin
  Writer.WriteBoolean(FSnapToGrid);
end;

procedure TvgCustomScene.WriteDesignSnapToLines(Writer: TWriter);
begin
  Writer.WriteBoolean(FSnapToLines);
end;

function TvgCustomScene.GetStyle: TvgResources;
begin
  Result := FStyle;
end;

function TvgCustomScene.GetRoot: TvgObject;
begin
  if (FChildren <> nil) and (FChildren.Count > 0) then
    Result := TvgObject(FChildren[0])
  else
    Result := nil;
end;

procedure TvgCustomScene.SetFocused(const Value: TvgVisualObject);
begin
  if FFocused <> Value then
  begin
    if FFocused <> nil then
      FFocused.KillFocus;
    FFocused := Value;
    if FFocused <> nil then
      FFocused.EnterFocus;
  end;
end;

procedure TvgCustomScene.DoDesignSelect(AObject: TObject);
begin
  if (AObject <> nil) and (AObject is TvgVisualObject) and (TvgVisualObject(AObject).FScene.GetComponent = Self) then
  begin
    FSelected := TvgVisualObject(AObject);
    FSelected.DesignSelect;
    AddUpdateRect(vgRect(0, 0, 1000, 1000));
    Draw;
  end;
end;

procedure TvgCustomScene.SetTransparency(const Value: boolean);
begin
  if FTransparency <> Value then
  begin
    FTransparency := Value;
    AddUpdateRect(vgRect(0, 0, 1000, 1000));
  end;
end;

procedure TvgCustomScene.BeginUpdate;
begin
  FDisableUpdate := true;
end;

procedure TvgCustomScene.EndUpdate;
begin
  FDisableUpdate := false;
  Invalidate;
end;

procedure TvgCustomScene.EndUpdateWOInvalidate;
begin
  FDisableUpdate := false;
end;

procedure TvgCustomScene.SetSelected(const Value: TvgVisualObject);
begin
  if FSelected <> Value then
  begin
    if FSelected <> nil then
      FSelected.Repaint;
    FSelected := Value;
    if FSelected <> nil then
      FSelected.Repaint;
    if Assigned(FDesignChangeSelection) then
      FDesignChangeSelection(Self);
    AddUpdateRect(vgRect(0, 0, Width, Height));
    Draw;
  end;
end;

function TvgCustomScene.GetDisableUpdate: boolean;
begin
  Result := FDisableUpdate;
end;

function TvgCustomScene.GetDesignTime: boolean;
begin
  Result := FDesignTime;
end;

function TvgCustomScene.GetCanvas: TvgCanvas;
begin
  Result := FCanvas;
end;

function TvgCustomScene.GetOwner: TComponent;
begin
  Result := Owner;
end;

function TvgCustomScene.GetComponent: TComponent;
begin
  Result := Self;
end;

function TvgCustomScene.GetSelected: TvgVisualObject;
begin
  Result := FSelected;
end;

function TvgCustomScene.GetDesignPlaceObject: TvgVisualObject;
begin
  Result := FDesignPlaceObject;
end;

procedure TvgCustomScene.SetDisableUpdate(Value: boolean);
begin
  FDisableUpdate := Value;
end;

function TvgCustomScene.GetUpdateRectsCount: integer;
begin
  Result := Length(FUpdateRects);
end;

function TvgCustomScene.GetUpdateRect(const Index: integer): TvgRect;
begin
  Result := FUpdateRects[Index];
end;

function TvgCustomScene.GetCaptured: TvgVisualObject;
begin
  Result := FCaptured;
end;

procedure TvgCustomScene.SetCaptured(const Value: TvgVisualObject);
begin
  FCaptured := Value;
  if Assigned(FCaptured) then
    if not Assigned(FOnFlush) then
      MouseCapture := true;
  if not Assigned(FCaptured) then
    if not Assigned(FOnFlush) then
      MouseCapture := false;
end;

function TvgCustomScene.GetTransparency: boolean;
begin
  Result := FTransparency;
end;

function TvgCustomScene.GetFocused: TvgVisualObject;
begin
  Result := FFocused;
end;

procedure TvgCustomScene.SetDesignRoot(const Value: TvgVisualObject);
begin
  FDesignRoot := Value;
end;

function TvgCustomScene.GetMousePos: TvgPoint;
begin
  Result := FMousePos;
end;

procedure TvgCustomScene.UpdateResource;
begin
  if Root <> nil then
    Root.UpdateResource;
end;

procedure TvgCustomScene.SetStyle(const Value: TvgResources);
begin
  if FStyle <> Value then
  begin
    if FStyle <> nil then
      FStyle.RemoveSceneUpdater(Self);
    FStyle := Value;
    if FStyle <> nil then
      FStyle.AddSceneUpdater(Self);

    UpdateResource;
  end;
end;

procedure TvgCustomScene.SetTextRendering(const Value: TvgTextRendering);
begin
  if FTextRendering <> Value then
  begin
    FTextRendering := Value;
    if FCanvas <> nil then
      FCanvas.SetTextRendering(FTextRendering);
    Invalidate;
  end;
end;

{ TvgResources ================================================================}

constructor TvgResources.Create(AOwner: TComponent);
begin
  inherited;
  FResource := TStringList.Create;
  TStringList(FResource).OnChange := DoResourceChanged;
  FSceneList := TList.Create;
end;

destructor TvgResources.Destroy;
begin
  if FRoot <> nil then
    FreeAndNil(FRoot);
  FreeAndNil(FSceneList);
  FResource.Free;
  inherited;
end;

procedure TvgResources.Loaded;
begin
  inherited ;
end;

procedure TvgResources.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('ResourcesBin', ReadResources, WriteResources, FRoot <> nil);
end;

procedure TvgResources.ReadResources(Stream: TStream);
begin
  if (FRoot <> nil) then
  begin
    FreeAndNil(FRoot);
  end;

  FRoot := CreateObjectFromBinStream(nil, Stream);
  if FRoot <> nil then
  begin
    if FRoot.IsVisual then
      FRoot.Visual.UpdateDesignHide(false);
    UpdateScenes;
  end;
end;

procedure TvgResources.WriteResources(Stream: TStream);
begin
  if FRoot <> nil then
    FRoot.SaveToBinStream(Stream);
end;

procedure TvgResources.FillStrings;
var
  M: TMemoryStream;
begin
  if FRoot <> nil then
  begin
    M := TMemoryStream.Create;
    FRoot.SaveToStream(M);
    M.Position := 0;
    TStringList(FResource).LoadFromStream(M);
    M.Free;
  end;
end;

procedure TvgResources.UpdateScenes;
var
  i: integer;
begin
  for i := 0 to FSceneList.Count - 1 do
    IvgScene(FSceneList[i]).UpdateResource;
end;

procedure TvgResources.DoResourceChanged(Sender: TObject);
var
  S: TStream;
begin
  if (FRoot <> nil) then
  begin
    FreeAndNil(FRoot);
  end;

  S := TMemoryStream.Create;
  try
    TStringList(FResource).SaveToStream(S);
    if S.Position > 0 then
    begin
      S.Position := 0;
      FRoot := CreateObjectFromStream(nil, S);
      if FRoot.IsVisual then
        FRoot.Visual.UpdateDesignHide(false);
    end;
  finally
    S.Free;
  end;
  UpdateScenes;
end;

procedure TvgResources.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

procedure TvgResources.SetResource(const Value: TStrings);
begin
  FResource.Assign(Value);
end;

procedure TvgResources.AddSceneUpdater(const Scene: IvgScene);
begin
  FSceneList.Add(Pointer(Scene));
end;

procedure TvgResources.RemoveSceneUpdater(const Scene: IvgScene);
begin
  if FSceneList <> nil then
    FSceneList.Remove(Pointer(Scene));
end;

initialization
  {$IFDEF DARWIN}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  {$ENDIF}
  {$IFDEF WIN32}
  User32Lib := LoadLibrary(User32);
  if User32Lib <> 0 then
  begin
    @SetLayeredWindowAttributes := GetProcAddress(User32Lib, 'SetLayeredWindowAttributes');
    @UpdateLayeredWindow := GetProcAddress(User32Lib, 'UpdateLayeredWindow');
    @PrintWindow := GetProcAddress(User32Lib, 'PrintWindow');
  end;
  {$ENDIF}
  RegisterClasses([TvgBitmap, TvgPathData, TvgBrush, TvgBounds, TvgPosition, TvgGradient, TvgGradientPoints, TvgGradientPoint, TvgVisual]);
  RegisterClasses([TvgCustomScene, TvgResources, TvgObject, TvgContent]);
  RegisterVGObjects('Layout', [TvgFrame, TvgControl]);
  RegisterVGObjects('Resources', [TvgBrushObject]);
  {$IFDEF VgSceneTrial}
  ShowVersion2;
  {$ENDIF}
  {$IFDEF WIN32}
  OleInitialize(nil);
  {$ENDIF}
finalization
  {$IFDEF WIN32}
  if User32Lib <> 0 then
    FreeLibrary(User32Lib);
  {$ENDIF}
  if DefaultStyles <> nil then
    FreeAndNil(DefaultStyles);
  if ObjectList <> nil then
    FreeAndNil(ObjectList);
  if ResourceList <> nil then
    FreeAndNil(ResourceList);
end.
