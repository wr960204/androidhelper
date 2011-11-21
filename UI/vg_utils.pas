unit vg_utils;

interface

{$I vg_define.inc}

uses
  {$IFDEF FPC}
  LCLType, LCLProc, LCLIntf,
  {$ENDIF}
  {$IFDEF WIN32}
  Windows,
  {$ENDIF}
  SysUtils, Classes, vg_scene;

const

  CSlash = '\';

function ExePath: string;
function ExeName: string;
function ExeFileName: string;
function ExeNameWithOutExt: string;

function FileNameWithOutExt(const S: string): string;

function IsFileReadOnly(AFileName: string): boolean;

function strToNumber(const S: wideString): wideString;

function GetToken(var S: Ansistring; Separators: Ansistring; Stop: Ansistring = ''): Ansistring;
function vgGetToken(var Pos: integer; const S: AnsiString; Separators: AnsiString; Stop: AnsiString = ''): AnsiString;

function RandomColor: cardinal;

//function FromRGB(Color: longword): longword;
procedure MoveLongword(const Src: Pointer; Dst: Pointer; Count: Integer);
procedure FillLongword(Src: Pointer; Count: Integer; Value: Longword);
procedure FillLongwordRect(Src: Pointer; W, H, X1, Y1, X2, Y2: Integer; Value: Longword);
procedure FillAlpha(Src: Pointer; Count: Integer; Alpha: byte);

function GetXmlStr(S, AParam: string): string;
function GetAttrStr(S, AParam: string): string;
procedure SetAttrStr(var S: string; AParam, AValue: string);

function MaxFloat(A1, A2: single): single;
function MinFloat(A1, A2: single): single;

procedure ReverseBytes(p: Pointer; Count: integer);

{$IFDEF WIN32}
function IsWin2kUp: boolean;
{$ENDIF}

function vgInterpolateLinear(T, B, C, D: Double): Double;
function vgInterpolateSine(T, B, C, D: Double; aType: TvgAnimationType): Double;
function vgInterpolateQuint(T, B, C, D: Double; aType: TvgAnimationType): Double;
function vgInterpolateQuart(T, B, C, D: Double; aType: TvgAnimationType): Double;
function vgInterpolateQuad(T, B, C, D: Double; aType: TvgAnimationType): Double;
function vgInterpolateExpo(T, B, C, D: Double; aType: TvgAnimationType): Double;
function vgInterpolateElastic(T, B, C, D, A, P: Double; aType: TvgAnimationType): Double;
function vgInterpolateCubic(T, B, C, D: Double; aType: TvgAnimationType): Double;
function vgInterpolateCirc(T, B, C, D: Double; aType: TvgAnimationType): Double;
function vgInterpolateBounce(T, B, C, D: Double; aType: TvgAnimationType): Double;
function vgInterpolateBack(T, B, C, D, S: Double; aType: TvgAnimationType): Double;

type

  TvgSplineVector = array [0..3] of single;
  TvgSplineMatrix = array of TvgSplineVector;

  TvgSpline = class(TObject)
  private
    matX, matY: TvgSplineMatrix;
    len: integer;
  public
    constructor Create(const Polygon: TvgPolygon);
    destructor Destroy; override;
    procedure SplineXY(const t: single; var X, Y: Single);
  end;

implementation {===============================================================}

uses Math;

{$IFDEF WIN32}
function IsWin2kUp: boolean;
var
  VI: TOSVersionInfo;
begin
  VI.dwOSVersionInfoSize := SizeOf(VI);
  GetVersionEx(VI);
  Result := (VI.dwPlatformId = VER_PLATFORM_WIN32_NT) and (VI.dwMajorVersion > 4);
end;
{$ENDIF}

procedure ReverseBytes(p: Pointer; Count: integer);
var
  p1: PChar;
  p2: PChar;
  c: Char;
begin
  p1:=PChar(p);
  p2:=PChar(p)+Count-1;
  while p1<p2 do begin
    c:=p1^;
    p1^:=p2^;
    p2^:=c;
    System.inc(p1);
    System.dec(p2);
  end;
end;

{ colors }

function MaxFloat(A1, A2: single): single;
begin
  if A1 > A2 then
    Result := A1
  else
    Result := A2;
end;

function MinFloat(A1, A2: single): single;
begin
  if A1 < A2 then
    Result := A1
  else
    Result := A2;
end;

function RandomColor: cardinal;
begin
  Result := $FF000000 or system.random($FFFFFF)
end;

{ ============================================================================= }

function GetXmlStr(S, AParam: string): string;
var
  FirstPos, EndPos: integer;
begin
  FirstPos := Pos('<' + AParam + '>', S);
  EndPos := Pos('</' + AParam + '>', S);
  if (FirstPos > 0) and (EndPos > 0) then
  begin
    Result := Copy(S, FirstPos + Length(AParam) + 2, EndPos - FirstPos - Length(AParam) - 2);
  end
  else
    Result := ''
end;

function GetAttrStr(S, AParam: string): string;
var
  FirstPos, EndPos: integer;
  EndS: string;
begin
  Result := '';
  FirstPos := Pos(' ' + LowerCase(AParam) + '="', LowerCase(S));
  if FirstPos > 0 then
  begin
    EndS := Copy(S, FirstPos + Length(AParam) + 3, Length(S));
    EndPos := Pos('"', EndS);
    if (FirstPos > 0) and (EndPos > 0) then
    begin
      Result := Copy(EndS, 1, EndPos - 1);
    end;
  end;
end;

procedure SetAttrStr(var S: string; AParam, AValue: string);
var
  FirstPos, EndPos: integer;
  EndS: string;
begin
  FirstPos := Pos(' ' + AParam + '="', S);
  if FirstPos > 0 then
  begin
    EndS := Copy(S, FirstPos + Length(AParam) + 3, Length(S));
    EndPos := Pos('"', EndS);
    if (FirstPos > 0) and (EndPos > 0) then
    begin
      Delete(EndS, 1, EndPos - 1);
      S := Copy(S, 1, FirstPos) + AParam + '="' + AValue + EndS;
    end;
  end;
end;

function strToNumber(const S: wideString): wideString;
var
  i: integer;
begin
  for i := 1 to Length(S) do
    Result := Result + intToHex(Word(S[i]), 4);
end;

{$ifndef FPC_BIG_ENDIAN}

function FromRGB(Color: longword): longword;
asm
  BSWAP   EAX
  MOV     AL, $FF
  ROR     EAX,8
end;

procedure MoveLongword(const Src: Pointer; Dst: Pointer; Count: Integer);
asm
  PUSH    ESI
  PUSH    EDI

  MOV     ESI,EAX
  MOV     EDI,EDX
  MOV     EAX,ECX
  CMP     EDI,ESI
  JE      @exit

  REP     MOVSD
@exit:
  POP     EDI
  POP     ESI
end;

procedure FillLongword(Src: Pointer; Count: Integer; Value: Longword);
asm
  PUSH    EDI

  MOV     EDI,EAX  // Point EDI to destination
  MOV     EAX,ECX
  MOV     ECX,EDX
  TEST    ECX,ECX
  JS      @exit

  REP     STOSD    // Fill count dwords
@exit:
  POP     EDI
end;

procedure FillAlpha(Src: Pointer; Count: Integer; Alpha: byte);
asm
  PUSH    EDI

  MOV     EDI,EAX  // Point EDI to destination
  MOV     AL, CL
  MOV     ECX,EDX
  TEST    ECX,ECX
  JS      @exit

@1:
  INC     EDI
  INC     EDI
  INC     EDI
  MOV     [EDI], AL
  INC     EDI
  LOOP    @1

@exit:
  POP     EDI
end;

procedure FillLongwordRect(Src: Pointer; W, H, X1, Y1, X2, Y2: Integer; Value: Longword);
var
  j: integer;
begin
  if x2 > W then x2 := W;
  if y2 > H then y2 := H;
  if x1 > x2 then x1 := x1;
  if y1 > y2 then y1 := y2;
  for j := y1 to y2 - 1 do
    FillLongword(@PvgColorArray(Src)[X1 + (j * W)], X2 - X1, Value);
end;


{$else}

procedure MoveLongword(const Src: Pointer; Dst: Pointer; Count: Integer);
begin
  System.Move(Src^, Dst^, Count * 4);
end;

procedure FillLongword(Src: Pointer; Count: Integer; Value: Longword);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    PvgColorArray(Src)[i] := Value;
end;

procedure FillAlpha(Src: Pointer; Count: Integer; Alpha: byte);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    PvgColorRecArray(Src)[i].A := Alpha;
end;

procedure FillLongwordRect(Src: Pointer; W, H, X1, Y1, X2, Y2: Integer; Value: Longword);
var
  i, j: integer;
begin
  for i := x1 to x2 - 1 do
    for j := y1 to y2 - 1 do
      PvgColorArray(Src)[i + (j * W)] := Value;
end;
{$endif}

function ExePath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function ExeName: string;
begin
  Result := ParamStr(0);
end;

function ExeFileName: string;
var
  I: Integer;
begin
  Result := ParamStr(0);
  I := LastDelimiter('/\', Result);
  Result := Copy(Result, I + 1, 1000);
end;

function ExeNameWithOutExt: string;
var
  I: Integer;
begin
  Result := ParamStr(0);
  if Pos('.', Result) > 0 then
  begin
    I := LastDelimiter('.', Result);
    Result := Copy(Result, 1, I - 1);
  end;
end;

function FileNameWithOutExt(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  if Pos('.', Result) > 0 then
  begin
    I := LastDelimiter('.', Result);
    Result := Copy(Result, 1, I - 1);
  end;
end;

function IsFileReadOnly(AFileName: string): boolean;
var
  A: integer;
begin
  if not FileExists(AFileName) then
  begin
    A := FileGetAttr(ExeName);
    Result := A and faReadOnly <> 0;
    Exit;
  end;

  A := FileGetAttr(AFileName);
  Result := A and faReadOnly <> 0;
end;

function GetToken(var S: Ansistring; Separators: Ansistring; Stop: Ansistring = ''): Ansistring;
var
  i, Len: integer;
  CopyS: Ansistring;
begin
  Result := '';
  CopyS := S;
  Len := Length(CopyS);
  for i := 1 to Len do
  begin
    if Pos(CopyS[i], Stop) > 0 then
      Break;
    Delete(S, 1, 1);
    if Pos(CopyS[i], Separators) > 0 then
    begin
      Result := Result;
      Break;
    end;
    Result := Result + CopyS[i];
  end;
  Result := Trim(Result);
  S := Trim(S);
end;

function vgGetToken(var Pos: integer; const S: AnsiString; Separators: AnsiString; Stop: AnsiString = ''): AnsiString;
var
  i, Len: integer;
begin
  Result := '';
  Len := Length(S);
  for i := Pos to Len do
  begin
    if System.Pos(S[i], Stop) > 0 then
      Break;
    if System.Pos(S[i], Separators) > 0 then
      Break;
    Result := Result + S[i];
  end;
  { skip separators }
  Pos := i;
  for i := Pos to Len do
    if System.Pos(S[i], Separators) <= 0 then
      Break;
  Pos := i;
end;

{ interpolations }

function vgInterpolateBack(T, B, C, D, S: Double; aType: TvgAnimationType): Double;
begin
  case aType of
    vgAnimationIn: begin
      if S = 0 then S:= 1.70158;
      T:= T / D;
      Result:= C * T * T * ((S + 1) * T - S) + B;
    end;
    vgAnimationOut: begin
      if S = 0 then S:= 1.70158;
      T:= T / D - 1;
      Result:= C * (T * T * ((S + 1) * T + S) + 1) + B;
    end;
    vgAnimationInOut: begin
      if S = 0 then S:= 1.70158;
      T:= T / (D / 2);
      if T < 1 then
      begin
        S:= S * 1.525;
        Result:= C / 2 * (T * T * ((S + 1) * T - S)) + B;
      end
      else
      begin
        T:= T - 2;
        S:= S * 1.525;
        Result:= C / 2 * (T * T * ((S + 1) * T + S) + 2) + B;
      end;
    end;
  end;
end;

function vgInterpolateBounce(T, B, C, D: Double; aType: TvgAnimationType): Double;
  function _EaseOut(T, B, C, D: Double): Double;
  begin
    T:= T / D;
    if T < 1 / 2.75 then
    begin
      Result:= C * (7.5625 * T * T) + B;
    end
    else if T < 2 / 2.72 then
    begin
      T:= T - (1.5 / 2.75);
      Result:= C * (7.5625 * T * T + 0.75) + B;
    end
    else if T < 2.5 / 2.75 then
    begin
      T:= T - (2.25 / 2.75);
      Result:= C * (7.5625 * T * T + 0.9375) + B;
    end
    else
    begin
      T:= T - (2.625 / 2.75);
      Result:= C * (7.5625 * T * T + 0.984375) + B;
    end;
  end;
  function _EaseIn(T, B, C, D: Double): Double;
  begin
    Result:= C - _EaseOut(D - T, 0, C, D) + B;
  end;
begin
  case aType of
    vgAnimationIn: begin
      Result:= _EaseIn(T, B, C, D);
    end;
    vgAnimationOut: begin
      Result:= _EaseOut(T, B, C, D);
    end;
    vgAnimationInOut: begin
      if T < D / 2 then
        Result:= _EaseIn(T * 2, 0, C, D) * 0.5 + B
      else
        Result:= _EaseOut(T * 2 - D, 0, C, D) * 0.5 + C * 0.5 + B;
    end;
  end;
end;

function vgInterpolateCirc(T, B, C, D: Double; aType: TvgAnimationType): Double;
begin
  case aType of
    vgAnimationIn: begin
      T:= T / D;
      Result:= -C * (Sqrt(1 - T * T) - 1) + B;
    end;
    vgAnimationOut: begin
      T:= T / D - 1;
      Result:= C * Sqrt(1 - T * T) + B;
    end;
    vgAnimationInOut: begin
      T:= T / (D / 2);
      if T < 1 then
        Result:= -C / 2 * (Sqrt(1 - T * T) - 1) + B
      else
      begin
        T:= T - 2;
        Result:= C / 2 * (Sqrt(1 - T * T) + 1) + B;
      end;
    end;
  end;
end;

function vgInterpolateCubic(T, B, C, D: Double; aType: TvgAnimationType): Double;
begin
  case aType of
    vgAnimationIn: begin
      T:= T / D;
      Result:= C * T * T * T + B;
    end;
    vgAnimationOut: begin
      T:= T / D - 1;
      Result:= C * (T * T * T + 1) + B;
    end;
    vgAnimationInOut: begin
      T:= T / (D / 2);
      if T < 1 then
        Result:= C / 2 * T * T * T + B
      else
      begin
        T:= T - 2;
        Result:= C / 2 * (T * T * T + 2) + B;
      end;
    end;
  end;
end;

function vgInterpolateElastic(T, B, C, D, A, P: Double; aType: TvgAnimationType): Double;
var
  S: Double;
begin
  case aType of
    vgAnimationIn: begin
      if T = 0 then
      begin
        Result:= B;
        Exit;
      end;
      T:= T / D;
      if T = 1 then
      begin
        Result:= B + C;
        Exit;
      end;
      if P = 0 then P:= D * 0.3;
      if (A = 0) or (A < Abs(C)) then
      begin
        A:= C;
        S:= P / 4;
      end
      else
      begin
        S:= P / (2 * Pi) * ArcSin(C / A);
      end;
      T:= T - 1;
      Result:= -(A * Power(2, 10 * T) * Sin((T * D - S) * (2 * Pi)/P)) + B;
    end;
    vgAnimationOut: begin
      if T = 0 then
      begin
        Result:= B;
        Exit;
      end;
      T:= T / D;
      if T = 1 then
      begin
        Result:= B + C;
        Exit;
      end;
      if P = 0 then P:= D * 0.3;
      if (A = 0) or (A < Abs(C)) then
      begin
        A:= C;
        S:= P / 4;
      end
      else
      begin
        S:= P / (2 * Pi) * ArcSin(C / A);
      end;
      Result:= A * Power(2, -10 * T) * Sin((T * D - S) * (2 * Pi) / P) + C + B;
    end;
    vgAnimationInOut: begin
      if T = 0 then
      begin
        Result:= B;
        Exit;
      end;
      T:= T / (D / 2);
      if T = 2 then
      begin
        Result:= B + C;
        Exit;
      end;
      if P = 0 then P:= D * (0.3 * 1.5);
      if (A = 0) or (A < Abs(C)) then
      begin
        A:= C;
        S:= P / 4;
      end
      else
      begin
        S:= P / (2 * Pi) * ArcSin(C / A);
      end;

      if T < 1 then
      begin
        T:= T - 1;
        Result:= -0.5 * (A * Power(2, 10 * T) * Sin((T * D -S) * (2 * Pi) / P)) + B;
      end
      else
      begin
        T:= T - 1;
        Result:= A * Power(2, -10 * T) * Sin((T * D - S) * (2 * Pi) / P) * 0.5 + C + B;
      end;
    end;
  end;
end;

function vgInterpolateExpo(T, B, C, D: Double; aType: TvgAnimationType): Double;
begin
  case aType of
    vgAnimationIn: begin
      If T = 0 Then
        Result := B
      else
        Result := C * Power(2, 10 * (T / D - 1)) + B;
    end;
    vgAnimationOut: begin
      If T = D then
        Result:=  B + C
      else
        Result:=  C * (-Power(2, -10 * T / D) + 1) + B;
    end;
    vgAnimationInOut: begin
      if T = 0 then
      begin
        Result:= B;
        Exit;
      end;
      if T = D then
      begin
        Result:= B + C;
        Exit;
      end;
      T:= T / (D / 2);
      if T < 1 then
        Result:= C / 2 * Power(2, 10 * (T - 1)) + B
      else
      begin
        T:= T - 1;
        Result:= C / 2 * (-Power(2, -10 * T) + 2) + B;
      end;
    end;
  end;
end;

function vgInterpolateLinear(T, B, C, D: Double): Double;
begin
  Result:= C * T / D + B;
end;

function vgInterpolateQuad(T, B, C, D: Double; aType: TvgAnimationType): Double;
begin
  case aType of
    vgAnimationIn: begin
      T:= T / D;
      Result:= C * T * T + B;
    end;
    vgAnimationOut: begin
      T:= T / D;
      Result:= -C * T * (T - 2) + B;
    end;
    vgAnimationInOut: begin
      T:= T / (D / 2);

      if T < 1 then
        Result:= C / 2 * T * T + B
      else
      begin
        T:= T - 1;
        Result:= -C / 2 * (T * (T - 2) - 1) + B;
      end;
    end;
  end;
end;

function vgInterpolateQuart(T, B, C, D: Double; aType: TvgAnimationType): Double;
begin
  case aType of
    vgAnimationIn: begin
      T:= T / D;
      Result:= C * T * T * T * T + B;
    end;
    vgAnimationOut: begin
      T:= T / D - 1;
      Result:= -C * (T * T * T * T - 1) + B;
    end;
    vgAnimationInOut: begin
      T:= T / (D / 2);
      if T < 1 then
        Result:= C / 2 * T * T * T * T +B
      else
      begin
        T:= T - 2;
        Result:= -C / 2 * (T * T * T * T - 2) + B;
      end;
    end;
  end;
end;

function vgInterpolateQuint(T, B, C, D: Double; aType: TvgAnimationType): Double;
begin
  case aType of
    vgAnimationIn: begin
      T:= T / D;
      Result:= C * T * T * T * T * T + B;
    end;
    vgAnimationOut: begin
      T:= T / D - 1;
      Result:= C * (T * T * T * T * T + 1) + B;
    end;
    vgAnimationInOut: begin
      T:= T / (D / 2);
      if T < 1 then
        Result:= C / 2 * T * T * T * T * T + B
      else
      begin
        T:= T - 2;
        Result:= C / 2 * (T * T * T * T * T + 2) + B;
      end;
    end;
  end;
end;

function vgInterpolateSine(T, B, C, D: Double; aType: TvgAnimationType): Double;
begin
  case aType of
    vgAnimationIn: begin
      Result:= -C * Cos(T / D * (Pi/2)) + C + B;
    end;
    vgAnimationOut: begin
      Result:= C * Sin(T / D * (Pi/2)) + B;
    end;
    vgAnimationInOut: begin
      Result:= -C / 2 * (Cos(Pi * T / D) - 1) + B;
    end;
  end;
end;


{ TvgSpline ===================================================================}

procedure VECCholeskyTriDiagResol(const b : array of Single; const nb : Integer;
  var Result : array of Single);
var
   Y, LDiag, LssDiag : array of Single;
   i, k, Debut, Fin: Integer;
begin
   Debut:=0;
   Fin:=nb-1;
   Assert(Length(b)>0);
   SetLength(LDiag, nb);
   SetLength(LssDiag, nb-1);
   LDiag[Debut]:=1.4142135; // = sqrt(2)
   LssDiag[Debut]:=1.0/1.4142135;
   for K:=Debut+1 to Fin-1 do begin
      LDiag[K]:=Sqrt(4-LssDiag[K-1]*LssDiag[K-1]);
      LssDiag[K]:=1.0/LDiag[K];
   end;
   LDiag[Fin]:=Sqrt(2-LssDiag[Fin-1]*LssDiag[Fin-1]);
   SetLength(Y, nb);
   Y[Debut]:=B[Debut]/LDiag[Debut];
   for I:=Debut+1 to Fin do
      Y[I]:=(B[I]-Y[I-1]*LssDiag[I-1])/LDiag[I];
   Assert(Length(Result)=nb);
   Result[Fin]:=Y[Fin]/LDiag[Fin];
   for i:=Fin-1 downto Debut do
      Result[I]:=(Y[I]-Result[I+1]*LssDiag[I])/LDiag[I];
end;

procedure MATInterpolationHermite(const ordonnees : array of single;
  var Result: TvgSplineMatrix);
var
   a, b, c, d : Single;
   i, n : Integer;
   bb, deriv : array of Single;
begin
   if (Length(ordonnees) > 0) then
   begin
      n := Length(ordonnees)-1;
      SetLength(bb, Length(ordonnees));
      bb[0]:=3*(ordonnees[1]-ordonnees[0]);
      bb[n]:=3*(ordonnees[n]-ordonnees[n-1]);
      for i:=1 to n-1 do
         bb[I]:=3*(ordonnees[I+1]-ordonnees[I-1]);
      SetLength(deriv, Length(ordonnees));
      VECCholeskyTriDiagResol(bb, Length(ordonnees), deriv);
      SetLength(Result, n);
      for i:=0 to n-1 do begin
         a:=ordonnees[I];
         b:=deriv[I];
         c:=3*(ordonnees[I+1]-ordonnees[I])-2*deriv[I]-deriv[I+1];
         d:=-2*(ordonnees[I+1]-ordonnees[I])+deriv[I]+deriv[I+1];
         Result[I][3]:=a+I*(I*(c-I*d)-b);
         Result[I][2]:=b+I*(3*I*d-2*c);
         Result[I][1]:=c-3*I*d;
         Result[I][0]:=d;
      end;
   end;
end;

function MATValeurSpline(const spline : TvgSplineMatrix; const x : Single;
                         const nb : Integer) : Single;
var
   i : Integer;
begin
   if Length(Spline)>0 then begin
      if x<=0 then
         i:=0
      else if x>nb-1 then
         i:=nb-1
      else i:=Integer(Trunc(x));
      { TODO : the following line looks like a bug... }
      if i=(nb-1) then Dec(i);
      Result:=((spline[i][0]*x+spline[i][1])*x+spline[i][2])*x+spline[i][3];
   end else Result:=0;
end;

constructor TvgSpline.Create(const Polygon: TvgPolygon);
var
  i: integer;
  X, Y: array of single;
begin
  inherited Create;
  len := Length(Polygon);
  SetLength(X, len);
  SetLength(Y, len);
  for i := 0 to len - 1 do
  begin
    X[i] := Polygon[i].X;
    Y[i] := Polygon[i].Y;
  end;
  MATInterpolationHermite(X, matX);
  MATInterpolationHermite(Y, matY);
end;

destructor TvgSpline.Destroy;
begin
  inherited;
end;

procedure TvgSpline.SplineXY(const t: single; var X, Y : Single);
begin
  X := MATValeurSpline(MatX, t, len);
  Y := MATValeurSpline(MatY, t, len);
end;

end.
