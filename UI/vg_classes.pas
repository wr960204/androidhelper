unit vg_classes;

{$I vg_define.inc}

interface

uses Classes, SysUtils, StringConsts;

const

  XorConst: word = $5555;

type

  { TStringHash - used internally by TMemIniFile to optimize searches. }

  PPHashItem = ^PHashItem;
  PHashItem = ^THashItem;
  THashItem = record
    Next: PHashItem;
    Key: WideString;
    Value: Integer;
  end;

  TvgStringHash = class
  private
    Buckets: array of PHashItem;
  protected
    function Find(const Key: WideString): PPHashItem;
    function HashOf(const Key: Widestring): Cardinal; virtual;
  public
    constructor Create(Size: Cardinal = 256);
    destructor Destroy; override;
    procedure Add(const Key: Widestring; Value: Integer);
    procedure Clear;
    procedure Remove(const Key: Widestring);
    function Modify(const Key: Widestring; Value: Integer): Boolean;
    function ValueOf(const Key: Widestring): Integer;
  end;

const

  WideNull = System.WideChar(CH_ZERO);
  Tabulator = System.WideChar(CHAR_9);
  Space = System.WideChar(CHAR_32);
  CarriageReturn = System.WideChar($D);
  LineFeed = System.WideChar($A);
  VerticalTab = System.WideChar($B);
  FormFeed = System.WideChar($C);
  LineSeparator = System.WideChar($2028);
  ParagraphSeparator = System.WideChar($2029);

  BOM_LSB_FIRST = System.WideChar($FEFF); // this is how the BOM appears on x86 systems when written by a x86 system
  BOM_MSB_FIRST = System.WideChar($FFFE);

type

  TvgWideStrings = class(TPersistent)
  private
    FUpdateCount: Integer;
    FCaseSensitive: Boolean;
    FSorted: Boolean;
    function GetCommaText: WideString;
    function GetName(Index: Integer): WideString;
    function GetValue(const Name: WideString): WideString;
    procedure ReadData(Reader: TReader);
    procedure SetCommaText(const Value: WideString);
    procedure SetValue(const Name, Value: WideString);
    procedure WriteData(Writer: TWriter);
    function GetValueFromIndex(Index: Integer): WideString;
    procedure SetValueFromIndex(Index: Integer; const Value: WideString);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetSorted(const Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Error(const Msg: String; Data: Integer);
    function CompareStrings(const S1, S2: WideString): Integer; virtual;
    function Get(Index: Integer): WideString; virtual; abstract;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual; abstract;
    function GetObject(Index: Integer): TObject; virtual;
    function GetTextStr: WideString; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetTextStr(const Value: WideString); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure AddStrings(Strings: TStrings); overload; virtual;
    procedure AddStrings(Strings: TvgWideStrings); overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function Equals(Strings: TvgWideStrings): Boolean;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function IndexOf(const S: WideString): Integer; virtual;
    function IndexOfName(const Name: WideString): Integer; virtual;
    function IndexOfObject(AObject: TObject): Integer;
    procedure Insert(Index: Integer; const S: WideString); virtual; abstract;
    procedure InsertObject(Index: Integer; const S: WideString; AObject: TObject);
    procedure LoadFromFile(const FileName: String); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToFile(const FileName: String); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure SetText(Text: PWideChar); virtual;
    procedure sort; virtual;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Names[Index: Integer]: WideString read GetName;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Values[const Name: WideString]: WideString read GetValue write SetValue;
    property ValueFromIndex[Index: Integer]: WideString read GetValueFromIndex write SetValueFromIndex;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property text: WideString read GetTextStr write SetTextStr stored true;
    function items(AIndex: integer): WideString;
    procedure setItem(index: integer; text: wideString);
    function add(const S: WideString): Integer; virtual;
    procedure Delete(Index: Integer); virtual; 
    procedure clear; virtual;
    property count: Integer read GetCount;
    property commaText: WideString read GetCommaText write SetCommaText stored false;
    property caseSensitive: Boolean read FCaseSensitive write SetCaseSensitive stored false;
    property sorted: Boolean read FSorted write SetSorted stored false;
  published
  end;

  // TvgWideStringList class
  PWideStringItem = ^TWideStringItem;
  TWideStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  PWideStringItemList = ^TWideStringItemList;
  TWideStringItemList = array[0..MaxListSize] of TWideStringItem;

  TvgWideStringList = class(TvgWideStrings)
  private
    FList: PWideStringItemList;
    FCount: Integer;
    FCapacity: integer;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    FsortByObject: boolean;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer);
    procedure QuickSortByObject(L, R: Integer);
    procedure InsertItem(Index: Integer; const S: WideString);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function CompareStrings(const S1, S2: WideString): Integer; override;
    function Get(Index: Integer): WideString; override;
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: WideString); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    destructor Destroy; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Find(const S: WideString; var Index: Integer): Boolean; virtual;
    function FindByObject(const S: TObject; var Index: Integer): Boolean; virtual;
    function IndexOf(const S: WideString): Integer; override;
    procedure Insert(Index: Integer; const S: WideString); override;
    procedure sort; override;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    function add(const S: WideString): Integer; override;
    procedure clear; override;
    property sortByObject: boolean read FsortByObject write FsortByObject default false;
  published
  end;

  { THashedStringList - A TStringList that uses TvgStringHash to improve the
    speed of Find }
  THashedStringList = class(TvgWideStringList)
  private
    FValueHash: TvgStringHash;
    FNameHash: TvgStringHash;
    FValueHashValid: Boolean;
    FNameHashValid: Boolean;
    procedure UpdateValueHash;
    procedure UpdateNameHash;
  protected
    procedure Changed; override;
  public
    CaseSensitive: boolean;
    destructor Destroy; override;
    function IndexOf(const S: WideString): Integer; override;
    function IndexOfName(const Name: WideString): Integer; override;
  end;

{ Lists }

const
  cDefaultListGrowthDelta = 16;

type

  PObject = ^TObject;

  TPointerObjectList = array [0..$FFFFFF shr 3] of TObject;
  PPointerObjectList = ^TPointerObjectList;

  TObjectListSortCompare = function (item1, item2 : TObject) : Integer;

  TvgObjectList = class(TPersistent)
  private
      { Private Declarations }
			FList: PPointerObjectList;
			FCount: Integer;
			FCapacity: Integer;
			FGrowthDelta : integer;
		protected
	      { Protected Declarations }
			procedure Error; virtual;
			function  Get(Index: Integer): TObject;
			procedure Put(Index: Integer; Item: TObject);
			procedure SetCapacity(newCapacity : Integer);
			procedure SetCount(NewCount: Integer);
			function GetFirst : TObject;
			procedure SetFirst(item : TObject);
			function GetLast : TObject;
			procedure SetLast(item : TObject);

      //: Default event for ReadFromFiler
      procedure AfterObjectCreatedByReader(Sender : TObject); virtual;
      procedure DoClean;
		public
      { Public Declarations }
			constructor Create;
			destructor Destroy; override;

			function Add(const item : TObject) : Integer;
         procedure AddNils(nbVals : Cardinal);
			procedure Delete(index : Integer);
         procedure DeleteItems(index : Integer; nbVals : Cardinal);
			procedure Exchange(Index1, Index2: Integer);
			procedure Insert(Index: Integer; Item: TObject);
         procedure InsertNils(index : Integer; nbVals : Cardinal);
			procedure Move(CurIndex, NewIndex: Integer);
			function Remove(Item: TObject): Integer;
			procedure DeleteAndFree(index : Integer);
			procedure DeleteAndFreeItems(index : Integer; nbVals : Cardinal);
			function RemoveAndFree(item : TObject) : Integer;

			property GrowthDelta : integer read FGrowthDelta write FGrowthDelta;
			function Expand: TvgObjectList;

			property Items[Index: Integer]: TObject read Get write Put; default;
			property Count: Integer read FCount write SetCount;
			property List: PPointerObjectList read FList;

			property Capacity: Integer read FCapacity write SetCapacity;
      {: Makes sure capacity is at least aCapacity. }
      procedure RequiredCapacity(aCapacity : Integer);
      {: Removes all "nil" from the list.<p>
            Note: Capacity is unchanged, no memory us freed, the list is just
            made shorter. This functions is orders of magnitude faster than
            its TList eponymous. }
      procedure Pack;
			procedure Clear; dynamic;
			procedure Clean; dynamic;
			procedure CleanFree;

			function IndexOf(Item: TObject): Integer;

			property First : TObject read GetFirst write SetFirst;
			property Last : TObject read GetLast write SetLast;
			procedure Push(item : TObject);
			function Pop : TObject;
         
			function AddObjects(const objectList : TvgObjectList) : Integer;
			procedure RemoveObjects(const objectList : TvgObjectList);
         procedure Sort(compareFunc : TObjectListSortCompare);
	end;


implementation {===============================================================}

function goodCompareObj(const a1, a2: TObject): Integer;
begin
  Result := integer(Pointer(a1)) - integer(Pointer(a2));
  if Result <> 0 then
  begin
    if Result < 0 then
      Result := 1
    else
      Result := -1;
    Exit;
  end;
end;

{ TvgStringHash =================================================================}

procedure TvgStringHash.Add(const Key: Widestring; Value: Integer);
var
  Hash: Integer;
  Bucket: PHashItem;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  New(Bucket);
  Bucket^.Key := Key;
  Bucket^.Value := Value;
  Bucket^.Next := Buckets[Hash];
  Buckets[Hash] := Bucket;
end;

procedure TvgStringHash.Clear;
var
  I: Integer;
  P, N: PHashItem;
begin
  for I := 0 to Length(Buckets) - 1 do
  begin
    P := Buckets[I];
    while P <> nil do
    begin
      N := P^.Next;
      Dispose(P);
      P := N;
    end;
    Buckets[I] := nil;
  end;
end;

constructor TvgStringHash.Create(Size: Cardinal);
begin
  inherited Create;
  SetLength(Buckets, Size);
end;

destructor TvgStringHash.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TvgStringHash.Find(const Key: Widestring): PPHashItem;
var
  Hash: Integer;
begin
  Hash := HashOf(Key) mod Cardinal(Length(Buckets));
  Result := @Buckets[Hash];
  while Result^ <> nil do
  begin
    if Result^.Key = Key then
      Exit
    else
      Result := @Result^.Next;
  end;
end;

function TvgStringHash.HashOf(const Key: Widestring): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to Length(Key) do
    Result := ((Result shl 2) or (Result shr (SizeOf(Result) * 8 - 2))) xor
      Ord(Key[I]);
end;

function TvgStringHash.Modify(const Key: Widestring; Value: Integer): Boolean;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
  begin
    Result := True;
    P^.Value := Value;
  end
  else
    Result := False;
end;

procedure TvgStringHash.Remove(const Key: Widestring);
var
  P: PHashItem;
  Prev: PPHashItem;
begin
  Prev := Find(Key);
  P := Prev^;
  if P <> nil then
  begin
    Prev^ := P^.Next;
    Dispose(P);
  end;
end;

function TvgStringHash.ValueOf(const Key: Widestring): Integer;
var
  P: PHashItem;
begin
  P := Find(Key)^;
  if P <> nil then
    Result := P^.Value
  else
    Result := -1;
end;

{ THashedStringList }

procedure THashedStringList.Changed;
begin
  inherited Changed;
  FValueHashValid := False;
  FNameHashValid := False;
end;

destructor THashedStringList.Destroy;
begin
  if FValueHash <> nil then
    FreeAndNil(FValueHash);
  if FNameHash <> nil then
    FreeAndNil(FNameHash);
  inherited Destroy;
end;

function THashedStringList.IndexOf(const S: WideString): Integer;
begin
  Result := inherited IndexOf(S);
  UpdateValueHash;
  if not CaseSensitive then
    Result :=  FValueHash.ValueOf(UpperCase(S))
  else
    Result :=  FValueHash.ValueOf(S);
end;

function THashedStringList.IndexOfName(const Name: WideString): Integer;
begin
  Result := inherited IndexOfName(Name);
  UpdateNameHash;
  if not CaseSensitive then
    Result := FNameHash.ValueOf(UpperCase(Name))
  else
    Result := FNameHash.ValueOf(Name);
end;

procedure THashedStringList.UpdateNameHash;
var
  I: Integer;
  P: Integer;
  Key: WideString;
begin
  if FNameHashValid then Exit;

  if FNameHash = nil then
    FNameHash := TvgStringHash.Create
  else
    FNameHash.Clear;
  for I := 0 to Count - 1 do
  begin
    Key := Get(I);
    P := Pos(WideString(VG_CHAR_EQ), Key);
    if P <> 0 then
    begin
      {$IFDEF KS_COMPILER6_UP}
      if not CaseSensitive then
        Key := WideUpperCase(Copy(Key, 1, P - 1))
      else
      {$ENDIF}
        Key := Copy(Key, 1, P - 1);
      FNameHash.Add(Key, I);
    end;
  end;
  FNameHashValid := True;
end;

procedure THashedStringList.UpdateValueHash;
var
  I: Integer;
begin
  if FValueHashValid then Exit;

  if FValueHash = nil then
    FValueHash := TvgStringHash.Create
  else
    FValueHash.Clear;
  for I := 0 to Count - 1 do
    {$IFDEF KS_COMPILER6_UP}
    if not CaseSensitive then
      FValueHash.Add(WideUpperCase(Self[I]), I)
    else
    {$ENDIF}
      FValueHash.Add(Self[I], I);
  FValueHashValid := True;
end;

//----------------- TvgWideStrings ---------------------------------------------------------------------------------------

function TvgWideStrings.CompareStrings(const S1, S2: WideString): Integer;
begin
  Result := 0;
end;

constructor TvgWideStrings.Create;

begin
  inherited Create;
  // there should seldom be the need to use a language other than the one of the system
end;

destructor TvgWideStrings.Destroy;
begin
  inherited;
end;

procedure TvgWideStrings.delete(Index: Integer);
begin
end;

function TvgWideStrings.Add(const S: WideString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

function TvgWideStrings.Items(AIndex: integer): WideString;
begin
  Result := Strings[AIndex];
end;

function TvgWideStrings.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  Result := Add(S);
  PutObject(Result, AObject);
end;

procedure TvgWideStrings.Append(const S: WideString);
begin
  Add(S);
end;

procedure TvgWideStrings.AddStrings(Strings: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;

procedure TvgWideStrings.AddStrings(Strings: TvgWideStrings);

var
  I: Integer;

begin
  BeginUpdate;
  try
    for I := 0 to Strings.Count - 1 do AddObject(Strings[I], Strings.Objects[I]);
  finally
    EndUpdate;
  end;
end;



procedure TvgWideStrings.Assign(Source: TPersistent);

// usual assignment routine, but able to assign wide and small strings

var
  I: Integer;

begin
  if Source is TvgWideStrings then
  begin
    BeginUpdate;
    try
      Clear;
      AddStrings(TvgWideStrings(Source));
    finally
      EndUpdate;
    end;
  end
  else
    if Source is TStrings then
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to TStrings(Source).Count - 1 do AddObject(TStrings(Source)[I], TStrings(Source).Objects[I]);
      finally
        EndUpdate;
      end;
    end
    else inherited Assign(Source);
end;



procedure TvgWideStrings.AssignTo(Dest: TPersistent);
var
  I: Integer;

begin
  if Dest is TStrings then
    with Dest as TStrings do
    begin
      BeginUpdate;
      try
        Clear;
        for I := 0 to Self.Count - 1 do AddObject(Self[I], Self.Objects[I]);
      finally
        EndUpdate;
      end;
    end
    else
      if Dest is TvgWideStrings then
        with Dest as TvgWideStrings do
        begin
          BeginUpdate;
          try
            Clear;
            AddStrings(Self);
          finally
            EndUpdate;
          end;
        end
        else inherited;
end;



procedure TvgWideStrings.BeginUpdate;

begin
  if FUpdateCount = 0 then SetUpdateState(True);
  System.Inc(FUpdateCount);
end;



procedure TvgWideStrings.clear;
begin

end;

procedure TvgWideStrings.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TvgWideStrings then Result := not Equals(TvgWideStrings(Filer.Ancestor))
    end
    else
      Result := Count > 0;
  end;

begin
  Filer.DefineProperty(VG_STRINGS, ReadData, WriteData, DoWrite);
end;



procedure TvgWideStrings.EndUpdate;

begin
  System.Dec(FUpdateCount);
  if FUpdateCount = 0 then SetUpdateState(False);
end;



function TvgWideStrings.Equals(Strings: TvgWideStrings): Boolean;
var
  I, Count: Integer;
begin
  Result := False;
  Count := GetCount;
  if Count <> Strings.GetCount then Exit;
  for I := 0 to Count - 1 do
    if Get(I) <> Strings.Get(I) then Exit;
  Result := True;
end;

procedure TvgWideStrings.Error(const Msg: String; Data: Integer);
begin

end;

procedure TvgWideStrings.Exchange(Index1, Index2: Integer);
var
  TempObject: TObject;
  TempString: WideString;
begin
  BeginUpdate;
  try
    TempString := Strings[Index1];
    TempObject := Objects[Index1];
    Strings[Index1] := Strings[Index2];
    Objects[Index1] := Objects[Index2];
    Strings[Index2] := TempString;
    Objects[Index2] := TempObject;
  finally
    EndUpdate;
  end;
end;



function TvgWideStrings.GetCapacity: Integer;

begin  // descendants may optionally override/replace this default implementation
  Result := Count;
end;



function TvgWideStrings.GetCommaText: WideString;

var
  S: WideString;
  P: PWideChar;
  I,
  Count: Integer;

begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = EmptyStr) then
    Result := EmptyStr
  else
  begin
    Result := EmptyStr;
    for I := 0 to Count - 1 do
    begin
      S := Get(I);
      P := PWideChar(S);
      while not (P^ in [WideNull..Space, System.WideChar(VG_CHAR_QUOTE), System.WideChar(VG_CHAR_DOT)]) do
       System.Inc(P);
      Result := Result + S + VG_CHAR_COMMA;
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;



function TvgWideStrings.GetName(Index: Integer): WideString;

var
  P: Integer;

begin
  Result := Get(Index);
  P := Pos(WideString(VG_CHAR_EQ), Result);
  if P > 0 then SetLength(Result, P - 1)
           else Result := EmptyStr;
end;



function TvgWideStrings.GetObject(Index: Integer): TObject;

begin
  Result := nil;
end;

function TvgWideStrings.GetTextStr: WideString;

var
  I, L,
  Size,
  Count: Integer;
  P: PWideChar;
  S: WideString;

begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do System.Inc(Size, Length(Get(I)) + 2);
  SetLength(Result, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, 2 * L);
      System.Inc(P, L);
    end;
    P^ := CarriageReturn;
    System.Inc(P);
    P^ := LineFeed;
    System.Inc(P);
  end;
end;



function TvgWideStrings.GetValueFromIndex(Index: Integer): WideString;
begin
  if Index >= 0 then
    Result := Copy(Get(Index), Length(Names[Index]) + 2, System.MaxInt) else
    Result := EmptyStr;
end;

procedure TvgWideStrings.SetValueFromIndex(Index: Integer; const Value: WideString);
begin
  if Value <> EmptyStr then
  begin
    if Index < 0 then Index := Add(EmptyStr);
    Put(Index, Names[Index] + VG_CHAR_EQ + Value);
  end
  else
    if Index >= 0 then Delete(Index);
end;

procedure TvgWideStrings.sort;
begin
end;

function TvgWideStrings.GetValue(const Name: WideString): WideString;

var
  I: Integer;

begin
  I := IndexOfName(Name);
  if I >= 0 then Result := Copy(Get(I), Length(Name) + 2, System.MaxInt)
            else Result := EmptyStr;
end;



function TvgWideStrings.IndexOf(const S: WideString): Integer;

begin
  for Result := 0 to GetCount - 1 do
  begin
    {$IFDEF KS_COMPILER6_UP}
    if WideCompareText(Get(Result), S) = 0 then Exit;
    {ELSE}
    if WideCompareText(Get(Result), S) = 0 then Exit;
    {$ENDIF}
  end;
  Result := -1;
end;



function TvgWideStrings.IndexOfName(const Name: WideString): Integer;

var
  P: Integer;
  S: WideString;

begin
  for Result := 0 to GetCount - 1 do
  begin
    S := Get(Result);
    P := Pos(WideString(VG_CHAR_EQ), S);
    {$IFDEF KS_COMPILER6_UP}
    if (P > 0) and (WideCompareText(Copy(S, 1, P - 1), Name) = 0) then Exit;
    {$ELSE}
    if (P > 0) and (CompareText(Copy(S, 1, P - 1), Name) = 0) then Exit;
    {$ENDIF}
  end;
  Result := -1;
end;



function TvgWideStrings.IndexOfObject(AObject: TObject): Integer;

begin
  for Result := 0 to GetCount - 1 do
    if GetObject(Result) = AObject then Exit;
  Result := -1;
end;



procedure TvgWideStrings.InsertObject(Index: Integer; const S: WideString; AObject: TObject);

begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;



procedure TvgWideStrings.LoadFromFile(const FileName: String);

var
  Stream: TStream;

begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    try
      LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    //RaiseLastWin32Error;
  end;
end;

procedure TvgWideStrings.LoadFromStream(Stream: TStream);

// usual loader routine, but enhanced to handle byte order marks in stream

var
  Size: Integer;
  Order: System.WideChar;
  SW: WideString;

begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    Stream.Read(Order, 2);
    if (Order = BOM_LSB_FIRST) or (Order = BOM_MSB_FIRST) then
    begin
      SetLength(SW, (Size - 2) div 2);
      Stream.Read(PWideChar(SW)^, Size - 2);
//!!!      if Order = BOM_MSB_FIRST then StrSwapByteOrder(PWideChar(SW));
      SetTextStr(SW);
    end;
  finally
    EndUpdate;
  end;
end;



procedure TvgWideStrings.Move(CurIndex, NewIndex: Integer);

var
  TempObject: TObject;
  TempString: WideString;

begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempObject := GetObject(CurIndex);
      Delete(CurIndex);
      InsertObject(NewIndex, TempString, TempObject);
    finally
      EndUpdate;
    end;
  end;
end;



procedure TvgWideStrings.Put(Index: Integer; const S: WideString);

var
  TempObject: TObject;

begin
  TempObject := GetObject(Index);
  Delete(Index);
  InsertObject(Index, S, TempObject);
end;



procedure TvgWideStrings.PutObject(Index: Integer; AObject: TObject);

begin
end;



procedure TvgWideStrings.ReadData(Reader: TReader);

begin
  Reader.ReadListBegin;
  BeginUpdate;
  try
    Clear;
    while not Reader.EndOfList do
      Add(Reader.ReadWideString);
  finally
    EndUpdate;
  end;
  Reader.ReadListEnd;
end;



procedure TvgWideStrings.SaveToFile(const FileName: String);

var
  Stream: TStream;

begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;



procedure TvgWideStrings.SaveToStream(Stream: TStream);

var
  SW, BOM: WideString;
  Allowed: Boolean;

begin
  // The application can decide in which format to save the content.
  // If FSaveUnicode is False then all strings are saved in standard ANSI format
  // which is also loadable by TStrings but you should be aware that all Unicode
  // strings are then converted to ANSI based on the current system locale.
  // An extra event is supplied to ask the user about the potential loss of information
  // when converting Unicode to ANSI strings.
  SW := GetTextStr;
  Allowed := True;
  if Allowed then
  begin
    // only save if allowed
    BOM := BOM_LSB_FIRST;
    Stream.WriteBuffer(PWideChar(BOM)^, 2);
    // SW has already been filled
    Stream.WriteBuffer(PWideChar(SW)^, 2 * Length(SW));
  end;
end;

procedure TvgWideStrings.SetCapacity(NewCapacity: Integer);
begin
  // do nothing - descendants may optionally implement this method
end;

procedure TvgWideStrings.SetCaseSensitive(const Value: Boolean);
begin
  if Value <> FCaseSensitive then
  begin
    FCaseSensitive := Value;
    if Sorted then Sort;
  end;
end;

procedure TvgWideStrings.SetCommaText(const Value: WideString);

var
  P, P1: PWideChar;
  S: WideString;

begin
  BeginUpdate;
  try
    Clear;
    P := PWideChar(Value);
    while P^ in [System.WideChar(CHAR_1)..Space] do System.Inc(P);
    while P^ <> WideNull do
    begin
      if P^ = VG_CHAR_QUOTE then
      else
      begin
        P1 := P;
        while (P^ > Space) and (P^ <> VG_CHAR_COMMA) do System.Inc(P);
        System.SetString(S, P1, P - P1);
      end;
      Add(S);

      while P^ in [System.WideChar(CHAR_1)..Space] do System.Inc(P);
      if P^ = VG_CHAR_COMMA then
        repeat
          System.Inc(P);
        until not (P^ in [System.WideChar(CHAR_1)..Space]);
    end;
  finally
    EndUpdate;
  end;
end;



procedure TvgWideStrings.setItem(index: integer; text: wideString);
begin
  Strings[index] := text;
end;

procedure TvgWideStrings.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TvgWideStrings.SetText(Text: PWideChar);
begin
  SetTextStr(Text);
end;

procedure TvgWideStrings.SetTextStr(const Value: WideString);
var
  Head,
  Tail: PWideChar;
  S: WideString;
begin
  BeginUpdate;
  try
    Clear;
    Head := PWideChar(Value);
    while Head^ <> WideNull do
    begin
      Tail := Head;
      while not (Tail^ in [WideNull, LineFeed, CarriageReturn, VerticalTab, FormFeed]) and
            (Tail^ <> LineSeparator) and
            (Tail^ <> ParagraphSeparator) do System.Inc(Tail);
      System.SetString(S, Head, Tail - Head);
      Add(S);
      Head := Tail;
      if Head^ <> WideNull then
      begin
        System.Inc(Head);
        if (Tail^ = CarriageReturn) and
           (Head^ = LineFeed) then System.Inc(Head);
      end;
    end;
  finally
    EndUpdate;
  end;
end;



procedure TvgWideStrings.SetUpdateState(Updating: Boolean);

begin
end;



procedure TvgWideStrings.SetValue(const Name, Value: WideString);

var
  I : Integer;

begin
  I := IndexOfName(Name);
  if Value <> EmptyStr then
  begin
    if I < 0 then I := Add(EmptyStr);
    Put(I, Name + VG_CHAR_EQ + Value);
  end
  else
    if I >= 0 then Delete(I);
end;



procedure TvgWideStrings.WriteData(Writer: TWriter);
var
  I: Integer;
begin
  Writer.WriteListBegin;
  for I := 0 to Count-1 do
    Writer.WriteWideString(Get(I));
  Writer.WriteListEnd;
end;

//----------------- TvgWideStringList ------------------------------------------------------------------------------------

destructor TvgWideStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;



function TvgWideStringList.Add(const S: WideString): Integer;
begin
  if not Sorted then
    Result := FCount
  else
  begin
    if sortByObject then
    begin
      Result := FCount
    end
    else
    begin
      if Find(S, Result) then
        case Duplicates of
          dupIgnore:
            Exit;
          dupError:
            Error(VG_D_STRING, 0);
        end;
    end;
  end;
  InsertItem(Result, S);
end;



procedure TvgWideStringList.Changed;

begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;



procedure TvgWideStringList.Changing;

begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;



procedure TvgWideStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    // this will automatically finalize the array
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;



function TvgWideStringList.CompareStrings(const S1, S2: WideString): Integer;
begin
  {$IFDEF KS_COMPILER6_UP}
  if CaseSensitive then
    Result := WideCompareStr(S1, S2)
  else
    Result := WideCompareText(S1, S2);
  {$ELSE}
  if CaseSensitive then
    Result := CompareStr(S1, S2)
  else
    Result := CompareText(S1, S2);
  {$ENDIF}
end;

procedure TvgWideStringList.Delete(Index: Integer);

begin
  if (Index < 0) or (Index >= FCount) then Error(VG_LIST_INDEX_ERR, Index);
  Changing;
  FList[Index].FString := EmptyStr;
  System.Dec(FCount);
  if Index < FCount then System.Move(FList[Index + 1], FList[Index], (FCount - Index) * SizeOf(TWideStringItem));
  Changed;
end;



procedure TvgWideStringList.Exchange(Index1, Index2: Integer);

begin
  if (Index1 < 0) or (Index1 >= FCount) then Error(VG_LIST_INDEX_ERR, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then Error(VG_LIST_INDEX_ERR, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;



procedure TvgWideStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: PWideStringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.FString);
  Integer(Item1^.FString) := Integer(Item2^.FString);
  Integer(Item2^.FString) := Temp;
  Temp := Integer(Item1^.FObject);
  Integer(Item1^.FObject) := Integer(Item2^.FObject);
  Integer(Item2^.FObject) := Temp;
end;

function TvgWideStringList.Find(const S: WideString; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    {$IFDEF KS_COMPILER6_UP}
    C := WideCompareText(FList^[I].FString, S);
    {$ELSE}
    C := CompareText(FList^[I].FString, S);
    {$ENDIF}
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TvgWideStringList.FindByObject(const S: TObject;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := goodCompareObj(FList^[I].FObject, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TvgWideStringList.Get(Index: Integer): WideString;

begin
  if (Index < 0) or (Index >= FCount) then Error(VG_LIST_INDEX_ERR, Index);
  Result := FList[Index].FString;
end;



function TvgWideStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;



function TvgWideStringList.GetCount: Integer;

begin
  Result := FCount;
end;



function TvgWideStringList.GetObject(Index: Integer): TObject;

begin
  if (Index < 0) or (Index >= FCount) then Error(VG_LIST_INDEX_ERR, Index);
  Result := FList[Index].FObject;
end;



procedure TvgWideStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;



function TvgWideStringList.IndexOf(const S: WideString): Integer;

begin
  if not Sorted then
    Result := inherited IndexOf(S)
  else
    if not Find(S, Result) then Result := -1;
end;



procedure TvgWideStringList.Insert(Index: Integer; const S: WideString);

begin
  if Sorted then Error(VG_SORT_LIST_ERR, 0);
  if (Index < 0) or (Index > FCount) then Error(VG_LIST_INDEX_ERR, Index);
  InsertItem(Index, S);
end;



procedure TvgWideStringList.InsertItem(Index: Integer; const S: WideString);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(TStringItem));
  with FList^[Index] do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  System.Inc(FCount);
  Changed;
end;



procedure TvgWideStringList.Put(Index: Integer; const S: WideString);

begin
  if Sorted then Error(VG_SORT_LIST_ERR, 0);
  if (Index < 0) or (Index >= FCount) then Error(VG_LIST_INDEX_ERR, Index);
  Changing;
  FList[Index].FString := S;
  Changed;
end;



procedure TvgWideStringList.PutObject(Index: Integer; AObject: TObject);

begin
  if (Index < 0) or (Index >= FCount) then Error(VG_LIST_INDEX_ERR, Index);
  Changing;
  FList[Index].FObject := AObject;
  Changed;
end;



procedure TvgWideStringList.QuickSort(L, R: Integer);

var
  I, J: Integer;
  P: WideString;

begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FString;
    repeat
      {$IFDEF KS_COMPILER6_UP}
      while WideCompareText(FList[I].FString, P) < 0 do System.Inc(I);
      while WideCompareText(FList[J].FString, P) > 0 do System.Dec(J);
      {ELSE}
      while CompareText(FList[I].FString, P) < 0 do System.Inc(I);
      while CompareText(FList[J].FString, P) > 0 do System.Dec(J);
      {$ENDIF}
      if I <= J then
      begin
        ExchangeItems(I, J);
        System.Inc(I);
        System.Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TvgWideStringList.QuickSortByObject(L, R: Integer);
var
  I, J: Integer;
  P: TObject;
begin
  repeat
    I := L;
    J := R;
    P := FList[(L + R) shr 1].FObject;
    repeat
      while goodCompareObj(FList[I].FObject, P) < 0 do System.Inc(I);
      while goodCompareObj(FList[J].FObject, P) > 0 do System.Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        System.Inc(I);
        System.Dec(J);
      end;
    until I > J;
    if L < J then QuickSortByObject(L, J);
    L := I;
  until I >= R;
end;

procedure TvgWideStringList.SetCapacity(NewCapacity: Integer);
begin
  ReallocMem(FList, NewCapacity * SizeOf(TWideStringItem));
  FCapacity := NewCapacity;
end;

procedure TvgWideStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing
              else Changed;
end;

procedure TvgWideStringList.sort;
begin
  if not Sorted and (Count > 1) then
  begin
    Changing;
    if FsortByObject then
      QuickSortByObject(0, FCount - 1)
    else
      QuickSort(0, FCount - 1);
    Changed;
  end;
end;

// ------------------
// ------------------ TvgObjectList ------------------
// ------------------

// Create
//
constructor TvgObjectList.Create;
begin
	inherited ;
	FGrowthDelta := cDefaultListGrowthDelta;
end;

// Destroy
//
destructor TvgObjectList.Destroy;
begin
	Clear;
	inherited Destroy;
end;

// Add (
//
function TvgObjectList.Add(const item : TObject): Integer;
begin
	Result:=FCount;
	if Result=FCapacity then
		SetCapacity(FCapacity+FGrowthDelta);
	FList[Result]:=Item;
	System.Inc(FCount);
end;

// AddNils
//
procedure TvgObjectList.AddNils(nbVals : Cardinal);
begin
   if Integer(nbVals)+Count>Capacity then
      SetCapacity(Integer(nbVals)+Count);
   FillChar(FList[FCount], Integer(nbVals)*SizeOf(TObject), 0);
   FCount:=FCount+Integer(nbVals);
end;

// AddObjects
//
function TvgObjectList.AddObjects(const objectList : TvgObjectList) : Integer;
begin
	if Assigned(objectList) then begin
   	Result:=FCount;
		SetCount(Result+objectList.Count);
		System.Move(objectList.FList^[0], FList^[Result],
						objectList.FCount*SizeOf(TObject));
	end else Result:=0;
end;

// RemoveObjects
//
procedure TvgObjectList.RemoveObjects(const objectList : TvgObjectList);
var
	i : Integer;
begin
	for i:=0 to objectList.Count-1 do
		Remove(objectList[i]);
end;

// Clear
//
procedure TvgObjectList.Clear;
begin
	if Assigned(Self) then begin
		SetCount(0);
		SetCapacity(0);
	end;
end;

// Delete
//
procedure TvgObjectList.Delete(index : Integer);
begin
{$IFOPT R+}
	if Cardinal(Index)>=Cardinal(FCount) then Error;
{$ENDIF}
	System.Dec(FCount);
	if index<FCount then
		System.Move(FList[index+1], FList[index], (FCount-index)*SizeOf(TObject));
end;

// DeleteItems
//
procedure TvgObjectList.DeleteItems(index : Integer; nbVals : Cardinal);
begin
{$IFOPT R+}
   Assert(Cardinal(index)<Cardinal(FCount));
{$ENDIF}
   if nbVals>0 then begin
      if index+Integer(nbVals)<FCount then begin
	   	System.Move(FList[index+Integer(nbVals)],
                     FList[index],
                     (FCount-index-Integer(nbVals))*SizeOf(TObject));
      end;
	   System.Dec(FCount, nbVals);
   end;
end;

// Exchange
//
procedure TvgObjectList.Exchange(index1, index2 : Integer);
var
	item : TObject;
   locList : PPointerObjectList;
begin
{$IFOPT R+}
	if (Cardinal(Index1)>=Cardinal(FCount))
		or	(Cardinal(Index2)>=Cardinal(FCount)) then Error;
{$ENDIF}
   locList:=FList;
	item:=locList[index1];
	locList[index1]:=locList[index2];
	locList[index2]:=item;
end;

// Expand
//
function TvgObjectList.Expand: TvgObjectList;
begin
	if FCount=FCapacity then
		SetCapacity(FCapacity+FGrowthDelta);
	Result:=Self;
end;

// GetFirst
//
function TvgObjectList.GetFirst: TObject;
begin
{$IFOPT R+}
	if Cardinal(FCount)=0 then Error;
{$ENDIF}
	Result:=FList[0];
end;

// SetFirst
//
procedure TvgObjectList.SetFirst(item : TObject);
begin
{$IFOPT R+}
	if Cardinal(FCount)=0 then Error;
{$ENDIF}
   FList[0]:=item;
end;

// Error
//
procedure TvgObjectList.Error;
begin
   raise EListError.Create(VG_LIST_INDEX_ERR);
end;

// Get
//
function TvgObjectList.Get(Index: Integer): TObject;
begin
{$IFOPT R+}
	if Cardinal(Index)>=Cardinal(FCount) then Error;
{$ENDIF}
	Result := FList^[Index];
end;

// IndexOf
//
function TvgObjectList.IndexOf(Item: TObject): Integer;
var
  {$IFDEF DARWIN}
  i: integer;
  {$ENDIF}
	c : Integer;
	p : ^TObject;
begin
	if FCount<=0 then
		Result:=-1
	else begin
		c:=FCount;
		p:=@FList^[0];

  {$IFDEF DARWIN}
  for i := 0 to c - 1 do
  begin
    if P^ = Item then
    begin
      Result := i;
      Exit;
    end;
    System.Inc(P);
  end;
  Result := -1;
  {$ELSE}
		asm
			mov eax, Item;
			mov ecx, c;
			mov edx, ecx;
			push edi;
			mov edi, p;
			repne scasd;
			je @@FoundIt
			mov edx, -1;
			jmp @@SetResult;
		@@FoundIt:
			sub edx, ecx;
			dec edx;
		@@SetResult:
			mov Result, edx;
			pop edi;
		end;
  {$ENDIF}
  end;
end;

// Insert
//
procedure TvgObjectList.Insert(index : Integer; item: TObject);
begin
{$IFOPT R+}
	if Cardinal(index)>Cardinal(FCount) then Error;
{$ENDIF}
	if FCount=FCapacity then
		SetCapacity(FCapacity+FGrowthDelta);
	if Index<FCount then
		System.Move(FList[index], FList[index+1],
						(FCount-index)*SizeOf(TObject));
	FList[index]:=item;
	System.Inc(FCount);
end;

// InsertNils
//
procedure TvgObjectList.InsertNils(index : Integer; nbVals : Cardinal);
var
   nc : Integer;
begin
{$IFOPT R+}
   Assert(Cardinal(Index)<Cardinal(FCount));
{$ENDIF}
   if nbVals>0 then begin
      nc:=FCount+Integer(nbVals);
	   if nc>FCapacity then
         SetCapacity(nc);
   	if Index<FCount then
	   	System.Move(FList[Index], FList[Index+Integer(nbVals)],
                     (FCount-Index)*SizeOf(TObject));
      FillChar(FList[Index], Integer(nbVals)*SizeOf(TObject), 0);
	   FCount:=nc;
   end;
end;

// GetLast
//
function TvgObjectList.GetLast: TObject;
begin
{$IFOPT R+}
	if Cardinal(FCount)=0 then Error;
{$ENDIF}
	Result:=FList[FCount-1];
end;

// SetLast
//
procedure TvgObjectList.SetLast(item : TObject);
begin
{$IFOPT R+}
	if Cardinal(FCount)=0 then Error;
{$ENDIF}
	FList[FCount-1]:=item;
end;

// Move
//
procedure TvgObjectList.Move(CurIndex, NewIndex: Integer);
var
   item : Pointer;
begin
   if curIndex<>newIndex then begin
{$IFOPT R+}
      if Cardinal(newIndex)>=Cardinal(Count) then Error;
      if Cardinal(curIndex)>=Cardinal(Count) then Error;
{$ENDIF}
      item:=List[curIndex];
      if curIndex<newIndex then begin
         // curIndex+1 necessarily exists since curIndex<newIndex and newIndex<Count
         System.Move(List[curIndex+1], List[curIndex], (newIndex-curIndex-1)*SizeOf(TObject));
      end else begin
         // newIndex+1 necessarily exists since newIndex<curIndex and curIndex<Count
         System.Move(List[newIndex], List[newIndex+1], (curIndex-newIndex-1)*SizeOf(TObject));
      end;
      List[newIndex]:=item;
   end;
end;

// Put
//
procedure TvgObjectList.Put(Index: Integer; Item: TObject);
begin
{$IFOPT R+}
	if Cardinal(Index)>=Cardinal(FCount) then Error;
{$ENDIF}
	FList^[Index] := Item;
end;

// Remove
//
function TvgObjectList.Remove(item : TObject) : Integer;
begin
	Result:=IndexOf(item);
	if Result>=0 then
      Delete(Result);
end;

// Pack
//
procedure TvgObjectList.Pack;
var
   i, j, n : Integer;
   p : PPointerObjectList;
   pk : PObject;
begin
   p:=List;
   n:=Count-1;
   while (n>=0) and (p[n]=nil) do System.Dec(n);
   for i:=0 to n do begin
      if p[i]=nil then begin
         pk:=@p[i];
         for j:=i+1 to n do begin
            if p[j]<>nil then begin
               pk^:=p[j];
               System.Inc(pk);
            end;
         end;
         SetCount((Integer(pk)-Integer(p)) div SizeOf(TObject));
         Exit;
      end;
   end;
   SetCount(n+1);
end;

// SetCapacity
//
procedure TvgObjectList.SetCapacity(newCapacity: Integer);
begin
	if newCapacity<>FCapacity then begin
      if newCapacity<FCount then
         FCount:=newCapacity;
		ReallocMem(FList, newCapacity*SizeOf(TObject));
		FCapacity:=newCapacity;
	end;
end;

// RequiredCapacity
//
procedure TvgObjectList.RequiredCapacity(aCapacity : Integer);
begin
   if FCapacity<aCapacity then
      SetCapacity(aCapacity);
end;

// SetCount
//
procedure TvgObjectList.SetCount(newCount : Integer);
begin
	if newCount>FCapacity then
		SetCapacity(newCount);
	if newCount>FCount then
		FillChar(FList[FCount], (newCount-FCount)*SizeOf(TObject), 0);
	FCount:=NewCount;
end;

// DeleteAndFree
//
procedure TvgObjectList.DeleteAndFree(index : Integer);
var
	obj : TObject;
begin
	obj:=Get(index);
	Delete(index);
	obj.Free;
end;

// DeleteAndFreeItems
//
procedure TvgObjectList.DeleteAndFreeItems(index : Integer; nbVals : Cardinal);
var
   i, n : Integer;
begin
{$IFOPT R+}
   Assert(Cardinal(index)<Cardinal(FCount));
{$ENDIF}
   n:=index+Integer(nbVals);
   if n>=FCount then
      n:=FCount-1;
   for i:=index to n do
      FList[i].Free;
   DeleteItems(index, nbVals);
end;

// RemoveAndFree
//
function TvgObjectList.RemoveAndFree(item : TObject) : Integer;
begin
   Result:=IndexOf(item);
	if Result>=0 then begin
      Delete(Result);
      item.Free;
   end;
end;

// DoClean
//
procedure TvgObjectList.DoClean;
var
	i : Integer;
begin
	i:=FCount-1; while i>=0 do begin
		if i<FCount then FList[i].Free;
		System.Dec(i);
	end;
end;


// Clean
//
procedure TvgObjectList.Clean;
begin
   DoClean;
	Clear;
end;

// CleanFree
//
procedure TvgObjectList.CleanFree;
begin
   if Self<>nil then begin
      Clean;
      Destroy;
	end;
end;

// AfterObjectCreatedByReader
//
procedure TvgObjectList.AfterObjectCreatedByReader(Sender : TObject);
begin
   // nothing
end;

// Push
//
procedure TvgObjectList.Push(item : TObject);
begin
	Add(item);
end;

// Pop
//
function TvgObjectList.Pop : TObject;
begin
	if FCount>0 then begin
		Result:=FList[FCount-1];
      System.Dec(FCount);
	end else Result:=nil;
end;

// POListQuickSort
//
procedure POListQuickSort(SortList : PPointerObjectList; L, R : Integer;
                          compareFunc : TObjectListSortCompare);
var
   I, J : Integer;
   P, T : Pointer;
begin
   repeat
      I:=L;
      J:=R;
      P:=SortList[(L+R) shr 1];
      repeat
         while compareFunc(SortList[I], P)<0 do System.Inc(I);
         while compareFunc(SortList[J], P)>0 do System.Dec(J);
         if I<=J then begin
            T := SortList^[I];
            SortList^[I] := SortList^[J];
            SortList^[J] := T;
            System.Inc(I);
            System.Dec(J);
         end;
      until I > J;
      if L<J then
         POListQuickSort(SortList, L, J, compareFunc);
      L:=I;
   until I>=R;
end;

// Sort
//
procedure TvgObjectList.Sort(compareFunc : TObjectListSortCompare);
begin
   if Count>1 then
      POListQuickSort(FList, 0, Count-1, compareFunc);
end;

initialization
  RegisterClasses([TvgWideStrings, TvgWideStringList]);
end.

