unit AndroidThreadBase;

interface

uses
  Classes, SysUtils, Controls, Windows, Variants;

type
  TAndroidMapTable = record
    hashName: string;
    hashValue: Variant;
  end;
  PAndroidMapTable = ^TAndroidMapTable;

  IAndroidMap = interface
    procedure put(hashName: string; hashValue: Variant); stdcall;
    function get(hashName: string): Variant; stdcall;
    function remove(hashName: string): Boolean; stdcall;
    function getTable(index: Integer): PAndroidMapTable; stdcall;
    procedure clear; stdcall;
    function size: Integer; stdcall;
  end; 

  TAndroidMap = class(TInterfacedObject, IAndroidMap)
  private
    FTable: array of TAndroidMapTable;
  public
    procedure put(hashName: string; hashValue: Variant); stdcall;
    function remove(hashName: string): Boolean; stdcall;
    function get(hashName: string): Variant; stdcall;
    function getTable(index: Integer): PAndroidMapTable; stdcall;
    function size: Integer; stdcall;
    procedure clear; stdcall;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

procedure TAndroidMap.clear;
begin
  SetLength(FTable, 0);
end;

constructor TAndroidMap.Create;
begin
  SetLength(FTable, 0);
end;

destructor TAndroidMap.Destroy;
begin
  SetLength(FTable, 0);
  inherited;
end;

function TAndroidMap.get(hashName: string): Variant;
var
  i: Integer;
begin
  Result := null;
  for i := 0 to Length(FTable) - 1 do
  begin
    if FTable[i].hashName = hashName then
    begin
      Result := FTable[i].hashValue;
      break;
    end;
  end;
end;

function TAndroidMap.getTable(index: Integer): PAndroidMapTable;
begin
  if (Length(FTable) <= index) or (index < 0) then
  begin
    Result := nil;
    Exit;
  end;
  Result := @FTable[index];
end;

procedure TAndroidMap.put(hashName: string; hashValue: Variant);
var
  i: Integer;
  len: Integer;
begin
  len := Length(FTable);
  for i := 0 to len - 1 do
  begin
    if FTable[i].hashName = hashName then
    begin
      FTable[i].hashValue := hashValue;
      Exit;
    end;
  end;
  SetLength(FTable, len + 1);
  FTable[len].hashName := hashName;
  FTable[len].hashValue := hashValue;
end;

function TAndroidMap.remove(hashName: string): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to Length(FTable) - 1 do
  begin
    if FTable[i].hashName = hashName then
    begin
      for j := i to Length(FTable) - 2 do
      begin
        FTable[j].hashName := FTable[j + 1].hashName;
        FTable[j].hashValue := FTable[j + 1].hashValue;
      end;
      SetLength(FTable, Length(FTable) - 1);
      Result := True;
      Break;
    end;
  end;
end;

function TAndroidMap.size: Integer;
begin
  Result := Length(FTable);
end;

end.
