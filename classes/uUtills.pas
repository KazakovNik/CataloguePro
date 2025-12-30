unit uUtills;

interface

uses
  System.Classes, System.SysUtils, Vcl.Controls, Vcl.Forms;

function ComponentToStringProc(Component: TComponent): string;
function StringToComponentProc(Value: string): TComponent;

procedure LoadComponentFromFile(Component: TComponent; const FileName: string);
procedure SaveComponentToFile(Component: TComponent; const FileName: string);

implementation

function ComponentToStringProc(Component: TComponent): string;
var
  BinStream: TMemoryStream;
  StrStream: TStringStream;
  s: string;
begin
  BinStream := TMemoryStream.Create;
  try
    StrStream := TStringStream.Create(s);
    try
      BinStream.WriteComponent(Component);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      Result:= StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

function StringToComponentProc(Value: string): TComponent;
var
  StrStream:TStringStream;
  BinStream: TMemoryStream;
begin
  StrStream := TStringStream.Create(Value);
  try
    BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      Result:= BinStream.ReadComponent(nil);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

procedure SaveComponentToFile(Component: TComponent; const FileName: string);
var
  FileStream : TFileStream;
  MemStream : TMemoryStream;
begin
  MemStream := nil;

  if not Assigned(Component) then
    raise Exception.Create('Component is not assigned');

  FileStream := TFileStream.Create(FileName, fmCreate);
  try
    MemStream := TMemoryStream.Create;
    MemStream.WriteComponent(Component);
    MemStream.Position := 0;
    ObjectBinaryToText(MemStream, FileStream);
  finally
    MemStream.Free;
    FileStream.Free;
  end;
end;

procedure LoadComponentFromFile(Component: TComponent; const FileName: string);
var
  FileStream : TFileStream;
  MemStream : TMemoryStream;
  i: Integer;
begin
  MemStream := nil;

  if not Assigned(Component) then
    raise Exception.Create('Component is not assigned');

  if FileExists(FileName) then
  begin
    FileStream := TFileStream.Create(FileName, fmOpenRead);
    try
      for i := Component.ComponentCount - 1 downto 0 do
      begin
        if Component.Components[i] is TControl then
          TControl(Component.Components[i]).Parent := nil;
        Component.Components[i].Free;
      end;

      MemStream := TMemoryStream.Create;
      ObjectTextToBinary(FileStream, MemStream);
      MemStream.Position := 0;
      MemStream.ReadComponent(Component);
      Application.InsertComponent(Component);
    finally
      MemStream.Free;
      FileStream.Free;
    end;
  end;
end;

end.
