unit uFileContentModel;

interface

type
  TFileContentModel = class
  private
    FFilePath: string;
  public
    constructor Create(const AFilePath: string);
    function LoadFileContent(): string;
  end;

implementation

uses
  System.SysUtils, System.Classes;

constructor TFileContentModel.Create(const AFilePath: string);
begin
  inherited Create();
  FFilePath := AFilePath;
end;

function TFileContentModel.LoadFileContent(): string;
var
  sl: TStringList;
begin
  if not FileExists(FFilePath) then
    raise Exception.Create('Файл не найден');

  sl := TStringList.Create();
  try
    sl.LoadFromFile(FFilePath);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

end.
