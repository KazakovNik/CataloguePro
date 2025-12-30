unit uFileContentController;

interface

uses
  uFileContentModel;

type
  TFileContentController = class
  private
    FModel: TFileContentModel;
    procedure SetFilePath(const Value: string);
    function GetFileContent(): string;
  public
    constructor Create();
    destructor Destroy(); override;
    property FilePath: string write SetFilePath;
    property Content: string read GetFileContent;
  end;

implementation

uses
  System.SysUtils;

constructor TFileContentController.Create();
begin
  inherited Create();
  FModel := nil;
end;

destructor TFileContentController.Destroy();
begin
  FreeAndNil(FModel);
  inherited;
end;

procedure TFileContentController.SetFilePath(const Value: string);
begin
  if Assigned(FModel) then
    FreeAndNil(FModel);
  FModel := TFileContentModel.Create(Value);
end;

function TFileContentController.GetFileContent(): string;
begin
  if not Assigned(FModel) then
    Exit('');
  Result := FModel.LoadFileContent();
end;

end.
