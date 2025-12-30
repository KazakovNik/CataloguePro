unit uSettingsController;

interface

uses
  StdCtrls, uSettingsModel, SysUtils, Vcl.Forms, System.Classes;

type
  TSettingsController = class(TObject)
  private
    FModel: TSettings;
    FFilePath: string;

    function GetMainFormHeight: integer;
    function GetMainFormLeft: integer;
    function GetMainFormTop: integer;
    function GetMainFormWidth: integer;
    procedure SetMainFormHeight(const Value: integer);
    procedure SetMainFormLeft(const Value: integer);
    procedure SetMainFormTop(const Value: integer);
    procedure SetMainFormWidth(const Value: integer);
    function GetHeapWidth: integer;
    procedure SetHeapWidth(const Value: integer);
  public
    constructor Create(const AFilePath: string);
    destructor Destroy; override;

    procedure Save;

    property MainFormLeft: integer read GetMainFormLeft write SetMainFormLeft;
    property MainFormTop: integer read GetMainFormTop write SetMainFormTop;
    property MainFormHeight: integer read GetMainFormHeight write SetMainFormHeight;
    property MainFormWidth: integer read GetMainFormWidth write SetMainFormWidth;
    property HeapWidth: integer read GetHeapWidth write SetHeapWidth;
  end;

implementation

constructor TSettingsController.Create(const AFilePath: string);
begin
  FFilePath := AFilePath;
  FModel := TSettings.Create(FFilePath);
end;

destructor TSettingsController.Destroy;
begin
  FreeAndNil(FModel);
  inherited Destroy;
end;

function TSettingsController.GetHeapWidth: integer;
begin
  Result := FModel.HeapWidth;
end;

function TSettingsController.GetMainFormHeight: integer;
begin
  Result := FModel.MainFormHeight;
end;

function TSettingsController.GetMainFormLeft: integer;
begin
  Result := FModel.MainFormLeft;
end;

function TSettingsController.GetMainFormTop: integer;
begin
  Result := FModel.MainFormTop;
end;

function TSettingsController.GetMainFormWidth: integer;
begin
  Result := FModel.MainFormWidth;
end;

procedure TSettingsController.SetHeapWidth(const Value: integer);
begin
  FModel.HeapWidth := Value;
end;

procedure TSettingsController.SetMainFormHeight(const Value: integer);
begin
  FModel.MainFormHeight := Value;
end;

procedure TSettingsController.SetMainFormLeft(const Value: integer);
begin
  FModel.MainFormLeft := Value;
end;

procedure TSettingsController.SetMainFormTop(const Value: integer);
begin
  FModel.MainFormTop := Value;
end;

procedure TSettingsController.SetMainFormWidth(const Value: integer);
begin
  FModel.MainFormWidth := Value;
end;

procedure TSettingsController.Save;
begin
  FModel.Save();
end;

end.
