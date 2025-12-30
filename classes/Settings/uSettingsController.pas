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
    function GetFileOpenDirectory: string;
    procedure SetFileOpenDirectory(const Value: string);
    function GetFileSaveDirectory: string;
    procedure SetFileSaveDirectory(const Value: string);
  public
    constructor Create(const AFilePath: string);
    destructor Destroy; override;

    procedure Save;

    property MainFormLeft: integer read GetMainFormLeft write SetMainFormLeft;
    property MainFormTop: integer read GetMainFormTop write SetMainFormTop;
    property MainFormHeight: integer read GetMainFormHeight write SetMainFormHeight;
    property MainFormWidth: integer read GetMainFormWidth write SetMainFormWidth;
    property HeapWidth: integer read GetHeapWidth write SetHeapWidth;
    property FileOpenDirectory: string read GetFileOpenDirectory write SetFileOpenDirectory;
    property FileSaveDirectory: string read GetFileSaveDirectory write SetFileSaveDirectory;
  end;

implementation

constructor TSettingsController.Create(const AFilePath: string);
begin
  FFilePath := AFilePath;
  FModel := TSettings.Create(FFilePath);
  if not FileExists(FFilePath) then
  begin
    MainFormHeight := Round(Screen.Height / 100 * 50);
    MainFormWidth := Round(Screen.Width / 100 * 50);
    MainFormLeft := (Screen.Width div 2) - (Application.MainForm.ClientWidth div 2);
    MainFormTop := (Screen.Height div 2) - (Application.MainForm.ClientHeight div 2);
    HeapWidth := 300;
  end;
end;

destructor TSettingsController.Destroy;
begin
  FreeAndNil(FModel);
  inherited Destroy;
end;

function TSettingsController.GetFileOpenDirectory: string;
begin
  Result := FModel.FileOpenDirectory;
  if Result = EmptyStr then
   Result := ExtractFilePath(Application.ExeName);
end;

function TSettingsController.GetFileSaveDirectory: string;
begin
  Result := FModel.FileSaveDirectory;
  if Result = EmptyStr then
   Result := ExtractFilePath(Application.ExeName);
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

procedure TSettingsController.SetFileOpenDirectory(const Value: string);
begin
  FModel.FileOpenDirectory := Value;
end;

procedure TSettingsController.SetFileSaveDirectory(const Value: string);
begin
  FModel.FileSaveDirectory := Value;
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
