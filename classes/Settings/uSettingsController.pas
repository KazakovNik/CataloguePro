unit uSettingsController;

interface

uses
  uSettingsModel, SysUtils, Vcl.Forms;

type
  TSettingsController = class;
  TUpdateEvent = procedure(Sender: TSettingsController) of object;

  TSettingsController = class(TObject)
  private
    FModel: TSettings;
    FFilePath: string;
    FOnUpdate: TUpdateEvent;
  private
    function GetMainFormHeight: integer;
    function GetMainFormLeft: integer;
    function GetMainFormTop: integer;
    function GetMainFormWidth: integer;
    procedure SetMainFormHeight(const aValue: integer);
    procedure SetMainFormLeft(const aValue: integer);
    procedure SetMainFormTop(const aValue: integer);
    procedure SetMainFormWidth(const aValue: integer);
    function GetHeapWidth: integer;
    procedure SetHeapWidth(const aValue: integer);
    function GetFileOpenDirectory: string;
    procedure SetFileOpenDirectory(const aValue: string);
    function GetFileSaveDirectory: string;
    procedure SetFileSaveDirectory(const aValue: string);
    function GetRecentFiles: string;
    procedure SetRecentFiles(const aValue: string);
    procedure SetMaxCountFileHistopy(const aValue: integer);
    function GetMaxCountFileHistopy: integer;
    procedure DoUpdate;
    function GetLoggerFileName: string;
    function GetWindowState: integer;
    procedure SetWindowState(const aValue: integer);
  public
    constructor Create(const aAFilePath: string);
    destructor Destroy; override;

    procedure Save;

    property OnUpdate: TUpdateEvent read FOnUpdate write FOnUpdate;

    property MainFormLeft: integer read GetMainFormLeft write SetMainFormLeft;
    property MainFormTop: integer read GetMainFormTop write SetMainFormTop;
    property MainFormHeight: integer read GetMainFormHeight write SetMainFormHeight;
    property MainFormWidth: integer read GetMainFormWidth write SetMainFormWidth;
    property HeapWidth: integer read GetHeapWidth write SetHeapWidth;
    property FileOpenDirectory: string read GetFileOpenDirectory write SetFileOpenDirectory;
    property FileSaveDirectory: string read GetFileSaveDirectory write SetFileSaveDirectory;
    property RecentFiles: string read GetRecentFiles write SetRecentFiles;
    property MaxCountFileHistopy: integer read GetMaxCountFileHistopy write SetMaxCountFileHistopy;
    property LoggerFileName: string read GetLoggerFileName;
    property WindowState: integer read GetWindowState write SetWindowState;
  end;

const
  cSettingsFileName = 'settings.ini';

implementation

const
  cDateTimeFormat = 'yyyymmdd';
  cLogName = 'log_%s.txt';

constructor TSettingsController.Create(const aAFilePath: string);
begin
  FFilePath := aAFilePath;
  FModel := TSettings.Create(FFilePath);
  if not FileExists(FFilePath) then
  begin
    MainFormHeight := Round(Screen.Height / 100 * 50);
    MainFormWidth := Round(Screen.Width / 100 * 50);
    MainFormLeft := (Screen.Width div 2) - (MainFormWidth div 2);
    MainFormTop := (Screen.Height div 2) - (MainFormHeight div 2);
    HeapWidth := 300;
    WindowState := 0; //wsNormal
    MaxCountFileHistopy := 10;
  end;
end;

destructor TSettingsController.Destroy;
begin
  FreeAndNil(FModel);
  inherited Destroy;
end;

procedure TSettingsController.DoUpdate;
begin
  if Assigned(FOnUpdate) then
    FOnUpdate(Self);
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

function TSettingsController.GetLoggerFileName: string;
begin
  Result :=
    ExtractFilePath(Application.ExeName)
     + Format(cLogName, [FormatDateTime(cDateTimeFormat, Now())]);
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

function TSettingsController.GetMaxCountFileHistopy: integer;
begin
  Result := FModel.MaxCountFileHistopy;
end;

function TSettingsController.GetRecentFiles: string;
begin
  Result := FModel.RecentFiles;
end;

function TSettingsController.GetWindowState: integer;
begin
  Result := FModel.WindowState;
end;

procedure TSettingsController.SetFileOpenDirectory(const aValue: string);
begin
  FModel.FileOpenDirectory := aValue;
end;

procedure TSettingsController.SetFileSaveDirectory(const aValue: string);
begin
  FModel.FileSaveDirectory := aValue;
end;

procedure TSettingsController.SetHeapWidth(const aValue: integer);
begin
  FModel.HeapWidth := aValue;
end;

procedure TSettingsController.SetMainFormHeight(const aValue: integer);
begin
  FModel.MainFormHeight := aValue;
end;

procedure TSettingsController.SetMainFormLeft(const aValue: integer);
begin
  FModel.MainFormLeft := aValue;
end;

procedure TSettingsController.SetMainFormTop(const aValue: integer);
begin
  FModel.MainFormTop := aValue;
end;

procedure TSettingsController.SetMainFormWidth(const aValue: integer);
begin
  FModel.MainFormWidth := aValue;
end;

procedure TSettingsController.SetMaxCountFileHistopy(const aValue: integer);
begin
  FModel.MaxCountFileHistopy := aValue;
  DoUpdate;
end;

procedure TSettingsController.SetRecentFiles(const aValue: string);
begin
  FModel.RecentFiles := aValue;
end;

procedure TSettingsController.SetWindowState(const aValue: integer);
begin
  FModel.WindowState := aValue;
end;

procedure TSettingsController.Save;
begin
  FModel.Save();
end;

end.
