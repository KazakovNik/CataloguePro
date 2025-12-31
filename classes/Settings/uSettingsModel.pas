unit uSettingsModel;

interface

uses
  SysUtils;

type
  TSettings = class(TObject)
  private
    procedure LoadFromFile;
  private
    FFilePath: string;
    FMainFormLeft: integer;
    FMainFormWidth: integer;
    FMainFormTop: integer;
    FMainFormHeight: integer;
    FHeapWidth: integer;
    FFileSaveDirectory: string;
    FFileOpenDirectory: string;
    FRecentFiles: string;
    FMaxCountFileHistopy: Integer;
    FWindowState: Integer;
  public
    constructor Create(const aFilePath: string); overload;
    procedure Save();

    property MainFormLeft: integer read FMainFormLeft write FMainFormLeft;
    property MainFormTop: integer read FMainFormTop write FMainFormTop;
    property MainFormHeight: integer read FMainFormHeight write FMainFormHeight;
    property MainFormWidth: integer read FMainFormWidth write FMainFormWidth;
    property HeapWidth: integer read FHeapWidth write FHeapWidth;
    property FileOpenDirectory: string read FFileOpenDirectory write FFileOpenDirectory;
    property FileSaveDirectory: string read FFileSaveDirectory write FFileSaveDirectory;
    property RecentFiles: string read FRecentFiles write FRecentFiles;
    property MaxCountFileHistopy: Integer read FMaxCountFileHistopy write FMaxCountFileHistopy;
    property WindowState: Integer read FWindowState write FWindowState;
  end;

implementation

uses
  System.IniFiles;

const
  cRoot = 'Settings';

constructor TSettings.Create(const aFilePath: string);
begin
  inherited Create;

  FFilePath := aFilePath;
  LoadFromFile();
end;

procedure TSettings.LoadFromFile;
var
  vIniFile: TIniFile;
begin
  if not FileExists(FFilePath) then
    Exit;

  vIniFile := TIniFile.Create(FFilePath);
  try
    MainFormLeft := vIniFile.ReadInteger(cRoot, 'MainFormLeft', 0);
    MainFormTop := vIniFile.ReadInteger(cRoot, 'MainFormTop', 0);
    MainFormHeight := vIniFile.ReadInteger(cRoot, 'MainFormHeight', 300);
    MainFormWidth := vIniFile.ReadInteger(cRoot, 'MainFormWidth', 650);
    HeapWidth := vIniFile.ReadInteger(cRoot, 'HeapWidth', 300);
    FileOpenDirectory := vIniFile.ReadString(cRoot, 'FileOpenDirectory', EmptyStr);
    FileSaveDirectory := vIniFile.ReadString(cRoot, 'FileSaveDirectory', EmptyStr);
    RecentFiles := vIniFile.ReadString(cRoot, 'RecentFiles', EmptyStr);
    MaxCountFileHistopy := vIniFile.ReadInteger(cRoot, 'MaxCountFileHistopy', 10);
    FWindowState := vIniFile.ReadInteger(cRoot, 'FWindowState', 0);
  finally
    FreeAndNil(vIniFile);
  end;
end;

procedure TSettings.Save;
var
  vIniFile: TIniFile;
begin
  vIniFile := TIniFile.Create(FFilePath);
  try
    vIniFile.WriteInteger(cRoot, 'MainFormLeft', MainFormLeft);
    vIniFile.WriteInteger(cRoot, 'MainFormTop', MainFormTop);
    vIniFile.WriteInteger(cRoot, 'MainFormHeight', MainFormHeight);
    vIniFile.WriteInteger(cRoot, 'MainFormWidth', MainFormWidth);
    vIniFile.WriteInteger(cRoot, 'HeapWidth', HeapWidth);
    vIniFile.WriteString(cRoot, 'FileOpenDirectory', FileOpenDirectory);
    vIniFile.WriteString(cRoot, 'FileSaveDirectory', FileSaveDirectory);
    vIniFile.WriteString(cRoot, 'RecentFiles', RecentFiles);
    vIniFile.WriteInteger(cRoot, 'MaxCountFileHistopy', MaxCountFileHistopy);
    vIniFile.WriteInteger(cRoot, 'FWindowState', FWindowState);
  finally
    FreeAndNil(vIniFile);
  end;
end;

end.
