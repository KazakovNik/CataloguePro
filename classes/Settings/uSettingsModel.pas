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
    constructor Create(const FilePath: string); overload;
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

constructor TSettings.Create(const FilePath: string);
begin
  inherited Create;

  FFilePath := FilePath;
  LoadFromFile();
end;

procedure TSettings.LoadFromFile;
var
  IniFile: TIniFile;
begin
  if not FileExists(FFilePath) then
    Exit;

  IniFile := TIniFile.Create(FFilePath);
  try
    MainFormLeft := IniFile.ReadInteger(cRoot, 'MainFormLeft', 0);
    MainFormTop := IniFile.ReadInteger(cRoot, 'MainFormTop', 0);
    MainFormHeight := IniFile.ReadInteger(cRoot, 'MainFormHeight', 300);
    MainFormWidth := IniFile.ReadInteger(cRoot, 'MainFormWidth', 650);
    HeapWidth := IniFile.ReadInteger(cRoot, 'HeapWidth', 300);
    FileOpenDirectory := IniFile.ReadString(cRoot, 'FileOpenDirectory', EmptyStr);
    FileSaveDirectory := IniFile.ReadString(cRoot, 'FileSaveDirectory', EmptyStr);
    RecentFiles := IniFile.ReadString(cRoot, 'RecentFiles', EmptyStr);
    MaxCountFileHistopy := IniFile.ReadInteger(cRoot, 'MaxCountFileHistopy', 10);
    FWindowState := IniFile.ReadInteger(cRoot, 'FWindowState', 0);
  finally
    FreeAndNil(IniFile);
  end;
end;

procedure TSettings.Save;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(FFilePath);
  try
    IniFile.WriteInteger(cRoot, 'MainFormLeft', MainFormLeft);
    IniFile.WriteInteger(cRoot, 'MainFormTop', MainFormTop);
    IniFile.WriteInteger(cRoot, 'MainFormHeight', MainFormHeight);
    IniFile.WriteInteger(cRoot, 'MainFormWidth', MainFormWidth);
    IniFile.WriteInteger(cRoot, 'HeapWidth', HeapWidth);
    IniFile.WriteString(cRoot, 'FileOpenDirectory', FileOpenDirectory);
    IniFile.WriteString(cRoot, 'FileSaveDirectory', FileSaveDirectory);
    IniFile.WriteString(cRoot, 'RecentFiles', RecentFiles);
    IniFile.WriteInteger(cRoot, 'MaxCountFileHistopy', MaxCountFileHistopy);
    IniFile.WriteInteger(cRoot, 'FWindowState', FWindowState);
  finally
    FreeAndNil(IniFile);
  end;
end;

end.
