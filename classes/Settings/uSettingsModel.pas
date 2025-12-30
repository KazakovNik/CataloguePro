unit uSettingsModel;

interface

uses
  Classes, SysUtils;

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
    MainFormLeft := IniFile.ReadInteger('Settings', 'MainFormLeft', 0);
    MainFormTop := IniFile.ReadInteger('Settings', 'MainFormTop', 0);
    MainFormHeight := IniFile.ReadInteger('Settings', 'MainFormHeight', 300);
    MainFormWidth := IniFile.ReadInteger('Settings', 'MainFormWidth', 650);
    HeapWidth := IniFile.ReadInteger('Settings', 'HeapWidth', 300);
    FileOpenDirectory := IniFile.ReadString('Settings', 'FileOpenDirectory', EmptyStr);
    FileSaveDirectory := IniFile.ReadString('Settings', 'FileSaveDirectory', EmptyStr);
    RecentFiles := IniFile.ReadString('Settings', 'RecentFiles', EmptyStr);
    MaxCountFileHistopy := IniFile.ReadInteger('Settings', 'MaxCountFileHistopy', 10);
    FWindowState := IniFile.ReadInteger('Settings', 'FWindowState', 0);
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
    IniFile.WriteInteger('Settings', 'MainFormLeft', MainFormLeft);
    IniFile.WriteInteger('Settings', 'MainFormTop', MainFormTop);
    IniFile.WriteInteger('Settings', 'MainFormHeight', MainFormHeight);
    IniFile.WriteInteger('Settings', 'MainFormWidth', MainFormWidth);
    IniFile.WriteInteger('Settings', 'HeapWidth', HeapWidth);
    IniFile.WriteString('Settings', 'FileOpenDirectory', FileOpenDirectory);
    IniFile.WriteString('Settings', 'FileSaveDirectory', FileSaveDirectory);
    IniFile.WriteString('Settings', 'RecentFiles', RecentFiles);
    IniFile.WriteInteger('Settings', 'MaxCountFileHistopy', MaxCountFileHistopy);
    IniFile.WriteInteger('Settings', 'FWindowState', FWindowState);
  finally
    FreeAndNil(IniFile);
  end;
end;

end.
