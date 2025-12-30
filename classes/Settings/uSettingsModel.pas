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
  public
    constructor Create(const FilePath: string); overload;
    procedure Save();

    property MainFormLeft: integer read FMainFormLeft write FMainFormLeft;
    property MainFormTop: integer read FMainFormTop write FMainFormTop;
    property MainFormHeight: integer read FMainFormHeight write FMainFormHeight;
    property MainFormWidth: integer read FMainFormWidth write FMainFormWidth;
    property HeapWidth: integer read FHeapWidth write FHeapWidth;
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
  finally
    FreeAndNil(IniFile);
  end;
end;

end.
