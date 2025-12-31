unit uSettingsFacade;

interface

uses
  uFormSettings, uSettingsController;

type
  TSettingsFacade = class
  private
    FSettings: TSettingsController;
    FOnOpen: TOpenEvent;
  public
    constructor Create(Settings: TSettingsController);
    procedure ShowModal;

    property OnOpen: TOpenEvent read FOnOpen write FOnOpen;
  end;

implementation

uses
  System.SysUtils, Vcl.StdCtrls, System.UITypes, uFramePatch;

{ TSettingsFacade }

constructor TSettingsFacade.Create(Settings: TSettingsController);
begin
  FSettings := Settings;
end;

procedure TSettingsFacade.ShowModal;
var
  Form: TFormSettings;
begin
  Form := TFormSettings.Create(nil);
  try
    Form.OnLastFolderOpen := OnOpen;
    Form.OnLastFolderSaveOpen := OnOpen;
    Form.OnLogFileOpen := OnOpen;
    Form.frmLastOpen.edtPatch.Text := FSettings.FileOpenDirectory;
    Form.frmLastSave.edtPatch.Text := FSettings.FileSaveDirectory;
    Form.frmLogFile.edtPatch.Text := FSettings.LoggerFileName;
    Form.edtMaxCountFileHistopy.Text := IntToStr(FSettings.MaxCountFileHistopy);

    if Form.ShowModal = mrOk then
    begin
      FSettings.FileOpenDirectory := Form.frmLastOpen.edtPatch.Text;
      FSettings.FileSaveDirectory := Form.frmLastSave.edtPatch.Text;
      FSettings.MaxCountFileHistopy := StrToInt(Form.edtMaxCountFileHistopy.Text);
    end;
  finally
    Form.Free;
  end;
end;

end.
