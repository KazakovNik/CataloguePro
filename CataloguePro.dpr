program CataloguePro;

uses
  Vcl.Forms,
  System.SysUtils,
  uMain in 'forms\uMain.pas' {FormMain},
  uFileContentModel in 'classes\FileLoader\uFileContentModel.pas',
  uFileContentController in 'classes\FileLoader\uFileContentController.pas',
  uSettingsModel in 'classes\Settings\uSettingsModel.pas',
  uSettingsController in 'classes\Settings\uSettingsController.pas';

{$R *.res}

//var
//  Settings: TSettingsController;
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

//
//  if Settings.MainForm = EmptyStr then
//
//  else
//  begin
//    StringToComponentProc(Settings.MainForm);
//  end;

  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
