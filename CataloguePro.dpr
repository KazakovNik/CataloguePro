program CataloguePro;

uses
  Vcl.Forms,
  System.SysUtils,
  uMain in 'forms\uMain.pas' {FormMain},
  uHeapController in 'classes\Heap\uHeapController.pas',
  uSettingsModel in 'classes\Settings\uSettingsModel.pas',
  uSettingsController in 'classes\Settings\uSettingsController.pas',
  uTreeController in 'classes\Tree\uTreeController.pas',
  uNodeModel in 'classes\Tree\uNodeModel.pas';

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
