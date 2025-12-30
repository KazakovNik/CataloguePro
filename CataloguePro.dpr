program CataloguePro;

uses
  Vcl.Forms,
  System.SysUtils,
  uMain in 'forms\uMain.pas' {FormMain},
  uHeapController in 'classes\Heap\uHeapController.pas',
  uSettingsModel in 'classes\Settings\uSettingsModel.pas',
  uSettingsController in 'classes\Settings\uSettingsController.pas',
  uTreeController in 'classes\Tree\uTreeController.pas',
  uNodeModel in 'classes\Tree\uNodeModel.pas',
  uMainFacade in 'classes\uMainFacade.pas',
  uRecentFilesController in 'classes\RecentFiles\uRecentFilesController.pas',
  uRecentFilesModel in 'classes\RecentFiles\uRecentFilesModel.pas',
  uLogger in 'classes\Log\uLogger.pas',
  uILogger in 'classes\Log\uILogger.pas',
  uUserInfo in 'classes\uUserInfo.pas',
  uFormAbout in 'forms\uFormAbout.pas' {FormAbout},
  uAboutFacade in 'classes\uAboutFacade.pas',
  uFormDialogBase in 'forms\uFormDialogBase.pas' {FormDialogBase},
  uFormSettings in 'forms\uFormSettings.pas' {FormSettings},
  uSettingsFacade in 'classes\uSettingsFacade.pas',
  uFramePatch in 'forms\uFramePatch.pas' {Frame1: TFrame},
  uFormInputBox in 'forms\uFormInputBox.pas' {FormInputBox};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
