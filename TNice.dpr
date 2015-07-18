program TNice;

{$R 'CRules.res' 'CRules.rc'}

uses
  Forms,
  untEditor in 'untEditor.pas' {frmEditor},
  untOptions in 'untOptions.pas' {frmMain},
  NiceTypes in 'NiceTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmEditor, frmEditor);
  Application.Run;
end.
