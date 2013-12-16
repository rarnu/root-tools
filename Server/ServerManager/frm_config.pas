unit frm_config;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

    { TFormConfig }

    TFormConfig = class(TForm)
        btnCancel: TButton;
        btnOk: TButton;
        edtServerAddress: TEdit;
        edtRootPassword: TEdit;
        edtServerPassword: TEdit;
        edtServerUser: TEdit;
        lblServerAddress: TLabel;
        lblRootPassword: TLabel;
        lblServerPassword: TLabel;
        lblServerUser: TLabel;
        Panel1: TPanel;
        Panel2: TPanel;
        Panel3: TPanel;
        Panel4: TPanel;
        Panel5: TPanel;
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    FormConfig: TFormConfig;

implementation

{$R *.lfm}

end.

