{%MainUnit ../LMessages.pp}

{*****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

const
  CM_BASE                 = $B000;
  CM_LCLOFFSET            = $100;
  CM_ACTIVATE             = CM_BASE + 0;
  CM_DEACTIVATE           = CM_BASE + 1;
  CM_GOTFOCUS             = CM_BASE + 2 unimplemented;
  CM_LOSTFOCUS            = CM_BASE + 3 unimplemented;
  CM_CANCELMODE           = CM_BASE + 4 unimplemented;
  CM_DIALOGKEY            = CM_BASE + 5 unimplemented;
  CM_DIALOGCHAR           = CM_BASE + 6 unimplemented;
  CM_FOCUSCHANGED         = CM_BASE + 7;
  CM_PARENTFONTCHANGED    = CM_BASE + 8;
  CM_PARENTCOLORCHANGED   = CM_BASE + 9;
  CM_HITTEST              = CM_BASE + 10;
  CM_VISIBLECHANGED       = CM_BASE + 11;
  CM_ENABLEDCHANGED       = CM_BASE + 12;
  CM_COLORCHANGED         = CM_BASE + 13;
  CM_FONTCHANGED          = CM_BASE + 14;
  CM_CURSORCHANGED        = CM_BASE + 15;
  CM_CTL3DCHANGED         = CM_BASE + 16 unimplemented;
  CM_PARENTCTL3DCHANGED   = CM_BASE + 17 unimplemented;
  CM_TEXTCHANGED          = CM_BASE + 18;
  CM_MOUSEENTER           = CM_BASE + 19;
  CM_MOUSELEAVE           = CM_BASE + 20;
  CM_MENUCHANGED          = CM_BASE + 21;
  CM_APPKEYDOWN           = CM_BASE + 22 unimplemented;
  CM_APPSYSCOMMAND        = CM_BASE + 23;
  CM_BUTTONPRESSED        = CM_BASE + 24;
  CM_SHOWINGCHANGED       = CM_BASE + 25;
  CM_ENTER                = CM_BASE + 26;
  CM_EXIT                 = CM_BASE + 27;
  CM_DESIGNHITTEST        = CM_BASE + 28; // implemented by custom controls
  CM_ICONCHANGED          = CM_BASE + 29;
  CM_WANTSPECIALKEY       = CM_BASE + 30;
  CM_INVOKEHELP           = CM_BASE + 31 unimplemented;
  CM_WINDOWHOOK           = CM_BASE + 32 unimplemented;
  CM_RELEASE              = CM_BASE + 33;
  CM_SHOWHINTCHANGED      = CM_BASE + 34;
  CM_PARENTSHOWHINTCHANGED= CM_BASE + 35;
  CM_SYSCOLORCHANGE       = CM_BASE + 36 unimplemented;
  CM_WININICHANGE         = CM_BASE + 37 unimplemented;
  CM_FONTCHANGE           = CM_BASE + 38;
  CM_TIMECHANGE           = CM_BASE + 39 unimplemented;
  CM_TABSTOPCHANGED       = CM_BASE + 40;
  CM_UIACTIVATE           = CM_BASE + 41;
  CM_UIDEACTIVATE         = CM_BASE + 42 unimplemented;
  CM_DOCWINDOWACTIVATE    = CM_BASE + 43 unimplemented;
  CM_CONTROLLISTCHANGE    = CM_BASE + 44;
  CM_GETDATALINK          = CM_BASE + 45;
  CM_CHILDKEY             = CM_BASE + 46;
  CM_DRAG                 = CM_BASE + 47 unimplemented; //Method TControl.DoDragMsg()
  CM_HINTSHOW             = CM_BASE + 48;
  CM_DIALOGHANDLE         = CM_BASE + 49 unimplemented;
  CM_ISTOOLCONTROL        = CM_BASE + 50 unimplemented;
  CM_RECREATEWND          = CM_BASE + 51 unimplemented;
  CM_INVALIDATE           = CM_BASE + 52 unimplemented;
  CM_SYSFONTCHANGED       = CM_BASE + 53;
  CM_CONTROLCHANGE        = CM_BASE + 54;
  CM_CHANGED              = CM_BASE + 55;
  CM_DOCKCLIENT           = CM_BASE + 56 unimplemented; //Method TWinControl.DoDockClientMsg()
  CM_UNDOCKCLIENT         = CM_BASE + 57 unimplemented; //Method TWinControl.DoUndockClientMsg()
  CM_FLOAT                = CM_BASE + 58 unimplemented; //Method TControl.DoFloatMsg()
  CM_BORDERCHANGED        = CM_BASE + 59;
  CM_BIDIMODECHANGED      = CM_BASE + 60;
  CM_PARENTBIDIMODECHANGED= CM_BASE + 61;
  CM_ALLCHILDRENFLIPPED   = CM_BASE + 62;
  CM_ACTIONUPDATE         = CM_BASE + 63;
  CM_ACTIONEXECUTE        = CM_BASE + 64;
  CM_HINTSHOWPAUSE        = CM_BASE + 65;
  CM_DOCKNOTIFICATION     = CM_BASE + 66;
  CM_MOUSEWHEEL           = CM_BASE + 67;
  // unsupported at the moment
  CM_ISSHORTCUT                  = CM_BASE + 68 unimplemented;
  CM_UPDATEACTIONS               = CM_BASE + 69 unimplemented;
  CM_INVALIDATEDOCKHOST          = CM_BASE + 70 unimplemented;
  CM_SETACTIVECONTROL            = CM_BASE + 71 unimplemented;
  CM_POPUPHWNDDESTROY            = CM_BASE + 72 unimplemented;
  CM_CREATEPOPUP                 = CM_BASE + 73 unimplemented;
  CM_DESTROYHANDLE               = CM_BASE + 74 unimplemented;
  CM_MOUSEACTIVATE               = CM_BASE + 75 unimplemented;
  CM_CONTROLLISTCHANGING         = CM_BASE + 76 unimplemented;
  CM_BUFFEREDPRINTCLIENT         = CM_BASE + 77 unimplemented;
  CM_UNTHEMECONTROL              = CM_BASE + 78 unimplemented;
  CM_DOUBLEBUFFEREDCHANGED       = CM_BASE + 79 unimplemented;
  CM_PARENTDOUBLEBUFFEREDCHANGED = CM_BASE + 80 unimplemented;
  CM_THEMECHANGED                = CM_BASE + 81 unimplemented;
  CM_GESTURE                     = CM_BASE + 82 unimplemented;
  CM_CUSTOMGESTURESCHANGED       = CM_BASE + 83 unimplemented;
  CM_GESTUREMANAGERCHANGED       = CM_BASE + 84 unimplemented;
  CM_STANDARDGESTURESCHANGED     = CM_BASE + 85 unimplemented;
  CM_INPUTLANGCHANGE             = CM_BASE + 86 unimplemented;
  CM_TABLETOPTIONSCHANGED        = CM_BASE + 87 unimplemented;
  CM_PARENTTABLETOPTIONSCHANGED  = CM_BASE + 88 unimplemented;
  // LCL only
  CM_APPSHOWBTNGLYPHCHANGED  = CM_BASE + CM_LCLOFFSET + 68;
  CM_APPSHOWMENUGLYPHCHANGED = CM_BASE + CM_LCLOFFSET + 69;

  CN_BASE              = $BC00;
  CN_CHARTOITEM        = CN_BASE + LM_CHARTOITEM;
  CN_COMMAND           = CN_BASE + LM_COMMAND;
  CN_COMPAREITEM       = CN_BASE + LM_COMPAREITEM;
  CN_CTLCOLORBTN       = CN_BASE + LM_CTLCOLORBTN;
  CN_CTLCOLORDLG       = CN_BASE + LM_CTLCOLORDLG;
  CN_CTLCOLOREDIT      = CN_BASE + LM_CTLCOLOREDIT;
  CN_CTLCOLORLISTBOX   = CN_BASE + LM_CTLCOLORLISTBOX;
  CN_CTLCOLORMSGBOX    = CN_BASE + LM_CTLCOLORMSGBOX;
  CN_CTLCOLORSCROLLBAR = CN_BASE + LM_CTLCOLORSCROLLBAR;
  CN_CTLCOLORSTATIC    = CN_BASE + LM_CTLCOLORSTATIC;
  CN_DELETEITEM        = CN_BASE + LM_DELETEITEM;
  CN_DRAWITEM          = CN_BASE + LM_DRAWITEM;
  CN_HSCROLL           = CN_BASE + LM_HSCROLL;
  CN_MEASUREITEM       = CN_BASE + LM_MEASUREITEM;
  CN_PARENTNOTIFY      = CN_BASE + LM_PARENTNOTIFY;
  CN_VKEYTOITEM        = CN_BASE + LM_VKEYTOITEM;
  CN_VSCROLL           = CN_BASE + LM_VSCROLL;
  CN_KEYDOWN           = CN_BASE + LM_KEYDOWN;
  CN_KEYUP             = CN_BASE + LM_KEYUP;
  CN_CHAR              = CN_BASE + LM_CHAR;
  CN_SYSKEYUP          = CN_BASE + LM_SYSKEYUP;
  CN_SYSKEYDOWN        = CN_BASE + LM_SYSKEYDOWN;
  CN_SYSCHAR           = CN_BASE + LM_SYSCHAR;
  CN_NOTIFY            = CN_BASE + LM_NOTIFY;

