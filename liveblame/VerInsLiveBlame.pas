{******************************************************************************}
{                                                                              }
{ RAD Studio Version Insight                                                   }
{                                                                              }
{ The contents of this file are subject to the Mozilla Public License          }
{ Version 1.1 (the "License"); you may not use this file except in compliance  }
{ with the License. You may obtain a copy of the License at                    }
{ http://www.mozilla.org/MPL/                                                  }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ The Original Code is VerInsLiveBlame.pas.                                    }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2006 - 2011 Uwe Schuster.   }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit VerInsLiveBlame;

interface

uses
  Windows, Messages, SysUtils, Classes, Contnrs, Generics.Collections, Graphics,
  Controls, Buttons, ExtCtrls, ComCtrls, Forms, ActnList, Menus, Themes,
  ActiveX, TypInfo, ToolsAPI, DockForm, FileHistoryAPI,
  DiffUnit, HashUnit;

type
  TLiveBlameWizard = class(TInterfacedObject, IOTAWizard, IOTANotifier, IOTAEditorNotifier)
  private
    NotifierIndex: Integer;
    FPanelList: TList;
  public
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Modified;
    procedure Destroyed;

    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;

    { IOTAEditorNotifier }
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure ViewActivated(const View: IOTAEditView);

    constructor Create;
    destructor Destroy; override;

    procedure RemovePanel(AObject: TObject);
  end;

procedure Register;

implementation

uses
  {$IFDEF SVNINTERNAL}
  SvnIDEClient, SvnClient, SvnIDETypes,
  {$ENDIF SVNINTERNAL}
  VerInsIDETypes;

procedure Register;
begin
  TLiveBlameWizard.Create;
end;

type
  TLiveBlamePaintBox = class(TCustomControl)
  private
    FOnPaint: TNotifyEvent;
    FOnHintMessage: TWndMethod;
  protected
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure Paint; override;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
    procedure InvalidateControl;
  published
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnHintMessage: TWndMethod read FOnHintMessage write FOnHintMessage;
  end;

procedure TLiveBlamePaintBox.CMHintShow(var Msg: TMessage);
begin
  if Assigned(FOnHintMessage) then
    FOnHintMessage(Msg)
  else
    inherited;
end;

constructor TLiveBlamePaintBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable, csOpaque];
end;

procedure TLiveBlamePaintBox.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

procedure TLiveBlamePaintBox.InvalidateControl;
begin
  InvalidateRect(Handle, nil, False);
end;

procedure TLiveBlamePaintBox.Paint;
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self)
end;

const
  WM_BLAME_SHOW = WM_USER + 1;
  WM_BLAME_UPDATE = WM_USER + 2;

type
  TJVCSLineHistoryRevision = class(TObject)
  private
    FDate: TDateTime;
    FDateStr: string;
    FRevisionStr: string;
    FOrgUserStr: string;
    FUserStr: string;
    FComment: string;
  public
    property Date: TDateTime read FDate;
    property DateStr: string read FDateStr;
    property OrgUserStr: string read FOrgUserStr;
    property RevisionStr: string read FRevisionStr;
    property UserStr: string read FUserStr;
    property Comment: string read FComment;
  end;

  TRevisionColor = class(TObject)
  private
    FDateColor: TColor;
    FRevisionColor: TColor;
    FLineHistoryRevision: TJVCSLineHistoryRevision;
    FUserColor: TColor;
  public
    constructor Create(ALineHistoryRevision: TJVCSLineHistoryRevision);
    property DateColor: TColor read FDateColor write FDateColor;
    property RevisionColor: TColor read FRevisionColor write FRevisionColor;
    property LineHistoryRevision: TJVCSLineHistoryRevision read FLineHistoryRevision;
    property UserColor: TColor read FUserColor write FUserColor;
  end;

  TJVCSLineHistoryUserSettingsItem = class(TPersistent)
  private
    FColor: TColor;
    FUserName: string;
    FVisibleName: string;
  public
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    property Color: TColor read FColor write FColor;
    property UserName: string read FUserName write FUserName;
    property VisibleName: string read FVisibleName write FVisibleName;
  end;

  TJVCSLineHistoryUserSettings = class(TPersistent)
  private
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TJVCSLineHistoryUserSettingsItem;
  public
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TJVCSLineHistoryUserSettingsItem;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    function IndexOfUser(const AUserName: string): Integer;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TJVCSLineHistoryUserSettingsItem read GetItems; default;
  end;

  TJVCSLineHistorySettings = class(TObject)
  private
    FColorBarOrderList: TList;
    FDateEndColor: TColor;
    FDateFormat: string;
    FDateStartColor: TColor;
    FLineColorMode: Integer;
    FPaintMethod: Integer;
    FRevisionEndColor: TColor;
    FRevisionStartColor: TColor;
    FShowLineNumbers: Boolean;
    FShowRevisionInfoColor: Boolean;
    FShowRevisionInfoText: Boolean;
    FShowDateInfoColor: Boolean;
    FShowDateInfoText: Boolean;
    FShowUserInfoColor: Boolean;
    FShowUserInfoText: Boolean;
    {$IFDEF LINEINFOEX}
    FShowRevisionCountInfoColor: Boolean;
    FShowRevisionCountInfoText: Boolean;
    FShowFirstRevisionInfoColor: Boolean;
    FShowFirstRevisionInfoText: Boolean;
    {$ENDIF LINEINFOEX}
    FShowOrderList: TList;
    FStaticUserColorList: TStringList;
    FSuppressRevisionTextZeroDot: Boolean;
    FUserSettingsList: TJVCSLineHistoryUserSettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(AValue: TJVCSLineHistorySettings);
    property ColorBarOrderList: TList read FColorBarOrderList;
    property DateEndColor: TColor read FDateEndColor write FDateEndColor;
    property DateFormat: string read FDateFormat write FDateFormat;
    property DateStartColor: TColor read FDateStartColor write FDateStartColor;
    property LineColorMode: Integer read FLineColorMode write FLineColorMode;
    property PaintMethod: Integer read FPaintMethod write FPaintMethod;
    property RevisionEndColor: TColor read FRevisionEndColor write FRevisionEndColor;
    property RevisionStartColor: TColor read FRevisionStartColor write FRevisionStartColor;
    property ShowLineNumbers: Boolean read FShowLineNumbers write FShowLineNumbers;
    property ShowRevisionInfoColor: Boolean read FShowRevisionInfoColor write FShowRevisionInfoColor;
    property ShowRevisionInfoText: Boolean read FShowRevisionInfoText write FShowRevisionInfoText;
    property ShowDateInfoColor: Boolean read FShowDateInfoColor write FShowDateInfoColor;
    property ShowDateInfoText: Boolean read FShowDateInfoText write FShowDateInfoText;
    property ShowUserInfoColor: Boolean read FShowUserInfoColor write FShowUserInfoColor;
    property ShowUserInfoText: Boolean read FShowUserInfoText write FShowUserInfoText;
    {$IFDEF LINEINFOEX}
    property ShowRevisionCountInfoColor: Boolean read FShowRevisionCountInfoColor write FShowRevisionCountInfoColor;
    property ShowRevisionCountInfoText: Boolean read FShowRevisionCountInfoText write FShowRevisionCountInfoText;
    property ShowFirstRevisionInfoColor: Boolean read FShowFirstRevisionInfoColor write FShowFirstRevisionInfoColor;
    property ShowFirstRevisionInfoText: Boolean read FShowFirstRevisionInfoText write FShowFirstRevisionInfoText;
    {$ENDIF LINEINFOEX}
    property ShowOrderList: TList read FShowOrderList;
    property StaticUserColorList: TStringList read FStaticUserColorList;
    property SuppressRevisionTextZeroDot: Boolean read FSuppressRevisionTextZeroDot write FSuppressRevisionTextZeroDot;
    property UserSettingsList: TJVCSLineHistoryUserSettings read FUserSettingsList;
  end;

  TRevisionRectangle = class(TObject)
  private
    FRect: TRect;
    FRevisionIDStr: string;
  public
    constructor Create(ARect: TRect; ARevisionIDStr: string);
    property Rect: TRect read FRect;
    property RevisionIDStr: string read FRevisionIDStr;
  end;

  TRevisionRectangleList = class(TObject)
  private
    FItems: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ARect: TRect; ARevisionIDStr: string);
    procedure Clear(AY: Integer = -1);
    function Find(AX, AY: Integer; var ATopY: Integer; var ARevisionIDStr: string): Boolean;
    function FindRect(AX, AY: Integer): TRevisionRectangle;
  end;

{ TRevisionColor }

constructor TRevisionColor.Create(ALineHistoryRevision: TJVCSLineHistoryRevision);
begin
  inherited Create;
  FDateColor := clNone;
  FRevisionColor := clNone;
  FLineHistoryRevision := ALineHistoryRevision;
  FUserColor := clNone;
end;

constructor TJVCSLineHistoryUserSettingsItem.Create;
begin
  inherited Create;
  FColor := clNone;
  FUserName := '';
  FVisibleName := '';
end;

procedure TJVCSLineHistoryUserSettingsItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TJVCSLineHistoryUserSettingsItem then
  begin
    TJVCSLineHistoryUserSettingsItem(Dest).Color := FColor;
    TJVCSLineHistoryUserSettingsItem(Dest).UserName := FUserName;
    TJVCSLineHistoryUserSettingsItem(Dest).VisibleName := FVisibleName;
  end
  else
    inherited AssignTo(Dest);
end;

constructor TJVCSLineHistoryUserSettings.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TJVCSLineHistoryUserSettings.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TJVCSLineHistoryUserSettings.Add: TJVCSLineHistoryUserSettingsItem;
begin
  FItems.Add(TJVCSLineHistoryUserSettingsItem.Create);
  Result := TJVCSLineHistoryUserSettingsItem(FItems.Last);
end;

procedure TJVCSLineHistoryUserSettings.AssignTo(Dest: TPersistent);
var
  I: Integer;
begin
  if Dest is TJVCSLineHistoryUserSettings then
  begin
    TJVCSLineHistoryUserSettings(Dest).Clear;
    for I := 0 to Pred(Count) do
      TJVCSLineHistoryUserSettings(Dest).Add.Assign(Items[I]);
  end
  else
    inherited AssignTo(Dest);
end;

procedure TJVCSLineHistoryUserSettings.Clear;
begin
  FItems.Clear;
end;

procedure TJVCSLineHistoryUserSettings.Delete(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

function TJVCSLineHistoryUserSettings.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJVCSLineHistoryUserSettings.GetItems(AIndex: Integer): TJVCSLineHistoryUserSettingsItem;
begin
  Result := TJVCSLineHistoryUserSettingsItem(FItems[AIndex]);
end;

function TJVCSLineHistoryUserSettings.IndexOfUser(const AUserName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Pred(Count) do
    if Items[I].UserName = AUserName then
    begin
      Result := I;
      Break;
    end;
end;

constructor TJVCSLineHistorySettings.Create;
begin
  inherited Create;
  FColorBarOrderList := TList.Create;
  FColorBarOrderList.Add(Pointer(1));
  FColorBarOrderList.Add(Pointer(2));
  FColorBarOrderList.Add(Pointer(3));
  FDateEndColor := clWhite;
  FDateFormat := 'yyyy"/"mm"/"dd';
  FDateStartColor := clRed;
  FLineColorMode := 0;
  FPaintMethod := 0;
  FRevisionEndColor := clWhite;
  FRevisionStartColor := clYellow;
  FShowLineNumbers := True;
  FShowRevisionInfoColor := True;
  FShowRevisionInfoText := True;
  FShowDateInfoColor := True;
  FShowDateInfoText := True;
  FShowUserInfoColor := True;
  FShowUserInfoText := True;
  {$IFDEF LINEINFOEX}
  FShowRevisionCountInfoColor := True;
  FShowRevisionCountInfoText := True;
  FShowFirstRevisionInfoColor := True;
  FShowFirstRevisionInfoText := True;
  {$ENDIF LINEINFOEX}
  FShowOrderList := TList.Create;
  FStaticUserColorList := TStringList.Create;
  FSuppressRevisionTextZeroDot := False;
  FUserSettingsList := TJVCSLineHistoryUserSettings.Create;
end;

destructor TJVCSLineHistorySettings.Destroy;
begin
  FColorBarOrderList.Free;
  FShowOrderList.Free;
  FStaticUserColorList.Free;
  FUserSettingsList.Free;
  inherited Destroy;
end;

{$IFNDEF DELPHI6_UP}
procedure AssignTList(ADest, ASource: TList);
var
  I: Integer;
begin
  ADest.Clear;
  for I := 0 to Pred(ASource.Count) do
    ADest.Add(ASource[I]);
end;
{$ENDIF ~DELPHI6_UP}

procedure TJVCSLineHistorySettings.Assign(AValue: TJVCSLineHistorySettings);
begin
  {$IFDEF DELPHI6_UP}
  FColorBarOrderList.Assign(AValue.ColorBarOrderList);
  {$ELSE}
  AssignTList(FColorBarOrderList, AValue.ColorBarOrderList);
  {$ENDIF DELPHI6_UP}
  FDateEndColor := AValue.DateEndColor;
  FDateFormat := AValue.DateFormat;
  FDateStartColor := AValue.DateStartColor;
  FLineColorMode := AValue.LineColorMode;
  FPaintMethod := AValue.PaintMethod;
  FRevisionEndColor := AValue.RevisionEndColor;
  FRevisionStartColor := AValue.RevisionStartColor;
  FShowLineNumbers := AValue.FShowLineNumbers;
  FShowRevisionInfoColor := AValue.ShowRevisionInfoColor;
  FShowRevisionInfoText := AValue.ShowRevisionInfoText;
  FShowDateInfoColor := AValue.ShowDateInfoColor;
  FShowDateInfoText := AValue.ShowDateInfoText;
  FShowUserInfoColor := AValue.ShowUserInfoColor;
  FShowUserInfoText := AValue.ShowUserInfoText;
  {$IFDEF LINEINFOEX}
  FShowRevisionCountInfoColor := AValue.ShowRevisionCountInfoColor;
  FShowRevisionCountInfoText := AValue.ShowRevisionCountInfoText;
  FShowFirstRevisionInfoColor := AValue.ShowFirstRevisionInfoColor;
  FShowFirstRevisionInfoText := AValue.ShowFirstRevisionInfoText;
  {$ENDIF LINEINFOEX}
  {$IFDEF DELPHI6_UP}
  FShowOrderList.Assign(AValue.ShowOrderList);
  {$ELSE}
  AssignTList(FShowOrderList, AValue.ShowOrderList);
  {$ENDIF DELPHI6_UP}
  FStaticUserColorList.Assign(AValue.StaticUserColorList);
  FSuppressRevisionTextZeroDot := AValue.SuppressRevisionTextZeroDot;
  FUserSettingsList.Assign(AValue.UserSettingsList);
end;

{ TRevisionRectangle }

constructor TRevisionRectangle.Create(ARect: TRect; ARevisionIDStr: string);
begin
  FRect := ARect;
  FRevisionIDStr := ARevisionIDStr;
end;

{ TRevisionRectangleList }

constructor TRevisionRectangleList.Create;
begin
  inherited Create;
  FItems := TObjectList.Create;
end;

destructor TRevisionRectangleList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TRevisionRectangleList.Add(ARect: TRect; ARevisionIDStr: string);
begin
  FItems.Add(TRevisionRectangle.Create(ARect, ARevisionIDStr));
end;

procedure TRevisionRectangleList.Clear(AY: Integer = -1);
var
  I: Integer;
begin
  if AY = -1 then
    FItems.Clear
  else
  for I := FItems.Count - 1 downto 0 do
    if TRevisionRectangle(FItems[I]).Rect.Top <= AY then
      FItems.Delete(I);
end;

function TRevisionRectangleList.Find(AX, AY: Integer; var ATopY: Integer; var ARevisionIDStr: string): Boolean;
var
  I: Integer;
  Rect: TRect;
begin
  Result := False;
  ATopY := -1;
  ARevisionIDStr := '';
  for I := 0 to FItems.Count - 1 do
  begin
    Rect := TRevisionRectangle(FItems[I]).Rect;
    if (Rect.Left <= AX) and (Rect.Right >= AX) and (Rect.Top <= AY) and (Rect.Bottom >= AY) then
    begin
      ATopY := Rect.Top;
      ARevisionIDStr := TRevisionRectangle(FItems[I]).RevisionIDStr;
      Result := True;
      Break;
    end;
  end;
end;

function TRevisionRectangleList.FindRect(AX, AY: Integer): TRevisionRectangle;
var
  I: Integer;
  Rect: TRect;
begin
  Result := nil;
  for I := 0 to FItems.Count - 1 do
  begin
    Rect := TRevisionRectangle(FItems[I]).Rect;
    if (Rect.Left <= AX) and (Rect.Right >= AX) and (Rect.Top <= AY) and (Rect.Bottom >= AY) then
    begin
      Result := TRevisionRectangle(FItems[I]);
      Break;
    end;
  end;
end;

type
  TRevisionClickEvent = procedure(ASender: TObject; ARevisionIDStr: string) of object;

  PBlameHintData = ^TBlameHintData;
  TBlameHintData = record
    Revision: TRevisionColor;
    Rect: TRect;
  end;

  TBlameHintWindow = class(THintWindow)
  private
    procedure PaintHint(ATargetCanvas: TCanvas; ARevision: TRevisionColor;
      ARect: TRect);
  protected
    FHintData: TBlameHintData;
    procedure WMKillFocus(var Msg: TMessage); message WM_ACTIVATE;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer;
      const AHint: string; AData: Pointer): TRect; override;
    function IsHintMsg(var Msg: TMsg): Boolean; override;
  end;

  TCustomLiveBlameData = class(TComponent)
  private
    FFileName: string;
    FBlameInfoReady: Boolean;
    FBlameInfoAvailable: Boolean;
    FLastAge: TDateTime;
    FLastStreamSize: Integer;
    FRevisions: TObjectList<TJVCSLineHistoryRevision>;
    FLines: TList<TJVCSLineHistoryRevision>;
    FOrgLines: TList<TJVCSLineHistoryRevision>;
    FBufferRevision: TJVCSLineHistoryRevision;
    FFileRevision: TJVCSLineHistoryRevision;
    FRevisionColorList: TObjectList<TRevisionColor>;
    FButtonDown: Boolean;
    FBlameCounter: Integer;
    FBlameRevision: Integer;
    FMaxRevision: Integer;
    FStage: Integer;
    FLoading: Boolean;
    FPaintBox: TLiveBlamePaintBox;
    FFirstRevisionIDStr: string;
    FLatestRevisionContent: AnsiString;
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure BuildLineHistory(ASettings: TJVCSLineHistorySettings); virtual; abstract;
    procedure Load; virtual; abstract;
  end;

  {$IFDEF SVNINTERNAL}
  TSvnLiveBlameData = class(TCustomLiveBlameData, IAsyncUpdate, IAnnotationCompletion)
  private
    FSvnItem: TSvnItem;
    procedure HandleBlameLoad(Sender: TObject);
  public
    procedure BuildLineHistory(ASettings: TJVCSLineHistorySettings); override;
    procedure Load; override;

    { IAsyncUpdate}
    procedure UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex, LastNewIndex: Integer;
      ForceUpdate: Boolean);
    procedure Completed;

    { IAnnotationCompletion }
    procedure AnnotationComplete(const AnnotationLineProvider: IAnnotationLineProvider);
  end;
  {$ENDIF SVNINTERNAL}

  TGenericLiveBlameData = class(TCustomLiveBlameData, IOTAAsynchronousHistoryUpdater, IOTAAnnotationCompletion)
  private
    FFileHistory: IOTAFileHistory;
    FAnnotationLineProvider: IOTAAnnotationLineProvider;
  public
    { IOTAAsynchronousHistoryUpdater }
    procedure Completed;
    function UpdateHistoryItems(FileHistory: IOTAFileHistory; FirstNewIndex, LastNewIndex: Integer): Boolean;
    { IOTAAnnotationCompletion }
    procedure AnnotationComplete(const AnnotationLineProvider: IOTAAnnotationLineProvider);

    procedure BuildLineHistory(ASettings: TJVCSLineHistorySettings); override;
    procedure Load; override;
  end;

  TLiveBlameData = TGenericLiveBlameData;

  TLiveBlameEditorPanel = class(TPanel)
  private
    FTimer: TTimer;
    FCheckShowTimer: TTimer;
    FPaintBox: TLiveBlamePaintBox;
    FCnt: Integer;
    FCY: Integer;
    FCursorLine: Integer;
    FTopLine: Integer;
    FSpeedButton: TSpeedButton;
    FWizard: TLiveBlameWizard;
    FActiveEditView1: IOTAEditView;
    FModule: IOTAModule;
    FLiveBlameData: TLiveBlameData;
    FLiveBlameDataList: TObjectList<TLiveBlameData>;
    FLineX: Integer;
    FRevisionX: Integer;
    FRevisionRectX1: Integer;
    FRevisionRectX2: Integer;
    FDateX: Integer;
    FDateRectX1: Integer;
    FDateRectX2: Integer;
    FUserX: Integer;
    FUserRectX1: Integer;
    FUserRectX2: Integer;
    FSettings: TJVCSLineHistorySettings;
    FColorList: TStringList;
    FInstalledHook: Boolean;
    FPainting: Boolean;
    FRevisionRectangles: TRevisionRectangleList;
    FRevisionHintRectangles: TRevisionRectangleList;
    FHighlightY: Integer;
    FLastHighlightY: Integer;
    FLastHighlightedRevisionIDStr: string;
    FOnRevisionClick: TRevisionClickEvent;

    FProgressBar: TProgressBar;
    FHintData: TBlameHintData;
    FPopupMenu: TPopupMenu;
    FMenuItem1: TMenuItem;
    FMenuItem2: TMenuItem;
    FMenuItem3: TMenuItem;
    procedure CheckInstallHook;
    procedure CreatePopupMenu;
    procedure BuildLineHistory;
    procedure UpdateLineHistory(ASourceEditor: IOTASourceEditor);
    procedure HandleDiffThreadReady(Sender: TObject);
    function DoGetRevisionColor(ALineHistoryRevision: TJVCSLineHistoryRevision): TRevisionColor;
    function GetNextColor: TColor;
    procedure UpdateGutterWidth;
    procedure OnTimer(Sender: TObject);
    procedure OnCheckTimer(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure WMMyShow(var Msg: TMessage); message WM_BLAME_SHOW;
    procedure WMUpdateWnd(var Msg: TMessage); message WM_BLAME_UPDATE;
    function GetEditControl: TObject;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleOnClick(Sender: TObject);
    procedure HandleRevisionClick(ASender: TObject; ARevisionIDStr: string);
    procedure HandleMouseLeave(Sender: TObject);
    procedure HandleHintShow(var Msg: TMessage);
    function GetHintTextAndSize(AX, AY: Integer; var ARevision: TRevisionColor; var ABlockRect, AHintRect: TRect): Boolean;
    procedure CMHintShow(var Msg: TMessage);
    function GetHintRect(ARevision: TRevisionColor; var ARect: TRect): Boolean;
    procedure HandlePopupMenu(Sender: TObject);
  protected
    procedure SetEnabled(AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EditControlVisible: Boolean;
    procedure ShowIfEditControl;
    procedure EnableCheckTimer;
    procedure ShowHidePanel(Sender: TObject);

    property Wizard: TLiveBlameWizard read FWizard write FWizard;
    property ActiveEditView1: IOTAEditView read FActiveEditView1 write FActiveEditView1;
    property ActiveModule: IOTAModule read FModule write FModule;
  end;

var
  CurrentPanel: TLiveBlameEditorPanel = nil;
  CurrentWindow: HWND = 0;
  CallWndProcHook: HHOOK;
  CallWndProcRetHook: HHOOK;
  GetMsgHook: HHOOK;
  EditControlHook: TControl = nil;
  ForceUpdate: Boolean = False;

constructor TBlameHintWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TBlameHintWindow.WMKillFocus(var Msg: TMessage);
begin
  Hide;
end;

procedure TBlameHintWindow.Paint;
begin
  if Assigned(FHintData.Revision) and (FHintData.Rect.Right - FHintData.Rect.Left > 0) and
    (FHintData.Rect.Bottom - FHintData.Rect.Top > 0) then
    PaintHint(Canvas, FHintData.Revision, FHintData.Rect);
end;

procedure TBlameHintWindow.PaintHint(ATargetCanvas: TCanvas; ARevision: TRevisionColor; ARect: TRect);
var
  S: string;
  ClipRect, R, RLogMessage: TRect;
begin
  R := ARect;
  if CheckWin32Version(6) and ThemeServices.ThemesEnabled and True then
  begin
    // Paint Vista gradient background if themes enabled
    ClipRect := R;
    ClipRect.Bottom := ClipRect.Bottom + 3;
    InflateRect(R, 4, 4);
    with ThemeServices do
      DrawElement(ATargetCanvas.Handle, GetElementDetails(tttStandardNormal), R, ClipRect);
    R := ClipRect;
  end
  else
  begin
    ATargetCanvas.Brush.Color := clInfoBk;
    ATargetCanvas.Rectangle(R);
  end;
  ATargetCanvas.Font.Style := [fsBold];
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(4, 4, 'Revision:');
  ATargetCanvas.TextOut(4, 22, 'Date:');
  ATargetCanvas.TextOut(4, 40, 'User:');
  ATargetCanvas.TextOut(4, 58, 'Logmessage:');
  ATargetCanvas.Font.Style := [];
  ATargetCanvas.Brush.Color := ARevision.RevisionColor;
  ATargetCanvas.Rectangle(Rect(60, 4, 74, 18));
  //ATargetCanvas.Brush.Color := clBtnFace;
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(80, 4, ARevision.LineHistoryRevision.RevisionStr);
  ATargetCanvas.Brush.Color := ARevision.DateColor;
  ATargetCanvas.Rectangle(Rect(60, 22, 74, 36));
  //ATargetCanvas.Brush.Color := clBtnFace;
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(80, 22, ARevision.LineHistoryRevision.DateStr);
  ATargetCanvas.Brush.Color := ARevision.UserColor;
  ATargetCanvas.Rectangle(Rect(60, 40, 74, 54));
  //ATargetCanvas.Brush.Color := clBtnFace;
  S := ARevision.LineHistoryRevision.UserStr;
  if (ARevision.LineHistoryRevision.OrgUserStr <> '') and (ARevision.LineHistoryRevision.OrgUserStr <> S) then
    S := S + ' (' + ARevision.LineHistoryRevision.OrgUserStr + ')';
  ATargetCanvas.Brush.Style := bsClear;
  ATargetCanvas.TextOut(80, 40, S);
  //ATargetCanvas.TextOut(2, 74, FRevision.LineHistoryRevision.LogMessage);
  S :=  ARevision.LineHistoryRevision.Comment;
  {
  R := Rect(2, 74, 200, 200);
  ATargetCanvas.TextRect(R, S, [tfCalcRect]);
  }
  RLogMessage := Rect(4, 76, 300, 300);
  ATargetCanvas.TextRect(RLogMessage, S, [tfCalcRect]);
  ATargetCanvas.TextRect(RLogMessage, S, []);
end;

function TBlameHintWindow.CalcHintRect(MaxWidth: Integer;
  const AHint: string; AData: Pointer): TRect;
begin
  if not Assigned(AData) then
    Result := Rect(0, 0, 0, 0)
  else
  begin
    FHintData := PBlameHintData(AData)^;
    Result := FHintData.Rect;
  end;
end;

function TBlameHintWindow.IsHintMsg(var Msg: TMsg): Boolean;
begin
  Result := inherited IsHintMsg(Msg) and HandleAllocated and IsWindowVisible(Handle);
  // Avoid that mouse moves over the non-client area or key presses cancel the current hint.
  if Result and ((Msg.Message = WM_NCMOUSEMOVE) or ((Msg.Message >= WM_KEYFIRST) and (Msg.Message <= WM_KEYLAST))) then
    Result := False;
end;

type
  TGetAgeResultKind = (garkYear, garkMonth, garkWeek, garkDay, garkHour, garkMinute, garkSecond, garkNow, garkFuture);

function GetAge(ABase, ADate: TDateTime; var AResultKind: TGetAgeResultKind; var ARest: TDateTime): Integer;
var
  DateDiff: TDateTime;
  y1, m1, y2, m2, dummy: Word;
  ym1, ym2: DWord;
begin
  DateDiff := ABase - ADate;
  if DateDiff < 0 then
  begin
    Result := -1;
    ARest := 0;
    AResultKind := garkFuture;
  end
  else
  if DateDiff < (1 / (24 * 60 * 60)) then
  begin
    Result := -1;
    ARest := 0;
    AResultKind := garkNow;
  end
  else
  if DateDiff < (1 / (24 * 60)) then
  begin
    Result := Round(DateDiff * 24 * 60 * 60);
    ARest := DateDiff - Result / ( 24 * 60 * 60);
    AResultKind := garkSecond;
  end
  else
  if DateDiff < (1 / 24) then
  begin
    Result := Round(DateDiff * 24 * 60);
    ARest :=  DateDiff - Result / (24 * 60);
    AResultKind := garkMinute;
  end
  else
  if DateDiff < 1 then
  begin
    Result := Round(DateDiff * 24);
    ARest := DateDiff - Result / 24;
    AResultKind := garkHour;
  end
  else
  if DateDiff < 7 then
  begin
    Result := Round(DateDiff);
    ARest := DateDiff - Result;
    AResultKind := garkDay;
  end
  else
  if DateDiff < (7 * 8) then
  begin
    Result := Round(DateDiff / 7);
    ARest := DateDiff - Result * 7;
    AResultKind := garkWeek;
  end
  else
  begin
    DecodeDate(ABase, y1, m1, dummy);
    ym1 := y1 * 12 + m1;
    DecodeDate(ADate, y2, m2, dummy);
    ym2 := y2 * 12 + m2;
    if ym1 - ym2 < 12 then
    begin
      Result := ym1 - ym2;
      ARest := DateDiff - (30.4375 * Result);
      AResultKind := garkMonth;
    end
    else
    begin
      Result := (ym1 - ym2) div 12;
      ARest := DateDiff - (365.25 * Result);
      AResultKind := garkYear;
    end;
  end;
  if ARest < 0 then
    ARest := 0;
end;

function GetAgeResultToStr(AnAgeInt: Integer; AResultKind: TGetAgeResultKind; ALong: Boolean): string;
const
  ShortSingularNames: array[Low(TGetAgeResultKind)..High(TGetAgeResultKind)] of string =
    ('yr', 'mo', 'wk', 'dy', 'hr', 'mn', 'sc', 'now', 'future');
  ShortPluralNames: array[Low(TGetAgeResultKind)..High(TGetAgeResultKind)] of string =
    ('yrs', 'mos', 'wks', 'dys', 'hrs', 'min', 'sc', 'now', 'future');
  LongSingularNames: array[Low(TGetAgeResultKind)..High(TGetAgeResultKind)] of string =
    ('year', 'month', 'week', 'day', 'hour', 'minute', 'second', 'now', 'future');
  LongPluralNames: array[Low(TGetAgeResultKind)..High(TGetAgeResultKind)] of string =
    ('years', 'months', 'weeks', 'days', 'hours', 'minutes', 'seconds', 'now', 'future');
var
  S: string;
begin
  if (AnAgeInt < 2) and ALong then
    S := LongSingularNames[AResultKind]
  else
  if ALong then
    S := LongPluralNames[AResultKind]
  else
  if AnAgeInt < 2 then
    S := ShortSingularNames[AResultKind]
  else
    S := ShortPluralNames[AResultKind];
  if AResultKind in [garkNow] then
    Result := S
  else
    Result := Format('%d %s', [AnAgeInt, S]);
end;

function GetAge2Str(ADateTime: TDateTime): string;
var
  LNow, DateDiffRest: TDateTime;
  ResultKind: TGetAgeResultKind;
  Age: Integer;
begin
  LNow := Now;
  Age := GetAge(LNow, ADateTime, ResultKind, DateDiffRest);
  Result := GetAgeResultToStr(Age, ResultKind, False);
  if not (ResultKind in [garkSecond, garkNow]) and (DateDiffRest > 0) then
  begin
    Age := GetAge(LNow, LNow - DateDiffRest, ResultKind, DateDiffRest);
    Result := Result + ' ' + GetAgeResultToStr(Age, ResultKind, False);
  end;
end;

constructor TCustomLiveBlameData.Create(const AFileName: string);
begin
  inherited Create(nil);
  FFileName := AFileName;
  FRevisions := TObjectList<TJVCSLineHistoryRevision>.Create;
  FLines := TList<TJVCSLineHistoryRevision>.Create;
  FOrgLines := TList<TJVCSLineHistoryRevision>.Create;
  FLastAge := 0;
  FBlameInfoAvailable := False;
  FBlameInfoReady := False;
  FRevisionColorList := TObjectList<TRevisionColor>.Create;
  FBlameCounter := 0;
end;

destructor TCustomLiveBlameData.Destroy;
begin
  FRevisionColorList.Free;
  FOrgLines.Free;
  FLines.Free;
  FRevisions.Free;
  inherited Destroy;
end;

{$IFDEF SVNINTERNAL}
{ TSvnLiveBlameData }

procedure TSvnLiveBlameData.AnnotationComplete(const AnnotationLineProvider: IAnnotationLineProvider);
begin
  FBlameInfoReady := True;
  FStage := 3;
  FPaintBox.Invalidate;
end;

procedure TSvnLiveBlameData.BuildLineHistory(ASettings: TJVCSLineHistorySettings);
var
  I, Idx: Integer;
  LHRevision: TJVCSLineHistoryRevision;
  RevisionsDict: TDictionary<Integer, TJVCSLineHistoryRevision>;
begin
  RevisionsDict := TDictionary<Integer, TJVCSLineHistoryRevision>.Create;
  try
    FOrgLines.Clear;
    FLines.Clear;
    FRevisions.Clear;
    FRevisionColorList.Clear;
    if FSvnItem.HistoryCount > 0 then
    begin
      for I := FSvnItem.HistoryCount - 1 downto 0 do
      begin
        FRevisions.Add(TJVCSLineHistoryRevision.Create);
        LHRevision := FRevisions.Last;
        RevisionsDict.Add(FSvnItem.HistoryItems[I].Revision, LHRevision);
        LHRevision.FRevisionStr := IntToStr(FSvnItem.HistoryItems[I].Revision);
        LHRevision.FOrgUserStr := FSvnItem.HistoryItems[I].Author;
        Idx := ASettings.UserSettingsList.IndexOfUser(FSvnItem.HistoryItems[I].Author);
        if Idx <> -1 then
          LHRevision.FUserStr := ASettings.UserSettingsList[Idx].VisibleName
        else
          LHRevision.FUserStr := FSvnItem.HistoryItems[I].Author;
        if ASettings.DateFormat = 'AGE2' then
          LHRevision.FDateStr := GetAge2Str(FSvnItem.HistoryItems[I].Time)
        else
          LHRevision.FDateStr := DateTimeToStr(FSvnItem.HistoryItems[I].Time);
        LHRevision.FDate := FSvnItem.HistoryItems[I].Time;
        LHRevision.FComment := TrimRight(FSvnItem.HistoryItems[I].LogMessage);
      end;
      FRevisions.Add(TJVCSLineHistoryRevision.Create);
      FBufferRevision := FRevisions.Last;
      FBufferRevision.FRevisionStr := 'Buff';
      FBufferRevision.FUserStr := 'User';//TODO:

      if ASettings.DateFormat = 'AGE2' then
        FBufferRevision.FDateStr := GetAge2Str(Now - 0.1 / 86400)
      else
        FBufferRevision.FDateStr := DateTimeToStr(Now);
      FBufferRevision.FDate := Now;
      FRevisions.Add(TJVCSLineHistoryRevision.Create);
      FFileRevision := FRevisions.Last;
      FFileRevision.FRevisionStr := 'File';
      FFileRevision.FUserStr := 'User';//TODO:
      if ASettings.DateFormat = 'AGE2' then
        FFileRevision.FDateStr := GetAge2Str(Now - 0.1 / 86400)
      else
        FFileRevision.FDateStr := DateTimeToStr(Now);
      FFileRevision.FDate := Now;
      for I := 1 to FSvnItem.HistoryItems[0].BlameCount do
      begin
        if not RevisionsDict.TryGetValue(FSvnItem.HistoryItems[0].BlameItems[I].Revision, LHRevision) then
          LHRevision := nil;
        FOrgLines.Add(LHRevision);
        FLines.Add(LHRevision);
      end;
    end;
  finally
    RevisionsDict.Free;
  end;
end;

procedure TSvnLiveBlameData.Completed;
begin
  if FSvnItem.HistoryCount > 0 then
  begin
    FSvnItem.HistoryItems[0].StartLoadingBlame(Self);
    FMaxRevision := FSvnItem.HistoryItems[0].Revision;
    FFirstRevisionIDStr := IntToStr(FSvnItem.HistoryItems[Pred(FSvnItem.HistoryCount)].Revision);
    FStage := 2;
  end;
end;

procedure TSvnLiveBlameData.HandleBlameLoad(Sender: TObject);
begin
  FBlameCounter := FBlameCounter + TSvnClient(Sender).DataCounter;
  FBlameRevision := TSvnClient(Sender).BlameRevision;
  FPaintBox.Invalidate;
end;

procedure TSvnLiveBlameData.Load;
begin
  if IDEClient.SvnClient.IsPathVersioned(FFileName) then
  begin
    if not Assigned(FSvnItem) or (FSvnItem.PathName <> FFileName) then
    begin
      FBlameInfoAvailable := False;
      FBlameInfoReady := False;
      FMaxRevision := -1;
      FBlameCounter := 0;
      if Assigned(FSvnItem) then
        FSvnItem.Free;
      IDEClient.SvnClient.OnBlameLoad := HandleBlameLoad;
      FSvnItem := TSvnItem.Create(IDEClient.SvnClient, nil, FFileName);
      FSvnItem.AsyncUpdate := Self;
      FStage := 1;
      FSvnItem.AsyncReloadHistory;
    end;
  end;
end;

procedure TSvnLiveBlameData.UpdateHistoryItems(SvnItem: TSvnItem; FirstNewIndex,
  LastNewIndex: Integer; ForceUpdate: Boolean);
begin
//
end;
{$ENDIF SVNINTERNAL}

{ TGenericLiveBlameData }

procedure TGenericLiveBlameData.AnnotationComplete(const AnnotationLineProvider: IOTAAnnotationLineProvider);
begin
  FAnnotationLineProvider := AnnotationLineProvider;
  FBlameInfoReady := True;
  FStage := 3;
  FPaintBox.Invalidate;
end;

procedure TGenericLiveBlameData.BuildLineHistory(ASettings: TJVCSLineHistorySettings);
var
  I, Idx: Integer;
  LHRevision: TJVCSLineHistoryRevision;
  RevisionsDict: TDictionary<string, TJVCSLineHistoryRevision>;
begin
  if Assigned(FAnnotationLineProvider) then
  begin
    RevisionsDict := TDictionary<string, TJVCSLineHistoryRevision>.Create;
    try
      FOrgLines.Clear;
      FLines.Clear;
      FRevisions.Clear;
      FRevisionColorList.Clear;
      if FFileHistory.Count > 0 then
      begin
        for I := FFileHistory.Count - 1 downto 0 do
        begin
          FRevisions.Add(TJVCSLineHistoryRevision.Create);
          LHRevision := FRevisions.Last;
          RevisionsDict.Add(FFileHistory.Ident[I], LHRevision);
          LHRevision.FRevisionStr := FFileHistory.Ident[I];
          LHRevision.FOrgUserStr := FFileHistory.Author[I];
          Idx := ASettings.UserSettingsList.IndexOfUser(FFileHistory.Author[I]);
          if Idx <> -1 then
            LHRevision.FUserStr := ASettings.UserSettingsList[Idx].VisibleName
          else
            LHRevision.FUserStr := FFileHistory.Author[I];
          if ASettings.DateFormat = 'AGE2' then
            LHRevision.FDateStr := GetAge2Str(FFileHistory.Date[I])
          else
            LHRevision.FDateStr := DateTimeToStr(FFileHistory.Date[I]);
          LHRevision.FDate := FFileHistory.Date[I];
          LHRevision.FComment := TrimRight(FFileHistory.Comment[I]);
        end;
        FRevisions.Add(TJVCSLineHistoryRevision.Create);
        FBufferRevision := FRevisions.Last;
        FBufferRevision.FRevisionStr := 'Buff';
        FBufferRevision.FUserStr := 'User';//TODO:

        if ASettings.DateFormat = 'AGE2' then
          FBufferRevision.FDateStr := GetAge2Str(Now - 0.1 / 86400)
        else
          FBufferRevision.FDateStr := DateTimeToStr(Now);
        FBufferRevision.FDate := Now;
        FRevisions.Add(TJVCSLineHistoryRevision.Create);
        FFileRevision := FRevisions.Last;
        FFileRevision.FRevisionStr := 'File';
        FFileRevision.FUserStr := 'User';//TODO:
        if ASettings.DateFormat = 'AGE2' then
          FFileRevision.FDateStr := GetAge2Str(Now - 0.1 / 86400)
        else
          FFileRevision.FDateStr := DateTimeToStr(Now);
        FFileRevision.FDate := Now;
        for I := 1 to FAnnotationLineProvider.Count do
        begin
          if not RevisionsDict.TryGetValue(Trim(FAnnotationLineProvider.GutterInfo[I]), LHRevision) then
            LHRevision := nil;
          FOrgLines.Add(LHRevision);
          FLines.Add(LHRevision);
        end;
      end;
    finally
      RevisionsDict.Free;
    end;
  end;
  FLoading := False;
end;

procedure TGenericLiveBlameData.Completed;
var
  MS: TMemoryStream;
  SA: TStreamAdapter;
  RC: IStream;
  R, W: Int64;
  StreamStat: TStatStg;
begin
  if Assigned(FFileHistory) and Supports(FFileHistory, IOTAAsynchronousAnnotationProvider) and
    (FFileHistory as IOTAAsynchronousAnnotationProvider).CanAnnotateFile(FFileName) and
    (FFileHistory.Count > 0) then
  begin
    FFirstRevisionIDStr := FFileHistory.Ident[FFileHistory.Count - 1];
    (FFileHistory as IOTAAsynchronousAnnotationProvider).StartAsynchronousUpdate(FFileName, 0, Self);

    RC := FFileHistory.Content[0];
    MS := TMemoryStream.Create;
    SA := TStreamAdapter.Create(MS);
    RC.Seek(0, 0, R);
    if RC.Stat(StreamStat, 0) = S_OK then
      RC.CopyTo(SA, StreamStat.cbSize, R, W);
    MS.Position := 0;
    SetLength(FLatestRevisionContent, MS.Size);
    MS.Read(PAnsiChar(FLatestRevisionContent)^, MS.Size);
  end;
  FStage := 2;
  FPaintBox.Invalidate;
end;

procedure TGenericLiveBlameData.Load;
var
  I: Integer;
  VersionControlServices: IOTAVersionControlServices;
  FileHistoryManager: IOTAFileHistoryManager;
  FileHistoryProvider: IOTAFileHistoryProvider;
  ProviderName: string;
  Idents: TStringList;
begin
  if (not FLoading) and not Assigned(FAnnotationLineProvider) then
  begin
    FAnnotationLineProvider := nil;

    ProviderName := '';
    VersionControlServices := BorlandIDEServices as IOTAVersionControlServices;
    Idents := TStringList.Create;
    try
      Idents.Add(FFileName);
      for I := 0 to VersionControlServices.Count - 1 do
        if VersionControlServices.Items[I].IsFileManaged(nil, Idents) then
        begin
          if Supports(VersionControlServices.Items[I], IOTAVersionControlNotifier150) then
          begin
            if (VersionControlServices.Items[I] as IOTAVersionControlNotifier150).Name = 'embarcadero.subversion' then
              ProviderName := 'TOndrej.SubversionFileHistoryProvider'
            else
            if (VersionControlServices.Items[I] as IOTAVersionControlNotifier150).Name = 'versioninsight.mercurial' then
              ProviderName := 'VersionInsight.HgFileHistoryProvider'
            else
            if (VersionControlServices.Items[I] as IOTAVersionControlNotifier150).Name = 'versioninsight.git' then
              ProviderName := 'VersionInsight.GitFileHistoryProvider';
          end;
          Break;
        end;
    finally
      Idents.Free;
    end;
    if ProviderName <> '' then
    begin
      FileHistoryManager := BorlandIDEServices as IOTAFileHistoryManager;
      FileHistoryProvider := nil;
      for I := 0 to FileHistoryManager.Count - 1 do
        if FileHistoryManager.FileHistoryProvider[I].Ident = ProviderName then
        begin
          FileHistoryProvider := FileHistoryManager.FileHistoryProvider[I];
          Break;
        end;
      if Assigned(FileHistoryProvider) then
      begin
        FLoading := True;
        (FileHistoryProvider as IOTAAsynchronousHistoryProvider).StartAsynchronousUpdate(FFileName, Self);
      end;
    end;
  end;
end;

function TGenericLiveBlameData.UpdateHistoryItems(FileHistory: IOTAFileHistory;
  FirstNewIndex, LastNewIndex: Integer): Boolean;
begin
  FFileHistory := FileHistory;
  Result := True;
end;

{ TLiveBlameEditorPanel }

procedure TLiveBlameEditorPanel.BuildLineHistory;
begin
  FLiveBlameData.BuildLineHistory(FSettings);
end;

function CallWndProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Msg: TMessage;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(CallWndProcHook, nCode, wParam, lParam);
    Exit;
  end;

  if nCode = HC_ACTION then
  begin
    FillChar(Msg, SizeOf(Msg), 0);
    Msg.Msg := PCWPStruct(lParam)^.message;
    Msg.LParam := PCWPStruct(lParam)^.lParam;
    Msg.WParam := PCWPStruct(lParam)^.wParam;
    if (Msg.Msg >= WM_KEYFIRST) and (Msg.Msg <= WM_KEYLAST) then
      asm
        nop
      end;
    if Assigned(CurrentPanel) and (CurrentWindow = PCWPRetStruct(lParam)^.hwnd) and
      ({(Msg.Msg = WM_VSCROLL) or (Msg.Msg = WM_HSCROLL) or }(Msg.Msg = WM_KEYDOWN) or (Msg.Msg = VK_DOWN) or (Msg.Msg = VK_UP)
       or (Msg.Msg = WM_SYSKEYDOWN) or (Msg.Msg = CM_DIALOGKEY)) then
      CurrentPanel.PaintBoxPaint(nil);
  end;

  Result := CallNextHookEx(CallWndProcHook, nCode, wParam, lParam);
end;

function CallWndProcRet(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Msg: TMessage;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(CallWndProcRetHook, nCode, wParam, lParam);
    Exit;
  end;

  if nCode = HC_ACTION then
  begin
    FillChar(Msg, SizeOf(Msg), 0);
    Msg.Msg := PCWPRetStruct(lParam)^.message;
    Msg.LParam := PCWPRetStruct(lParam)^.lParam;
    Msg.WParam := PCWPRetStruct(lParam)^.wParam;
    Msg.Result := PCWPRetStruct(lParam)^.lResult;
    if (Msg.Msg >= WM_KEYFIRST) and (Msg.Msg <= WM_KEYLAST) then
      asm
        nop
      end;
    if Assigned(CurrentPanel) and (CurrentWindow = PCWPRetStruct(lParam)^.hwnd) and
      ((Msg.Msg = WM_VSCROLL) or (Msg.Msg = WM_HSCROLL) or (Msg.Msg = WM_KEYDOWN) or (Msg.Msg = VK_DOWN) or (Msg.Msg = VK_UP)
       or (Msg.Msg = WM_SYSKEYDOWN) or (Msg.Msg = CM_DIALOGKEY)) then
      CurrentPanel.PaintBoxPaint(nil);
    if (Msg.Msg = WM_SETCURSOR) and (CurrentPanel.Cursor = crHandPoint) then
      Windows.SetCursor(Screen.Cursors[crHandPoint]);
  end;

  Result := CallNextHookEx(CallWndProcRetHook, nCode, wParam, lParam);
end;

function GetMsgProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Msg: TMessage;
  Msg2: TWMMouseMove absolute Msg;
begin
  if nCode < 0 then
  begin
    Result := CallNextHookEx(GetMsgHook, nCode, wParam, lParam);
    Exit;
  end;

  if nCode = HC_ACTION then
  begin
    if wParam = PM_REMOVE then
    begin
      FillChar(Msg, SizeOf(Msg), 0);
      Msg.Msg := PMsg(lParam)^.message;
      Msg.LParam := PMsg(lParam)^.lParam;
      Msg.WParam := PMsg(lParam)^.wParam;
      if Assigned(CurrentPanel) and (CurrentWindow = PMsg(lParam)^.hwnd) then
      begin
        if Msg.Msg = WM_KEYDOWN then
          PostMessage(CurrentPanel.Handle, WM_BLAME_UPDATE, 0, 0)
        else
        if Msg.Msg = WM_LBUTTONDOWN then
          CurrentPanel.HandleOnClick(nil)
        else
        if Msg.Msg = WM_LBUTTONUP then
          PostMessage(CurrentPanel.Handle, WM_BLAME_UPDATE, 0, 0);
      end;
    end;
  end;

  Result := CallNextHookEx(GetMsgHook, nCode, wParam, lParam);
end;

procedure TLiveBlameEditorPanel.CheckInstallHook;
var
  EditControl: TObject;
begin
  if not FInstalledHook then
  begin
    EditControl := GetEditControl;
    if EditControl is TControl then
    begin
      EditControlHook := TControl(EditControl);
      CallWndProcHook := SetWindowsHookEx(WH_CALLWNDPROC, CallWndProc, 0, GetCurrentThreadId);
      CallWndProcRetHook := SetWindowsHookEx(WH_CALLWNDPROCRET, CallWndProcRet, 0, GetCurrentThreadId);
      GetMsgHook := SetWindowsHookEx(WH_GETMESSAGE, GetMsgProc, 0, GetCurrentThreadId);
      FInstalledHook := True;
    end;
  end;
end;

procedure TLiveBlameEditorPanel.CMHintShow(var Msg: TMessage);
begin
  inherited;
end;

constructor TLiveBlameEditorPanel.Create(AOwner: TComponent);
var
  User: TJVCSLineHistoryUserSettingsItem;
begin
  inherited Create(AOwner);
  FWizard := nil;
  Align := alLeft;
  Width := 30 + 190;
  BevelOuter := bvNone;
  FTimer := TTimer.Create(Self);
  FTimer.Interval := 10000;
  FPaintBox := TLiveBlamePaintBox.Create(Self);
  FPaintBox.Parent := Self;
  FPaintBox.Align := alClient;
  FPaintBox.OnHintMessage := HandleHintShow;
  FPaintBox.Visible := True;
  //DoubleBuffered := True;
  FCnt := 0;
  FCY := 1;
  FCursorLine := 0;
  FTopLine := 0;
  FActiveEditView1 := nil;
  FModule := nil;
  FTimer.OnTimer := OnTimer;
  FPaintBox.OnPaint := PaintBoxPaint;
  FPaintBox.OnMouseLeave := HandleMouseLeave;
  FPaintBox.OnClick := HandleOnClick;
  FCheckShowTimer := TTimer.Create(Self);
  FCheckShowTimer.Interval := 2;
  FCheckShowTimer.Enabled := False;
  FCheckShowTimer.OnTimer := OnCheckTimer;
  FLiveBlameData := TLiveBlameData.Create('');
  FLiveBlameData.FPaintBox := FPaintBox;
  FLiveBlameDataList := TObjectList<TLiveBlameData>.Create;
  FLiveBlameDataList.Add(FLiveBlameData);
  FSettings := TJVCSLineHistorySettings.Create;
  FSettings.ShowLineNumbers := False;
  FSettings.DateFormat := 'AGE2';
  FSettings.DateStartColor := $F4F4F4;
  FSettings.DateEndColor := clRed;
  FSettings.RevisionStartColor := $F4F4F4;
  FSettings.RevisionEndColor := clAqua;

  User := FSettings.UserSettingsList.Add;
  User.UserName := 'uschuster';
  User.VisibleName := 'US';
  User.Color := clLime;//this is optional

  FColorList := TStringList.Create;
  FColorList.AddObject('', TObject(GetNextColor));
  FRevisionRectangles := TRevisionRectangleList.Create;
  FRevisionHintRectangles := TRevisionRectangleList.Create;
  FHighlightY := -1;
  FLastHighlightY := -1;
  FPaintBox.OnMouseMove := HandleMouseMove;
  FOnRevisionClick := HandleRevisionClick;
  FProgressBar := TProgressBar.Create(Self);
  FProgressBar.Parent := Self;
  FProgressBar.Visible := False;

  CreatePopupMenu;
  Visible := False;
end;

procedure TLiveBlameEditorPanel.CreatePopupMenu;
begin
  FPopupMenu := TPopupMenu.Create(Self);
  FMenuItem1 := TMenuItem.Create(FPopupMenu);
  FMenuItem1.Caption := 'Preset 1';
  FMenuItem1.OnClick := HandlePopupMenu;
  FPopupMenu.Items.Add(FMenuItem1);
  FMenuItem2 := TMenuItem.Create(FPopupMenu);
  FMenuItem2.Caption := 'Preset 2';
  FMenuItem2.OnClick := HandlePopupMenu;
  FPopupMenu.Items.Add(FMenuItem2);
  FMenuItem3 := TMenuItem.Create(FPopupMenu);
  FMenuItem3.Caption := 'Preset 3';
  FMenuItem3.OnClick := HandlePopupMenu;
  FPopupMenu.Items.Add(FMenuItem3);
  FPaintBox.PopupMenu := FPopupMenu;
  PopupMenu := FPopupMenu;
end;

destructor TLiveBlameEditorPanel.Destroy;
begin
  FLiveBlameDataList.Free;
  FRevisionHintRectangles.Free;
  FRevisionRectangles.Free;
  FColorList.Free;
  FSettings.Free;
  FLiveBlameData.Free;
  FCheckShowTimer.Enabled := False;
  FTimer.Enabled := False;
  FPaintBox.OnPaint := nil;
  if Assigned(FWizard) then
    FWizard.RemovePanel(Self);
  inherited Destroy;
end;

function CalcColor(AStartColor, AEndColor: TColor; AFactor: Double): TColor;
var
  StartRGB: array [0..2] of Byte;
begin
  AStartColor := ColorToRGB(AStartColor);
  AEndColor := ColorToRGB(AEndColor);
  StartRGB[0] := GetRValue(AStartColor);
  StartRGB[1] := GetGValue(AStartColor);
  StartRGB[2] := GetBValue(AStartColor);
  Result := RGB(StartRGB[0] + Trunc((GetRValue(AEndColor) - StartRGB[0]) * AFactor),
    StartRGB[1] + Trunc((GetGValue(AEndColor) - StartRGB[1]) * AFactor),
    StartRGB[2] + Trunc((GetBValue(AEndColor) - StartRGB[2]) * AFactor));
end;

function TLiveBlameEditorPanel.DoGetRevisionColor(ALineHistoryRevision: TJVCSLineHistoryRevision): TRevisionColor;
var
  DC, DL: Double;
  I, Idx, IdxS: Integer;
  MinDate, MaxDate: TDateTime;
begin
  Result := nil;
  for I := 0 to Pred(FLiveBlameData.FRevisionColorList.Count) do
    if TRevisionColor(FLiveBlameData.FRevisionColorList[I]).LineHistoryRevision = ALineHistoryRevision then
    begin
      Result := TRevisionColor(FLiveBlameData.FRevisionColorList[I]);
      Break;
    end;
  if not Assigned(Result) and (ALineHistoryRevision.RevisionStr = 'Buff') then
  begin
    FLiveBlameData.FRevisionColorList.Add(TRevisionColor.Create(ALineHistoryRevision));
    Result := TRevisionColor(FLiveBlameData.FRevisionColorList.Last);
    Result.DateColor := clYellow;
    Result.RevisionColor := clYellow;
    Result.UserColor := clYellow;
  end
  else
  if not Assigned(Result) and (ALineHistoryRevision.RevisionStr = 'File') then
  begin
    FLiveBlameData.FRevisionColorList.Add(TRevisionColor.Create(ALineHistoryRevision));
    Result := TRevisionColor(FLiveBlameData.FRevisionColorList.Last);
    Result.DateColor := clLime;
    Result.RevisionColor := clLime;
    Result.UserColor := clLime;
  end
  else
  if not Assigned(Result) then
  begin
    FLiveBlameData.FRevisionColorList.Add(TRevisionColor.Create(ALineHistoryRevision));
    Result := TRevisionColor(FLiveBlameData.FRevisionColorList.Last);

    DL := FLiveBlameData.FRevisions.Count;
    if DL > 0 then
    begin
      DC := 0;
      for I := 0 to Pred(FLiveBlameData.FRevisions.Count) do
        if FLiveBlameData.FRevisions[I] = ALineHistoryRevision then
        begin
          DC := I + 1;
          Break;
        end;
    end
    else
      DC := 0;
    if DL = 0 then
      DC := 0
    else
      DC := DC / DL;
    Result.RevisionColor := CalcColor(FSettings.RevisionStartColor, FSettings.RevisionEndColor, DC);

    MinDate := MaxInt;
    MaxDate := 0;
    for I := 0 to Pred(FLiveBlameData.FRevisions.Count) do
    begin
      if FLiveBlameData.FRevisions[I].Date < MinDate then
        MinDate := FLiveBlameData.FRevisions[I].Date;
      if FLiveBlameData.FRevisions[I].Date > MaxDate then
        MaxDate := FLiveBlameData.FRevisions[I].Date;
    end;
    DC := 0;
    DL := 0;
    if (MinDate < MaxInt) and (MaxDate > 0) then
    begin
      if Assigned(ALineHistoryRevision) then
      begin
        DC := ALineHistoryRevision.Date - MinDate;
        DL := MaxDate - MinDate;
      end;
    end;
    if DL = 0 then
      DC := 0
    else
      DC := DC / DL;
    Result.DateColor := CalcColor(FSettings.DateStartColor, FSettings.DateEndColor, DC);

    Idx := 0;
    if Assigned(ALineHistoryRevision) then
    begin
      IdxS := FSettings.UserSettingsList.IndexOfUser(ALineHistoryRevision.OrgUserStr);
      if (IdxS <> -1) and (FSettings.UserSettingsList[IdxS].Color <> clNone) and
        (FSettings.UserSettingsList[IdxS].Color <> clDefault) then
      begin
        Idx := FColorList.IndexOf(ALineHistoryRevision.OrgUserStr);
        if Idx = -1 then
        begin
          FColorList.AddObject(ALineHistoryRevision.OrgUserStr, TObject(FSettings.UserSettingsList[IdxS].Color));
          Idx := Pred(FColorList.Count);
        end;
      end
      else
      begin
        Idx := FColorList.IndexOf(ALineHistoryRevision.OrgUserStr);
        if Idx = -1 then
        begin
          FColorList.AddObject(ALineHistoryRevision.OrgUserStr, TObject(GetNextColor));
          Idx := Pred(FColorList.Count);
        end;
      end;
    end;
    if Idx < FColorList.Count then
      Result.UserColor := Integer(FColorList.Objects[Idx])
    else
      Result.UserColor := clNone;
  end;
end;

function FindControlAtPt(Control: TWinControl; Pt: TPoint; MinClass: TClass): TControl;
var
  I: Integer;
begin
  Result := nil;
  for I := Control.ControlCount - 1 downto 0 do
    if (Control.Controls[I] is MinClass) and PtInRect(Control.Controls[I].BoundsRect, Pt) then
    begin
      Result := Control.Controls[I];
      Break;
    end;
end;

procedure TLiveBlameEditorPanel.OnCheckTimer(Sender: TObject);
begin
  OutputDebugString('OnCheckTimer');
  ShowIfEditControl;
  FCheckShowTimer.Enabled := False;
end;

function TLiveBlameEditorPanel.EditControlVisible: Boolean;
var
  I: Integer;
  tempControl: TWinControl;
  tempControl2: TControl;
begin
  Result := False;
  if Owner.ClassName = 'TEditWindow' then
  begin
   OutputDebugString('EditControlVisible: Owner is TEditWindow');
    with Owner do
      for I := 0 to ComponentCount - 1 do
        if (Components[I] is TWinControl) and (Components[I].ClassName = 'TEditControl') then
        begin
          OutputDebugString('EditControlVisible: Found TEditControl');
          tempControl := TWinControl(Components[I]);
          Result := tempControl.Visible;
          if Result then
          begin
            tempControl2 := FindControlAtPt(tempControl.Parent,
              Point(tempControl.Left, tempControl.Top), TWinControl);
            Result := tempControl = tempControl2;
          end;
          Break;
        end;
  end;
end;

function TLiveBlameEditorPanel.GetEditControl: TObject;
var
  I: Integer;
begin
  Result := nil;
  if Owner.ClassName = 'TEditWindow' then
  begin
    with Owner do
      for I := 0 to ComponentCount - 1 do
        if (Components[I] is TWinControl) and (Components[I].ClassName = 'TEditControl') then
        begin
          Result := Components[I];
          CurrentWindow := TWinControl(Components[I]).Handle;
          Break;
        end;
  end;
end;

function TLiveBlameEditorPanel.GetHintRect(ARevision: TRevisionColor; var ARect: TRect): Boolean;
var
  B: TBitmap;
  TargetCanvas: TCanvas;
  S: string;
  R, RLogMessage: TRect;
  TextExt: TSize;
begin
  B := TBitmap.Create;
  try
    TargetCanvas := B.Canvas;
    TargetCanvas.Font := Screen.HintFont;
    R := TargetCanvas.ClipRect;
    R := Rect(0, 0, 80, 76);
    S := ARevision.LineHistoryRevision.Comment;
    RLogMessage := Rect(4, 76, 300, 300);
    TargetCanvas.TextRect(RLogMessage, S, [tfCalcRect]);
    R.Bottom := RLogMessage.Bottom + 4;
    if R.Right < RLogMessage.Right + 4 then
      R.Right := RLogMessage.Right + 4;
    TextExt := TargetCanvas.TextExtent(ARevision.LineHistoryRevision.RevisionStr);
    if R.Right < 80 + TextExt.cx + 4 then
      R.Right := 80 + TextExt.cx + 4;
    TextExt := TargetCanvas.TextExtent(ARevision.LineHistoryRevision.DateStr);
    if R.Right < 80 + TextExt.cx + 4 then
      R.Right := 80 + TextExt.cx + 4;
    S := ARevision.LineHistoryRevision.UserStr;
    if (ARevision.LineHistoryRevision.OrgUserStr <> '') and (ARevision.LineHistoryRevision.OrgUserStr <> S) then
      S := S + ' (' + ARevision.LineHistoryRevision.OrgUserStr + ')';
    TextExt := TargetCanvas.TextExtent(S);
    if R.Right < 80 + TextExt.cx + 4 then
      R.Right := 80 + TextExt.cx + 4;
    ARect := R;
    Result := True;
  finally
    B.Free;
  end;
end;

function TLiveBlameEditorPanel.GetHintTextAndSize(AX, AY: Integer; var ARevision: TRevisionColor;
  var ABlockRect, AHintRect: TRect): Boolean;
var
  I: Integer;
  RevisionRect: TRevisionRectangle;
begin
  AHintRect := Rect(0, 0, 0, 0);
  ABlockRect := Rect(0, 0, 0, 0);
  ARevision := nil;
  Result := False;
  RevisionRect := FRevisionHintRectangles.FindRect(AX, AY);
  if Assigned(RevisionRect) then
  begin
    for I := 0 to FLiveBlameData.FRevisionColorList.Count - 1 do
      if FLiveBlameData.FRevisionColorList[I].LineHistoryRevision.RevisionStr = RevisionRect.RevisionIDStr then
      begin
        ARevision := FLiveBlameData.FRevisionColorList[I];
        Break;
      end;
    if Assigned(ARevision) and GetHintRect(ARevision, AHintRect) then
    begin
      ABlockRect := RevisionRect.Rect;
      Result := True;
    end;
  end;
end;

function TLiveBlameEditorPanel.GetNextColor: TColor;
begin
  case (14 - FColorList.Count mod 15) of
    0: Result := RGB(128, 128, 128);
    1: Result := RGB(255, 128, 128);
    2: Result := RGB(128, 255, 128);
    3: Result := RGB(128, 128, 255);
    4: Result := RGB(255, 255, 128);
    5: Result := RGB(128, 255, 255);
    6: Result := RGB(255, 128, 255);
    7: Result := RGB(192, 192, 192);
    8: Result := RGB(255, 192, 192);
    9: Result := RGB(192, 255, 192);
   10: Result := RGB(192, 192, 255);
   11: Result := RGB(255, 255, 192);
   12: Result := RGB(192, 255, 255);
   13: Result := RGB(255, 192, 255);
   14: Result := RGB(255, 255, 255);
   else
     Result := RGB(255, 255, 255);
  end;
end;

procedure TLiveBlameEditorPanel.HandleHintShow(var Msg: TMessage);
var
  BlockRect, HintRect: TRect;
  P, PClient: TPoint;
  FoundHint: Boolean;
  Revision: TRevisionColor;
begin
  GetCursorPos(P);
  PClient := ScreenToClient(P);
  FoundHint := GetHintTextAndSize(PClient.X, PClient.Y, Revision, BlockRect, HintRect);
  if FoundHint then
  begin
    InflateRect(BlockRect, 2, 2);
    FHintData.Revision := Revision;
    FHintData.Rect := HintRect;
    with PHintInfo(Msg.LParam)^ do
    begin
      HintStr := ' ';
      //HideTimeOut :=
      HintWindowClass := TBlameHintWindow;
      CursorRect := BlockRect;
      //HintPos.Y := Max(HintPos.Y, ClientToScreen(BlockRect.BottomRight).Y) + 2;
      HintData := @FHintData;
    end;
    Msg.Result := 0;
  end
  else
    Msg.Result := 1;
end;

procedure TLiveBlameEditorPanel.HandleMouseLeave(Sender: TObject);
begin
  Cursor := crDefault;
  FPaintBox.Cursor := crDefault;
  FHighlightY := -1;
  FPaintBox.Invalidate;
end;

procedure TLiveBlameEditorPanel.HandleMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  NewCursor: TCursor;
  TopY: Integer;

  EditControl: TControl;
begin
  if Assigned(FOnRevisionClick) then
  begin
    if FRevisionRectangles.Find(X, Y, TopY, FLastHighlightedRevisionIDStr) and
      (FLiveBlameData.FFirstRevisionIDStr <> FLastHighlightedRevisionIDStr) then
      NewCursor := crHandPoint
    else
    begin
      NewCursor := crDefault;
      TopY := -1;
    end;
    if (NewCursor <> Cursor) or (FHighlightY <> TopY) then
    begin
      FPaintBox.Cursor := NewCursor;
      FHighlightY := TopY;
      FPaintBox.Invalidate;

      EditControl := TControl(GetEditControl);
      if Assigned(EditControl) then
        EditControl.Cursor := NewCursor;
      Cursor := NewCursor;
      Windows.SetCursor(Screen.Cursors[NewCursor]);
    end;
  end;
end;

procedure TLiveBlameEditorPanel.HandleOnClick(Sender: TObject);
begin
  if (Cursor = crHandPoint) and Assigned(FOnRevisionClick) and
    (FLastHighlightedRevisionIDStr <> '') then
    FOnRevisionClick(Self, FLastHighlightedRevisionIDStr);
end;

procedure TLiveBlameEditorPanel.HandlePopupMenu(Sender: TObject);
begin
  if Sender = FMenuItem1 then
  begin
    FSettings.FShowRevisionInfoText := False;
    FSettings.FShowRevisionInfoColor := True;
    FSettings.FShowDateInfoText := False;
    FSettings.FShowDateInfoColor := False;
    FSettings.FShowUserInfoText := False;
    FSettings.FShowUserInfoColor := False;
  end
  else
  if Sender = FMenuItem2 then
  begin
    FSettings.FShowRevisionInfoText := True;
    FSettings.FShowRevisionInfoColor := True;
    FSettings.FShowDateInfoText := False;
    FSettings.FShowDateInfoColor := False;
    FSettings.FShowUserInfoText := False;
    FSettings.FShowUserInfoColor := False;
  end
  else
  if Sender = FMenuItem3 then
  begin
    FSettings.FShowRevisionInfoText := True;
    FSettings.FShowRevisionInfoColor := True;
    FSettings.FShowDateInfoText := True;
    FSettings.FShowDateInfoColor := True;
    FSettings.FShowUserInfoText := True;
    FSettings.FShowUserInfoColor := True;
  end;
  UpdateGutterWidth;
end;

procedure TLiveBlameEditorPanel.HandleRevisionClick(ASender: TObject;
  ARevisionIDStr: string);
var
  CompareRevisionThread: TCompareRevisionThread;
begin
  {$IFDEF SVNINTERNAL}
  CompareRevisionThread := TCompareRevisionThread.Create;
  CompareRevisionThread.AddFile(FLiveBlameData.FSvnItem.PathName, StrToIntDef(ARevisionIDStr, 0), StrToIntDef(ARevisionIDStr, 0) - 1);
  {$ELSE}
  CompareRevisionThread := TCompareRevisionThread.Create;
  CompareRevisionThread.AddFile(FLiveBlameData.FFileName, ARevisionIDStr, '', FLiveBlameData.FFileHistory);
  {$ENDIF}
  CompareRevisionThread.Start;
end;

procedure TLiveBlameEditorPanel.SetEnabled(AValue: Boolean);
begin
//
end;

procedure TLiveBlameEditorPanel.ShowHidePanel(Sender: TObject);
begin
  if Sender is TSpeedButton then
    Visible := TSpeedButton(Sender).Down
  else
  if Sender is TAction then
    Visible := TAction(Sender).Checked;
  if Assigned(FLiveBlameData) then
    FLiveBlameData.FButtonDown := Visible;
end;

procedure TLiveBlameEditorPanel.ShowIfEditControl;
var
  I: Integer;
  State, ButtonState, ModuleChanged: Boolean;
  CurrentFileName: string;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
begin
  State := EditControlVisible;
  ButtonState := State;
  ModuleChanged := False;
  if State then
  begin
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    if Assigned(ModuleServices) then
    begin
      Module := ModuleServices.CurrentModule;
      CurrentFileName := Module.CurrentEditor.FileName;
    end
    else
      CurrentFileName := '';
    if CurrentFileName <> '' then
    begin
      if (not Assigned(FLiveBlameData)) or (FLiveBlameData.FFileName <> CurrentFileName) then
      begin
        FLiveBlameData := nil;
        for I := 0 to FLiveBlameDataList.Count - 1 do
          if FLiveBlameDataList[I].FFileName = CurrentFileName then
          begin
            FLiveBlameData := FLiveBlameDataList[I];
            Break;
          end;
        if not Assigned(FLiveBlameData) then
        begin
          FLiveBlameDataList.Add(TLiveBlameData.Create(CurrentFileName));
          FLiveBlameData := FLiveBlameDataList.Last;
          FLiveBlameData.FPaintBox := FPaintBox;
        end;
        FSpeedButton.Down := FLiveBlameData.FButtonDown;
        ModuleChanged := True;
      end;
    end;
    State := ((not Assigned(FSpeedButton)) or FSpeedButton.Down);
  end;
  Visible := State;
  FSpeedButton.Visible := ButtonState;
  if Visible and ModuleChanged then
  begin
    UpdateGutterWidth;
    FPaintBox.Invalidate;
  end;
end;

procedure TLiveBlameEditorPanel.UpdateGutterWidth;
var
  MaxWidthRevision, MaxLineWidth, MaxWidthUser, MaxWidthDate, CurrentWidth: Integer;
  I, NextX: Integer;
  LineInformation: TJVCSLineHistoryRevision;
  FontBitmap: TBitmap;
  OrderList: TList;
  {$IFDEF LINEINFOEX}
  MaxWidthRevisionCount, MaxWidthFirstRevision: Integer;
  LineInformationEx: TJVCSLineHistoryLineInfo;
  {$ENDIF LINEINFOEX}

  procedure CalcTextPosAndColorRect(AShowText, AShowColor: Boolean;
    AMaxTextWidth: Integer; var ACurrentX, ATextX, AColorRectX1, AColorRectX2: Integer);
  begin
    if AShowText then
    begin
      ATextX := ACurrentX + 5;
    end
    else
    begin
      AMaxTextWidth := 0;
      ATextX := -1;
    end;
    if AShowColor then
    begin
      AColorRectX1 := ACurrentX;
      AColorRectX2 := ACurrentX + AMaxTextWidth + 11;
    end
    else
    begin
      AColorRectX1 := -1;
      AColorRectX2 := -1;
    end;
    if AShowText or AShowColor then
      Inc(ACurrentX, AMaxTextWidth + 10);
  end;

begin
  MaxWidthRevision := 0;
  {$IFDEF LINEINFOEX}
  MaxWidthRevisionCount := 0;
  MaxWidthFirstRevision := 0;
  {$ENDIF LINEINFOEX}
  MaxWidthUser := 0;
  MaxWidthDate := 0;
  MaxLineWidth := 0;
  FontBitmap := TBitmap.Create;
  try
    FontBitmap.Canvas.Font.Name := 'Courier New';
    FontBitmap.Canvas.Font.Size := 10;
    if Assigned(FLiveBlameData.FLines) then
      for I := 0 to Pred(FLiveBlameData.FLines.Count) do
      begin
        LineInformation := FLiveBlameData.FLines[I];
        if Assigned(LineInformation) then
        begin
          CurrentWidth := FontBitmap.Canvas.TextWidth(LineInformation.RevisionStr);
          if CurrentWidth > MaxWidthRevision then
            MaxWidthRevision := CurrentWidth;
          CurrentWidth := FontBitmap.Canvas.TextWidth(LineInformation.UserStr);
          if CurrentWidth > MaxWidthUser then
            MaxWidthUser := CurrentWidth;
          CurrentWidth := FontBitmap.Canvas.TextWidth(LineInformation.DateStr);
          if CurrentWidth > MaxWidthDate then
            MaxWidthDate := CurrentWidth;
          CurrentWidth := FontBitmap.Canvas.TextWidth(IntToStr(I + 1));
          if CurrentWidth > MaxLineWidth then
            MaxLineWidth := CurrentWidth;
        end;
      end;
    {$IFDEF LINEINFOEX}
    if Assigned(LineInformationListEx) then
      for I := 0 to Pred(LineInformationListEx.Count) do
        begin
          LineInformationEx := LineInformationListEx[I];
          if Assigned(LineInformationEx) then
          begin
            CurrentWidth := FontBitmap.Canvas.TextWidth(IntToStr(LineInformationEx.Count));
            if CurrentWidth > MaxWidthRevisionCount then
              MaxWidthRevisionCount := CurrentWidth;
            if (LineInformationEx.Count > 0) and Assigned(LineInformationEx.Revision[0]) then
            begin
              CurrentWidth := FontBitmap.Canvas.TextWidth(LineInformationEx.Revision[0].RevisionStr);
              if CurrentWidth > MaxWidthFirstRevision then
                MaxWidthFirstRevision := CurrentWidth;
            end;
          end;
        end;
    {$ENDIF LINEINFOEX}
    NextX := 0;
    OrderList := TList.Create;
    try
      for I := 0 to Pred(FSettings.ShowOrderList.Count) do
        OrderList.Add(FSettings.ShowOrderList[I]);
      for I := 1 to {$IFDEF LINEINFOEX} 6 {$ELSE} 4 {$ENDIF} do
        if OrderList.IndexOf(Pointer(I)) = -1 then
          OrderList.Add(Pointer(I));
      for I := 0 to Pred(OrderList.Count) do
      begin
        if OrderList[I] = Pointer(1) then
        begin
          if FSettings.ShowLineNumbers then
          begin
            FLineX := NextX + 5;
            Inc(NextX, MaxLineWidth + 10);
          end
          else
            FLineX := -1;
        end
        else
        if OrderList[I] = Pointer(2) then
          CalcTextPosAndColorRect(FSettings.ShowRevisionInfoText, FSettings.ShowRevisionInfoColor,
            MaxWidthRevision, NextX, FRevisionX, FRevisionRectX1, FRevisionRectX2)
        else
        if OrderList[I] = Pointer(3) then
          CalcTextPosAndColorRect(FSettings.ShowDateInfoText, FSettings.ShowDateInfoColor,
            MaxWidthDate, NextX, FDateX, FDateRectX1, FDateRectX2)
        else
        {$IFDEF LINEINFOEX}
        if OrderList[I] = Pointer(5) then
          CalcTextPosAndColorRect(FSettings.ShowRevisionCountInfoText, FSettings.ShowRevisionCountInfoColor,
            MaxWidthRevisionCount, NextX, FRevisionCountX, FRevisionCountRectX1, FRevisionCountRectX2)
        else
        if OrderList[I] = Pointer(6) then
          CalcTextPosAndColorRect(FSettings.ShowFirstRevisionInfoText, FSettings.ShowFirstRevisionInfoColor,
            MaxWidthFirstRevision, NextX, FFirstRevisionX, FFirstRevisionRectX1, FFirstRevisionRectX2)
        else
        {$ENDIF LINEINFOEX}
        if OrderList[I] = Pointer(4) then
          CalcTextPosAndColorRect(FSettings.ShowUserInfoText, FSettings.ShowUserInfoColor,
            MaxWidthUser, NextX, FUserX, FUserRectX1, FUserRectX2);
      end;
    finally
      OrderList.Free;
    end;
    Width := NextX;
  finally
    FontBitmap.Free;
  end;
end;

procedure BuildDiff(ALines1, ALines2: TStringList; ANewObject: TObject);
var
  HashList1, HashList2: TList;
  I, J, K: Integer;
  Diff: TDiff;
begin
  HashList1 := TList.Create;
  HashList2 := TList.Create;
  Diff := TDiff.Create(nil);
  try
    for I := 0 to ALines1.Count - 1 do
      HashList1.Add(HashLine(ALines1[I], True, True));
    for I := 0 to ALines2.Count - 1 do
      HashList2.Add(HashLine(ALines2[I], True, True));
    Diff.Execute(PIntArray(HashList1.List),PIntArray(HashList2.List),
      HashList1.count, HashList2.count);
    J := 0;
    K := 0;
    with Diff do
      for I := 0 to ChangeCount-1 do
        with Changes[I] do
        begin
          while J < x do
          begin
            ALines2.Objects[K] := Alines1.Objects[J];
            Inc(J);
            Inc(K);
          end;
          if Kind = ckAdd then
          begin
            for J := K to K + Range - 1 do
              Alines2.Objects[J] := ANewObject;
            J := x;
            K := y + Range;
          end
          else
          if Kind = ckModify then
          begin
            for J := 0 to Range - 1 do
              Alines2.Objects[K + J] := ANewObject;
            J := x + Range;
            K := y + Range;
          end
          else //Kind = ckDel
            J := x + Range;
        end;
    while J < Alines1.count do
    begin
      Alines2.Objects[K] := Alines1.Objects[J];
      Inc(J);
      Inc(K);
    end;
  finally
    Diff.Free;
    HashList2.Free;
    HashList1.Free;
  end;
end;

type
  TLineUpdateThread = class(TThread)
  private
    FFileName: string;
    FLatestRevisionContent: string;
    FEditorContent: string;
    FLines: TList<TJVCSLineHistoryRevision>;
    FOrgLines: TList<TJVCSLineHistoryRevision>;
    FFileRevision: TJVCSLineHistoryRevision;
    FBufferRevision: TJVCSLineHistoryRevision;
  protected
    procedure Execute; override;
  public
    constructor Create(AFileName: string; ALatestRevisionContent, AEditorContent: AnsiString; AOnFinished: TNotifyEvent;
      AOrgLines: TList<TJVCSLineHistoryRevision>; AFileRevision, ABufferRevision: TJVCSLineHistoryRevision);
    destructor Destroy; override;
  end;

{ TLineUpdateThread }

constructor TLineUpdateThread.Create(AFileName: string; ALatestRevisionContent, AEditorContent: AnsiString; AOnFinished: TNotifyEvent;
  AOrgLines: TList<TJVCSLineHistoryRevision>; AFileRevision, ABufferRevision: TJVCSLineHistoryRevision);
begin
  inherited Create(False);
  FFileName := AFileName;
  FLatestRevisionContent := ALatestRevisionContent;
  FEditorContent := AEditorContent;
  FreeOnTerminate := True;
  OnTerminate := AOnFinished;
  FLines := TList<TJVCSLineHistoryRevision>.Create;
  FFileRevision := AFileRevision;
  FBufferRevision := ABufferRevision;
  FOrgLines := AOrgLines;
end;

destructor TLineUpdateThread.Destroy;
begin
  FLines.Free;
  inherited Destroy;
end;

procedure TLineUpdateThread.Execute;
var
  I: Integer;
  TSL, TSL2, SL: TStringList;
  DT: TDateTime;
begin
  TSL := TStringList.Create;
  TSL2 := TStringList.Create;
  SL := TStringList.Create;
  try
    TSL.LoadFromFile(FFileName);
    FileAge(FFileName, DT);
    FFileRevision.FDate := DT - 1 / 86400;
    {
    TSL2.LoadFromFile(ExtractFilePath(FFileName) + '.svn\text-base\' + ExtractFileName(FFileName) + '.svn-base');//TODO:remove
    }
    TSL2.Text := UTF8ToString(FLatestRevisionContent);
    for I := 0 to TSL2.Count - 1 do
      if FOrgLines.Count > I then
        TSL2.Objects[I] := FOrgLines[I];
    BuildDiff(TSL2, TSL, TObject(2));
    SL.Text := UTF8ToString(FEditorContent);
    BuildDiff(TSL, SL, TObject(1));
    FLines.Clear;
    for I := 0 to SL.Count - 1 do
      if SL.Objects[I] = TObject(1) then
        FLines.Add(FBufferRevision)
      else
      if SL.Objects[I] = TObject(2) then
        FLines.Add(FFileRevision)
      else
        FLines.Add(TJVCSLineHistoryRevision(SL.Objects[I]));
  finally
    SL.Free;
    TSL2.Free;
    TSL.Free;
  end;
end;

procedure TLiveBlameEditorPanel.HandleDiffThreadReady(Sender: TObject);
begin
  FLiveBlameData.FLines.Clear;
  FLiveBlameData.FLines.AddRange(TLineUpdateThread(Sender).FLines);
  FPaintBox.Invalidate;
end;

procedure TLiveBlameEditorPanel.UpdateLineHistory(ASourceEditor: IOTASourceEditor);
var
  //I, Idx: Integer;
  EC: IOTAEditorContent;
  C: IStream;
  S: AnsiString;
  R, W: Int64;
  MS: TMemoryStream;
  SA: TStreamAdapter;
  //TSL, TSL2, SL: TStringList;
  StreamStat: TStatStg;
  Update: Boolean;
  //DT: TDateTime;
begin
  EC := ASourceEditor as IOTAEditorContent;
  Update := (FLiveBlameData.FLastAge = 0) or (EC.GetContentAge <> FLiveBlameData.FLastAge);
  C := EC.Content;
  if C.Stat(StreamStat, 0) = S_OK then
    Update := FLiveBlameData.FLastStreamSize <> StreamStat.cbSize;
  Update := Update or ForceUpdate;
  ForceUpdate := False;
  if Update then
  begin
    FLiveBlameData.FLastAge := EC.GetContentAge;
    FLiveBlameData.FLastStreamSize := StreamStat.cbSize;
    FLiveBlameData.FBufferRevision.FDate := Now;//FLastAge + 1 / 24 - 1 / 86400;
    if FSettings.DateFormat = 'AGE2' then
      FLiveBlameData.FBufferRevision.FDateStr := GetAge2Str(Now - 1 / 86400)
    else
      FLiveBlameData.FBufferRevision.FDateStr := DateTimeToStr(FLiveBlameData.FLastAge);
    MS := TMemoryStream.Create;
    try
      SA := TStreamAdapter.Create(MS);
      try
        C.Seek(0, 0, R);
        C.CopyTo(SA, FLiveBlameData.FLastStreamSize, R, W);
        MS.Position := 0;
        SetLength(S, MS.Size);
        MS.Read(PAnsiChar(S)^, MS.Size);
      finally
        //SA.Free;
      end;

      TLineUpdateThread.Create(ASourceEditor.FileName, FLiveBlameData.FLatestRevisionContent, S,
        HandleDiffThreadReady, FLiveBlameData.FOrgLines, FLiveBlameData.FFileRevision, FLiveBlameData.FBufferRevision);
      {//TODO:remove
      TSL := TStringList.Create;
      TSL2 := TStringList.Create;
      SL := TStringList.Create;
      try
        TSL.LoadFromFile(ASourceEditor.FileName);
        FileAge(ASourceEditor.FileName, DT);
        FFileRevision.FDate := DT - 1 / 86400;
        TSL2.LoadFromFile(ExtractFilePath(ASourceEditor.FileName) + '.svn\text-base\' + ExtractFileName(ASourceEditor.FileName) + '.svn-base');
        for I := 0 to TSL2.Count - 1 do
          if FOrgLines.Count > I then
            TSL2.Objects[I] := FOrgLines[I];
        BuildDiff(TSL2, TSL, TObject(2));
        SL.Text := UTF8ToString(S);
        BuildDiff(TSL, SL, TObject(1));
        FLines.Clear;
        for I := 0 to SL.Count - 1 do
          if SL.Objects[I] = TObject(1) then
            FLines.Add(FBufferRevision)
          else
          if SL.Objects[I] = TObject(2) then
            FLines.Add(FFileRevision)
          else
            FLines.Add(TJVCSLineHistoryRevision(SL.Objects[I]));
      finally
        SL.Free;
        TSL2.Free;
        TSL.Free;
      end;
      }
    finally
      MS.Free;
    end;
  end;
end;

procedure TLiveBlameEditorPanel.EnableCheckTimer;
begin
  FCheckShowTimer.Enabled := True;
end;

procedure TLiveBlameEditorPanel.WMMyShow(var Msg: TMessage);
begin
  ShowIfEditControl;
end;

procedure TLiveBlameEditorPanel.WMUpdateWnd(var Msg: TMessage);
begin
  PaintBoxPaint(nil);
end;

type
  TIdeNotifier = class(TNotifierObject,IOTANotifier, IOTAIDENotifier, IOTAIDENotifier50,IOTAEditorNotifier)
  private
    FList: TList;
    FWizard: TLiveBlameWizard;
  protected
    constructor Create(AWizard: TLiveBlameWizard; AList: TList);
    procedure AfterCompile(Succeeded: Boolean);overload;
    procedure BeforeCompile(const Project: IOTAProject;
      var Cancel: Boolean);overload;
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean);overload;
    procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean;
      var Cancel: Boolean);overload;
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure ViewActivated(const View: IOTAEditView);
  end;

  TMyModuleNotifier = class(TNotifierObject, IOTANotifier, IOTAModuleNotifier,IOTAEditorNotifier)
  private
    FInfo: string;
    FList: TList;
    FWizard: TLiveBlameWizard;
    FModule: IOTAModule;
  public
    constructor Create(AModule: IOTAModule; AWizard: TLiveBlameWizard; AList: TList; AInfo: string);
    procedure ModuleRenamed(const NewName: string);
    function CheckOverwrite: Boolean;

    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    procedure ViewNotification(const View: IOTAEditView; Operation: TOperation);
    procedure ViewActivated(const View: IOTAEditView);

    property Module: IOTAModule read FModule;
  end;

  TMyINTANotifier = class(TNotifierObject, INTAEditServicesNotifier)
  private
    FList: TList;
    FWizard: TLiveBlameWizard;
  public
    constructor Create(AWizard: TLiveBlameWizard; AList: TList);
    procedure WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
    procedure WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
    procedure WindowActivated(const EditWindow: INTAEditWindow);
    procedure WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
    procedure EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
    procedure DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
    procedure DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
  end;

{$IFDEF DEBUGMESSAGES}
function MsgServices: IOTAMessageServices;
begin
  Result := (BorlandIDEServices as IOTAMessageServices);
  Assert(Result <> nil, 'IOTAMessageServices not available');
end;
{$ENDIF DEBUGMESSAGES}

constructor TIdeNotifier.Create(AWizard: TLiveBlameWizard; AList: TList);
begin
  inherited Create;
  FList := AList;
  FWizard := AWizard;
end;

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
var
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  I, J: Integer;
  Editor: IOTAEditor;
  InfoStr: string;
  Dummy: IUnknown;
begin
  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddTitleMessage(Format('%s: %s',
    [GetEnumName(TypeInfo(TOTAFIleNotification), Ord(NotifyCode)), FileName]));
  MsgServices.AddToolMessage('',Format('%s: %s',
    [GetEnumName(TypeInfo(TOTAFIleNotification), Ord(NotifyCode)), FileName]), 'Notifier',0,0);
  {$ENDIF DEBUGMESSAGES}

  if NotifyCode = ofnFileOpened then
  begin
    ModuleServices := BorlandIDEServices as IOTAModuleServices;
    if Assigned(ModuleServices) then
      for I := 0 to Pred(ModuleServices.ModuleCount) do
      begin
        Module := ModuleServices.Modules[I];
        if Assigned(Module) and SameText(Module.FileName, FileName) then
        begin
          Module.AddNotifier(TMyModuleNotifier.Create(Module, FWizard, FList, ' ModuleNotifier[' +  FileName + ']'));
          {$IFDEF DEBUGMESSAGES}
          MsgServices.AddToolMessage('','FileNotification added Notifier', 'Notifier',0,0);
          {$ENDIF DEBUGMESSAGES}
          for J := 0 to Pred(Module.GetModuleFileCount) do
          begin
            Editor := Module.GetModuleFileEditor(J);
            InfoStr := FileName;
            if Supports(Editor, IOTASourceEditor, Dummy) then
              InfoStr := InfoStr + ', IOTASourceEditor';
            if Supports(Editor, IOTAFormEditor, Dummy) then
              InfoStr := InfoStr + ', IOTAFormEditor';
            if Supports(Editor, IOTAProjectResource, Dummy) then
              InfoStr := InfoStr + ', IOTAProjectResource';
            if Supports(Editor, IOTATypeLibEditor, Dummy) then
              InfoStr := InfoStr + ', IOTATypeLibEditor';
            Editor.AddNotifier(TMyModuleNotifier.Create(Module, FWizard, FList, ' ModuleEditorNotifier[' +  InfoStr + ']'));
            {$IFDEF DEBUGMESSAGES}
            MsgServices.AddToolMessage('','FileNotification added EditorNotifier', 'Notifier',0,0);
            {$ENDIF DEBUGMESSAGES}
          end;
        end;
      end;
  end;
end;

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean);
begin

end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean);
begin
end;

procedure TIdeNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
begin
  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddToolMessage('','ViewNotification', 'Notifier',0,0);
  {$ENDIF DEBUGMESSAGES}
end;

procedure TIdeNotifier.ViewActivated(const View: IOTAEditView);
begin
  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddToolMessage('','ViewActivated', 'Notifier',0,0);
  {$ENDIF DEBUGMESSAGES}
end;

constructor TMyModuleNotifier.Create(AModule: IOTAModule; AWizard: TLiveBlameWizard; AList: TList; AInfo: string);
begin
  inherited Create;
  FList := AList;
  FInfo := AInfo;
  FWizard := AWizard;
  FModule := AModule;
end;

procedure TMyModuleNotifier.AfterSave;
begin
  ForceUpdate := True;
  //PostMessage(CurrentPanel.Handle, WM_BLAME_UPDATE, 0, 0);
  if Assigned(CurrentPanel) then
    CurrentPanel.PaintBoxPaint(nil);
end;

procedure TMyModuleNotifier.BeforeSave;
begin

end;

procedure TMyModuleNotifier.Destroyed;
begin

end;

procedure TMyModuleNotifier.Modified;
begin

end;

constructor TMyINTANotifier.Create(AWizard: TLiveBlameWizard; AList: TList);
begin
  inherited Create;
  FList := AList;
  FWizard := AWizard;
end;

procedure TMyINTANotifier.WindowShow(const EditWindow: INTAEditWindow; Show, LoadedFromDesktop: Boolean);
begin
  OutputDebugString('TMyINTANotifier.WindowShow');
end;

procedure TMyINTANotifier.WindowNotification(const EditWindow: INTAEditWindow; Operation: TOperation);
begin
  OutputDebugString('TMyINTANotifier.WindowNotification');
end;

procedure TMyINTANotifier.WindowActivated(const EditWindow: INTAEditWindow);
var
  I: Integer;
begin
  OutputDebugString(PChar(Format('TMyINTANotifier.WindowActivated', [])));
  if Assigned(FList) then
    for I := 0 to Pred(FList.Count) do
    begin
      if TLiveBlameEditorPanel(FList[I]).EditControlVisible then
        OutputDebugString(PChar(Format('  [%d] Visible', [I])))
      else
        OutputDebugString(PChar(Format('  [%d] not Visible!', [I])));      
    end;
end;

procedure TMyINTANotifier.WindowCommand(const EditWindow: INTAEditWindow; Command, Param: Integer; var Handled: Boolean);
begin
  OutputDebugString('TMyINTANotifier.WindowCommand');
end;

function CheckAddPanel(AWizard: TLiveBlameWizard; View: IOTAEditView; AList: TList): TLiveBlameEditorPanel; forward;
function GetEditViewModule(AEditView: IOTAEditView): IOTAModule; forward;

procedure TMyINTANotifier.EditorViewActivated(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
var
  I: Integer;
  EP: TLiveBlameEditorPanel;
begin
  OutputDebugString('TMyINTANotifier.EditorViewActivated');
  EP := CheckAddPanel(FWizard, EditView, FList);
  EP.ActiveEditView1 := EditView;
  EP.ActiveModule := GetEditViewModule(EditView);
  if Assigned(FList) then
    for I := 0 to Pred(FList.Count) do
    begin
      if TLiveBlameEditorPanel(FList[I]).EditControlVisible then
        OutputDebugString(PChar(Format('  [%d] Visible', [I])))
      else
        OutputDebugString(PChar(Format('  [%d] not Visible!', [I])));
      TLiveBlameEditorPanel(FList[I]).EnableCheckTimer;
    end;
end;

procedure TMyINTANotifier.EditorViewModified(const EditWindow: INTAEditWindow; const EditView: IOTAEditView);
begin
  OutputDebugString('TMyINTANotifier.EditorViewModified');
end;

procedure TMyINTANotifier.DockFormVisibleChanged(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  OutputDebugString('TMyINTANotifier.DockFormVisibleChanged');
end;

procedure TMyINTANotifier.DockFormUpdated(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
var
  I: Integer;
begin
  OutputDebugString('TMyINTANotifier.DockFormUpdated');
  if Assigned(FList) then
    for I := 0 to Pred(FList.Count) do
    begin
      if TLiveBlameEditorPanel(FList[I]).EditControlVisible then
        OutputDebugString(PChar(Format('  [%d] Visible', [I])))
      else
        OutputDebugString(PChar(Format('  [%d] not Visible!', [I])));
      TLiveBlameEditorPanel(FList[I]).EnableCheckTimer;
      PostMessage(TLiveBlameEditorPanel(FList[I]).Handle, WM_BLAME_SHOW, 0, 0);
    end;
end;

procedure TMyINTANotifier.DockFormRefresh(const EditWindow: INTAEditWindow; DockForm: TDockableForm);
begin
  OutputDebugString('TMyINTANotifier.DockFormRefresh');
end;

procedure AddBlameButton(EditorPanel: TLiveBlameEditorPanel);
var
  tempComponent, P, C, ImgList: TComponent;
  BlameButtonPanel: TPanel;
  SpeedButton: TSpeedButton;
  ActionLst: TActionList;
  Action: TAction;
  I, X: Integer;
begin
  tempComponent := Application.FindComponent('EditWindow_0');
  if Assigned(tempComponent) then
  begin
    P := tempComponent.FindComponent('StatusBar');
    X := 0;
    if Assigned(P) then
    begin
      {
      if (TStatusBar(P).Panels.Count > 3) and (TStatusBar(P).Panels[3].Width < 80) then
        TStatusBar(P).Panels[3].Width := TStatusBar(P).Panels[3].Width + 32;
      }
      if (TStatusBar(P).Panels.Count > 3) and (TStatusBar(P).Panels[3].Width < 80 + 84) then
        TStatusBar(P).Panels[3].Width := TStatusBar(P).Panels[3].Width + 32 + 84;//TODO:84?
      if (TStatusBar(P).Panels.Count > 3) then
      begin
        for I := 0 to 2 do
          X := X + TStatusBar(P).Panels[I].Width;
        X := X + 75;
        X := X + 84;//TODO:84?
      end;
    end;
    tempComponent := tempComponent.FindComponent('Panel2');
    if Assigned(tempComponent) then
    begin
      //{
      P := tempComponent.FindComponent('BlameButtonPanel');
      if not Assigned(P) then
      begin
        BlameButtonPanel := TPanel.Create(tempComponent);
        BlameButtonPanel.Align := alNone;
        BlameButtonPanel.Width := 32;
        BlameButtonPanel.Parent := TWinControl(tempComponent);
        BlameButtonPanel.Name := 'BlameButtonPanel';
        BlameButtonPanel.Left := X;
        BlameButtonPanel.Top := 1;
        BlameButtonPanel.Height := 20;
        BlameButtonPanel.Visible := True;
        BlameButtonPanel.Caption := '';
        BlameButtonPanel.BevelOuter := bvNone;
        BlameButtonPanel.ParentBackground := False;


        ImgList := nil;
        I := 0;
        while (I < Screen.FormCount) do
        begin
          C := Screen.Forms[I].FindComponent('FileHistoryFrame');
          if Assigned(C) then
          begin
            ImgList := C.FindComponent('ImageList1');
            Break;
          end;
          Inc(I);
        end;
        ActionLst := TActionList.Create(BlameButtonPanel);
        ActionLst.Images := TImageList(ImgList);
        Action := TAction.Create(BlameButtonPanel);
        Action.Name := 'BlameEditorAction';
        Action.ImageIndex := 16;
        Action.Caption := 'Show Blame';
        Action.Hint := 'Show Blame';
        Action.ActionList := ActionLst;
        Action.OnExecute := EditorPanel.ShowHidePanel;
        Action.AutoCheck := True;
        Action.GroupIndex := 123;

        SpeedButton := TSpeedButton.Create(BlameButtonPanel);
        SpeedButton.Name := 'BlameSpeedButton';
        SpeedButton.Parent := BlameButtonPanel;
        SpeedButton.Left := 0;
        SpeedButton.Top := 0;
        SpeedButton.Height := 20;
        SpeedButton.Width := 20;
        SpeedButton.Flat := True;
        SpeedButton.Action := Action;
        SpeedButton.Caption := '';
        SpeedButton.AllowAllUp := True;
        SpeedButton.OnClick := EditorPanel.ShowHidePanel;
        P := BlameButtonPanel;
      end
      else
      if not TPanel(P).Visible then
        TPanel(P).Visible := True
      else
        TPanel(P).BringToFront;

      P := P.FindComponent('BlameSpeedButton');
      if Assigned(P) then
      begin
        TSpeedButton(P).Down := EditorPanel.Visible or
          (Assigned(EditorPanel.FLiveBlameData) and EditorPanel.FLiveBlameData.FButtonDown);
        EditorPanel.FSpeedButton := TSpeedButton(P);
      end;
    end;
  end;
end;

function CheckAddPanel(AWizard: TLiveBlameWizard; View: IOTAEditView; AList: TList): TLiveBlameEditorPanel;
var
  EW: INTAEditWindow;
  EF: TCustomForm;
  I, Idx: Integer;
  EP: TLiveBlameEditorPanel;
  EFEP: TWinControl;
  OStr: string;
begin
  Result := nil;
  if Assigned(View) then
  begin
    EW := View.GetEditWindow;
    EF := EW.GetForm;
    if Assigned(EF) then
    begin
      OStr := OStr + ', ' + EF.Name;
      EP := nil;
      if Assigned(AList) and Assigned(EF) then
      begin
        Idx := -1;
        for I := 0 to Pred(AList.Count) do
          if Assigned(AList[I]) and (TLiveBlameEditorPanel(AList[I]).Owner = EF) then
          begin
            Idx := I;
            Break;
          end;
        if Idx = -1 then
        begin
          EP := TLiveBlameEditorPanel.Create(EF);
          AList.Add(EP);
          EP.Wizard := AWizard;
          EFEP := TWinControl(EF.FindComponent('EditorPanel'));
          for I := 0 to EF.ComponentCount - 1 do
            if (EF.Components[I] is TWinControl) and
              (EF.Components[I].ClassName = 'TEditControl') then
            begin
              EFEP := TWinControl(EF.Components[I]).Parent;
              OutputDebugString(PChar('Found TEditControl.Parent: ' + EFEP.ClassName + ' ' + EFEP.Name));
              Break;
            end;
          EP.Parent := EFEP;
        end
        else
          EP := TLiveBlameEditorPanel(AList[Idx]);
        Result := EP;
        //EP.ShowIfEditControl;
      end;
      if Assigned(EP) then
        AddBlameButton(EP);//TODO:?
    end;
  end;
end;

procedure TMyModuleNotifier.ViewNotification(const View: IOTAEditView; Operation: TOperation);
var
  OStr: string;
begin
  if Operation = opRemove then
    OStr := 'opRemove'
  else
    OStr := 'opInsert';

  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddToolMessage('','ViewNotification[' + OStr + ']' + FInfo, 'MyModuleNotifier',0,0);
  {$ENDIF DEBUGMESSAGES}
  OutputDebugString(PChar('ViewNotification[' + OStr + ']' + FInfo));
  {//TODO:?
  if Operation = opInsert then
    CheckAddPanel(FWizard, View, FList);
  }
end;

procedure TMyModuleNotifier.ViewActivated(const View: IOTAEditView);
var
  OStr: string;
  EW: INTAEditWindow;
  EP: TLiveBlameEditorPanel;
begin
  if Assigned(View) then
  begin
    EW := View.GetEditWindow;
    if Assigned(EW.GetForm) then
      OStr := OStr + ', ' + EW.GetForm.Name;
  end;
  {$IFDEF DEBUGMESSAGES}
  MsgServices.AddToolMessage('','ViewActivated[' + OStr + ']' + FInfo, 'MyModuleNotifier',0,0);
  {$ENDIF DEBUGMESSAGES}
  OutputDebugString(PChar('ViewActivated[' + OStr + ']' + FInfo));
  EP := CheckAddPanel(FWizard, View, FList);
  if Assigned(EP) then
  begin
    OutputDebugString(PChar(Format('ViewActivated EP.ActiveView=%p bf', [Pointer(View)])));
    EP.ActiveEditView1 := View;
    EP.ActiveModule := FModule;
    OutputDebugString(PChar(Format('ViewActivated EP.ActiveView=%p af', [Pointer(View)])));
  end
  else
    OutputDebugString(PChar('ViewActivated EP not assigned'));
end;

procedure TMyModuleNotifier.ModuleRenamed(const NewName: string);
begin
end;

function TMyModuleNotifier.CheckOverwrite: Boolean;
begin
  Result := True;
end;

const
  DiffFontColor = $CC9999;
  DiffBackGroundColor = $F4F4F4;
  DiffColorBarLineColor = clLime;

procedure TLiveBlameEditorPanel.OnTimer(Sender: TObject);
begin
  FPaintBox.Invalidate;
end;

function GetEditViewModule(AEditView: IOTAEditView): IOTAModule;
var
  ModuleServices: IOTAModuleServices;
  I, J, K: Integer;
  SourceEdit: IOTASourceEditor;
  Found: Boolean;
begin
  Result := nil;
  ModuleServices := BorlandIDEServices as IOTAModuleServices;
  if Assigned(ModuleServices) and Assigned(AEditView) then
  begin
    Found := False;
    for I := 0 to Pred(ModuleServices.ModuleCount) do
    begin
      if Supports(ModuleServices.Modules[I].CurrentEditor, IOTASourceEditor, SourceEdit) then
      begin
        for J := 0 to Pred(SourceEdit.EditViewCount) do
          if SourceEdit.EditViews[J] = AEditView then
          begin
            Found := True;
            Break;
          end;
      end;
      if not Found then
      begin
        for K := 0 to Pred(ModuleServices.Modules[I].GetModuleFileCount) do
        begin
          if Supports(ModuleServices.Modules[I].GetModuleFileEditor(K), IOTASourceEditor, SourceEdit) then
            for J := 0 to Pred(SourceEdit.EditViewCount) do
              if SourceEdit.EditViews[J] = AEditView then
              begin
                Found := True;
                Break;
              end;
          if Found then
            Break;
        end;
      end;
      if Supports(ModuleServices.Modules[I].CurrentEditor, IOTASourceEditor, SourceEdit) then
      begin
        for J := 0 to Pred(SourceEdit.EditViewCount) do
        begin
          if SourceEdit.EditViews[J].SameView(AEditView) then
          begin
            Found := True;
            Break;
          end;
        end;
      end;
      if not Found then
      begin
        for K := 0 to Pred(ModuleServices.Modules[I].GetModuleFileCount) do
        begin
          if Supports(ModuleServices.Modules[I].GetModuleFileEditor(K), IOTASourceEditor, SourceEdit) then
            for J := 0 to Pred(SourceEdit.EditViewCount) do
            begin
              if SourceEdit.EditViews[J].SameView(AEditView) then
              begin
                Found := True;
                Break;
              end;
            end;
          if Found then
            Break;
        end;
      end;

      if Found then
      begin
        Result := ModuleServices.Modules[I];
        Break;
      end;
    end;
  end;
end;

function GetEditViewFilename(AEditView: IOTAEditView): string;
var
  Module: IOTAModule;
begin
  Module := GetEditViewModule(AEditView);
  if Assigned(Module) then
    Result := ExtractFileName(Module.FileName)
  else
    Result := '';
end;

//------------------------------------------------------------------------------

const
  coreide = 'coreide150.bpl';
  SLineIsElidedName = '@Editorcontrol@TCustomEditControl@LineIsElided$qqri';

function LineIsElided(Self: TObject; LineNum: Integer): Boolean; external coreide name SLineIsElidedName;

//------------------------------------------------------------------------------

type
  TPaintBlock = class(TObject)
  private
    FStartRow: Integer;
    FEndRow: Integer;
    FViewStartRow: Integer;
    FViewEndRow: Integer;
  public
    constructor Create(AStartRow, AViewStartRow: Integer);
    property StartRow: Integer read FStartRow;
    property EndRow: Integer read FEndRow;
    property ViewStartRow: Integer read FViewStartRow;
    property ViewEndRow: Integer read FViewEndRow;
  end;

{ TPaintBlock }

constructor TPaintBlock.Create(AStartRow, AViewStartRow: Integer);
begin
  FStartRow := AStartRow;
  FViewStartRow := AViewStartRow;
  FEndRow := -1;
  FViewEndRow := -1;
end;

//------------------------------------------------------------------------------

procedure TLiveBlameEditorPanel.PaintBoxPaint(Sender: TObject);

var
  LH: Integer;

type
  TBlockType = (btRevision, btDate, btUser);

  procedure PaintBlocksX(ADestCanvas: TCanvas; ABlockType: TBlockType; ARectX1, ARectX2, ATextX: Integer; APaintBlock: TPaintBlock);

    function IsSameInfo(AInfo1, AInfo2: TJVCSLineHistoryRevision): Boolean;
    begin
      Result := AInfo1 = AInfo2;
      if ABlockType <> btRevision then
      begin
        if Assigned(AInfo1) and Assigned(AInfo2) then
        begin
          case ABlockType of
            btDate: Result := AInfo1.DateStr = AInfo2.DateStr;
            btUser: Result := AInfo1.UserStr = AInfo2.UserStr;
            else
              Result := False;
          end;
        end
        else
          Result := False;
      end;
    end;

    function GetColor(AInfo: TJVCSLineHistoryRevision): TColor;
    var
      RevisionColor: TRevisionColor;
    begin
      RevisionColor := DoGetRevisionColor(AInfo);
      case ABlockType of
        btRevision: Result := RevisionColor.RevisionColor;
        btDate: Result := RevisionColor.DateColor;
        btUser: Result := RevisionColor.UserColor;
        else
          Result := clBtnFace;
      end;
    end;

  var
    LastBlockRevision: TJVCSLineHistoryRevision;
    LastBlockStartY, LastBlockEndY{, LH}: Integer;

    procedure PaintLastBlock;
    var
      S, RevisionIDStr: string;
      RevisionRect, RevisionHintRect: TRect;
      RevisionTextExtent: TSize;
      OldFontColor: TColor;
      OldFontStyle: TFontStyles;
    begin
      if (LastBlockEndY > -1) and Assigned(LastBlockRevision) then
      begin
        ADestCanvas.Brush.Style := bsSolid;
        ADestCanvas.Brush.Color := GetColor(LastBlockRevision);
        ADestCanvas.Rectangle(ARectX1, LastBlockStartY, ARectX2, LastBlockEndY);
        if ATextX <> -1 then
        begin
          ADestCanvas.Brush.Style := bsClear;
          case ABlockType of
            btRevision: S := LastBlockRevision.RevisionStr;
            btDate: S := LastBlockRevision.DateStr;
            btUser: S := LastBlockRevision.UserStr;
            else
              S := '';
          end;
          if ABlockType = btRevision then
          begin
            OldFontColor := ADestCanvas.Font.Color;
            OldFontStyle := ADestCanvas.Font.Style;
            try
              if (LastBlockStartY + LastBlockEndY - LH) shr 1 = FHighlightY then
              begin
                ADestCanvas.Font.Color := clHighlight;
                ADestCanvas.Font.Style := [fsUnderline];
              end;
              ADestCanvas.TextOut(ATextX, (LastBlockStartY + LastBlockEndY - LH) shr 1, S);
              RevisionTextExtent := ADestCanvas.TextExtent(S);
              RevisionRect.Left := ATextX;
              RevisionRect.Right := RevisionRect.Left + RevisionTextExtent.cx;
              RevisionRect.Top := (LastBlockStartY + LastBlockEndY - LH) shr 1;
              RevisionRect.Bottom := RevisionRect.Top + RevisionTextExtent.cy;
              {
              if LastBlockRevision.RevisionID > 0 then
                RevisionIDStr := IntToStr(LastBlockRevision.RevisionID)
              else
                RevisionIDStr := LastBlockRevision.OrgRevisionStr;
              }
              RevisionIDStr := S;
              FRevisionRectangles.Add(RevisionRect, RevisionIDStr);
            finally
              ADestCanvas.Font.Color := OldFontColor;
              ADestCanvas.Font.Style := OldFontStyle;
            end;
          end
          else
            ADestCanvas.TextOut(ATextX, (LastBlockStartY + LastBlockEndY - LH) shr 1, S);
        end;
        //TODO: check if rectangle is added multiple times
        RevisionHintRect.Left := ARectX1;
        RevisionHintRect.Right := ARectX2;
        if FDateRectX2 > RevisionHintRect.Right then
          RevisionHintRect.Right := FDateRectX2;
        if FUserRectX2 > RevisionHintRect.Right then
          RevisionHintRect.Right := FUserRectX2;
        RevisionHintRect.Top := LastBlockStartY;
        RevisionHintRect.Bottom := LastBlockEndY;
        FRevisionHintRectangles.Add(RevisionHintRect, LastBlockRevision.RevisionStr);
      end;
    end;

  var
    Revision: TJVCSLineHistoryRevision;
    PaintLine, RealLine, Y: Integer;
  begin
    if (ARectX1 <> -1) or (ATextX <> -1) and (FLiveBlameData.FLines.Count > 0) then
    begin
      //LH := FSynEdit.LineHeight;
      LastBlockStartY := -1;
      LastBlockEndY := -1;
      LastBlockRevision := nil;
      for PaintLine := APaintBlock.FViewStartRow to APaintBlock.FViewEndRow do
      begin
        RealLine := PaintLine - APaintBlock.FViewStartRow + APaintBlock.FStartRow - 1;
        Y := LH * (PaintLine - 1);
        if RealLine < FLiveBlameData.FLines.Count then
        begin
          Revision := FLiveBlameData.FLines[RealLine];
          if not (IsSameInfo(LastBlockRevision, Revision) {and not IsDifferent(PaintLine)}) then//TODO:IsDifferent
          begin
            PaintLastBlock;
            LastBlockStartY := Y;
            LastBlockRevision := Revision;
          end;
          LastBlockEndY := Y + LH + 1;
        end;
      end;
      PaintLastBlock;
    end;
  end;

var
  I, EH, {LH, }LN: Integer;
  ModuleServices: IOTAModuleServices;
  Module: IOTAModule;
  CurrentEditor: IOTAEditor;
  SourceEdit: IOTASourceEditor;
  Canvas: TCanvas;
  Bitmap: TBitmap;
  EditControl: TObject;
  Elided: Boolean;
  LastRow, BottomRow: Integer;
  PaintBlocks: TObjectList<TPaintBlock>;
  PaintBlock: TPaintBlock;
  FileName: string;
begin
  if FPainting then
    Exit;
  FPainting := True;
  try
    CurrentPanel := Self;
    CheckInstallHook;
    ShowIfEditControl;
    if not Visible then
      Exit;
    try
      ModuleServices := BorlandIDEServices as IOTAModuleServices;
      LastRow := -1;
      BottomRow := -1;
      if Assigned(ModuleServices) then
      begin
        Module := ModuleServices.CurrentModule;
        FileName := Module.CurrentEditor.FileName;
        FLiveBlameData.Load;
        if Assigned(Module) then
        begin
          CurrentEditor := Module.CurrentEditor;
          if Supports(CurrentEditor, IOTASourceEditor, SourceEdit) and
            (SourceEdit.EditViewCount > 0) then
          begin
            for I := 0 to Pred(SourceEdit.EditViewCount) do
            if Assigned(SourceEdit.EditViews[I]) and Assigned(SourceEdit.EditViews[I].GetEditWindow) and
              (SourceEdit.EditViews[I].GetEditWindow.Form = Owner) then
            begin
              FCursorLine := SourceEdit.EditViews[I].CursorPos.Line - 1;
              FTopLine := SourceEdit.EditViews[I].TopRow - 1;
              BottomRow := SourceEdit.EditViews[I].BottomRow;
              LastRow := SourceEdit.EditViews[I].Position.LastRow;
              FCY := SourceEdit.EditViews[I].ViewSize.cy - 1;
            end;
            if FLiveBlameData.FBlameInfoAvailable then
              UpdateLineHistory(SourceEdit);
          end;
        end;
      end;
      Bitmap := TBitmap.Create;
      Bitmap.Width := FPaintBox.Width;
      Bitmap.Height := FPaintBox.Height;
      Canvas := Bitmap.Canvas;
      Canvas.Brush.Color := DiffBackGroundColor;
      Canvas.FillRect(Canvas.ClipRect);
      Canvas.Font.Color := DiffFontColor;
      Canvas.Pen.Width := 5;
      Canvas.Pen.Color := DiffColorBarLineColor;
      EH := Height - GetSystemMetrics(SM_CYHSCROLL);
      LH := EH div FCY;
      Canvas.Font.Name := 'Courier New';
      Canvas.Font.Size := 10;
      LN := FTopLine + 1;
      EditControl := GetEditControl;
      {//moved down
      if FBlameInfoReady then
      begin
        BuildLineHistory;
        UpdateGutterWidth;
        FBlameInfoReady := False;
        FBlameInfoAvailable := True;
      end;
      }
      if FLiveBlameData.FBlameInfoReady then
        FLiveBlameData.FStage := 3;
      if FLiveBlameData.FBlameInfoAvailable then
      begin
        if FSettings.DateFormat = 'AGE2' then
          FLiveBlameData.FBufferRevision.FDateStr := GetAge2Str(FLiveBlameData.FBufferRevision.FDate);
        if FSettings.DateFormat = 'AGE2' then
          FLiveBlameData.FFileRevision.FDateStr := GetAge2Str(FLiveBlameData.FFileRevision.FDate);
      end;

      FRevisionRectangles.Clear;
      FRevisionHintRectangles.Clear;
      PaintBlocks := TObjectList<TPaintBlock>.Create;
      try
        PaintBlock := nil;
        for I := FTopLine to BottomRow - 1 do
          if LN <= LastRow then
          begin
            Elided := LineIsElided(EditControl, LN + 1);
            if Elided or not Assigned(PaintBlock) then
              PaintBlock := PaintBlocks[PaintBlocks.Add(TPaintBlock.Create(LN, I - FTopLine + 1))];
            if Elided then
            begin
              PaintBlock.FViewEndRow := PaintBlock.ViewStartRow;
              while Elided do
              begin
                Inc(LN);
                Elided := LineIsElided(EditControl, LN);
                if Elided then
                  PaintBlock.FEndRow := LN;
              end;
              PaintBlock := nil;
            end
            else
            begin
              PaintBlock.FEndRow := LN;
              PaintBlock.FViewEndRow := I - FTopLine + 1;
              Inc(LN);
            end;
          end;
        if (PaintBlocks.Count > 0) and (PaintBlocks.Last.EndRow = -1) then
          PaintBlocks.Delete(PaintBlocks.Count - 1);
        if FLiveBlameData.FBlameInfoAvailable and (PaintBlocks.Count > 0) then
        begin
          Canvas.Brush.Color := clBtnFace;
          Canvas.Brush.Style := bsSolid;
          Canvas.FillRect(Rect(0, 0, Bitmap.Width, Bitmap.Height));
          Canvas.Brush.Style := bsClear;
          Canvas.Font.Color := clWindowText;
          Canvas.Pen.Width := 1;
          Canvas.Pen.Color := clSilver;
          Canvas.Pen.Style := psSolid;
          for I := 0 to PaintBlocks.Count - 1 do
            if (PaintBlocks[I].ViewStartRow <> PaintBlocks[I].ViewEndRow) or
              ((PaintBlocks[I].EndRow - PaintBlocks[I].StartRow) =
              (PaintBlocks[I].ViewStartRow - PaintBlocks[I].ViewEndRow)) then
            begin
              PaintBlocksX(Canvas, btRevision, FRevisionRectX1, FRevisionRectX2, FRevisionX, PaintBlocks[I]);
              PaintBlocksX(Canvas, btDate, FDateRectX1, FDateRectX2, FDateX, PaintBlocks[I]);
              PaintBlocksX(Canvas, btUser, FUserRectX1, FUserRectX2, FUserX, PaintBlocks[I]);
            end
            else
            begin
              Canvas.TextOut(2, LH * PaintBlocks[I].ViewStartRow - LH, Format('... %d lines hidden', [PaintBlocks[I].EndRow - PaintBlocks[I].StartRow]));
            end;
        end;
        if not FLiveBlameData.FBlameInfoAvailable then
        begin
          if FLiveBlameData.FStage = 1 then
            Canvas.TextOut(6, 0, 'Loading History...')
          else
          if FLiveBlameData.FStage = 2 then
            Canvas.TextOut(6, 0, 'Loading Blame..')
          else
          if FLiveBlameData.FStage = 3 then
            Canvas.TextOut(6, 0, 'Prepare View..')
          else
            Canvas.TextOut(6, 0, 'Loading...');
          if FLiveBlameData.FBlameCounter > 0 then
            Canvas.TextOut(6, LH * 2, FormatFloat('#,#', FLiveBlameData.FBlameCounter) + ' Bytes received');
          if (FLiveBlameData.FBlameRevision <> -1) and (FLiveBlameData.FMaxRevision <> 0) then
          begin
            if not FProgressBar.Visible and (FLiveBlameData.FMaxRevision > 0) then
            begin
              FProgressBar.Position := 0;
              FProgressBar.Max := FLiveBlameData.FMaxRevision + FLiveBlameData.FMaxRevision div 5;
              FProgressBar.Top := LH * 3;
              FProgressBar.Left := 6;
              FProgressBar.Width := Self.Width - 12;
              FProgressBar.Visible := True;
            end;
            FProgressBar.Position := FLiveBlameData.FBlameRevision + FLiveBlameData.FMaxRevision div 10;
          end;
        end
        else
          FProgressBar.Visible := False;
      finally
        PaintBlocks.Free;
      end;
      FPaintBox.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height), Bitmap.Canvas, Rect(0, 0, Bitmap.Width, Bitmap.Height));
      Application.ProcessMessages;
      if FLiveBlameData.FBlameInfoReady then
      begin
        BuildLineHistory;
        UpdateGutterWidth;
        FLiveBlameData.FBlameInfoReady := False;
        FLiveBlameData.FBlameInfoAvailable := True;
        FPaintBox.Invalidate;
      end;
      Bitmap.Free;
    except
      OutputDebugString('Exception in PaintBoxPaint');
    end;
  finally
    FPainting := False;
  end;
end;

{ TLiveBlameWizard }

function TLiveBlameWizard.GetIDString: string;
begin
  Result := 'USc.Live Blame';
end;

function TLiveBlameWizard.GetName: string;
begin
  Result := 'Live Blame';
end;

procedure TLiveBlameWizard.AfterSave;
begin
end;

procedure TLiveBlameWizard.BeforeSave;
begin
end;

procedure TLiveBlameWizard.Destroyed;
begin
end;

procedure TLiveBlameWizard.Execute;
begin
end;

function TLiveBlameWizard.GetState: TWizardState;
begin
  Result := [];
end;

procedure TLiveBlameWizard.Modified;
begin
end;

procedure TLiveBlameWizard.ViewNotification(const View: IOTAEditView; Operation: TOperation); //IOTAEditorNotifier
begin
  if Operation = opInsert then
    OutputDebugString('TLiveBlameWizard.ViewActivated inserted')
  else
    OutputDebugString('TLiveBlameWizard.ViewActivated removed');
end;

procedure TLiveBlameWizard.ViewActivated(const View: IOTAEditView); //IOTAEditorNotifier
begin
  OutputDebugString('TLiveBlameWizard.ViewActivated');
end;

constructor TLiveBlameWizard.Create;
var
  Services: IOTAServices;
  EditorServices: IOTAEditorServices;
begin
  inherited;
  FPanelList := TList.Create;
  EditorServices := BorlandIDEServices as IOTAEditorServices;
  NotifierIndex := EditorServices.AddNotifier(TMyINTANotifier.Create(Self, FPanelList));

  Services := BorlandIDEServices as IOTAServices;
  Services.AddNotifier(TIdeNotifier.Create(Self, FPanelList));//TODO:?
end;

destructor TLiveBlameWizard.Destroy;
var
  EditorServices: IOTAEditorServices;
  I: Integer;
begin
  if NotifierIndex <> -1 then
  begin
    EditorServices := BorlandIDEServices as IOTAEditorServices;
    EditorServices.RemoveNotifier(NotifierIndex);
  end;
  for I := Pred(FPanelList.Count) downto 0 do
    TObject(FPanelList[I]).Free;
  FPanelList.Free;
  inherited Destroy;
end;

procedure TLiveBlameWizard.RemovePanel(AObject: TObject);
var
  I: Integer;
begin
  OutputDebugString('TLiveBlameWizard.RemovePanel');
  for I := Pred(FPanelList.Count) downto 0 do
    if FPanelList[I] = AObject then
    begin
      FPanelList.Delete(I);
      Break;
    end;
end;

end.
