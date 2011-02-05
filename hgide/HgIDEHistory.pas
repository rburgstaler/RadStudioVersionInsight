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
{ The Original Code is HgIDEHistory.pas.                                       }
{                                                                              }
{ The Initial Developer of the Original Code is Uwe Schuster.                  }
{ Portions created by Uwe Schuster are Copyright © 2010 Uwe Schuster. All      }
{ Rights Reserved.                                                             }
{                                                                              }
{ Contributors:                                                                }
{ Uwe Schuster (uschuster)                                                     }
{                                                                              }
{******************************************************************************}

unit HgIDEHistory;

interface

uses
  Classes, FileHistoryAPI, HgClient, HgIDEClient;

type
  TDispInterfacedObject = class(TInterfacedObject, IDispatch)
  protected
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;

  THgFileHistoryProvider = class(TDispInterfacedObject, IOTAFileHistoryProvider)
  private
    FClient: THgClient;
    FItems: TStringList;
    FHgIDEClient: THgIDEClient;

    procedure ClearItems;
    function CheckHgInitalize: Boolean;

    { IOTAFileHistoryProvider }
    function Get_Ident: WideString; safecall;
    function Get_Name: WideString; safecall;
    function GetFileHistory(const AFileName: WideString): IOTAFileHistory; safecall;
  public
    constructor Create(HgIDEClient: THgIDEClient);
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

  IHgFileHistory = interface(IOTAFileHistory)
    ['{F9294E84-FC13-4B24-9596-7250135CBDA8}']
    function GetItem: THgItem; safecall;
    property Item: THgItem read GetItem;
  end;

implementation

uses
  ComObj, ActiveX, SysUtils, Forms, Windows, ExtCtrls;

const
  HgFileHistoryProvider = 'VersionInsight.HgFileHistoryProvider';  //Do not internationalize

type
  THgFileHistory = class(TDispInterfacedObject, IOTAFileHistory, IHgFileHistory,
    IOTAFileHistoryHint)
  private
    FItem: THgItem;

    { IOTAFileHistory }
    function Get_Count: Integer; safecall;
    function GetAuthor(Index: Integer): WideString; safecall;
    function GetComment(Index: Integer): WideString; safecall;
    function GetContent(Index: Integer): IStream; safecall;
    function GetDate(Index: Integer): TDateTime; safecall;
    function GetIdent(Index: Integer): WideString; safecall;
    function GetHistoryStyle(Index: Integer): TOTAHistoryStyle; safecall;
    function GetLabelCount(Index: Integer): Integer; safecall;
    function GetLabels(Index, LabelIndex: Integer): WideString; safecall;

    { IHgFileHistory }
    function GetItem: THgItem; safecall;

    { IOTAFileHistoryHint }
    function GetHintStr(Index: Integer): string;
  public
    constructor Create(AItem: THgItem);
    destructor Destroy; override;

    function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
  end;

{ TDispInterfacedObject }

function TDispInterfacedObject.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDispInterfacedObject.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Result := E_NOTIMPL;
end;

function TDispInterfacedObject.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Result := S_OK;
  Count := 0;
end;

function TDispInterfacedObject.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
  Result := E_NOTIMPL;
end;

{ THgFileHistoryProvider }

function THgFileHistoryProvider.CheckHgInitalize: Boolean;
begin
  try
    Result := FHgIDEClient.HgInitialize;
    if Result then
      FClient := FHgIDEClient.HgClient;
  except
    Result := False;
  end;
end;

procedure THgFileHistoryProvider.ClearItems;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems.Objects[I].Free;
  FItems.Clear;
end;

constructor THgFileHistoryProvider.Create(HgIDEClient: THgIDEClient);
begin
  inherited Create;
  FClient := nil;
  FHgIDEClient := HgIDEClient;
  FItems := TStringList.Create;
  FItems.CaseSensitive := False;
  FItems.Duplicates := dupError;
  FItems.Sorted := True;
end;

destructor THgFileHistoryProvider.Destroy;
begin
  ClearItems;
  FItems.Free;
  FClient := nil;
  inherited;
end;

function THgFileHistoryProvider.GetFileHistory(
  const AFileName: WideString): IOTAFileHistory;
var
  Index: Integer;
  Item: THgItem;
begin
  Result := nil;
  if not CheckHgInitalize then
    Exit;

  if not FClient.IsVersioned(AFileName) then
    Exit;

  if FItems.Find(AFileName, Index) then
  begin
    Item := THgItem(FItems.Objects[Index]);
  end
  else
  begin
    Item := THgItem.Create(FClient, AFileName);
    try
      Item.LoadHistory;
      FItems.AddObject({Item.PathName}AFileName, Item);
    except
      Item.Free;
      raise;
    end;
  end;
  Result := THgFileHistory.Create(Item);
end;

function THgFileHistoryProvider.Get_Ident: WideString;
begin
  Result := HgFileHistoryProvider;
end;

function THgFileHistoryProvider.Get_Name: WideString;
begin
  Result := 'Hg history provider'; // Do not internationalize
end;

function THgFileHistoryProvider.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistoryProvider, '', '');
end;

{ THgFileHistory }

constructor THgFileHistory.Create(AItem: THgItem);
begin
  inherited Create;
  FItem := AItem;
end;

destructor THgFileHistory.Destroy;
begin
//  FItem.Tag := 1;
  inherited;
end;

function THgFileHistory.GetAuthor(Index: Integer): WideString;
begin
  Result := THgHistoryItem(FItem.HistoryItems[Index]).Author;
end;

function THgFileHistory.GetComment(Index: Integer): WideString;
begin
  Result := THgHistoryItem(FItem.HistoryItems[Index]).Subject + #13#10 +
    THgHistoryItem(FItem.HistoryItems[Index]).Body;
end;

function THgFileHistory.GetContent(Index: Integer): IStream;
var
  Item: THgHistoryItem;
begin
  Item := FItem.HistoryItems[Index];
  Result := TStreamAdapter.Create(TStringStream.Create(Item.GetFile), soOwned);
end;

function THgFileHistory.GetDate(Index: Integer): TDateTime;
begin
  Result := THgHistoryItem(FItem.HistoryItems[Index]).Date;
end;

function THgFileHistory.GetHintStr(Index: Integer): string;
begin
  Result := '';//FItem.HintStrings[Index];
end;

function THgFileHistory.GetHistoryStyle(Index: Integer): TOTAHistoryStyle;
{
var
  Item: THgHistoryItem;
}
begin
  {
  Item := FItem.HistoryItems[Index];

  if Item.Revision = Item.Owner.CommittedRevision then
    Result := hsActiveRevision
  else
  }
    Result := hsRemoteRevision;
end;

function THgFileHistory.GetIdent(Index: Integer): WideString;
begin
  Result := IntToStr(FItem.HistoryItems[Index].ChangeSetID);
end;

function THgFileHistory.GetItem: THgItem;
begin
  Result := FItem;
end;

function THgFileHistory.GetLabelCount(Index: Integer): Integer;
begin
  Result := 1;
end;

function THgFileHistory.GetLabels(Index, LabelIndex: Integer): WideString;
begin
  case LabelIndex of
    0: Result := FItem.HistoryItems[Index].ChangeSet;
  else
    Result := '';
  end;
end;

function THgFileHistory.Get_Count: Integer;
begin
  Result := FItem.HistoryCount;
end;

function THgFileHistory.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, IOTAFileHistory, '', '');
end;

initialization

end.
