unit cvEasyUI;
(*
  Libray for building html pages

  Copyright (C) 2014 Costas Velissariou
  velissariouc@gmail.com

  See the file LICENSE.txt, included in this distribution,
  for details about the copyright.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, cvElement;
type

{ TeasyuiRegion }
TeasyuiRegionDataOptions = record
  title: string;   // The layout panel title text.
  region: string;  // Defines the layout panel position, the value is one of
                   // following: north, south, east, west, center
  border: boolean; //	True to show layout panel border.
  split: boolean;  //	True to show a split bar which user can change the panel size.
  iconCls: string; //	An icon CSS class to show a icon on panel header.
  href: string;    //	An URL to load data from remote site.
  collapsible: boolean; // Defines if to show collapsible button.
  minWidth: integer; // 	The minimum panel width. 	10
  minHeight: integer; // 	The minimum panel height. 	10
  maxWidth: integer; // 	The maximum panel width. 	10000
  maxHeight: integer; // 	The maximum panel height.       10000
end;
TeasyuiRegion = class( TDiv)
protected
  function GetExtraAttributes: string; override;
public
  //options: TeasyuiRegionDataOptions;
  //title: string;   // The layout panel title text.
  region: string;  // Defines the layout panel position, the value is one of
                   // following: north, south, east, west, center
  border: boolean; //	True to show layout panel border.
  split: boolean;  //	True to show a split bar which user can change the panel size.
  iconCls: string; //	An icon CSS class to show a icon on panel header.
  href: string;    //	An URL to load data from remote site.
  collapsible: boolean; // Defines if to show collapsible button.
  minWidth: integer; // 	The minimum panel width. 	10
  minHeight: integer; // 	The minimum panel height. 	10
  maxWidth: integer; // 	The maximum panel width. 	10000
  maxHeight: integer; // 	The maximum panel height.       10000
  constructor create( aParent: TElement; aRegion: string);
end;



{ TeasyuiLayout }

TeasyuiLayout = class( TDiv)
protected
  function GetExtraAttributes: string; override;
public
  north, south, east, west, center: TeasyuiRegion;
  fit: boolean; //Set to true to set the layout size fit its parent container.
                //When creating layout on 'body' tag, it will be auto maximized
                //to the full size of whole page.
  constructor create( aParent: TElement);
end;



{ TeasyuiTabPanel }

TeasyuiTabPanel = class( TDiv)
protected
  function GetExtraAttributes: string; override;
public
  closable: boolean; //When set to true, the tab panel will show a closable button which can click to close the tab panel. 	false
  selected: boolean; //When set to true, tab tab panel will be selected. 	false
constructor create( aParent: TElement);
end;

{ TeasyuiTabs }

TeasyuiTabs = class( TDiv)
protected
  function GetExtraAttributes: string; override;
public
  fit: boolean;
  constructor create( aParent: TElement);
end;

TeasyUI = class
  Doc: THtml;
  procedure OnBeforeHtml;
  constructor create( aDoc: THtml);
end;


implementation

{ TeasyUI }

procedure TeasyUI.OnBeforeHtml();
begin
  doc.head.SetCssLink('EasyUI-Themes', '/jquery-easyui-1.4.1/themes/default/easyui.css');
  doc.head.SetCssLink('EasyUI-Themes-Icon', '/jquery-easyui-1.4.1/themes/icon.css');
  doc.head.AddCssLink('/jquery-easyui-1.4.1/demo/demo.css');
  doc.head.SetJScriptSrc('JQuery', '/jquery-easyui-1.4.1/jquery.min.js');
  doc.head.SetJScriptSrc('JQuery-EasyUI', '/jquery-easyui-1.4.1/jquery.easyui.min.js');

end;

constructor TeasyUI.create(aDoc: THtml);
begin
  Doc := aDoc;
  Doc.UseLibrary( @OnBeforeHtml);
end;

{ TeasyuiLayout }

function TeasyuiLayout.GetExtraAttributes: string;
var
  s: string;
begin
  s:='';

  if fit <> false then
    s := s + 'fit:''true''';

  if s <> '' then
    s := ' data-options="' + s + '" ';
  result := s;
end;

constructor TeasyuiLayout.create(aParent: TElement);
begin
  inherited create( aParent, 'easyui-layout');
  fit:=false;
  north := TeasyuiRegion.create( self, 'north');
  south := TeasyuiRegion.create( self, 'south');
  east := TeasyuiRegion.create( self, 'east');
  west := TeasyuiRegion.create( self, 'west');
  center := TeasyuiRegion.create( self, 'center');
end;

{ TeasyuiTabPanel }

function TeasyuiTabPanel.GetExtraAttributes: string;
var
  s: string;
begin
  s:='';

  if closable <> false then
    s := s + 'closable:''true''';
  if selected <> false then
    s := s + 'selected:''true''';

  if s <> '' then
    s := ' data-options="' + s + '" ';
  result := s;
end;

constructor TeasyuiTabPanel.create(aParent: TElement);
begin
  inherited create( aParent);
  closable:=false;
  selected:=false;
end;

{ TeasyuiTabs }

function TeasyuiTabs.GetExtraAttributes: string;
var
  s: string;
begin
  s:='';

  if fit <> false then
    s := s + 'fit:''true''';

  if s <> '' then
    s := ' data-options="' + s + '" ';
  result := s;
end;

constructor TeasyuiTabs.create(aParent: TElement);
begin
  inherited create( aParent, 'easyui-tabs');
  fit:=false;

end;


{ TeasyuiRegion }

function TeasyuiRegion.GetExtraAttributes: string;
var
  s: string;
begin
  s := 'region:''' + region +'''';

  //if title <> '' then
  //  s := s + ',title:''' + title + '''';

  if iconCls <> '' then
    s := s + ',iconCls:''' + iconCls + '''';

  if href <> '' then
    s := s + ',href:''' + href + '''';

  if border <> true then
    s := s + ',border:''false''';

  if split <> false then
    s := s + ',split:''true''';

  if collapsible <> true then
    s := s + ',collapsible:''false''';

  if minWidth <> 10 then
    s := s + ',minWidth:'+ IntToStr( minWidth);

  if minHeight <> 10 then
    s := s + ',minHeight:'+ IntToStr( minHeight);

  if maxWidth <> 10000 then
    s := s + ',maxWidth:'+ IntToStr( maxWidth);

  if maxHeight <> 10000 then
    s := s + ',maxHeight:'+ IntToStr( maxHeight);


  s := ' data-options="' + s + '" ';
  result := s;
end;

constructor TeasyuiRegion.create( aParent: TElement; aRegion: string);
begin
  inherited create( aParent);
  region := aRegion;
  title := '';
  border := True;
  split := false;
  iconCls := '';
  href := '';
  collapsible := true;
  minWidth := 10;
  minHeight := 10;
  maxWidth := 10000;
  maxHeight := 10000;
end;


initialization

end.

